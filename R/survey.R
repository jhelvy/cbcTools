#' Create a survey from a conjoint design
#'
#' This function takes a conjoint design and replicates it across a specified
#' number of respondents to create a complete survey design.
#'
#' @param design A `cbc_design` object created by `cbc_design()`. If the
#'   `"random"` method was used to create the design, then the design will
#'   be ignored and a fully randomized survey will be generated where each
#'   respondent sees a unique, random design.
#' @param n_resp Number of survey respondents
#' @param randomize_questions Logical. Whether to randomize question order for each respondent
#' @param randomize_alts Logical. Whether to randomize alternative order within questions
#' @return A data frame with class `cbc_survey` containing the complete survey design
#' @export
#' @examples
#' # Create profiles and design
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C")
#' )
#'
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # Create survey for 100 respondents
#' survey <- cbc_survey(
#'   design = design,
#'   n_resp = 100
#' )
cbc_survey <- function(
    design,
    n_resp,
    randomize_questions = TRUE,
    randomize_alts = TRUE
) {
  # Validate input class
  if (!inherits(design, "cbc_design")) {
    stop("design must be a cbc_design object created by cbc_design()")
  }

  # Extract the design components
  design_df <- design$design
  n_blocks <- design$n_blocks
  n_q <- design$n_q
  n_alts <- design$n_alts
  no_choice <- design$no_choice
  label <- design$label
  method <- design$method
  profiles <- design$profiles

  # Check if we have enough blocks for the requested respondents
  if (n_blocks > 1 && n_resp < n_blocks) {
    warning(sprintf(
      "Design has %d blocks but only %d respondents requested. Some blocks will not be used.",
      n_blocks, n_resp
    ))
  }

  # Create the survey
  if (method == 'random') {
    # For random designs, create fresh random survey for each respondent
    survey <- make_random_survey(
      profiles, n_blocks, n_resp, n_alts, n_q, label
    )
    if (no_choice) {
      survey <- add_no_choice(survey, n_alts)
    }
  } else {
    # For other designs, replicate the design across respondents
    survey <- repeat_sets(
      design_df, n_resp, n_alts, n_q, n_blocks,
      randomize_questions, randomize_alts
    )
  }

  # Calculate survey statistics
  survey_stats <- calculate_survey_stats(survey, design, n_resp)

  # Create survey metadata
  survey_info <- list(
    n_resp = n_resp,
    created_at = Sys.time(),
    total_questions = n_resp * n_q,
    total_observations = n_resp * n_q,
    total_alternatives = nrow(survey),
    randomize_questions = randomize_questions,
    randomize_alts = randomize_alts,
    design_method = method,
    blocks_per_respondent = if (n_blocks > 1) ceiling(n_resp / n_blocks) else 1,
    stats = survey_stats
  )

  # Store references to original objects
  attr(survey, "design_ref") <- design
  attr(survey, "survey_info") <- survey_info

  class(survey) <- c("cbc_survey", "data.frame")
  return(survey)
}

# Helper functions

calculate_survey_stats <- function(survey, design, n_resp) {
  # Profile usage statistics
  profile_usage <- table(survey$profileID)
  unique_profiles_used <- length(unique(survey$profileID))
  total_profiles_available <- nrow(design$profiles)

  stats <- list(
    unique_profiles_used = unique_profiles_used,
    total_profiles_available = total_profiles_available,
    profile_usage_rate = unique_profiles_used / total_profiles_available,
    avg_profile_repetitions = mean(profile_usage),
    min_profile_repetitions = min(profile_usage),
    max_profile_repetitions = max(profile_usage)
  )

  # Add efficiency metrics if available from design
  if (!is.null(design$design_info$efficiency)) {
    eff <- design$design_info$efficiency
    stats$balance_score <- eff$balance_score
    stats$overlap_score <- eff$overlap_score
  }

  return(stats)
}

repeat_sets <- function(
    choice_sets,
    n_resp,
    n_alts,
    n_q,
    n_blocks,
    randomize_questions,
    randomize_alts
) {
  # Repeat choice sets to match number of respondents
  if (n_blocks > 1) {
    choice_sets <- split(choice_sets, choice_sets$blockID)
    n_resp_block <- ceiling(n_resp / n_blocks)
    n_reps <- ceiling(n_resp_block / (nrow(choice_sets[[1]]) / n_alts / n_q))
    design <- list()
    for (i in seq_len(n_blocks)) {
      set <- choice_sets[[i]]
      temp <- set[rep(seq_len(nrow(set)), n_reps), ]
      design[[i]] <- temp[1:(n_resp_block*n_q*n_alts), ]
    }
    design <- do.call(rbind, design)
  } else {
    design <- choice_sets[rep(seq_len(nrow(choice_sets)), n_resp), ]
  }
  design <- design[1:(n_resp*n_q*n_alts), ]
  design <- add_metadata(design, n_resp, n_alts, n_q)

  # Randomize question and/or alternative order if requested
  if (randomize_questions | randomize_alts) {
    design <- randomize_design(
      design, n_resp, n_alts, n_q, n_blocks,
      randomize_questions, randomize_alts
    )
  }

  return(design)
}

randomize_design <- function(
    design, n_resp, n_alts, n_q, n_blocks,
    randomize_questions, randomize_alts
) {
  # Split design by respondent
  resp_designs <- split(design, design$respID)

  # For each respondent
  for (r in seq_along(resp_designs)) {
    resp_design <- resp_designs[[r]]

    # Randomize question order if requested
    if (randomize_questions) {
      new_q_order <- sample(1:n_q)

      # Create mapping from old to new question order
      q_map <- data.frame(
        old_qID = seq(n_q),
        new_qID = new_q_order
      )

      # Update question IDs
      resp_design$qID <- q_map$new_qID[match(resp_design$qID, q_map$old_qID)]
    }

    # Randomize alternative order if requested
    if (randomize_alts) {
      for (q in 1:n_q) {
        q_rows <- which(resp_design$qID == q)
        new_alt_order <- sample(1:n_alts)
        resp_design$altID[q_rows] <- new_alt_order
      }
    }

    resp_designs[[r]] <- resp_design
  }

  # Recombine designs and update obsID
  design <- do.call(rbind, resp_designs)
  design <- design[order(design$respID, design$qID, design$altID), ]
  design$obsID <- rep(seq(n_resp * n_q), each = n_alts)
  row.names(design) <- NULL

  return(design)
}

# Helper functions from design.R (need to be available)
add_metadata <- function(design, n_resp, n_alts, n_q) {
  design$respID <- rep(seq(n_resp), each = n_alts * n_q)
  design$qID    <- rep(seq(n_q), each = n_alts)
  design$altID  <- rep(seq(n_alts), n_resp * n_q)
  design$obsID  <- rep(seq(n_resp * n_q), each = n_alts)
  return(design)
}

add_no_choice <- function(design, n_alts) {
  # Must dummy code categorical variables to include an outside good
  design <- dummy_code(design)
  # Create outside good rows
  design_og <- design[which(design$altID == 1), ]
  design_og[,!names(design_og) %in% c("respID", "qID", "altID", "obsID")] <- 0
  design_og$altID <- n_alts + 1
  design_og$no_choice <- 1
  # Insert outside good rows into design
  design$no_choice <- 0
  design <- rbind(design, design_og)
  design <- design[order(design$obsID), ]
  return(design)
}

dummy_code <- function(design) {
  types <- get_col_types(design)
  nonnumeric <- names(types[!types %in% c("integer", "numeric")])
  if (length(nonnumeric) > 0) {
    design <- fastDummies::dummy_cols(design, nonnumeric)
    design[, nonnumeric] <- NULL
  }
  return(design)
}

get_col_types <- function(data) {
  types <- lapply(data, class)
  test <- function(x) { x[1] }
  return(unlist(lapply(types, test)))
}

make_random_survey <- function(
    profiles, n_blocks, n_resp, n_alts, n_q, label
) {
  if (is.null(label)) {
    survey <- survey_rand_sample(profiles, n_resp, n_alts, n_q)
  } else {
    survey <- survey_rand_sample_label(profiles, n_resp, n_alts, n_q, label)
  }
  survey <- set_block_ids(survey, n_blocks)
  survey <- reorder_cols(survey)
  row.names(survey) <- NULL
  return(survey)
}

survey_rand_sample <- function(profiles, n_resp, n_alts, n_q) {
  survey <- sample_profiles(profiles, size = n_resp * n_alts * n_q)
  survey <- add_metadata(survey, n_resp, n_alts, n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  dup_rows_obs <- get_dup_obs(survey, n_alts)
  dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    dup_rows <- unique(c(dup_rows_obs, dup_rows_resp))
    new_rows <- sample_profiles(profiles, size = length(dup_rows))
    survey[dup_rows, 1:ncol(new_rows)] <- new_rows
    # Recalculate duplicates
    dup_rows_obs <- get_dup_obs(survey, n_alts)
    dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  }
  return(survey)
}

survey_rand_sample_label <- function(profiles, n_resp, n_alts, n_q, label) {
  n_alts <- override_label_alts(profiles, label, n_alts)
  # Randomize rows by label
  labels <- split(profiles, profiles[label])
  survey <- sample_profiles_by_group(labels, size = n_resp * n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  survey <- add_metadata(survey, n_resp, n_alts, n_q)
  dup_rows_obs <- get_dup_obs(survey, n_alts)
  dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    dup_rows <- unique(c(dup_rows_obs, dup_rows_resp))
    new_rows <- sample_profiles_by_group(labels, size = length(dup_rows) / n_alts)
    survey[dup_rows, 1:ncol(new_rows)] <- new_rows
    # Recalculate duplicates
    dup_rows_obs <- get_dup_obs(survey, n_alts)
    dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  }
  return(survey)
}

sample_profiles <- function(profiles, size) {
  return(profiles[sample(
    x = seq_len(nrow(profiles)), size = size, replace = TRUE), ]
  )
}

sample_profiles_by_group <- function(labels, size) {
  design <- lapply(labels, function(x) sample_profiles(x, size = size))
  design <- lapply(design, function(x) add_label_id(x))
  design <- do.call(rbind, design)
  design <- design[order(design$labelID), ]
  design$labelID <- NULL
  return(design)
}

add_label_id <- function(design) {
  design$labelID <- seq(nrow(design))
  return(design)
}

override_label_alts <- function(profiles, label, n_alts) {
  n_levels <- length(unique(profiles[, label]))
  if (n_levels != n_alts) {
    warning(
      "The supplied 'n_alts' argument is being ignored and set to ", n_levels,
      " to match the number of unique levels in the ", label,
      " variable.\n"
    )
    # Over-ride user-provided n_alts as it is determined by the label
    n_alts <- n_levels
  }
  return(n_alts)
}

get_dup_obs <- function(design, n_alts) {
  # Identify duplicate profiles for each observation (each choice set)
  counts <- tapply(
    design$profileID, design$obsID,
    FUN = function(x) length(unique(x))
  )
  dup_ids <- which(counts != n_alts)
  dup_rows <- which(design$obsID %in% dup_ids)
  return(dup_rows)
}

get_dup_resp <- function(design, n_resp, n_q) {
  # Identify duplicate choice sets for each respondent
  dup_ids <- unlist(lapply(
    1:n_resp,
    function(x) dup_obs_by_resp(design[which(design$respID == x),])
  ))
  dup_rows <- which(design$obsID %in% dup_ids)
  return(dup_rows)
}

dup_obs_by_resp <- function(df) {
  profiles_list <- tapply(
    df$profileID, df$obsID,
    FUN = function(x) sort(x)
  )
  # Convert vector list to a data frame to check for duplicates
  dupe_df <- do.call(rbind, profiles_list)
  dup_ids <- which(duplicated(dupe_df))
  if (length(dup_ids) > 0) {
    return(as.numeric(names(dup_ids)))
  }
  return(NULL)
}

set_block_ids <- function(design, n_blocks) {
  if (n_blocks > 1) {
    design$blockID <- rep(seq(n_blocks), each = nrow(design) / n_blocks)
  } else {
    design$blockID <- 1
  }
  return(design)
}

reorder_cols <- function(design) {
  design <- as.data.frame(design)[, c(get_id_names(), get_var_names(design))]
  return(design)
}


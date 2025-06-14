#' Create a survey from profiles or design
#'
#' This function creates a complete survey design either by:
#' 1. Taking profiles and creating both the experimental design and survey, or
#' 2. Taking an existing design and replicating it across respondents
#'
#' @param x Either a `cbc_profiles` object created by `cbc_profiles()`, or a
#'   `cbc_design` object created by `cbc_design()`.
#' @param n_resp Number of survey respondents
#' @param method Choose the design method (only when `x` is profiles).
#'   Options: `"sequential"` for D-efficient designs, or `"random"` for random surveys.
#' @param n_alts Number of alternatives per choice question (only when `x` is profiles).
#' @param n_q Number of questions per respondent (only when `x` is profiles).
#' @param priors A `cbc_priors` object (only when `x` is profiles for non-random methods).
#' @param randomize_questions Randomize question order for each respondent? Defaults to `TRUE`.
#' @param randomize_alts Randomize alternative order within questions? Defaults to `TRUE`.
#' @param ... Additional arguments:
#'   - When `x` is profiles: passed to `cbc_design()` for design creation
#'   - When `x` is design: currently unused
#'
#' @details
#' **Two workflows supported:**
#'
#' **Workflow 1: Profiles → Survey (integrated)**
#' ```r
#' survey <- cbc_survey(
#'   profiles,
#'   n_resp = 100,
#'   method = "sequential",
#'   n_alts = 3,
#'   n_q = 6,
#'   priors = priors
#' )
#' ```
#'
#' **Workflow 2: Design → Survey (two-step)**
#' ```r
#' design <- cbc_design(profiles, method = "sequential", n_alts = 3, n_q = 6, priors = priors)
#' survey <- cbc_survey(design, n_resp = 100)
#' ```
#'
#' **When to use each workflow:**
#' - **Integrated (Workflow 1)**: Most common use case, simplest for new users
#' - **Two-step (Workflow 2)**: When you want to inspect/modify the design before creating the survey,
#'   or when using the design in external platforms
#'
#' @return A data frame with class `cbc_survey` containing the complete survey design.
#'   The underlying experimental design is stored as an attribute and can be
#'   extracted using `cbc_extract_design()`.
#'
#' @export
#' @examples
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C")
#' )
#'
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3)
#' )
#'
#' # Workflow 1: Integrated (profiles → survey)
#' survey1 <- cbc_survey(
#'   profiles,
#'   n_resp = 100,
#'   method = "sequential",
#'   n_alts = 2,
#'   n_q = 4,
#'   priors = priors
#' )
#'
#' # Workflow 2: Two-step (design → survey)
#' design <- cbc_design(
#'   profiles,
#'   method = "sequential",
#'   n_alts = 2,
#'   n_q = 4,
#'   priors = priors
#' )
#' # Inspect design if needed: cbc_inspect_balance(design)
#' survey2 <- cbc_survey(design, n_resp = 100)
#'
#' # Both surveys have extractable designs
#' design1 <- cbc_extract_design(survey1)
#' design2 <- cbc_extract_design(survey2)
cbc_survey <- function(x, n_resp, ...) {
  UseMethod("cbc_survey")
}

#' @rdname cbc_survey
#' @export
cbc_survey.cbc_profiles <- function(
    x,
    n_resp,
    method = "sequential",
    n_alts,
    n_q,
    priors = NULL,
    randomize_questions = TRUE,
    randomize_alts = TRUE,
    ...
) {

  # Validate inputs
  if (!inherits(x, "cbc_profiles")) {
    stop("x must be a cbc_profiles object created by cbc_profiles()")
  }

  profiles <- x

  # Check required arguments for design creation
  if (missing(n_alts) || missing(n_q)) {
    stop("n_alts and n_q are required when creating survey from profiles")
  }

  # Validate method
  valid_methods <- c("sequential", "random")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  if (method == "random") {
    # Create random survey directly (no underlying design)
    survey <- create_random_survey_direct(
      profiles = profiles,
      n_resp = n_resp,
      n_alts = n_alts,
      n_q = n_q,
      randomize_questions = randomize_questions,
      randomize_alts = randomize_alts,
      priors = priors,
      ...
    )

    # No underlying design for random surveys
    attr(survey, "base_design") <- NULL

  } else {
    # Step 1: Create design using cbc_design
    message("Creating experimental design...")
    design <- cbc_design(
      profiles = profiles,
      method = method,
      n_alts = n_alts,
      n_q = n_q,
      priors = priors,
      ...
    )

    # Step 2: Create survey from design
    message("Replicating design across ", n_resp, " respondents...")
    survey <- cbc_survey.cbc_design(
      x = design,
      n_resp = n_resp,
      randomize_questions = randomize_questions,
      randomize_alts = randomize_alts
    )
  }

  return(survey)
}

#' @rdname cbc_survey
#' @export
cbc_survey.cbc_design <- function(
    x,
    n_resp,
    randomize_questions = TRUE,
    randomize_alts = TRUE,
    ...
) {

  # Validate inputs
  if (!inherits(x, "cbc_design")) {
    stop("x must be a cbc_design object created by cbc_design()")
  }

  base_design <- x

  # Extract design parameters
  params <- attr(base_design, "design_params")
  profiles <- attr(base_design, "profiles")

  if (is.null(params)) {
    stop("Invalid cbc_design object: missing design parameters")
  }

  n_blocks <- params$n_blocks
  n_q <- params$n_q
  n_alts <- params$n_alts
  no_choice <- params$no_choice
  label <- params$label
  method <- params$method

  # Check if we have enough blocks for the requested respondents
  if (n_blocks > 1 && n_resp < n_blocks) {
    warning(sprintf(
      "Design has %d blocks but only %d respondents requested. Some blocks will not be used.",
      n_blocks, n_resp
    ))
  }

  # Replicate the design across respondents
  survey <- repeat_sets(
    base_design, n_resp, n_alts, n_q, n_blocks,
    randomize_questions, randomize_alts
  )

  # Calculate survey statistics
  survey_stats <- calculate_survey_stats(survey, base_design, n_resp)

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
    stats = survey_stats,
    workflow = "two_step"  # Indicate this came from design → survey
  )

  # Store references to original objects
  attr(survey, "base_design") <- base_design
  attr(survey, "survey_info") <- survey_info

  class(survey) <- c("cbc_survey", "data.frame")
  return(survey)
}

# Helper function for random surveys (when created directly from profiles)
create_random_survey_direct <- function(
    profiles,
    n_resp,
    n_alts,
    n_q,
    randomize_questions,
    randomize_alts,
    priors = NULL,
    no_choice = FALSE,
    label = NULL,
    remove_dominant = FALSE,
    dominance_types = c("total", "partial"),
    dominance_threshold = 0.8,
    max_dominance_attempts = 100,
    ...
) {

  # Validate dominance parameters
  if (remove_dominant && is.null(priors)) {
    warning("remove_dominant requires priors. Setting remove_dominant = FALSE.")
    remove_dominant <- FALSE
  }

  # Use existing random survey creation logic
  survey <- make_random_survey(
    profiles, n_blocks = 1, n_resp, n_alts, n_q, label,
    priors, remove_dominant, dominance_types, dominance_threshold, max_dominance_attempts
  )

  if (no_choice) {
    survey <- add_no_choice(survey, n_alts)
  }

  # Apply randomization if needed (random surveys are already randomized, but this allows control)
  if (randomize_questions || randomize_alts) {
    survey <- randomize_design(
      survey, n_resp, n_alts, n_q, n_blocks = 1,
      randomize_questions, randomize_alts
    )
  }

  # Calculate survey statistics
  minimal_design_info <- list(
    profiles = profiles,
    design_info = list(efficiency = NULL)
  )
  survey_stats <- calculate_survey_stats(survey, minimal_design_info, n_resp)

  # Create survey metadata
  survey_info <- list(
    n_resp = n_resp,
    created_at = Sys.time(),
    total_questions = n_resp * n_q,
    total_observations = n_resp * n_q,
    total_alternatives = nrow(survey),
    randomize_questions = randomize_questions,
    randomize_alts = randomize_alts,
    design_method = "random",
    blocks_per_respondent = 1,
    stats = survey_stats,
    workflow = "integrated"  # Indicate this came from profiles → survey directly
  )

  # Store survey metadata
  attr(survey, "survey_info") <- survey_info

  class(survey) <- c("cbc_survey", "data.frame")
  return(survey)
}

#' Check if object is a cbc_survey object
#' @param x Object to check
#' @return Logical indicating if x is a cbc_survey object
#' @export
is.cbc_survey <- function(x) {
  inherits(x, "cbc_survey")
}

# Helper functions

calculate_survey_stats <- function(survey, design, n_resp) {
  # Profile usage statistics
  profile_usage <- table(survey$profileID)
  unique_profiles_used <- length(unique(survey$profileID))

  # Handle both design objects and minimal design info
  if (inherits(design, "cbc_design")) {
    profiles <- attr(design, "profiles")
    summary_info <- attr(design, "design_summary")
    total_profiles_available <- nrow(profiles)
    efficiency <- summary_info$efficiency
  } else {
    # For minimal design info (from random surveys)
    total_profiles_available <- nrow(design$profiles)
    efficiency <- design$design_info$efficiency
  }

  stats <- list(
    unique_profiles_used = unique_profiles_used,
    total_profiles_available = total_profiles_available,
    profile_usage_rate = unique_profiles_used / total_profiles_available,
    avg_profile_repetitions = mean(profile_usage),
    min_profile_repetitions = min(profile_usage),
    max_profile_repetitions = max(profile_usage)
  )

  # Add efficiency metrics if available from design
  if (!is.null(efficiency)) {
    stats$balance_score <- efficiency$balance_score
    stats$overlap_score <- efficiency$overlap_score
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
    profiles, n_blocks, n_resp, n_alts, n_q, label,
    priors = NULL, remove_dominant = FALSE, dominance_types = c("total", "partial"),
    dominance_threshold = 0.8, max_dominance_attempts = 100
) {
  if (is.null(label)) {
    survey <- survey_rand_sample(
      profiles, n_resp, n_alts, n_q, priors, remove_dominant,
      dominance_types, dominance_threshold, max_dominance_attempts
    )
  } else {
    survey <- survey_rand_sample_label(
      profiles, n_resp, n_alts, n_q, label, priors, remove_dominant,
      dominance_types, dominance_threshold, max_dominance_attempts
    )
  }
  survey <- set_block_ids(survey, n_blocks)
  survey <- reorder_cols(survey)
  row.names(survey) <- NULL
  return(survey)
}

survey_rand_sample <- function(
    profiles, n_resp, n_alts, n_q, priors = NULL, remove_dominant = FALSE,
    dominance_types = c("total", "partial"), dominance_threshold = 0.8, max_dominance_attempts = 100
) {
  survey <- sample_profiles(profiles, size = n_resp * n_alts * n_q)
  survey <- add_metadata(survey, n_resp, n_alts, n_q)

  # Replace rows with duplicated profiles, duplicated choice sets, or dominant choice sets
  attempts <- 0
  repeat {
    dup_rows_obs <- get_dup_obs(survey, n_alts)
    dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
    dom_rows <- if (remove_dominant) get_dominant_obs(survey, priors, dominance_types, dominance_threshold) else c()

    problematic_rows <- unique(c(dup_rows_obs, dup_rows_resp, dom_rows))

    if (length(problematic_rows) == 0) {
      break  # No more problems
    }

    # Replace problematic rows
    new_rows <- sample_profiles(profiles, size = length(problematic_rows))
    survey[problematic_rows, 1:ncol(new_rows)] <- new_rows

    # Prevent infinite loops
    attempts <- attempts + 1
    if (attempts > max_dominance_attempts) {
      if (remove_dominant && length(dom_rows) > 0) {
        warning("Could not eliminate all dominant choice sets after ", max_dominance_attempts, " attempts. Some may remain.")
      }
      break
    }
  }

  return(survey)
}

survey_rand_sample_label <- function(
    profiles, n_resp, n_alts, n_q, label, priors = NULL, remove_dominant = FALSE,
    dominance_types = c("total", "partial"), dominance_threshold = 0.8, max_dominance_attempts = 100
) {
  n_alts <- override_label_alts(profiles, label, n_alts)
  # Randomize rows by label
  labels <- split(profiles, profiles[label])
  survey <- sample_profiles_by_group(labels, size = n_resp * n_q)
  survey <- add_metadata(survey, n_resp, n_alts, n_q)

  # Replace rows with duplicated profiles, duplicated choice sets, or dominant choice sets
  attempts <- 0
  repeat {
    dup_rows_obs <- get_dup_obs(survey, n_alts)
    dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
    dom_rows <- if (remove_dominant) get_dominant_obs(survey, priors, dominance_types, dominance_threshold) else c()

    problematic_rows <- unique(c(dup_rows_obs, dup_rows_resp, dom_rows))

    if (length(problematic_rows) == 0) {
      break  # No more problems
    }

    # Replace problematic rows (for labeled designs, need to sample by groups)
    new_rows <- sample_profiles_by_group(labels, size = length(problematic_rows) / n_alts)
    survey[problematic_rows, 1:ncol(new_rows)] <- new_rows

    # Prevent infinite loops
    attempts <- attempts + 1
    if (attempts > max_dominance_attempts) {
      if (remove_dominant && length(dom_rows) > 0) {
        warning("Could not eliminate all dominant choice sets after ", max_dominance_attempts, " attempts. Some may remain.")
      }
      break
    }
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

get_id_names <- function() {
  return(c("profileID", "blockID", "respID", "qID", "altID", "obsID"))
}

get_var_names <- function(design) {
  return(setdiff(names(design), get_id_names()))
}

# Helper function to get rows from dominant choice sets
get_dominant_obs <- function(survey, priors, dominance_types, dominance_threshold) {
  if (is.null(priors)) {
    return(c())
  }

  # Get unique choice sets (obsIDs)
  unique_obs <- unique(survey$obsID)
  dominant_obs <- c()

  for (obs_id in unique_obs) {
    choice_set_rows <- survey[survey$obsID == obs_id, ]

    # Use the same dominance checking logic as in optdesign.R
    is_dominant <- check_choice_set_dominance(
      choice_set_rows, priors, NULL, dominance_types, dominance_threshold
    )

    if (is_dominant) {
      dominant_obs <- c(dominant_obs, obs_id)
    }
  }

  # Return row indices for dominant choice sets
  return(which(survey$obsID %in% dominant_obs))
}

#' Generate survey designs for choice experiments
#'
#' This function creates experimental designs (DOE) for choice-based conjoint experiments.
#' The design represents the choice questions that will be shown to a single respondent
#' (or single block). To create a complete survey with multiple respondents, pass the
#' result to `cbc_survey()` or use `cbc_survey()` directly with profiles.
#'
#' @param profiles A data frame of class `cbc_profiles` created using the
#'   `cbc_profiles()` function.
#' @param method Choose the design method to use. Currently supports `"sequential"`
#'   for D-efficient designs and `"random"` for random designs.
#'   Defaults to `"random"`.
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL` for
#'   random designs. Required for D-efficient designs.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent (or per block).
#' @param n_resp Number of survey respondents.
#' @param n_blocks Number of blocks in the design. Each block contains `n_q` questions.
#'   Defaults to `1`. When `n_blocks > 1`, the design will contain multiple versions
#'   that can be distributed across respondents.
#' @param n_start A numeric value indicating the number of random start designs
#'   to use in obtaining a D-efficient design. Defaults to `5`.
#' @param no_choice Include a "no choice" option in the choice sets? Defaults to
#'   `FALSE`. If `TRUE`, the total number of alternatives per question will be
#'   one more than the provided `n_alts` argument.
#' @param label The name of the variable to use in a "labeled" design (also
#'   called an "alternative-specific" design). If used, the `n_alts` argument
#'   will be ignored. Defaults to `NULL`.
#' @param randomize_questions Randomize question order for each respondent? Defaults to `TRUE`.
#' @param randomize_alts Randomize alternative order within questions? Defaults to `TRUE`.
#' @param max_iter Maximum number of iterations for D-efficient optimization.
#'   Defaults to `50`.
#' @param parallel Logical value indicating whether to use parallel processing.
#'   Defaults to `FALSE`.
#' @param remove_dominant Logical. If `TRUE`, removes choice sets where one
#'   alternative dominates others based on the provided priors. Only works when
#'   `priors` are provided. Defaults to `FALSE`.
#' @param dominance_types Character vector specifying which types of dominance
#'   to check for. Options are `"total"` and/or `"partial"`. Defaults to `c("total", "partial")`.
#' @param dominance_threshold Numeric. Threshold for total dominance detection.
#'   Defaults to `0.8`.
#' @param max_dominance_attempts Integer. Maximum number of replacement
#'   attempts when removing dominant choice sets. Defaults to `10`.
#' @return A `cbc_design` object containing the experimental design with these components:
#' - **Design data**: Each row is an alternative, with columns for `profileID`, `blockID`,
#'   `respID`, `qID`, `altID`, `obsID`, and attribute values
#' - **Metadata**: Stored as attributes including design parameters, efficiency metrics,
#'   and the original profiles
#' - **D-error**: Design efficiency measure (lower is better)
#'
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3, 4),
#'   type = c("A", "B", "C"),
#'   quality = c("Low", "High")
#' )
#'
#' # Create priors
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3),
#'   quality = 0.4
#' )
#'
#' # Generate D-efficient design
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 3,
#'   n_q = 6,
#'   priors = priors,
#'   method = "sequential"
#' )
#'
#' # Inspect the design
#' print(design)
#' cbc_inspect_balance(design)
#'
#' # Check D-error
#' d_error <- cbc_error(design, priors = priors)
cbc_design <- function(
    profiles,
    method = "random",
    priors = NULL,
    n_alts,
    n_q,
    n_resp,
    n_blocks = 1,
    n_start = 5,
    no_choice = FALSE,
    label = NULL,
    randomize_questions = TRUE,
    randomize_alts = TRUE,
    max_iter = 50,
    parallel = FALSE,
    remove_dominant = FALSE,
    dominance_types = c("total", "partial"),
    dominance_threshold = 0.8,
    max_dominance_attempts = 10
) {

  # Validate inputs
  validate_profiles(profiles)
  validate_priors(priors, profiles)
  validate_method(method)
  if (remove_dominant) {
    if (is.null(priors)) {
      warning(
        "'remove_dominant' requires priors. ",
        "Consider creating priors with 'cbc_priors()'. ",
        "Setting remove_dominant = FALSE."
      )
      remove_dominant <- FALSE
    } else {
      validate_dominance_inputs(
        dominance_types, dominance_threshold, max_dominance_attempts
      )
    }
  }

  # Check design feasibility
  check_inputs_design(
    profiles, n_alts, n_q, n_blocks, n_start, no_choice, label, method,
    priors, max_iter, parallel
  )

  profiles_df <- as.data.frame(profiles)

  # Method-specific design creation
  if (method == 'sequential') {

    # Inform user about Bayesian approach if using random parameters
    if (!is.null(priors)) {
      par_draws <- priors$par_draws
      if (!is.null(par_draws)) {
        message(
          "Using Bayesian approach: Averaging error metrics across ",
          nrow(par_draws), " draws from each of ",
          ncol(par_draws), " random parameters"
        )
      }
    }

    result <- make_design_sequential(
      profiles_df, n_blocks, n_alts, n_q, n_blocks,
      priors, max_iter, label, n_start,
      remove_dominant, dominance_types, dominance_threshold
    )
    design <- result$design
    d_error <- result$d_error

    # Apply randomization if needed
    design <- repeat_sets(
        design,
        n_resp,
        n_alts,
        n_q,
        n_blocks,
        randomize_questions,
        randomize_alts
    )

  } else if (method == 'random') {

    # Create random survey directly (no underlying design)

    # Use existing random survey creation logic
    design <- make_random_survey(
      profiles, n_blocks = 1, n_resp, n_alts, n_q, label,
      priors, remove_dominant, dominance_types, dominance_threshold,
      max_dominance_attempts
    )

    # Compute D-error for random design (useful for comparison with other methods)
    d_error <- cbc_error(design, errors = "d", priors = priors)

  }
  # Future methods can be added here

  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }

  # Store essential information as attributes
  attr(design, "profiles") <- profiles
  attr(design, "priors") <- priors
  attr(design, "design_params") <- list(
    method = method,
    n_q = n_q,
    n_alts = n_alts,
    n_blocks = n_blocks,
    no_choice = no_choice,
    label = label,
    d_error = d_error,
    created_at = Sys.time(),
    remove_dominant = remove_dominant,
    dominance_types = if (remove_dominant) dominance_types else NULL,
    purpose = "experimental_design"  # Indicate this is a pure DOE
  )

  # Store computed metrics for printing
  efficiency_info <- calculate_efficiency_metrics(design, profiles, priors)
  attr(design, "design_summary") <- list(
    n_profiles_available = nrow(profiles),
    n_profiles_used = length(unique(design$profileID)),
    profile_usage_rate = length(unique(design$profileID)) / nrow(profiles),
    n_choice_sets = n_blocks * n_q,
    total_alternatives = nrow(design),
    restrictions_applied = !is.null(attr(profiles, "restrictions_applied")),
    n_restrictions = length(attr(profiles, "restrictions_applied") %||% character(0)),
    efficiency = efficiency_info
  )

  # Set class and return data frame directly
  class(design) <- c("cbc_design", "data.frame")
  return(design)
}

#' Remove dominant choice sets from a design
#'
#' Core function for removing choice sets where one alternative dominates others
#' based on utility calculations from provided priors.
#'
#' @param design A data frame containing the design
#' @param profiles A data frame of profiles to sample replacements from
#' @param priors A cbc_priors object for dominance checking
#' @param dominance_types Character vector of dominance types to check
#' @param dominance_threshold Numeric threshold for total dominance
#' @param max_replacements Maximum number of replacement attempts
#' @param n_alts Number of alternatives per question
#' @param label Label variable for labeled designs
#' @return Updated design with dominant choice sets replaced
#' @keywords internal

# Helper function to calculate efficiency metrics
calculate_efficiency_metrics <- function(design, profiles, priors) {
  efficiency <- list()

  # Calculate attribute balance score
  tryCatch({
    balance_info <- get_balance_metrics(design)
    efficiency$balance_score <- balance_info$score
    efficiency$balance_details <- balance_info$details
  }, error = function(e) {
    efficiency$balance_score <- NA
  })

  # Calculate attribute overlap score
  tryCatch({
    overlap_info <- get_overlap_metrics(design)
    efficiency$overlap_score <- overlap_info$score
    efficiency$overlap_details <- overlap_info$details
  }, error = function(e) {
    efficiency$overlap_score <- NA
  })

  return(efficiency)
}

# Helper function to compute balance metrics
get_balance_metrics <- function(design) {
  attr_cols <- get_var_names(design)

  # For each attribute, calculate how balanced the levels are
  balance_scores <- list()

  for (attr in attr_cols) {
    counts <- table(design[[attr]])
    expected_count <- nrow(design) / length(counts)
    # Calculate deviation from perfect balance (coefficient of variation)
    cv <- sd(counts) / mean(counts)
    balance_scores[[attr]] <- 1 / (1 + cv)  # Higher is better, max = 1
  }

  overall_score <- mean(unlist(balance_scores))

  return(list(
    score = overall_score,
    details = balance_scores
  ))
}

# Helper function to compute overlap metrics
get_overlap_metrics <- function(design) {
  attr_cols <- get_var_names(design)

  overlap_scores <- list()

  for (attr in attr_cols) {
    # Count how many choice sets have all alternatives with the same level
    overlap_counts <- tapply(
      design[[attr]], design$obsID,
      FUN = function(x) length(unique(x)) == 1
    )
    overlap_rate <- mean(overlap_counts)
    overlap_scores[[attr]] <- overlap_rate
  }

  overall_score <- mean(unlist(overlap_scores))

  return(list(
    score = overall_score,
    details = overlap_scores
  ))
}

# General helpers ----

get_profile_list <- function(profiles) {
  profile_lvls <- profiles[, 2:ncol(profiles)]
  varnames <- names(profile_lvls)
  type_ids <- get_type_ids(profiles)
  profile_list <- list()
  for (i in seq_len(ncol(profile_lvls))) {
    if (type_ids$discrete[i]) {
      profile_list[[i]] <- levels(profile_lvls[,i])
    } else {
      profile_list[[i]] <- unique(profile_lvls[,i])
    }
  }
  names(profile_list) <- varnames
  return(profile_list)
}

get_type_ids <- function(profiles) {
  types <- get_col_types(profiles[, 2:ncol(profiles)])
  ids <- list()
  ids$discrete <- types %in% c("factor", "character")
  ids$continuous <- !ids$discrete
  return(ids)
}

join_profiles <- function(design, profiles) {

  # Preserve row order

  design$row_order <- seq(nrow(design))

  # Before joining profiles, ensure that all the data types are the same
  # as in profiles, otherwise join won't work properly

  type_ids <- get_type_ids(profiles)

  # Convert numeric columns to actual numbers
  for (id in which(type_ids$continuous)) {
    design[,id] <- as.numeric(as.character(design[,id]))
  }

  # Convert character types to factors and set same levels as profiles
  for (id in which(type_ids$discrete)) {
    design[,id] <- factor(design[,id], levels = levels(profiles[,id+1]))
  }

  # Join on profileIDs, then reorder to retain design order
  varnames <- names(profiles[, 2:ncol(profiles)])
  design <- merge(design, profiles, by = varnames, all.x = TRUE, sort = FALSE)
  design <- design[order(design$row_order),]
  design$row_order <- NULL
  if ('blockID' %in% names(design)) { varnames <- c(varnames, 'blockID') }
  design <- design[c('profileID', varnames)]
  return(design)
}

add_metadata <- function(design, n_resp, n_alts, n_q) {
  design$respID <- rep(seq(n_resp), each = n_alts * n_q)
  design$qID    <- rep(seq(n_q), each = n_alts)
  design$altID  <- rep(seq(n_alts), n_resp * n_q)
  design$obsID  <- rep(seq(n_resp * n_q), each = n_alts)
  return(design)
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

reorder_cols <- function(design) {
  design <- as.data.frame(design)[, c(get_id_names(), get_var_names(design))]
  return(design)
}

# Choice sets ----

make_random_sets <- function(profiles, n_alts) {
  n_q <- nrow(profiles)
  design <- sample_random_sets(profiles, n_alts, n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  dup_rows_obs <- get_dup_obs(design, n_alts)
  dup_rows_resp <- get_dup_resp(design, n_resp = 1, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    design <- sample_random_sets(profiles, n_alts, n_q)
    dup_rows_obs <- get_dup_obs(design, n_alts)
    dup_rows_resp <- get_dup_resp(design, n_resp = 1, n_q)
  }
  return(design)
}

make_random_sets_by_block <- function(profiles, n_alts, n_blocks) {
  # Make choice sets for each set of profileIDs
  profiles <- split(profiles, profiles$blockID)
  choice_sets <- list()
  for (i in 1:n_blocks) {
    choice_sets[[i]] <- make_random_sets(profiles[[i]], n_alts)
  }
  choice_sets <- do.call(rbind, choice_sets)
  return(choice_sets)
}

sample_random_sets <- function(profiles, n_alts, n_q) {
  # Make a randomized copy of the profiles for each alternative
  sets <- lapply(seq(n_alts), function(x) profiles[order(stats::runif(n_q)),])
  sets <- lapply(sets, function(x) {
    x$order <- seq(nrow(x))
    return(x)
  })
  sets <- do.call(rbind, sets)
  sets <- sets[order(sets$order),]
  sets <- add_metadata(sets, n_resp = 1, n_alts, n_q)
  sets$order <- NULL
  return(sets)
}

# Random Design ----

# Sample from profiles with replacement to create randomized choice sets

make_random_survey <- function(
    profiles, n_blocks, n_resp, n_alts, n_q, label,
    priors, remove_dominant, dominance_types,
    dominance_threshold, max_dominance_attempts
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
    profiles, n_resp, n_alts, n_q, priors, remove_dominant,
    dominance_types, dominance_threshold, max_dominance_attempts
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
    profiles, n_resp, n_alts, n_q, label, priors, remove_dominant,
    dominance_types, dominance_threshold, max_dominance_attempts
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

# Sequential D-Efficient Design ----

make_design_sequential <- function(
    profiles,
    n_resp,
    n_alts,
    n_q,
    n_blocks,
    priors,
    max_iter,
    label,
    n_start,
    remove_dominant,
    dominance_types,
    dominance_threshold
) {
  # Set up parallel processing
  n_cores <- set_num_cores(NULL)
  message("Running ", n_start, " design searches using ", n_cores, " cores...")

  # Create list of different random starting designs
  start_designs <- lapply(1:n_start, function(i) {
    design <- make_random_survey(
        profiles, n_blocks, n_resp = n_blocks, n_alts, n_q, label,
        priors, remove_dominant, dominance_types, dominance_threshold,
        max_dominance_attempts
    )

    design$qID <- design$obsID
    return(design)
  })

  n_questions <- n_q * n_blocks
  varNames <- get_var_names(start_designs[[1]])

  # Run optimization in parallel
  # Different parallel operation for Windows vs Mac
  if (Sys.info()[['sysname']] == 'Windows') {
    cl <- parallel::makeCluster(n_cores, "PSOCK")
    # Export necessary functions to cluster
    parallel::clusterExport(cl, c(
      "optimize_design", "compute_info_matrix", "get_X_matrix",
      "logit", "logit_draws", "get_eligible_profiles",
      "check_choice_set_dominance", "check_partial_dominance"
    ), envir = environment())

    results <- suppressMessages(suppressWarnings(
      parallel::parLapply(
        cl = cl,
        seq_along(start_designs),
        function(i) {
          result <- optimize_design(
            start_designs[[i]], profiles, priors, varNames,
            n_questions, n_alts, max_iter, n_blocks, label,
            remove_dominant, dominance_types, dominance_threshold
          )
          result$start_number <- i
          return(result)
        }
      )
    ))
    parallel::stopCluster(cl)
  } else {
    results <- suppressMessages(suppressWarnings(
      parallel::mclapply(
        seq_along(start_designs),
        function(i) {
          result <- optimize_design(
            start_designs[[i]], profiles, priors, varNames,
            n_questions, n_alts, max_iter, n_blocks, label
          )
          result$start_number <- i
          return(result)
        },
        mc.cores = n_cores
      )
    ))
  }

  # Find best design based on D-error
  d_errors <- sapply(results, function(x) x$d_error)
  best_index <- which.min(d_errors)
  best_result <- results[[best_index]]

  # Merge designs
  design <- best_result$design
  d_error <- best_result$d_error
  design <- set_block_ids(design, n_blocks)

  # Print summary of all starts
  message("\nD-error results from all starts:")
  sorted_results <- sort(d_errors)
  for (i in seq_along(sorted_results)) {
    idx <- which(d_errors == sorted_results[i])[1]
    message(sprintf(
      "Start %d: %.6f %s",
      results[[idx]]$start_number,
      sorted_results[i],
      if(idx == best_index) "  (Best)" else ""
    ))
  }
  return(results[[best_index]])
}

set_block_ids <- function(design, n_blocks) {
  if (n_blocks > 1) {
    design$blockID <- rep(seq(n_blocks), each = nrow(design) / n_blocks)
  } else {
    design$blockID <- 1
  }
  return(design)
}

set_num_cores <- function(n_cores) {
  cores_available <- parallel::detectCores()
  max_cores <- cores_available - 1
  # CRAN checks limits you to 2 cores, see this SO issue:
  # https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(chk) && (chk != "false")) {
    # use 2 cores in CRAN/Travis/AppVeyor
    return(2L)
  }
  if (is.null(n_cores)) {
    return(max_cores)
  } else if (!is.numeric(n_cores)) {
    warning(
      "Non-numeric value provided for n_cores...setting n_cores to ",
      max_cores
    )
    return(max_cores)
  } else if (n_cores > cores_available) {
    warning(
      "Cannot use ", n_cores, " cores because your machine only has ",
      cores_available, " available...setting n_cores to ", max_cores
    )
    return(max_cores)
  }
  return(n_cores)
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

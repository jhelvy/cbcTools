#' Generate experimental designs for choice experiments
#'
#' This function creates experimental designs (DOE) for choice-based conjoint experiments.
#' The design represents the choice questions that will be shown to a single respondent
#' (or single block). To create a complete survey with multiple respondents, pass the
#' result to `cbc_survey()` or use `cbc_survey()` directly with profiles.
#'
#' @param profiles A data frame of class `cbc_profiles` created using the
#'   `cbc_profiles()` function.
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL` for
#'   random designs. Required for D-efficient designs.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent (or per block).
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
#' @param method Choose the design method to use. Currently supports `"sequential"`
#'   for D-efficient designs and `"random"` for random designs. Defaults to `"sequential"`.
#' @param prior_no_choice Prior utility value for the "no choice" alternative.
#'   Only required if `no_choice = TRUE` and using D-efficient methods. Defaults to `NULL`.
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
#' @param max_dominance_replacements Integer. Maximum number of replacement
#'   attempts when removing dominant choice sets. Defaults to `10`.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' **Purpose:** Generate experimental designs for choice experiments
#'
#' **When to use `cbc_design()`:**
#' - You need just the experimental design for use in external survey platforms (Qualtrics, etc.)
#' - You want to inspect or modify the design before creating the full survey
#' - You're conducting pure experimental design research
#' - You need to optimize computational performance (no survey replication overhead)
#'
#' **When to use `cbc_survey()` instead:**
#' - You want to create a complete survey for choice analysis within R
#' - You're conducting choice simulation or power analysis
#' - You want the simplest workflow (profiles → complete survey)
#'
#' **Design methods:**
#' - `"sequential"`: Creates D-efficient designs using sequential optimization
#' - `"random"`: Creates random designs by sampling from profiles
#'
#' **Typical workflows:**
#' ```r
#' # DOE for external use
#' design <- cbc_design(profiles, method = "sequential", n_alts = 3, n_q = 6, priors = priors)
#' # → Export to Qualtrics, save as CSV, etc.
#'
#' # DOE → Survey (two-step)
#' design <- cbc_design(profiles, method = "sequential", n_alts = 3, n_q = 6, priors = priors)
#' survey <- cbc_survey(design, n_resp = 100)
#'
#' # Profiles → Survey (integrated, often easier)
#' survey <- cbc_survey(profiles, n_resp = 100, method = "sequential", n_alts = 3, n_q = 6, priors = priors)
#' ```
#'
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
#'
#' # Convert to survey for analysis
#' survey <- cbc_survey(design, n_resp = 100)
#'
#' # Or create survey directly (alternative approach)
#' survey2 <- cbc_survey(
#'   profiles, n_resp = 100, method = "sequential",
#'   n_alts = 3, n_q = 6, priors = priors
#' )
cbc_design <- function(
    profiles,
    priors = NULL,
    n_alts,
    n_q,
    n_blocks = 1,
    n_start = 5,
    no_choice = FALSE,
    label = NULL,
    method = "sequential",
    prior_no_choice = NULL,
    max_iter = 50,
    parallel = FALSE,
    remove_dominant = FALSE,
    dominance_types = c("total", "partial"),
    dominance_threshold = 0.8,
    max_dominance_replacements = 10,
    ...
) {

  # Validate input classes
  if (!inherits(profiles, "cbc_profiles")) {
    stop("profiles must be a cbc_profiles object created by cbc_profiles()")
  }

  if (!is.null(priors)) {
    if (!inherits(priors, "cbc_priors")) {
      stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    # Validate compatibility between priors and profiles
    validate_priors_profiles(priors, profiles)
  }

  # Validate method
  valid_methods <- c("sequential", "random")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Check if priors are recommended for the chosen method
  if (method == "sequential" && is.null(priors)) {
    message("The 'sequential' method works better with priors. Consider creating priors with cbc_priors().")
  }

  # Validate dominance parameters
  if (remove_dominant) {
    if (is.null(priors)) {
      warning("remove_dominant requires priors. Setting remove_dominant = FALSE.")
      remove_dominant <- FALSE
    } else {
      # Validate dominance_types
      valid_types <- c("total", "partial")
      if (!all(dominance_types %in% valid_types)) {
        stop("dominance_types must be one or more of: ", paste(valid_types, collapse = ", "))
      }

      # Validate threshold
      if (dominance_threshold <= 0 || dominance_threshold >= 1) {
        stop("dominance_threshold must be between 0 and 1")
      }

      # Validate max replacements
      if (max_dominance_replacements < 1) {
        stop("max_dominance_replacements must be at least 1")
      }
    }
  }

  # Check design feasibility
  check_inputs_design(
    profiles, n_alts, n_q, n_blocks, n_start, no_choice, label, method,
    priors, prior_no_choice, max_iter, parallel
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

  } else if (method == 'random') {

    # For random designs, create a single respondent survey then convert to design
    temp_survey <- make_random_survey(
      profiles, n_blocks = n_blocks, n_resp = n_blocks, n_alts, n_q, label,
      priors, remove_dominant, dominance_types, dominance_threshold, max_dominance_replacements
    )
    design <- temp_survey

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

#' Display attribute levels and dummy coding for a CBC design
#'
#' Shows how categorical variables will be dummy coded and what each coefficient
#' represents in the utility function.
#'
#' @param design A `cbc_design` object created by `cbc_design()`, or a data frame
#'   containing a choice experiment design
#' @param exclude Optional character vector of attribute names to exclude
#' @return Invisibly returns a list containing the coding information, but primarily
#'   prints formatted information to the console
#' @export
#' @examples
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price     = seq(1, 5, 0.5),
#'   type      = c('Fuji', 'Gala', 'Honeycrisp'),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Generate design
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 3,
#'   n_q = 6
#' )
#'
#' # View attribute levels and coding
#' cbc_levels(design)
cbc_levels <- function(design, exclude = NULL) {
  # Handle both cbc_design objects and data frames
  if (inherits(design, "cbc_design")) {
    design_df <- design  # design is now the data frame directly
    params <- attr(design, "design_params")
    profiles <- attr(design, "profiles")
    priors <- attr(design, "priors")

    cat("CBC Design Attribute Information\n")
    cat("================================\n")
    cat(sprintf("Design method: %s\n", params$method))
    if (!is.null(params$d_error)) {
      cat(sprintf("D-error: %.6f\n", params$d_error))
    }
    cat("\n")
  } else {
    design_df <- design
    profiles <- NULL
    priors <- NULL

    cat("CBC Design Attribute Information\n")
    cat("================================\n\n")
  }

  # Get attribute columns (excluding metadata)
  attr_cols <- get_var_names(design_df)

  if (!is.null(exclude)) {
    attr_cols <- setdiff(attr_cols, exclude)
  }

  # Process each attribute
  attr_info <- list()

  for (attr in attr_cols) {
    values <- design_df[[attr]]
    if (is.numeric(values)) {
      # Continuous variable
      cat(sprintf("%-12s: Continuous variable\n", attr))
      cat(sprintf("              Range: %.2f to %.2f\n",
                  min(values), max(values)))
      cat("              Coefficient represents effect of one-unit change\n\n")

      attr_info[[attr]] <- list(
        type = "continuous",
        range = range(values)
      )

    } else {
      # Categorical variable
      levels <- unique(sort(as.character(values)))
      n_levels <- length(levels)
      base_level <- levels[1]
      coded_levels <- levels[-1]

      cat(sprintf("%-12s: Categorical variable (%d levels)\n", attr, n_levels))
      cat("              Base level:", base_level, "\n")
      for (i in seq_along(coded_levels)) {
        cat(sprintf("              β%-2d: %s\n",
                    i, coded_levels[i]))
      }
      cat("\n")

      attr_info[[attr]] <- list(
        type = "categorical",
        base_level = base_level,
        coded_levels = coded_levels
      )
    }
  }

  # Show priors if available
  if (!is.null(priors)) {
    cat("Priors used in this design:\n")
    cat("--------------------------\n")
    print(priors)
    cat("\n")
  }

  # Example prior specification
  cat("Example prior specification:\n")
  cat("----------------------------\n")
  cat("priors <- cbc_priors(\n")
  cat("    profiles = profiles,\n")

  for (attr in attr_cols) {
    if (attr_info[[attr]]$type == "continuous") {
      cat(sprintf("    %-12s = 0,  # Effect of one-unit change\n", attr))
    } else {
      coefs <- rep("0", length(attr_info[[attr]]$coded_levels))
      cat(sprintf("    %-12s = c(%s),  # vs %s\n",
                  attr,
                  paste(coefs, collapse = ", "),
                  attr_info[[attr]]$base_level))
    }
  }

  cat("    # Add sd = list(...) for random parameters\n")
  cat(")\n")

  invisible(attr_info)
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
    # cat('Number dupe rows by obs:', length(dup_rows_obs), '\n')
    # cat('Number dupe rows by resp:', length(dup_rows_resp), '\n')
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
    # cat('Number dupe rows by obs:', length(dup_rows_obs), '\n')
    # cat('Number dupe rows by resp:', length(dup_rows_resp), '\n')
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
    n_start = 1,
    remove_dominant = FALSE,
    dominance_types = c("total", "partial"),
    dominance_threshold = 0.8
) {
  # Set up parallel processing
  n_cores <- set_num_cores(NULL)
  message("Running ", n_start, " design searches using ", n_cores, " cores...")

  # Create list of different random starting designs
  start_designs <- lapply(1:n_start, function(i) {
    design <- make_random_survey(
      profiles, n_blocks, n_resp = n_blocks, n_alts, n_q, label,
      priors = NULL, remove_dominant = FALSE
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

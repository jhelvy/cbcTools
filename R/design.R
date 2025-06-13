#' Make a choice-based conjoint survey design
#'
#' This function creates a data frame containing a choice-based conjoint survey
#' design where each row is an alternative. Generate a variety of survey
#' designs, including random design, D-efficient designs, and Bayesian
#' D-efficient designs as well as designs with "no choice" options, blocking,
#' and "labeled" designs (also known as "alternative specific" designs).
#'
#' @keywords experiment design mnl mxl mixed logit logitr idefix DoE.base
#' @param profiles A data frame of class `cbc_profiles` created using the
#'   `cbc_profiles()` function.
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL` for
#'   random designs. Required for D-efficient designs.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent.
#' @param n_blocks Number of blocks used D-efficient or Bayesian D-efficient
#'   designs. Max allowable is one block per respondent. Defaults to `1`,
#'   meaning every respondent sees the same choice set.
#' @param n_start A numeric value indicating the number of random start designs
#'   to use in obtaining a D-efficient or Bayesian D-efficient design.
#'   The default is `1`. Increasing `n_start` can result in a more efficient
#'   design at the expense of increased computational time.
#' @param no_choice Include a "no choice" option in the choice sets? Defaults to
#'   `FALSE`. If `TRUE`, the total number of alternatives per question will be
#'   one more than the provided `n_alts` argument.
#' @param label The name of the variable to use in a "labeled" design (also
#'   called an "alternative-specific" design) such that each set of alternatives
#'   contains one of each of the levels in the `label` attribute. If used, the
#'   `n_alts` argument will be ignored as its value is defined by the unique
#'   number of levels in the `label` attribute Defaults to `NULL`.
#' @param method Choose the design method to use. Options include `"random"`
#'   and `"sequential"`. Defaults to `"random"`. See details below for complete
#'   description of each method.
#' @param randomize Logical. If `TRUE`, both the choice question order and the
#'   order of the alternatives within a given choice question will be
#'   randomized across each respondent. Does not apply to design created using
#'   the `"random"` method. Defaults to `TRUE`.
#' @param prior_no_choice Prior utility value for the "no choice" alternative.
#'   Only required if `no_choice = TRUE`. Defaults to `NULL`.
#' @param max_iter A numeric value indicating the maximum number allowed
#'   iterations when searching for a D-efficient or Bayesian D-efficient design.
#'   The default is 50.
#' @param parallel Logical value indicating whether to use parallel processing.
#'   Defaults to `FALSE`.
#' @param remove_dominant Logical. If `TRUE`, removes choice sets where one
#'   alternative dominates others based on the provided priors. Only works when
#'   `priors` are provided. Defaults to `FALSE`.
#' @param dominance_types Character vector specifying which types of dominance
#'   to check for. Options are `"total"` (high choice probability) and/or
#'   `"partial"` (best on all attributes). Defaults to `c("total", "partial")`.
#' @param dominance_threshold Numeric. Threshold for total dominance detection.
#'   If one alternative has a choice probability above this threshold, the choice
#'   set is considered dominant. Defaults to `0.8`.
#' @param max_dominance_replacements Integer. Maximum number of replacement
#'   attempts when removing dominant choice sets. Defaults to `10`.
#'
#' @details The `method` argument determines the design method used. Options
#'   are:
#'
#' - `"random"`: Creates a design by randomly sampling from profiles
#' - `"sequential"`: Creates a D-efficient design using sequential improvement. Bayesian D-efficient designs can be obtained by specifying a prior model with the `priors` argument that include a covariance matrix.
#'
#'   All methods ensure that the two following criteria are met:
#'
#'   1. No two profiles are the same within any one choice set.
#'   2. No two choice sets are the same within any one respondent.
#'
#'   The table below summarizes method compatibility with other design options:
#'
#'   Method        | No choice? | Labeled designs? | Restricted profiles? | Blocking?
#'   --------------|------------|------------------|----------------------|----------
#'   `"random"`    | Yes        | Yes              | Yes                  | No
#'   `"sequential"` | No         | Yes              | Yes                  | Yes
#'
#' The `"sequential"` method creates a design by sequentially improving D-efficiency:
#' 1. Start with a random design
#' 2. Compute initial D-error
#' 3. For each question, alternative, and attribute:
#'    - Try all possible level changes to the attribute.
#'    - Keep changes that improve the D-error.
#' 4. Repeat until no further improvements or max iterations are reached
#'
#' @return The returned `design` object contains a choice-based conjoint
#' survey design with class `cbc_design`. It includes the following components:
#'
#' - `design`: Data frame where each row is an alternative, including:
#'   - `profileID`: Identifies the profile in `profiles`.
#'   - `blockID`: If blocking is used, identifies each unique block.
#'   - `respID`: Identifies each survey respondent.
#'   - `qID`: Identifies the choice question answered by the respondent.
#'   - `altID`: Identifies the alternative in any one choice observation.
#'   - `obsID`: Identifies each unique choice observation across all respondents.
#' - `profiles`: The original profiles used to create the design
#' - `priors`: The priors used (if any)
#' - `method`: Design method used
#' - `n_q`, `n_alts`, `n_blocks`: Design parameters
#' - `d_error`: D-error of the design
#' - `design_info`: Additional metadata about the design
#'
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Make a random survey design (default)
#' design_random <- cbc_design(
#'   profiles = profiles,
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Create priors
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3),
#'   freshness = c(0.4, 0.8)
#' )
#'
#' # Make a D-efficient design with priors using the "sequential" method
#' design <- cbc_design(
#'   profiles = profiles,
#'   priors = priors,
#'   n_alts = 3,
#'   n_q = 6,
#'   method = "sequential"
#' )
#'
#' # Make a design with dominance checking
#' design_no_dominant <- cbc_design(
#'   profiles = profiles,
#'   priors = priors,
#'   n_alts = 3,
#'   n_q = 6,
#'   remove_dominant = TRUE,
#'   dominance_types = c("total", "partial")
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
    method = "random",
    prior_no_choice = NULL,
    max_iter = 50,
    parallel = FALSE,
    # New dominance parameters
    remove_dominant = FALSE,
    dominance_types = c("total", "partial"),
    dominance_threshold = 0.8,
    max_dominance_replacements = 10
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

  # Check if priors are required for the chosen method
  if (method == "sequential" && is.null(priors)) {
    message("The 'sequential' method works better with priors, consider creating priors with cbc_priors().")
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

  profiles_restricted <- nrow(expand.grid(get_profile_list(profiles))) > nrow(profiles)

  check_inputs_design(
    profiles,
    n_alts,
    n_q,
    n_blocks,
    n_start,
    no_choice,
    label,
    method,
    priors,
    prior_no_choice,
    max_iter,
    parallel
  )

  profiles_df <- as.data.frame(profiles)

  # Overrides ----

  # Override n_start for "random" method
  if ((method == "random") & (n_start > 1)) {
    n_start <- 1
  }

  # Override randomize_alts for labeled designs
  if (!is.null(label) & randomize_alts & method != 'random') {
    message(
      "Alternative order randomization is disabled for labeled designs.\n",
      "Setting randomize_alts <- FALSE\n"
    )
    randomize_alts <- FALSE
  }

  # Override n_blocks and priors for "random" method
  if (method == "random") {
    if (!is.null(priors)) {
      message(
        'priors are ignored for designs using the "random" method.\n',
        "Setting prior <- NULL\n"
      )
      priors <- NULL
    }
  }

  # Create design ----
  if (method == 'sequential') {
    result <- make_design_sequential(
      profiles_df, n_blocks, n_alts, n_q, n_blocks,
      priors, max_iter, label, n_start
    )
    design <- result$design
    d_error <- result$d_error
  } else {
    design <- make_random_survey(
      profiles_df, n_blocks, n_resp = n_blocks, n_alts, n_q, label
    )
    d_error <- cbc_error(design, errors = "d", priors = priors)
  }

  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }

  # Post-generation dominance checking and replacement
  if (remove_dominant && !is.null(priors)) {
    message("Checking for dominant choice sets...")
    design <- remove_dominant_choice_sets(
      design = design,
      profiles = profiles_df,
      priors = priors,
      dominance_types = dominance_types,
      dominance_threshold = dominance_threshold,
      max_replacements = max_dominance_replacements,
      n_alts = n_alts,
      label = label
    )

    # Recalculate D-error after dominance removal
    d_error <- cbc_error(design, errors = 'd', priors)
    message("Dominance checking complete. Final D-error: ", round(d_error, 6))
  }

  # Calculate efficiency metrics
  efficiency_info <- calculate_efficiency_metrics(design, profiles, priors)

  # Create design metadata
  design_info <- list(
    created_at = Sys.time(),
    n_profiles_available = nrow(profiles),
    n_profiles_used = length(unique(design$profileID)),
    profile_usage_rate = length(unique(design$profileID)) / nrow(profiles),
    n_choice_sets = n_blocks * n_q,
    total_alternatives = nrow(design),
    restrictions_applied = !is.null(attr(profiles, "restrictions_applied")),
    n_restrictions = length(attr(profiles, "restrictions_applied") %||% character(0)),
    dominance_removed = remove_dominant,
    dominance_types = if (remove_dominant) dominance_types else NULL,
    efficiency = efficiency_info
  )

  # Create return object
  result <- list(
    design = design,
    profiles = profiles,
    priors = priors,
    method = method,
    n_q = n_q,
    n_alts = n_alts,
    n_blocks = n_blocks,
    no_choice = no_choice,
    label = label,
    d_error = d_error,
    design_info = design_info
  )

  # Set class and return
  class(result) <- c("cbc_design", "list")
  return(result)
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
remove_dominant_choice_sets <- function(
    design,
    profiles,
    priors,
    dominance_types = c("total", "partial"),
    dominance_threshold = 0.8,
    max_replacements = 10,
    n_alts,
    label = NULL
) {

  replacements_made <- 0

  while (replacements_made < max_replacements) {
    # Set profiles attribute for validation
    attr(design, "profiles") <- profiles

    # Check for dominance using the S3 method
    flagged <- cbc_inspect_dominance(
      design,
      priors = priors,
      total_threshold = dominance_threshold,
      exclude = NULL
    )

    # Determine which choice sets to replace based on dominance_types
    replace_mask <- rep(FALSE, nrow(flagged))

    if ("total" %in% dominance_types) {
      replace_mask <- replace_mask | flagged$dominant_total
    }
    if ("partial" %in% dominance_types) {
      replace_mask <- replace_mask | flagged$dominant_partial
    }

    # Get unique question IDs that need replacement
    questions_to_replace <- unique(flagged$qID[replace_mask])

    if (length(questions_to_replace) == 0) {
      # No more dominant choice sets
      message("No dominant choice sets found.")
      break
    }

    # Replace each dominant choice set
    for (qID in questions_to_replace) {
      design <- replace_single_choice_set(
        design = design,
        qID = qID,
        profiles = profiles,
        n_alts = n_alts,
        label = label
      )
    }

    replacements_made <- replacements_made + 1

    # Print progress
    message(sprintf("Dominance replacement %d: Replaced %d choice sets",
                    replacements_made, length(questions_to_replace)))
  }

  if (replacements_made >= max_replacements) {
    warning(sprintf(
      "Reached maximum dominance replacements (%d). Some dominant choice sets may remain.",
      max_replacements
    ))
  }

  # Clean up the temporary profiles attribute
  attr(design, "profiles") <- NULL

  return(design)
}

#' Replace a single choice set with new random draws
#'
#' @param design The design data frame
#' @param qID The question ID to replace
#' @param profiles The profiles to sample from
#' @param n_alts Number of alternatives per question
#' @param label Label variable for labeled designs
#' @return Updated design with the specified choice set replaced
#' @keywords internal
replace_single_choice_set <- function(design, qID, profiles, n_alts, label = NULL) {
  # Get rows for this question
  question_rows <- which(design$qID == qID)

  # Generate new profiles for this choice set
  max_attempts <- 100
  attempts <- 0

  while (attempts < max_attempts) {
    if (is.null(label)) {
      # Random sampling without label constraints
      new_profiles <- sample_profiles(profiles, size = n_alts)
    } else {
      # Labeled design sampling
      n_alts_adjusted <- override_label_alts(profiles, label, n_alts)
      labels <- split(profiles, profiles[[label]])
      new_profiles <- sample_profiles_by_group(labels, size = 1)
    }

    # Check that we don't have duplicate profiles within this choice set
    if (length(unique(new_profiles$profileID)) == nrow(new_profiles)) {
      break
    }
    attempts <- attempts + 1
  }

  if (attempts >= max_attempts) {
    warning("Could not find non-duplicate profiles for choice set replacement. Using current attempt.")
  }

  # Replace the profiles while preserving metadata columns
  metadata_cols <- c("profileID", "blockID", "respID", "qID", "altID", "obsID")
  attr_cols <- setdiff(names(design), metadata_cols)

  # Update the design with new profiles
  design[question_rows, c("profileID", attr_cols)] <- new_profiles[, c("profileID", attr_cols)]

  return(design)
}

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
    design_df <- design$design
    profiles <- design$profiles
    priors <- design$priors

    cat("CBC Design Attribute Information\n")
    cat("================================\n")
    cat(sprintf("Design method: %s\n", design$method))
    if (!is.null(design$d_error)) {
      cat(sprintf("D-error: %.6f\n", design$d_error))
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
    n_start = 1
) {
  # Set up parallel processing
  n_cores <- set_num_cores(NULL)
  message("Running ", n_start, " design searches using ", n_cores, " cores...")

  # Create list of different random starting designs
  start_designs <- lapply(1:n_start, function(i) {
    design <- make_random_survey(
      profiles, n_blocks, n_resp = n_blocks, n_alts, n_q, label
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
      "logit", "logit_draws", "get_eligible_profiles"
    ), envir = environment())

    results <- suppressMessages(suppressWarnings(
      parallel::parLapply(
        cl = cl,
        seq_along(start_designs),
        function(i) {
          result <- optimize_design(
            start_designs[[i]], profiles, priors, varNames,
            n_questions, n_alts, max_iter, n_blocks, label
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

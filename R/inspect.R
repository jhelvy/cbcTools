#' Inspect attribute balance in design
#'
#' This function prints out a summary of the individual and pairwise counts of
#' each level for each attribute across all choice questions in the design.
#' @keywords logitr mnl mxl mixed logit balance overlap
#'
#' @param x Either a `cbc_design` object created by `cbc_design()` or a data frame
#'   containing a choice experiment design
#' @param ... Additional arguments passed to methods
#' @return Invisibly returns the balance statistics, but primarily prints
#'   formatted information to the console
#' @export
#' @examples
#' library(cbcTools)
#'
#' # A simple conjoint experiment about apples
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Make a survey design from all possible profiles
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Inspect the design balance
#' cbc_inspect_balance(design)
cbc_inspect_balance <- function(x, ...) {
  UseMethod("cbc_inspect_balance")
}

#' @rdname cbc_inspect_balance
#' @export
cbc_inspect_balance.cbc_design <- function(x, ...) {
  # Extract information from attributes
  params <- attr(x, "design_params")
  summary_info <- attr(x, "design_summary")

  cat("Design Balance Report\n")
  cat("=====================\n")
  cat(sprintf("Design method: %s\n", params$method))
  if (!is.null(params$d_error)) {
    cat(sprintf("D-error: %.6f\n", params$d_error))
  }

  # Profile usage info
  cat(sprintf(
    "Profiles used: %d/%d (%.1f%%)\n",
    summary_info$n_profiles_used,
    summary_info$n_profiles_available,
    summary_info$profile_usage_rate * 100
  ))

  # Show balance score if available
  if (!is.null(summary_info$efficiency$balance_score)) {
    cat(sprintf(
      "Overall balance score: %.3f (higher is better)\n",
      summary_info$efficiency$balance_score
    ))
  }

  cat("\n")

  # Call the data frame method for detailed balance (x is now the data frame)
  result <- cbc_inspect_balance.data.frame(x, ...)

  # Add design-specific information to result
  result$design_info <- list(
    method = params$method,
    d_error = params$d_error,
    balance_score = summary_info$efficiency$balance_score,
    profiles_used = summary_info$n_profiles_used,
    profiles_available = summary_info$n_profiles_available
  )

  invisible(result)
}

#' @rdname cbc_inspect_balance
#' @export
cbc_inspect_balance.data.frame <- function(x, ...) {
  # Get attribute columns
  atts <- setdiff(
    names(x),
    c("respID", "qID", "altID", "obsID", "profileID", "blockID")
  )

  # Get counts of each individual attribute
  counts <- lapply(atts, function(attr) table(x[[attr]]))
  names(counts) <- atts

  # Get pairwise counts matrix for each pair of attributes
  if (length(atts) > 1) {
    pairs <- data.frame(utils::combn(atts, 2))
    counts_pair <- lapply(pairs, function(pair) {
      table(x[[pair[1]]], x[[pair[2]]])
    })
  } else {
    pairs <- data.frame()
    counts_pair <- list()
  }

  # Calculate balance metrics for each attribute
  balance_metrics <- calculate_balance_metrics(counts)

  # Print individual attribute level counts
  cat("=====================================\n")
  cat("Individual attribute level counts\n\n")
  for (i in seq_along(counts)) {
    attr_name <- names(counts)[i]
    cat(attr_name, ":\n", sep = "")
    print(counts[[i]])

    # Show balance metric
    metric <- balance_metrics[[attr_name]]
    cat(sprintf(
      "  Balance score: %.3f (CV: %.3f)\n",
      metric$balance_score,
      metric$cv
    ))
    cat("\n")
  }

  # Print pairwise counts if we have multiple attributes
  if (length(counts_pair) > 0) {
    cat("=====================================\n")
    cat("Pairwise attribute level counts\n\n")
    for (i in seq_along(counts_pair)) {
      pair_names <- pairs[, i]
      counts1 <- counts[[pair_names[1]]]
      counts2 <- counts[[pair_names[2]]]
      cat(paste0(pair_names, collapse = " x "), ":\n\n", sep = "")
      print(rbind(
        c(NA, counts2),
        cbind(counts1, counts_pair[[i]])
      ))
      cat("\n")
    }
  }

  # Create return object
  result <- list(
    individual_counts = counts,
    pairwise_counts = counts_pair,
    balance_metrics = balance_metrics,
    overall_balance = mean(sapply(balance_metrics, function(x) x$balance_score))
  )

  class(result) <- c("cbc_inspect_balance", "list")
  invisible(result)
}

#' Inspect attribute overlap in design
#'
#' This function prints out a summary of the amount of "overlap" across
#' attributes within the choice questions. For example, for each attribute, the
#' count under `"1"` is the number of choice questions in which the same level
#' was shown across all alternatives for that attribute (because there was only
#' one level shown). Likewise, the count under `"2"` is the number of choice
#' questions in which only two unique levels of that attribute were shown, and
#' so on.
#' @param x Either a `cbc_design` object created by `cbc_design()` or a data frame
#'   containing a choice experiment design
#' @param ... Additional arguments passed to methods
#' @return Invisibly returns the overlap statistics, but primarily prints
#'   formatted information to the console
#' @export
#' @examples
#' library(cbcTools)
#'
#' # A simple conjoint experiment about apples
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   freshness = c("Excellent", "Average", "Poor"),
#'   type      = c("Fuji", "Gala", "Honeycrisp")
#' )
#'
#' # Make a randomized survey design
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts   = 3, # Number of alternatives per question
#'   n_q      = 6 # Number of questions per respondent
#' )
#'
#' # Inspect the design overlap
#' cbc_inspect_overlap(design)
cbc_inspect_overlap <- function(x, ...) {
  UseMethod("cbc_inspect_overlap")
}

#' @rdname cbc_inspect_overlap
#' @export
cbc_inspect_overlap.cbc_design <- function(x, ...) {
  # Extract information from attributes
  params <- attr(x, "design_params")
  summary_info <- attr(x, "design_summary")

  cat("Design Overlap Report\n")
  cat("=====================\n")
  cat(sprintf("Design method: %s\n", params$method))
  if (!is.null(params$d_error)) {
    cat(sprintf("D-error: %.6f\n", params$d_error))
  }

  # Show overlap score if available
  if (!is.null(summary_info$efficiency$overlap_score)) {
    cat(sprintf(
      "Overall overlap score: %.3f (lower is better)\n",
      summary_info$efficiency$overlap_score
    ))
  }

  cat("\n")

  # Call the data frame method for detailed overlap (x is now the data frame)
  result <- cbc_inspect_overlap.data.frame(x, ...)

  # Add design-specific information to result
  result$design_info <- list(
    method = params$method,
    d_error = params$d_error,
    overlap_score = summary_info$efficiency$overlap_score,
    n_choice_sets = summary_info$n_choice_sets
  )

  invisible(result)
}

#' @rdname cbc_inspect_overlap
#' @export
cbc_inspect_overlap.data.frame <- function(x, ...) {
  # Get attribute columns
  atts <- setdiff(
    names(x),
    c("respID", "qID", "altID", "obsID", "profileID", "blockID")
  )

  # Calculate overlap for each attribute
  overlap_counts <- lapply(atts, function(attr) get_att_overlap_counts(attr, x))
  names(overlap_counts) <- atts

  # Calculate overlap metrics
  overlap_metrics <- calculate_overlap_metrics(overlap_counts, x)

  cat("==============================\n")
  cat("Counts of attribute overlap:\n")
  cat("(# of questions with N unique levels)\n\n")

  total_questions <- max(x$obsID, na.rm = TRUE)

  for (i in seq_along(overlap_counts)) {
    attr_name <- atts[i]
    attr_data <- overlap_counts[[i]]

    cat(attr_name, ": ", sep = "")

    if (attr_data$type == "continuous") {
      cat("Continuous variable\n")
      cat("  Unique levels: ", paste(names(attr_data$value_counts), collapse = ", "), "\n")
    } else {
      cat("Categorical variable\n")
      cat("  Levels: ", paste(attr_data$max_possible_unique), " (",
          paste(unique(x[[attr_name]]), collapse = ", "), ")\n", sep = "")
    }

    cat("  Questions by # unique levels:\n")

    # Get the overlap distribution
    unique_counts <- attr_data$unique_per_question
    max_levels <- attr_data$max_possible_unique

    # Process each level count
    for (level in 1:max_levels) {
      level_str <- as.character(level)
      count <- if (level_str %in% names(unique_counts)) unique_counts[[level_str]] else 0
      percentage <- (count / total_questions) * 100

      # Create descriptive labels
      if (level == 1) {
        label <- " (complete overlap): "
      } else if (level == max_levels) {
        label <- " (no overlap):       "
      } else {
        label <- " (partial overlap):  "
      }

      cat(sprintf("  %d%s%5.1f%%  (%d / %d questions)\n",
                  level, label, percentage, count, total_questions))
    }

    # Show average unique levels
    metric <- overlap_metrics[[attr_name]]
    cat(sprintf("  Average unique levels per question: %.2f\n",
                metric$avg_unique_levels))
    cat("\n")
  }

  # Create return object
  result <- list(
    overlap_counts = overlap_counts,
    overlap_metrics = overlap_metrics,
    overall_overlap = mean(sapply(overlap_metrics, function(x) {
      x$complete_overlap_rate
    }))
  )

  class(result) <- c("cbc_inspect_overlap", "list")
  invisible(result)
}

#' Compare designs and surveys across multiple metrics
#'
#' This function compares multiple designs and/or surveys across various efficiency metrics
#' including error criteria, balance, overlap, and profile usage. Provides
#' flexible error type selection and comprehensive or simplified output.
#'
#' @param ... Multiple `cbc_design` or `cbc_survey` objects to compare, or named
#'   arguments where each argument is a design or survey object
#' @param errors Character vector specifying which error metrics to compute.
#'   Options: "d" (D-error), "a" (A-error), "g" (G-error), "e" (E-error),
#'   "all" (comprehensive metrics). Defaults to "d". When priors contain
#'   random parameters, Bayesian versions are automatically computed.
#' @param include_metrics Logical. Include balance, overlap, and profile usage
#'   metrics in addition to error metrics? Defaults to TRUE for comprehensive
#'   comparison. Set to FALSE for error-only comparison.
#' @param priors Optional `cbc_priors` object to use for all designs/surveys. If not
#'   specified, each object's own priors will be used.
#' @param exclude Character vector of attribute names to exclude from error
#'   calculations
#' @return A data frame comparing metrics across designs, sorted by primary
#'   error metric (D-error by default)
#' @export
#' @examples
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C")
#' )
#'
#' # Create different designs
#' design_random <- cbc_design(profiles, n_alts = 2, n_q = 4, method = "random")
#'
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3)
#' )
#'
#' design_sequential <- cbc_design(
#'   profiles, priors = priors, n_alts = 2, n_q = 4, method = "sequential"
#' )
#'
#' # Comprehensive comparison (default)
#' cbc_compare(
#'   Random = design_random,
#'   Sequential = design_sequential
#' )
#'
#' # Multiple error types with full metrics
#' cbc_compare(
#'   Random = design_random,
#'   Sequential = design_sequential,
#'   errors = c("d", "a", "g")
#' )
#'
#' # Error-only comparison
#' cbc_compare(
#'   Random = design_random,
#'   Sequential = design_sequential,
#'   errors = c("d", "a"),
#'   include_metrics = FALSE
#' )
#'
#' # Compare surveys
#' survey1 <- cbc_survey(design_random, n_resp = 100)
#' survey2 <- cbc_survey(design_sequential, n_resp = 100)
#' cbc_compare(Sequential = survey1, Random = survey2)
cbc_compare <- function(..., errors = "d", include_metrics = TRUE,
                        priors = NULL, exclude = NULL) {
    objects <- list(...)

    # Get object names
    object_names <- names(objects)
    if (is.null(object_names)) {
        object_names <- paste0("Object_", seq_along(objects))
    }

    # Validate all inputs are cbc_design or cbc_survey objects
    for (i in seq_along(objects)) {
        if (!inherits(objects[[i]], c("cbc_design", "cbc_survey"))) {
            stop(sprintf(
                "Argument %d (%s) must be a cbc_design or cbc_survey object",
                i,
                object_names[i]
            ))
        }
    }

    # Compute error metrics for each object
    error_metrics <- lapply(objects, function(obj) {
        if (length(errors) == 1 && errors[1] == "d") {
            # Single error type returns scalar
            error_val <- cbc_error(obj, errors = errors, priors = priors, exclude = exclude)
            result <- list()
            result[[paste0(errors[1], "_error")]] <- error_val
            return(result)
        } else {
            # Multiple error types or "all" returns list
            return(cbc_error(obj, errors = errors, priors = priors, exclude = exclude))
        }
    })

    # Extract design metrics if needed
    design_metrics <- if (include_metrics) {
        lapply(objects, function(obj) {
            extract_metrics(obj)
        })
    } else {
        NULL
    }

    # Build result data frame
    result <- data.frame(design = object_names, stringsAsFactors = FALSE)

    # Add method column if including metrics
    if (include_metrics) {
        result$method <- sapply(design_metrics, function(x) x$method)
    }

    # Add error columns
    error_names <- if ("all" %in% errors) {
        c("d_error", "a_error", "g_error", "e_error")
    } else {
        paste0(errors, "_error")
    }

    for (error_name in error_names) {
        result[[error_name]] <- sapply(error_metrics, function(x) {
            x[[error_name]] %||% NA
        })
    }

    # Add design metrics if requested
    if (include_metrics) {
        result$balance_score <- sapply(design_metrics, function(x) x$balance_score)
        result$overlap_score <- sapply(design_metrics, function(x) x$overlap_score)
        result$profile_usage_rate <- sapply(design_metrics, function(x) x$profile_usage_rate)
        result$profiles_used <- sapply(design_metrics, function(x) x$n_profiles_used)
        result$profiles_available <- sapply(design_metrics, function(x) x$n_profiles_available)
    }

    # Sort by primary error metric (first one specified, or d_error)
    primary_error <- if (errors[1] == "all") "d_error" else paste0(errors[1], "_error")
    result <- result[order(result[[primary_error]], na.last = TRUE), ]
    rownames(result) <- NULL

    # Add rankings for error metrics
    for (error_name in error_names) {
        if (error_name %in% names(result)) {
            rank_name <- gsub("_error", "_rank", error_name)
            if (error_name == "e_error") {
                result[[rank_name]] <- rank(-result[[error_name]], na.last = TRUE) # Higher is better for E-error
            } else {
                result[[rank_name]] <- rank(result[[error_name]], na.last = TRUE) # Lower is better for others
            }
        }
    }

    # Add rankings for design metrics if included
    if (include_metrics) {
        result$balance_rank <- rank(-result$balance_score, na.last = TRUE) # Higher is better
        result$overlap_rank <- rank(result$overlap_score, na.last = TRUE) # Lower is better
    }

    # Set appropriate class
    if (include_metrics) {
        class(result) <- c("cbc_comparison", "data.frame")
    } else {
        class(result) <- c("cbc_error_comparison", "data.frame")
    }

    return(result)
}

# Helper function to extract or calculate metrics from objects
extract_metrics <- function(obj) {
    if (inherits(obj, "cbc_design")) {
        # For designs, extract from stored attributes
        params <- attr(obj, "design_params")
        summary_info <- attr(obj, "design_summary")

        return(list(
            method = params$method %||% "unknown",
            balance_score = summary_info$efficiency$balance_score %||% NA,
            overlap_score = summary_info$efficiency$overlap_score %||% NA,
            profile_usage_rate = summary_info$profile_usage_rate %||% NA,
            n_profiles_used = summary_info$n_profiles_used %||% NA,
            n_profiles_available = summary_info$n_profiles_available %||% NA
        ))

    } else if (inherits(obj, "cbc_survey")) {
        # For surveys, extract from pre-calculated efficiency metadata
        survey_info <- attr(obj, "survey_info")
        method <- survey_info$design_method %||% "unknown"

        # Extract from pre-calculated efficiency metrics (stored at survey creation)
        efficiency <- survey_info$stats$efficiency

        return(list(
            method = method,
            balance_score = efficiency$balance_score %||% NA,
            overlap_score = efficiency$overlap_score %||% NA,
            profile_usage_rate = efficiency$profile_usage_rate %||% NA,
            n_profiles_used = efficiency$profiles_used %||% NA,
            n_profiles_available = efficiency$profiles_available %||% NA
        ))

    } else {
        # Fallback for unknown object types
        return(list(
            method = "unknown",
            balance_score = NA,
            overlap_score = NA,
            profile_usage_rate = NA,
            n_profiles_used = NA,
            n_profiles_available = NA
        ))
    }
}

# Helper function to calculate profile usage from survey data
calculate_profile_usage_from_data <- function(survey) {
    # Get unique profiles used in survey
    unique_profiles_used <- length(unique(survey$profileID))

    # Try to get total available profiles from base design or survey info
    base_design <- attr(survey, "base_design")
    if (!is.null(base_design)) {
        profiles <- attr(base_design, "profiles")
        total_available <- nrow(profiles)
    } else {
        survey_info <- attr(survey, "survey_info")
        total_available <- survey_info$stats$total_profiles_available %||% unique_profiles_used
    }

    usage_rate <- unique_profiles_used / total_available

    return(list(
        n_used = unique_profiles_used,
        n_available = total_available,
        usage_rate = usage_rate
    ))
}

#' Print method for comparisons
#' @param x A cbc_comparison or cbc_error_comparison object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_comparison <- function(x, ...) {
    is_error_only <- inherits(x, "cbc_error_comparison")

    if (is_error_only) {
        cat("CBC Error Comparison\n")
        cat("====================\n\n")
    } else {
        cat("CBC Comparison\n")
        cat("==============\n\n")
    }

    # Determine which columns to show
    base_cols <- "design"
    error_cols <- names(x)[grepl("_error$", names(x))]

    if (is_error_only) {
        display_cols <- c(base_cols, error_cols)
    } else {
        # Include method and design metrics
        design_cols <- c("method", "balance_score", "overlap_score", "profile_usage_rate")
        display_cols <- c(base_cols, intersect(design_cols, names(x)), error_cols)
    }

    # Create formatted version for printing
    print_df <- x[, display_cols, drop = FALSE]

    # Format error columns
    for (col in error_cols) {
        if (col %in% names(print_df)) {
            print_df[[col]] <- sprintf("%.6f", print_df[[col]])
        }
    }

    # Format other numeric columns if present
    if ("balance_score" %in% names(print_df)) {
        print_df$balance_score <- ifelse(
            is.na(x$balance_score),
            "NA",
            sprintf("%.3f", print_df$balance_score)
        )
    }

    if ("overlap_score" %in% names(print_df)) {
        print_df$overlap_score <- ifelse(
            is.na(x$overlap_score),
            "NA",
            sprintf("%.3f", print_df$overlap_score)
        )
    }

    if ("profile_usage_rate" %in% names(print_df)) {
        print_df$profile_usage_rate <- ifelse(
            is.na(x$profile_usage_rate),
            "NA",
            sprintf("%.1f%%", x$profile_usage_rate * 100)
        )
    }

    # Rename columns for display
    display_names <- names(print_df)
    display_names[display_names == "design"] <- "Design"
    display_names[display_names == "method"] <- "Method"
    display_names[display_names == "d_error"] <- "D-Error"
    display_names[display_names == "a_error"] <- "A-Error"
    display_names[display_names == "g_error"] <- "G-Error"
    display_names[display_names == "e_error"] <- "E-Error"
    display_names[display_names == "balance_score"] <- "Balance"
    display_names[display_names == "overlap_score"] <- "Overlap"
    display_names[display_names == "profile_usage_rate"] <- "Profile Usage"
    names(print_df) <- display_names

    # Remove class to avoid recursion
    class(print_df) <- "data.frame"
    print(print_df, row.names = FALSE)

    # Add interpretation
    cat("\nInterpretation:\n")
    if ("D-Error" %in% names(print_df)) cat("- D-Error: Lower is better (design efficiency)\n")
    if ("A-Error" %in% names(print_df)) cat("- A-Error: Lower is better (average variance)\n")
    if ("G-Error" %in% names(print_df)) cat("- G-Error: Lower is better (max prediction variance)\n")
    if ("E-Error" %in% names(print_df)) cat("- E-Error: Higher is better (min eigenvalue)\n")
    if ("Balance" %in% names(print_df)) cat("- Balance: Higher is better (level distribution)\n")
    if ("Overlap" %in% names(print_df)) cat("- Overlap: Lower is better (attribute variation)\n")
    if ("Profile Usage" %in% names(print_df)) cat("- Profile Usage: Higher means more profiles used\n")

    # Highlight best performing design for each metric
    if (nrow(x) > 1) {
        cat("\nBest performers:\n")

        for (error_col in error_cols) {
            if (error_col %in% names(x) && !all(is.na(x[[error_col]]))) {
                if (error_col == "e_error") {
                    best_idx <- which.max(x[[error_col]])
                    best_val <- max(x[[error_col]], na.rm = TRUE)
                } else {
                    best_idx <- which.min(x[[error_col]])
                    best_val <- min(x[[error_col]], na.rm = TRUE)
                }

                error_name <- gsub("_error", "", error_col)
                error_name <- paste0(toupper(substring(error_name, 1, 1)), substring(error_name, 2))

                cat(sprintf(
                    "- %s-Error: %s (%.6f)\n",
                    error_name,
                    x$design[best_idx],
                    best_val
                ))
            }
        }

        if (!is_error_only) {
            if ("balance_score" %in% names(x) && !all(is.na(x$balance_score))) {
                best_balance <- x$design[which.max(x$balance_score)]
                cat(sprintf(
                    "- Balance: %s (%.3f)\n",
                    best_balance,
                    max(x$balance_score, na.rm = TRUE)
                ))
            }

            if ("overlap_score" %in% names(x) && !all(is.na(x$overlap_score))) {
                best_overlap <- x$design[which.min(x$overlap_score)]
                cat(sprintf(
                    "- Overlap: %s (%.3f)\n",
                    best_overlap,
                    min(x$overlap_score, na.rm = TRUE)
                ))
            }
        }
    }

    invisible(x)
}

#' @rdname print.cbc_comparison
#' @export
print.cbc_error_comparison <- function(x, ...) {
    print.cbc_comparison(x, ...)
}

# Helper functions

calculate_balance_metrics <- function(counts) {
  metrics <- list()

  for (attr in names(counts)) {
    attr_counts <- counts[[attr]]
    expected_count <- sum(attr_counts) / length(attr_counts)

    # Calculate coefficient of variation
    cv <- sd(attr_counts) / mean(attr_counts)

    # Balance score: higher is better, max = 1 (perfect balance)
    balance_score <- 1 / (1 + cv)

    metrics[[attr]] <- list(
      balance_score = balance_score,
      cv = cv,
      expected_count = expected_count,
      actual_counts = attr_counts
    )
  }

  return(metrics)
}

calculate_overlap_metrics <- function(overlap_counts, design) {
  metrics <- list()
  total_questions <- max(design$obsID, na.rm = TRUE)

  for (attr in names(overlap_counts)) {
    attr_data <- overlap_counts[[attr]]

    # Extract the unique_per_question counts regardless of continuous/categorical
    if (attr_data$type == "continuous") {
      counts <- attr_data$unique_per_question
    } else {
      counts <- attr_data$unique_per_question
    }

    # Complete overlap is when only 1 unique level appears
    complete_overlap_count <- if ("1" %in% names(counts)) counts[["1"]] else 0
    complete_overlap_rate <- complete_overlap_count / total_questions

    # Average unique levels per question
    unique_levels <- as.numeric(names(counts))
    question_counts <- as.numeric(counts)
    avg_unique_levels <- sum(unique_levels * question_counts) /
      sum(question_counts)

    metrics[[attr]] <- list(
      complete_overlap_count = complete_overlap_count,
      complete_overlap_rate = complete_overlap_rate,
      avg_unique_levels = avg_unique_levels,
      total_questions = total_questions,
      distribution = counts,
      max_possible = attr_data$max_possible_unique
    )
  }

  return(metrics)
}

# Helper function - updated to handle the new display format
get_att_overlap_counts <- function(attr_name, design) {
  # Check if this is a continuous variable
  values <- design[[attr_name]]
  is_continuous <- is.numeric(values)

  if (is_continuous) {
    # For continuous variables, show the actual unique values that appear
    # and how many questions each appears in
    unique_vals <- sort(unique(values))

    # Count how many times each unique value appears
    val_counts <- table(values)

    # Also calculate how many unique values appear per question
    unique_per_q <- tapply(
      design[[attr_name]],
      design$obsID,
      FUN = function(x) length(unique(x))
    )
    unique_counts <- table(unique_per_q)

    # Return both pieces of information
    return(list(
      type = "continuous",
      value_counts = val_counts,
      unique_per_question = unique_counts,
      total_unique_values = length(unique_vals),
      max_possible_unique = length(unique_vals)
    ))
  } else {
    # For categorical variables, use the original logic
    counts <- tapply(
      design[[attr_name]],
      design$obsID,
      FUN = function(x) length(unique(x))
    )
    unique_counts <- table(counts)

    # Also get the total possible levels
    total_levels <- length(unique(values))

    return(list(
      type = "categorical",
      unique_per_question = unique_counts,
      max_possible_unique = total_levels
    ))
  }
}

#' Inspect design for dominant choice sets
#'
#' This function identifies choice sets where one alternative dominates others
#' either through high total utility or by having the best partial utility
#' across all attributes. Dominant choice sets provide little information
#' and may reduce the efficiency of the design.
#'
#' @param x Either a `cbc_design` object created by `cbc_design()`, a `cbc_survey`
#'   object, or a data frame containing a choice experiment design
#' @param priors A `cbc_priors` object created by `cbc_priors()` that will be
#'   used to compute utilities. Required for dominance detection.
#' @param total_threshold Numeric threshold for total utility dominance detection.
#'   If one alternative has a choice probability above this threshold, the choice
#'   set is flagged. Default is 0.8.
#' @param exclude Character vector of attribute names to exclude from dominance
#'   calculations
#' @return The original design with two additional columns:
#'   \itemize{
#'     \item dominant_total: Logical indicating if choice set has total utility dominance
#'     \item dominant_partial: Logical indicating if choice set has partial utility dominance
#'   }
#' @details
#' Two types of dominance are detected:
#' \itemize{
#'   \item **Total utility dominance**: One alternative has a choice probability
#'         above the threshold when all attributes are considered together
#'   \item **Partial utility dominance**: One alternative has the highest partial
#'         utility for every single attribute (regardless of total utility)
#' }
#'
#' Partial utility dominance can occur even when total utility dominance doesn't,
#' particularly when the dominant alternative isn't much better overall but is
#' consistently better across all attributes.
#' @export
#' @examples
#' # Create profiles with clear dominance potential
#' profiles <- cbc_profiles(
#'   price = c(10, 20, 30),
#'   quality = c("Low", "Medium", "High"),
#'   warranty = c(1, 2, 3)
#' )
#'
#' # Create priors favoring lower price, higher quality, longer warranty
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.1,        # negative: prefer lower prices
#'   quality = c(0.5, 1.0), # prefer Medium and High over Low
#'   warranty = 0.3       # positive: prefer longer warranty
#' )
#'
#' design <- cbc_design(profiles, n_alts = 3, n_q = 8, priors = priors)
#'
#' # Inspect for dominance
#' design_flagged <- cbc_inspect_dominance(design, priors)
#'
#' # Check results
#' table(design_flagged$dominant_total)
#' table(design_flagged$dominant_partial)
cbc_inspect_dominance <- function(
  x,
  priors,
  total_threshold = 0.8,
  exclude = NULL
) {
  UseMethod("cbc_inspect_dominance")
}

#' @rdname cbc_inspect_dominance
#' @export
cbc_inspect_dominance.cbc_design <- function(
  x,
  priors,
  total_threshold = 0.8,
  exclude = NULL
) {
  # x is now the data frame directly, profiles are already an attribute
  result <- cbc_inspect_dominance.data.frame(
    x,
    priors,
    total_threshold,
    exclude
  )

  # Copy over the design attributes to the result
  attr(result, "profiles") <- attr(x, "profiles")
  attr(result, "priors") <- attr(x, "priors")
  attr(result, "design_params") <- attr(x, "design_params")
  attr(result, "design_summary") <- attr(x, "design_summary")

  return(result)
}

#' @rdname cbc_inspect_dominance
#' @export
cbc_inspect_dominance.cbc_survey <- function(
  x,
  priors,
  total_threshold = 0.8,
  exclude = NULL
) {
  # Apply to the survey data frame and return it with flags
  cbc_inspect_dominance.data.frame(x, priors, total_threshold, exclude)
}

#' @rdname cbc_inspect_dominance
#' @export
cbc_inspect_dominance.data.frame <- function(
    x,
    priors,
    total_threshold = 0.8,
    exclude = NULL
) {
  # Validate inputs
  if (!inherits(priors, "cbc_priors")) {
    stop("priors must be a cbc_priors object created by cbc_priors()")
  }

  if (!all(get_id_names() %in% names(x))) {
    stop(
      "Design must be created by cbc_design() or contain the required ID columns: ",
      paste(get_id_names(), collapse = ", ")
    )
  }

  if (total_threshold <= 0 || total_threshold >= 1) {
    stop("total_threshold must be between 0 and 1")
  }

  # Get attribute columns
  attr_cols <- get_var_names(x)

  # Remove excluded attributes
  if (!is.null(exclude)) {
    attr_cols <- setdiff(attr_cols, exclude)
  }
  if (length(attr_cols) == 0) {
    stop("No attribute columns found in design after exclusions")
  }

  # Validate priors against design - only if profiles attribute is available
  profiles_attr <- attr(x, "profiles")
  if (!is.null(profiles_attr) && inherits(profiles_attr, "cbc_profiles")) {
    validate_priors_profiles(priors, profiles_attr)
  }

  # Get random parameters for encoding
  randPars <- get_rand_pars(priors)

  # Encode the design matrix
  codedData <- logitr::recodeData(x, attr_cols, randPars)
  X <- codedData$X

  # Split by choice questions
  questions <- split(x, x$qID)
  obsID_list <- split(seq_len(nrow(x)), x$qID)

  # Initialize flags
  x$dominant_total <- FALSE
  x$dominant_partial <- FALSE

  # Check each choice question
  for (q_name in names(questions)) {
    q_data <- questions[[q_name]]
    q_indices <- obsID_list[[q_name]]

    # Get design matrix for this question
    X_q <- X[q_indices, , drop = FALSE]

    # Compute total utilities
    total_utilities <- X_q %*% priors$pars

    # Convert to probabilities using logit model
    exp_utils <- exp(total_utilities)
    choice_probs <- exp_utils / sum(exp_utils)

    # Check total utility dominance
    max_prob <- max(choice_probs)
    has_total_dominance <- max_prob > total_threshold

    # Check partial utility dominance
    has_partial_dominance <- check_partial_dominance(
      q_data,
      X_q,
      priors,
      attr_cols
    )

    # Flag all rows in this choice question
    x$dominant_total[q_indices] <- has_total_dominance
    x$dominant_partial[q_indices] <- has_partial_dominance
  }

  # Add summary attributes
  n_total_dominant <- length(unique(x$qID[x$dominant_total]))
  n_partial_dominant <- length(unique(x$qID[x$dominant_partial]))
  n_total_questions <- length(unique(x$qID))

  attr(x, "dominance_summary") <- list(
    total_dominant_questions = n_total_dominant,
    partial_dominant_questions = n_partial_dominant,
    total_questions = n_total_questions,
    total_threshold = total_threshold
  )

  class(x) <- c("cbc_design_flagged", class(x))
  return(x)
}

#' Print method for flagged designs
#' @param x A cbc_design_flagged object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_design_flagged <- function(x, ...) {
  # Get summary information
  summary_info <- attr(x, "dominance_summary")

  cat("CBC Design with Dominance Flags\n")
  cat("===============================\n\n")

  if (!is.null(summary_info)) {
    cat("Dominance Detection Summary:\n")
    cat(sprintf(
      "  Total questions:           %d\n",
      summary_info$total_questions
    ))
    cat(sprintf(
      "  Total dominance flagged:   %d (%.1f%%)\n",
      summary_info$total_dominant_questions,
      100 * summary_info$total_dominant_questions / summary_info$total_questions
    ))
    cat(sprintf(
      "  Partial dominance flagged: %d (%.1f%%)\n",
      summary_info$partial_dominant_questions,
      100 *
        summary_info$partial_dominant_questions /
        summary_info$total_questions
    ))
    cat(sprintf(
      "  Total threshold used:      %.2f\n",
      summary_info$total_threshold
    ))
    cat("\n")
  }

  # Show breakdown by question
  if (nrow(x) > 0) {
    question_summary <- aggregate(
      cbind(dominant_total, dominant_partial) ~ qID,
      data = x,
      FUN = function(x) x[1] # Take first value (all should be same within question)
    )

    cat("Questions flagged:\n")
    flagged_questions <- question_summary[
      question_summary$dominant_total | question_summary$dominant_partial,
    ]

    if (nrow(flagged_questions) > 0) {
      for (i in seq_len(nrow(flagged_questions))) {
        q <- flagged_questions[i, ]
        flags <- c()
        if (q$dominant_total) {
          flags <- c(flags, "total")
        }
        if (q$dominant_partial) {
          flags <- c(flags, "partial")
        }
        cat(sprintf(
          "  Q%s: %s dominance\n",
          q$qID,
          paste(flags, collapse = " & ")
        ))
      }
    } else {
      cat("  None\n")
    }
    cat("\n")
  }

  cat("Note: Use remove_dominant=TRUE in cbc_design() or cbc_survey_random() to prevent dominant choice sets\n")

  # Call the regular print method for the underlying object
  NextMethod("print")
}

# Backward compatibility aliases

#' @rdname cbc_inspect_balance
#' @export
cbc_balance <- function(x, ...) {
  cbc_inspect_balance(x, ...)
}

#' @rdname cbc_inspect_overlap
#' @export
cbc_overlap <- function(x, ...) {
  cbc_inspect_overlap(x, ...)
}

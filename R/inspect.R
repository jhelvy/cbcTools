#' Inspect design quality metrics
#'
#' This function provides comprehensive inspection of choice experiment designs
#' across multiple quality dimensions including attribute balance, overlap, and
#' dominance patterns.
#'
#' @param design A `cbc_design` object created by `cbc_design()`
#' @param metrics Character vector specifying which metrics to inspect.
#'   Options: "balance", "overlap", "dominance", or "all" (default).
#'   Can specify multiple: `c("balance", "overlap")`
#' @param verbose Logical. If TRUE, shows detailed technical metrics like
#'   coefficient of variation. If FALSE (default), shows simplified output.
#' @param priors A `cbc_priors` object created by `cbc_priors()`. Required
#'   for dominance inspection.
#' @param total_threshold Numeric threshold for total utility dominance detection.
#'   Default is 0.8.
#' @param exclude Character vector of attribute names to exclude from calculations
#' @return Invisibly returns the inspection results, but primarily prints
#'   formatted information to the console
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles and design
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C"),
#'   quality = c("Low", "High")
#' )
#'
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # Inspect all metrics (default, simple output)
#' cbc_inspect(design)
#'
#' # Detailed output with technical metrics
#' cbc_inspect(design, verbose = TRUE)
#'
#' # Inspect specific metrics
#' cbc_inspect(design, metrics = "balance")
#' cbc_inspect(design, metrics = c("balance", "overlap"))
#'
#' # Inspect dominance (requires priors)
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3),
#'   quality = 0.4
#' )
#' cbc_inspect(design, metrics = "dominance", priors = priors)
cbc_inspect <- function(design, metrics = "all", verbose = FALSE, priors = NULL,
                        total_threshold = 0.8, exclude = NULL) {

  # Validate inputs
  if (!inherits(design, "cbc_design")) {
    stop("design must be a cbc_design object created by cbc_design()")
  }

  # Handle "all" metrics
  if ("all" %in% metrics) {
    metrics <- c("balance", "overlap")
  }

  # Validate metrics
  valid_metrics <- c("balance", "overlap", "dominance")
  invalid_metrics <- setdiff(metrics, valid_metrics)
  if (length(invalid_metrics) > 0) {
    stop("Invalid metrics: ", paste(invalid_metrics, collapse = ", "),
         ". Valid options are: ", paste(valid_metrics, collapse = ", "))
  }

  # Extract design information
  params <- attr(design, "design_params")
  summary_info <- attr(design, "design_summary")

  # Print header
  cat("Design Quality Inspection\n")
  cat("=========================\n")
  cat(sprintf("Design method: %s\n", params$method))
  if (!is.null(params$d_error_prior)) {
    cat(sprintf("D-error: %.6f\n", params$d_error_prior))
  } else if (!is.null(params$d_error_null)) {
    cat(sprintf("D-error (null): %.6f\n", params$d_error_null))
  }
  cat("\n")

  # Initialize results list
  results <- list()

  # Inspect balance
  if ("balance" %in% metrics) {
    cat("ATTRIBUTE BALANCE\n")
    cat("=================\n")

    # Use pre-computed metrics if available
    if (!is.null(summary_info$efficiency$balance_score)) {
      cat(sprintf("Overall balance score: %.3f (higher is better)\n\n",
                  summary_info$efficiency$balance_score))

      # Show detailed balance from pre-computed data
      balance_details <- summary_info$efficiency$balance_details
      results$balance <- inspect_balance_detailed(design, balance_details, verbose)
    } else {
      # Compute on the fly if not pre-computed
      results$balance <- inspect_balance_detailed(design, verbose = verbose)
    }
    cat("\n")
  }

  # Inspect overlap
  if ("overlap" %in% metrics) {
    cat("ATTRIBUTE OVERLAP\n")
    cat("=================\n")

    # Use pre-computed metrics if available
    if (!is.null(summary_info$efficiency$overlap_score)) {
      cat(sprintf("Overall overlap score: %.3f (lower is better)\n\n",
                  summary_info$efficiency$overlap_score))

      # Show detailed overlap from pre-computed data
      overlap_details <- summary_info$efficiency$overlap_details
      results$overlap <- inspect_overlap_detailed(design, overlap_details, verbose)
    } else {
      # Compute on the fly if not pre-computed
      results$overlap <- inspect_overlap_detailed(design, verbose = verbose)
    }
    cat("\n")
  }

  # Add design information to results
  results$design_info <- list(
    method = params$method,
    d_error = params$d_error_prior %||% params$d_error_null,
    n_choice_sets = summary_info$n_choice_sets,
    profiles_used = summary_info$n_profiles_used,
    profiles_available = summary_info$n_profiles_available
  )

  class(results) <- c("cbc_inspection", "list")
  invisible(results)
}

# Detailed balance inspection
inspect_balance_detailed <- function(design, balance_details = NULL, verbose = FALSE) {
  if (is.null(balance_details)) {
    # Compute balance metrics
    balance_result <- compute_balance_metrics_internal(design)
    counts <- balance_result$individual_counts
    balance_metrics <- balance_result$balance_metrics
  } else {
    # Use pre-computed data - need to recompute counts for display
    atts <- setdiff(names(design), c("respID", "qID", "altID", "obsID", "profileID", "blockID"))
    counts <- lapply(atts, function(attr) table(design[[attr]]))
    names(counts) <- atts
    balance_metrics <- balance_details
  }

  # Print individual attribute level counts
  cat("Individual attribute level counts:\n")
  for (i in seq_along(counts)) {
    attr_name <- names(counts)[i]
    cat(sprintf("\n%s:\n", attr_name))
    print(counts[[i]])

    # Show balance metric with optional verbose details
    if (!is.null(balance_metrics[[attr_name]])) {
      metric <- balance_metrics[[attr_name]]
      if (verbose) {
        cat(sprintf("  Balance score: %.3f (higher is better), CV: %.3f (lower is better)\n",
                    metric$balance_score, metric$cv))
      } else {
        cat(sprintf("  Balance score: %.3f (higher is better)\n",
                    metric$balance_score))
      }
    }
  }

  return(list(
    individual_counts = counts,
    balance_metrics = balance_metrics,
    overall_balance = mean(sapply(balance_metrics, function(x) x$balance_score))
  ))
}

# Detailed overlap inspection
inspect_overlap_detailed <- function(design, overlap_details = NULL, verbose = FALSE) {
  if (is.null(overlap_details)) {
    # Compute overlap metrics
    overlap_result <- compute_overlap_metrics_internal(design)
    overlap_counts <- overlap_result$overlap_counts
    overlap_metrics <- overlap_result$overlap_metrics
  } else {
    # Use pre-computed data - need to recompute counts for display
    atts <- setdiff(names(design), c("respID", "qID", "altID", "obsID", "profileID", "blockID"))
    overlap_counts <- lapply(atts, function(attr) get_att_overlap_counts(attr, design))
    names(overlap_counts) <- atts
    overlap_metrics <- overlap_details
  }

  total_questions <- max(design$obsID, na.rm = TRUE)

  cat("Counts of attribute overlap:\n")
  cat("(# of questions with N unique levels)\n\n")

  for (i in seq_along(overlap_counts)) {
    attr_name <- names(overlap_counts)[i]
    attr_data <- overlap_counts[[i]]

    cat(sprintf("%s: ", attr_name))

    if (attr_data$type == "continuous") {
      cat("Continuous variable\n")
      if (verbose) {
        cat("  Unique levels: ", paste(names(attr_data$value_counts), collapse = ", "), "\n")
      }
    } else {
      cat("Categorical variable\n")
      if (verbose) {
        cat("  Levels: ", paste(attr_data$max_possible_unique), " (",
            paste(unique(design[[attr_name]]), collapse = ", "), ")\n", sep = "")
      }
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
    if (!is.null(overlap_metrics[[attr_name]])) {
      metric <- overlap_metrics[[attr_name]]
      cat(sprintf("  Average unique levels per question: %.2f\n",
                  metric$avg_unique_levels))
    }
    cat("\n")
  }

  return(list(
    overlap_counts = overlap_counts,
    overlap_metrics = overlap_metrics,
    overall_overlap = mean(sapply(overlap_metrics, function(x) x$complete_overlap_rate))
  ))
}

#' Print method for cbc_inspection objects
#' @param x A cbc_inspection object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_inspection <- function(x, ...) {
  cat("CBC Design Inspection Results\n")
  cat("=============================\n\n")

  # Print summary of what was inspected
  metrics_inspected <- setdiff(names(x), "design_info")
  cat("Metrics inspected:", paste(metrics_inspected, collapse = ", "), "\n")

  if (!is.null(x$design_info)) {
    info <- x$design_info
    cat(sprintf("Design method: %s\n", info$method))
    if (!is.null(info$d_error)) {
      cat(sprintf("D-error: %.6f\n", info$d_error))
    }
    cat(sprintf("Choice sets: %d\n", info$n_choice_sets))
    cat(sprintf("Profile usage: %d/%d\n", info$profiles_used, info$profiles_available))
  }

  # Print summary scores
  cat("\nSummary Scores:\n")
  if (!is.null(x$balance)) {
    cat(sprintf("  Balance: %.3f (higher is better)\n", x$balance$overall_balance))
  }
  if (!is.null(x$overlap)) {
    cat(sprintf("  Overlap: %.3f (lower is better)\n", x$overlap$overall_overlap))
  }
  if (!is.null(x$dominance)) {
    summary_info <- x$dominance$summary
    if (!is.null(summary_info)) {
      total_pct <- 100 * summary_info$total_dominant_questions / summary_info$total_questions
      partial_pct <- 100 * summary_info$partial_dominant_questions / summary_info$total_questions
      cat(sprintf("  Total dominance: %.1f%% of questions\n", total_pct))
      cat(sprintf("  Partial dominance: %.1f%% of questions\n", partial_pct))
    }
  }

  cat("\nNote: Detailed results shown when cbc_inspect() was called.\n")
  invisible(x)
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

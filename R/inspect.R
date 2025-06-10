#' Counts of attribute balance
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
#' cbc_balance(design)
cbc_balance <- function(x, ...) {
  UseMethod("cbc_balance")
}

#' @rdname cbc_balance
#' @export
cbc_balance.cbc_design <- function(x, ...) {
  cat("Design Balance Report\n")
  cat("=====================\n")
  cat(sprintf("Design method: %s\n", x$method))
  if (!is.null(x$d_error)) {
    cat(sprintf("D-error: %.6f\n", x$d_error))
  }

  # Profile usage info
  info <- x$design_info
  cat(sprintf("Profiles used: %d/%d (%.1f%%)\n",
              info$n_profiles_used, info$n_profiles_available,
              info$profile_usage_rate * 100))

  # Show balance score if available
  if (!is.null(info$efficiency$balance_score)) {
    cat(sprintf("Overall balance score: %.3f (higher is better)\n",
                info$efficiency$balance_score))
  }

  cat("\n")

  # Call the data frame method for detailed balance
  result <- cbc_balance.data.frame(x$design, ...)

  # Add design-specific information to result
  result$design_info <- list(
    method = x$method,
    d_error = x$d_error,
    balance_score = info$efficiency$balance_score,
    profiles_used = info$n_profiles_used,
    profiles_available = info$n_profiles_available
  )

  invisible(result)
}

#' @rdname cbc_balance
#' @export
cbc_balance.data.frame <- function(x, ...) {
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
    cat(sprintf("  Balance score: %.3f (CV: %.3f)\n",
                metric$balance_score, metric$cv))
    cat("\n")
  }

  # Print pairwise counts if we have multiple attributes
  if (length(counts_pair) > 0) {
    cat("=====================================\n")
    cat("Pairwise attribute level counts\n\n")
    for (i in seq_along(counts_pair)) {
      pair_names <- pairs[,i]
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

  class(result) <- c("cbc_balance", "list")
  invisible(result)
}

#' Counts of attribute overlap
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
#' cbc_overlap(design)
cbc_overlap <- function(x, ...) {
  UseMethod("cbc_overlap")
}

#' @rdname cbc_overlap
#' @export
cbc_overlap.cbc_design <- function(x, ...) {
  cat("Design Overlap Report\n")
  cat("=====================\n")
  cat(sprintf("Design method: %s\n", x$method))
  if (!is.null(x$d_error)) {
    cat(sprintf("D-error: %.6f\n", x$d_error))
  }

  # Show overlap score if available
  info <- x$design_info
  if (!is.null(info$efficiency$overlap_score)) {
    cat(sprintf("Overall overlap score: %.3f (lower is better)\n",
                info$efficiency$overlap_score))
  }

  cat("\n")

  # Call the data frame method for detailed overlap
  result <- cbc_overlap.data.frame(x$design, ...)

  # Add design-specific information to result
  result$design_info <- list(
    method = x$method,
    d_error = x$d_error,
    overlap_score = info$efficiency$overlap_score,
    n_choice_sets = info$n_choice_sets
  )

  invisible(result)
}

#' @rdname cbc_overlap
#' @export
cbc_overlap.data.frame <- function(x, ...) {
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

  for (i in seq_along(overlap_counts)) {
    attr_name <- atts[i]
    attr_data <- overlap_counts[[i]]

    cat(attr_name, ":\n", sep = "")

    if (attr_data$type == "continuous") {
      cat("  Continuous variable\n")
      cat("  Unique values in design:", paste(names(attr_data$value_counts), collapse = ", "), "\n")
      cat("  Value frequencies:\n")
      print(attr_data$value_counts)
      cat("  Questions by # unique levels:\n")
      print(attr_data$unique_per_question)
    } else {
      cat("  Categorical variable\n")
      cat("  Questions by # unique levels:\n")
      print(attr_data$unique_per_question)
    }

    # Show overlap metrics
    metric <- overlap_metrics[[attr_name]]
    cat(sprintf("  Complete overlap rate: %.1f%% (%d of %d questions)\n",
                metric$complete_overlap_rate * 100,
                metric$complete_overlap_count,
                metric$total_questions))
    cat(sprintf("  Average unique levels per question: %.2f\n",
                metric$avg_unique_levels))
    cat(sprintf("  Max possible unique levels: %d\n",
                attr_data$max_possible_unique))
    cat("\n")
  }

  # Create return object
  result <- list(
    overlap_counts = overlap_counts,
    overlap_metrics = overlap_metrics,
    overall_overlap = mean(sapply(overlap_metrics, function(x) x$complete_overlap_rate))
  )

  class(result) <- c("cbc_overlap", "list")
  invisible(result)
}

#' Compare balance and overlap across multiple designs
#'
#' @param ... Multiple `cbc_design` objects to compare, or named arguments where
#'   each argument is a `cbc_design` object
#' @return A data frame comparing balance and overlap metrics across designs
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
#' # Compare designs
#' compare_designs(
#'   Random = design_random,
#'   Sequential = design_sequential
#' )
compare_designs <- function(...) {
  designs <- list(...)

  # Get design names
  design_names <- names(designs)
  if (is.null(design_names)) {
    design_names <- paste0("Design_", seq_along(designs))
  }

  # Validate all inputs are cbc_design objects
  for (i in seq_along(designs)) {
    if (!inherits(designs[[i]], "cbc_design")) {
      stop(sprintf("Argument %d (%s) must be a cbc_design object",
                   i, design_names[i]))
    }
  }

  # Extract metrics from each design
  metrics <- lapply(designs, function(design) {
    info <- design$design_info
    list(
      method = design$method,
      d_error = design$d_error,
      balance_score = info$efficiency$balance_score %||% NA,
      overlap_score = info$efficiency$overlap_score %||% NA,
      profile_usage_rate = info$profile_usage_rate,
      n_profiles_used = info$n_profiles_used,
      n_profiles_available = info$n_profiles_available
    )
  })

  # Create comparison data frame
  result <- data.frame(
    design = design_names,
    method = sapply(metrics, function(x) x$method),
    d_error = sapply(metrics, function(x) x$d_error),
    balance_score = sapply(metrics, function(x) x$balance_score),
    overlap_score = sapply(metrics, function(x) x$overlap_score),
    profile_usage_rate = sapply(metrics, function(x) x$profile_usage_rate),
    profiles_used = sapply(metrics, function(x) x$n_profiles_used),
    profiles_available = sapply(metrics, function(x) x$n_profiles_available),
    stringsAsFactors = FALSE
  )

  # Add rankings
  result$d_error_rank <- rank(result$d_error, na.last = TRUE)
  result$balance_rank <- rank(-result$balance_score, na.last = TRUE)  # Higher is better
  result$overlap_rank <- rank(result$overlap_score, na.last = TRUE)   # Lower is better

  class(result) <- c("cbc_design_comparison", "data.frame")
  return(result)
}

#' Print method for design comparisons
#' @param x A cbc_design_comparison object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_design_comparison <- function(x, ...) {
  cat("Design Comparison\n")
  cat("=================\n\n")

  # Create formatted version for printing
  print_df <- x[, c("design", "method", "d_error", "balance_score",
                    "overlap_score", "profile_usage_rate")]

  # Format numeric columns
  print_df$d_error <- sprintf("%.6f", print_df$d_error)
  print_df$balance_score <- ifelse(is.na(x$balance_score), "NA",
                                   sprintf("%.3f", print_df$balance_score))
  print_df$overlap_score <- ifelse(is.na(x$overlap_score), "NA",
                                   sprintf("%.3f", print_df$overlap_score))
  print_df$profile_usage_rate <- sprintf("%.1f%%", print_df$profile_usage_rate * 100)

  # Rename columns for display
  names(print_df) <- c("Design", "Method", "D-Error", "Balance", "Overlap", "Profile Usage")

  # Remove class to avoid recursion
  class(print_df) <- "data.frame"
  print(print_df, row.names = FALSE)

  cat("\nInterpretation:\n")
  cat("- D-Error: Lower is better (design efficiency)\n")
  cat("- Balance: Higher is better (level distribution)\n")
  cat("- Overlap: Lower is better (attribute variation)\n")
  cat("- Profile Usage: Higher means more profiles used\n")

  # Highlight best performing design for each metric
  if (nrow(x) > 1) {
    cat("\nBest performers:\n")

    best_d_error <- x$design[which.min(x$d_error)]
    cat(sprintf("- D-Error: %s (%.6f)\n", best_d_error, min(x$d_error, na.rm = TRUE)))

    if (!all(is.na(x$balance_score))) {
      best_balance <- x$design[which.max(x$balance_score)]
      cat(sprintf("- Balance: %s (%.3f)\n", best_balance, max(x$balance_score, na.rm = TRUE)))
    }

    if (!all(is.na(x$overlap_score))) {
      best_overlap <- x$design[which.min(x$overlap_score)]
      cat(sprintf("- Overlap: %s (%.3f)\n", best_overlap, min(x$overlap_score, na.rm = TRUE)))
    }
  }

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
    avg_unique_levels <- sum(unique_levels * question_counts) / sum(question_counts)

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
      design[[attr_name]], design$obsID,
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
      design[[attr_name]], design$obsID,
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

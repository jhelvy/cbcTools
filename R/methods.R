#' Print method for cbc_profiles objects
#' @param x A cbc_profiles object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_profiles <- function(x, ...) {
  cat("CBC Profiles\n")
  cat("============\n")

  # Display attribute information
  attr_info <- attr(x, "attribute_info")
  for (attr in names(attr_info)) {
    cat(sprintf("%-12s: %s\n", attr, attr_info[[attr]]$summary))
  }

  # Display profile counts
  original_count <- attr(x, "original_count")
  current_count <- nrow(x)
  total_removed <- attr(x, "total_removed") %||% 0

  cat(sprintf("\nProfiles: %d", current_count))
  if (total_removed > 0) {
    cat(sprintf(" (originally %d, %d removed by restrictions)",
                original_count, total_removed))
  }
  cat("\n")

  # Display restrictions if any
  restrictions <- attr(x, "restrictions_applied")
  if (!is.null(restrictions) && length(restrictions) > 0) {
    cat("\nRestrictions applied:\n")
    for (i in seq_along(restrictions)) {
      cat(sprintf("  %d. %s\n", i, restrictions[i]))
    }
    cat("\n")
  }

  cat("First few rows:\n")
  # Remove the class temporarily to avoid infinite recursion
  class(x) <- "data.frame"
  print(utils::head(x))
  if (nrow(x) > 6) {
    cat(sprintf("... and %d more rows\n", nrow(x) - 6))
  }

  invisible(x)
}

#' Print method for cbc_priors objects
#' @export
print.cbc_priors <- function(x, ...) {
  cat("CBC Prior Specifications:\n\n")

  for (attr in names(x$attrs)) {
    info <- x$attrs[[attr]]

    # Print attribute name
    cat(attr, ":\n", sep = "")

    # Print variable type and levels
    if (info$continuous) {
      cat("  Continuous variable\n")
      cat("  Levels:", paste(info$levels, collapse = ", "), "\n")
      ref_level <- NULL
    } else {
      cat("  Categorical variable\n")
      cat("  Levels:", paste(info$levels, collapse = ", "), "\n")
      # Determine reference level for categorical variables
      if (!is.null(names(info$mean))) {
        ref_level <- setdiff(info$levels, names(info$mean))[1]
      } else {
        ref_level <- info$levels[1]
      }
      cat("  Reference level: ", ref_level, "\n", sep = "")
    }

    # Print parameter specifications
    if (info$random) {
      dist_names <- c(
        n = "Normal",
        ln = "Log-normal",
        cn = "Censored normal"
      )
      cat("  Random - ", dist_names[info$dist], " distribution\n", sep = "")

      if (!info$continuous && !is.null(names(info$mean))) {
        # Named categorical parameters
        for (level in names(info$mean)) {
          cat("    ", level, ":\n", sep = "")
          cat("      Mean: ", round(info$mean[level], 3), "\n", sep = "")
          cat("      SD:   ", round(info$sd[level], 3), "\n", sep = "")
        }
      } else {
        # Continuous or unnamed categorical parameters
        cat("    Mean: ", round(info$mean, 3),
            if (!is.null(ref_level) && is.vector(info$mean))
              paste0(" (vs ", ref_level, ")")
            else "",
            "\n", sep = "")
        cat("    SD:   ", round(info$sd, 3), "\n", sep = "")
      }
    } else {
      cat("  Fixed parameter\n")
      if (!info$continuous && !is.null(names(info$mean))) {
        # Named categorical parameters
        for (level in names(info$mean)) {
          cat("    ", level, ": ", round(info$mean[level], 3), "\n", sep = "")
        }
      } else {
        # Continuous or unnamed categorical parameters
        cat("    Coefficient: ", round(info$mean, 3),
            if (!is.null(ref_level) && is.vector(info$mean))
              paste0(" (vs ", ref_level, ")")
            else "",
            "\n", sep = "")
      }
    }
    cat("\n")
  }

  if (!is.null(x$correlation)) {
    cat("Correlation Matrix:\n")
    print(round(x$correlation, 3))
  }

  invisible(x)
}

#' Enhanced print method for cbc_design objects
#' @param x A cbc_design object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_design <- function(x, ...) {
    # Extract information from attributes
    params <- attr(x, "design_params")
    summary_info <- attr(x, "design_summary")
    priors <- attr(x, "priors")

    cat("Choice-Based Conjoint Design\n")
    cat("============================\n")
    cat(sprintf("Method: %s\n", params$method))
    cat(sprintf("Respondents: %d\n", params$n_resp))
    cat(sprintf("Alternatives per question: %d\n", params$n_alts))
    cat(sprintf("Questions per respondent: %d\n", params$n_q))
    cat(sprintf("Total questions generated: %d\n", summary_info$method_specific$total_questions_generated))
    cat(sprintf("Number of blocks: %d\n", params$n_blocks))

    if (params$no_choice) {
        cat("No-choice option: Yes\n")
    }
    if (!is.null(params$label)) {
        cat(sprintf("Labeled design using: %s\n", params$label))
    }

    # Method-specific information
    if (params$method == "sequential") {
        if (!is.na(params$randomize_questions)) {
            cat(sprintf("Randomize questions: %s\n", params$randomize_questions))
        }
        if (!is.na(params$randomize_alts)) {
            cat(sprintf("Randomize alternatives: %s\n", params$randomize_alts))
        }
    }

    # Design efficiency
    if (!is.null(params$d_error)) {
        cat(sprintf("D-error: %.6f\n", params$d_error))
    }

    # Profile usage
    cat(sprintf("Profiles used: %d/%d (%.1f%%)\n",
                summary_info$n_profiles_used, summary_info$n_profiles_available,
                summary_info$profile_usage_rate * 100))

    # Method-specific details
    if (!is.null(summary_info$method_specific)) {
        if (params$method == "random") {
            cat(sprintf("Optimization attempts: %d\n", summary_info$method_specific$optimization_attempts))
        } else if (params$method == "sequential") {
            cat("Base design optimized: Yes\n")
            if (summary_info$method_specific$repeated_across_respondents) {
                cat("Repeated across respondents: Yes\n")
            }
        }
    }

    # Dominance information
    if (!is.null(params$remove_dominant) && params$remove_dominant) {
        cat(sprintf("Dominance removal: %s (threshold: %.2f)\n",
                    paste(params$dominance_types, collapse = ", "),
                    attr(x, "design_params")$dominance_threshold %||% 0.8))
    }

    # Priors information
    if (!is.null(priors)) {
        cat("\nPriors summary:\n")
        n_fixed <- sum(!sapply(priors$attrs, function(a) a$random))
        n_random <- sum(sapply(priors$attrs, function(a) a$random))
        cat(sprintf("  Fixed parameters: %d\n", n_fixed))
        cat(sprintf("  Random parameters: %d\n", n_random))
        if (!is.null(priors$correlation)) {
            cat("  Parameter correlations: Yes\n")
        }
    }

    cat(sprintf("\nDesign created: %s\n", format(params$created_at, "%Y-%m-%d %H:%M:%S")))
    cat("\nFirst few rows of design:\n")

    # Remove class temporarily to avoid infinite recursion
    design_df <- x
    class(design_df) <- "data.frame"
    print(utils::head(design_df))
    if (nrow(design_df) > 6) {
        cat(sprintf("... and %d more rows\n", nrow(design_df) - 6))
    }

    invisible(x)
}

#' Print method for D-error comparisons
#' @param x A cbc_d_error_comparison object
#' @param ... Additional arguments passed to print
#' @export
#' Print method for D-error comparisons
#' @param x A cbc_d_error_comparison object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_d_error_comparison <- function(x, ...) {
  cat("D-Error Comparison\n")
  cat("==================\n")
  cat("(Lower D-error is better)\n\n")

  # Format the output nicely and remove custom class
  print_df <- x
  print_df$d_error <- sprintf("%.6f", print_df$d_error)
  print_df$relative_efficiency <- sprintf("%.2f", print_df$relative_efficiency)

  # Remove custom class to avoid infinite recursion
  class(print_df) <- "data.frame"
  print(print_df, row.names = FALSE)

  cat(sprintf("\nBest design: %s (D-error = %.6f)\n",
              x$design[1], x$d_error[1]))

  if (nrow(x) > 1) {
    worst_idx <- nrow(x)
    improvement <- (x$d_error[worst_idx] - x$d_error[1]) / x$d_error[worst_idx] * 100
    cat(sprintf("Improvement over worst: %.1f%%\n", improvement))
  }

  invisible(x)
}

#' Enhanced print method for cbc_survey objects
#' @param x A cbc_survey object
#' @param ... Additional arguments passed to print
#' @export
# Updated print method for surveys to show efficiency metrics
print.cbc_survey <- function(x, ...) {
  cat("Choice-Based Conjoint Survey\n")
  cat("============================\n")

  # Get survey info
  info <- attr(x, "survey_info")
  base_design <- attr(x, "base_design")

  # Basic survey structure
  cat(sprintf("Respondents: %d\n", info$n_resp))
  cat(sprintf("Questions per respondent: %d\n", max(x$qID)))
  cat(sprintf("Alternatives per question: %d\n", max(x$altID)))

  # Design information
  if (!is.null(base_design)) {
    design_params <- attr(base_design, "design_params")
    cat(sprintf("Design method: %s\n", info$design_method))
    if (!is.null(design_params$d_error)) {
      cat(sprintf("Original design D-error: %.6f\n", design_params$d_error))
    }
    if (design_params$n_blocks > 1) {
      cat(sprintf("Blocks: %d (%.1f respondents per block on average)\n",
                  design_params$n_blocks, info$n_resp / design_params$n_blocks))
    }
    if (!is.null(design_params$label)) {
      cat(sprintf("Labeled design using: %s\n", design_params$label))
    }
    if (design_params$no_choice) {
      cat("No-choice option: Yes\n")
    }
  } else {
    cat(sprintf("Design method: %s\n", info$design_method))
  }

  # Randomization settings
  if (info$randomize_questions || info$randomize_alts) {
    cat("Randomization: ")
    randomizations <- c()
    if (info$randomize_questions) randomizations <- c(randomizations, "questions")
    if (info$randomize_alts) randomizations <- c(randomizations, "alternatives")
    cat(paste(randomizations, collapse = " and "), "\n")
  }

  # Survey statistics and efficiency metrics
  if (!is.null(info$stats)) {
    stats <- info$stats
    cat("\nSurvey Statistics:\n")
    cat(sprintf("  Profile usage: %d/%d unique profiles (%.1f%%)\n",
                stats$unique_profiles_used, stats$total_profiles_available,
                stats$profile_usage_rate * 100))
    cat(sprintf("  Profile repetitions: %.1f times on average\n", stats$avg_profile_repetitions))

    # Show efficiency metrics if available
    if (!is.null(stats$efficiency)) {
      eff <- stats$efficiency
      if (!is.null(eff$balance_score)) {
        cat(sprintf("  Attribute balance: %.3f (higher is better)\n", eff$balance_score))
      }
      if (!is.null(eff$overlap_score)) {
        cat(sprintf("  Attribute overlap: %.3f (lower is better)\n", eff$overlap_score))
      }
    }
  }

  cat(sprintf("\nSurvey created: %s\n", format(info$created_at, "%Y-%m-%d %H:%M:%S")))
  cat("\nFirst few rows:\n")

  # Remove class temporarily to avoid infinite recursion
  survey_df <- x
  class(survey_df) <- "data.frame"
  print(utils::head(survey_df))
  if (nrow(survey_df) > 6) {
    cat(sprintf("... and %d more rows\n", nrow(survey_df) - 6))
  }

  invisible(x)
}

#' Print method for cbc_choices objects
#' @param x A cbc_choices object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_choices <- function(x, ...) {
  cat("CBC Choice Data\n")
  cat("===============\n")

  # Get choice info
  choice_info <- attr(x, "choice_info")

  # Basic structure
  n_obs <- max(x$obsID, na.rm = TRUE)
  n_alts <- sum(x$obsID == 1, na.rm = TRUE)
  n_resp <- if ("respID" %in% names(x)) max(x$respID, na.rm = TRUE) else 1
  n_choices <- sum(x$choice, na.rm = TRUE)

  cat(sprintf("Observations: %d choice tasks\n", n_obs))
  cat(sprintf("Alternatives per task: %d\n", n_alts))
  if (n_resp > 1) {
    cat(sprintf("Respondents: %d\n", n_resp))
    cat(sprintf("Questions per respondent: %d\n", n_obs / n_resp))
  }
  cat(sprintf("Total choices made: %d\n", n_choices))

  # Choice simulation info
  if (!is.null(choice_info)) {
    cat(sprintf("\nSimulation method: %s\n", choice_info$simulation_method))
    if (!is.na(choice_info$d_error)) {
      cat(sprintf("Original design D-error: %.6f\n", choice_info$d_error))
    }
    if (choice_info$priors_used) {
      cat("Priors: Used for utility-based simulation\n")
    } else {
      cat("Priors: None (random choices)\n")
    }
    cat(sprintf("Simulated at: %s\n", format(choice_info$simulated_at, "%Y-%m-%d %H:%M:%S")))
  }

  # Choice distribution by alternative
  if ("altID" %in% names(x)) {
    choice_by_alt <- tapply(x$choice, x$altID, sum, na.rm = TRUE)
    choice_rates <- choice_by_alt / n_obs
    cat("\nChoice rates by alternative:\n")
    for (i in seq_along(choice_rates)) {
      cat(sprintf("  Alt %d: %.1f%% (%d choices)\n",
                  i, choice_rates[i] * 100, choice_by_alt[i]))
    }
  }

  # No-choice option if present
  if ("no_choice" %in% names(x)) {
    no_choice_rate <- mean(x$choice[x$no_choice == 1], na.rm = TRUE)
    cat(sprintf("\nNo-choice rate: %.1f%%\n", no_choice_rate * 100))
  }

  cat("\nFirst few rows:\n")

  # Remove class temporarily to avoid infinite recursion
  choices_df <- x
  class(choices_df) <- "data.frame"
  print(utils::head(choices_df))
  if (nrow(choices_df) > 6) {
    cat(sprintf("... and %d more rows\n", nrow(choices_df) - 6))
  }

  invisible(x)
}

#' Methods for cbc_models objects
#'
#' Miscellaneous methods for `cbc_models` class objects.
#'
#' @name miscmethods.cbc_models
#' @aliases print.cbc_models
#' @param x is an object of class `cbc_models`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param ... further arguments.
#' @return No return value, prints a summary of estimated models.
#' @rdname miscmethods.cbc_models
#' @export
print.cbc_models <- function (
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  cat("A list of models estimated with the following sample sizes:")
  cat("\n\n", paste0(names(x), "\n"), "\n")
  cat("Each model contains estimates for the following parameters:")
  cat("\n\n", paste0(names(stats::coef(x[[1]])), "\n"))
  invisible(x)
}

#' Methods for cbc_power_errors objects
#'
#' Miscellaneous methods for `cbc_power_errors` class objects.
#'
#' @name miscmethods.cbc_power_errors
#' @aliases plot.cbc_power_errors
#' @param x is an object of class `cbc_power_errors`.
#' @param ... further arguments.
#' @return Returns a ggplot2 object plotting standard errors versus sample
#' size.
#' @importFrom ggplot2 ggplot aes geom_hline geom_point expand_limits theme_bw
#' theme element_blank labs
#' @importFrom rlang .data
#' @rdname miscmethods.cbc_power_errors
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
#' # (This is the default setting where method = 'full' for "full factorial")
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Simulate random choices
#' data <- cbc_choices(
#'   design = design,
#'   obsID  = "obsID"
#' )
#'
#' # Conduct a power analysis
#' power <- cbc_power(
#'   data    = data,
#'   pars    = c("price", "type", "freshness"),
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   nbreaks = 10,
#'   n_q     = 6
#' )
#'
#' # Visualize the results
#' plot(power)
plot.cbc_power_errors <- function (x, ...) {
  plot <- ggplot2::ggplot(x) +
    geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
    geom_point(
      aes(x = .data$sampleSize, y = .data$se, color = .data$coef),
      size = 1.8
    ) +
    expand_limits(y = 0) +
    theme_bw(base_size = 14) +
    theme(panel.grid.minor = element_blank()) +
    labs(
      color = "Coefficient",
      x = "Sample size",
      y = "Standard error"
    )
  return(plot)
}

#' Print method for design error metrics
#' @param x A cbc_errors object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_errors <- function(x, ...) {
  cat("Design Error Metrics\n")
  cat("====================\n\n")

  if (!is.null(x$d_error)) {
    cat(sprintf("D-error (lower is better): %.6f\n", x$d_error))
  }
  if (!is.null(x$a_error)) {
    cat(sprintf("A-error (lower is better): %.6f\n", x$a_error))
  }
  if (!is.null(x$g_error)) {
    cat(sprintf("G-error (lower is better): %.6f\n", x$g_error))
  }
  if (!is.null(x$e_error)) {
    cat(sprintf("E-error (higher is better): %.6f\n", x$e_error))
  }

  invisible(x)
}

#' Enhanced print method for cbc_power_errors objects
#' @param x A cbc_power_errors object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_power_errors <- function(x, ...) {
  cat("CBC Power Analysis Results\n")
  cat("==========================\n")

  # Get choice info if available
  choice_info <- attr(x, "choice_info")
  if (!is.null(choice_info)) {
    cat(sprintf("Simulation method: %s\n", choice_info$simulation_method))
    if (!is.na(choice_info$d_error)) {
      cat(sprintf("Design D-error: %.6f\n", choice_info$d_error))
    }
    if (!is.null(choice_info$n_respondents)) {
      cat(sprintf("Data from: %d respondents\n", choice_info$n_respondents))
    }
    cat("\n")
  }

  # Basic statistics
  n_breaks <- length(unique(x$sampleSize))
  sample_range <- range(x$sampleSize)
  n_pars <- length(unique(x$coef))

  cat(sprintf("Sample sizes: %d to %d (%d breaks)\n",
              sample_range[1], sample_range[2], n_breaks))
  cat(sprintf("Parameters: %d (%s)\n",
              n_pars, paste(unique(x$coef), collapse = ", ")))
  cat("\n")

  # Summary of results at different sample sizes
  cat("Standard errors by sample size:\n")
  cat("(Showing every few sample sizes)\n\n")

  # Show results for a few sample sizes
  sample_sizes <- unique(x$sampleSize)
  show_sizes <- sample_sizes[seq(1, length(sample_sizes), length.out = min(5, length(sample_sizes)))]

  for (size in show_sizes) {
    subset_data <- x[x$sampleSize == size, ]
    cat(sprintf("n = %d:\n", size))
    for (i in seq_len(nrow(subset_data))) {
      cat(sprintf("  %-12s: SE = %.4f\n",
                  subset_data$coef[i], subset_data$se[i]))
    }
    cat("\n")
  }

  cat("Use plot() to visualize power curves.\n")

  invisible(x)
}

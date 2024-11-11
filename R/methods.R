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

#' Methods for cbc_errors objects
#'
#' Miscellaneous methods for `cbc_errors` class objects.
#'
#' @name miscmethods.cbc_errors
#' @aliases plot.cbc_errors
#' @param x is an object of class `cbc_errors`.
#' @param ... further arguments.
#' @return Returns a ggplot2 object plotting standard errors versus sample
#' size.
#' @importFrom ggplot2 ggplot aes geom_hline geom_point expand_limits theme_bw
#' theme element_blank labs
#' @importFrom rlang .data
#' @rdname miscmethods.cbc_errors
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
plot.cbc_errors <- function (x, ...) {
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

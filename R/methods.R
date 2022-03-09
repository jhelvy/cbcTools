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
#'
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
#' @importFrom ggplot2 ggplot aes geom_hline geom_point expand_limits theme_bw
#' theme element_blank labs
#' @importFrom rlang .data
#'
#' @rdname miscmethods.cbc_errors
#' @export
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

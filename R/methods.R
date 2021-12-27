#' Methods for cjmodels objects
#'
#' Miscellaneous methods for `cjmodels` class objects.
#'
#' @name miscmethods.cjmodels
#' @aliases print.cjmodels
#' @param x is an object of class `cjmodels`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param ... further arguments.
#'
#' @rdname miscmethods.cjmodels
#' @export
print.cjmodels <- function (
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  cat("A list of models estimated with the following sample sizes:\n")
  cat(names(x), "\n")
  invisible(x)
}

#' Methods for cbc objects
#'
#' Miscellaneous methods for `cbc` class objects.
#'
#' @name miscmethods.cbc
#' @aliases print.cbc
#' @param x is an object of class `cbc`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param ... further arguments.
#'
#' @rdname miscmethods.cbc
#' @export
print.cbc <- function (
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  cat("A list of models estimated with the following sample sizes:\n")
  cat(names(x), "\n")
  invisible(x)
}

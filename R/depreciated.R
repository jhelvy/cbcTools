#' Convert dummy-coded CBC data back to categorical format
#'
#' This function is depreciated. Use `cbc_encode()` instead
#'
#' @param data A `cbc_design` or `cbc_choices` object with dummy-coded categorical variables
#' @return The input object with categorical variables restored to their original format
#' @export
cbc_decode <- function(data) {
    # v0.6.5
    .Deprecated(
        "This function was depreciated in v0.13.0; use the cbc_encode() function instead"
    )
}

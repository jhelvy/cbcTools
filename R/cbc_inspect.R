#' Counts of attribute overlap
#'
#' This function prints out a summary of the amount of "overlap" across
#' attributes within the choice questions. For example, for each attribute, the
#' count under `"1"` is the number of choice questions in which the same level
#' was shown across all alternatives for that attribute (because there was only
#' one level shown). Likewise, the count under `"2"` is the number of choice
#' questions in which only two unique levels of that attribute were shown, and
#' so on:
#' @param design A data frame of a survey design.
#' @param atts A vector of attributes. Defaults to `NULL`, in which case all
#' of the column names of the `design` data frame are used except for those
#' that end in `"ID"`.
#' @return A list showing counts of the number of choice questions that contain
#' the unique number of levels for each attribute.
#' @export
#' @examples
#' # Insert examples
cbc_overlap <- function(design, atts = NULL) {
  if (is.null(atts)) {
    atts <- setdiff(
      names(design), c("respID", "qID", "altID", "obsID", "profileID"))
  }
  results <- list()
  for (i in 1:length(atts)) {
    counts <- tapply(
      design[[atts[i]]], design$obsID,
      FUN = function(x) length(unique(x))
    )
    counts <- as.vector(table(counts))
    names(counts) <- seq(length(counts))
    results[[i]] <- counts
  }
  names(results) <- atts
  print(results)
}

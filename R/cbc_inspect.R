#' Counts of attribute overlap
#'
#' This function helps understand the degree of overlap for each level of each
#' attribute in a given survey design. The list that is returned shows the
#' number of choice questions that contain a given number of unique levels. For
#' example, the number under `"1"` means the number of questions where the
#' same attribute level was shown across all alternatives (because there was
#' only one level shown).
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
  return(results)
}

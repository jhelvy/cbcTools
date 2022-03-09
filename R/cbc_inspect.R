#' Counts of attribute balance
#'
#' This function prints out a summary of the counts of each level for each
#' attribute across all choice questions as well as the two-way counts across
#' all pairs of attributes for a given design.
#' @param design A data frame of a survey design.
#' @param atts A vector of attributes. Defaults to `NULL`, in which case all
#' of the column names of the `design` data frame are used except for those
#' that end in `"ID"`.
#' @return A list showing counts of the number of choice questions that contain
#' the unique number of levels for each attribute.
#' @export
#' @examples
#' # Insert examples
cbc_balance <- function(design, atts = NULL) {
  if (is.null(atts)) {
    atts <- setdiff(
      names(design), c("respID", "qID", "altID", "obsID", "profileID"))
  }
  cat("==============================\n")
  cat("Attribute counts:\n\n")
  for (i in seq_len(length(atts))) {
    counts <- table(design[[atts[i]]])
    cat(atts[i], ":\n", sep = "")
    print(counts)
    cat("\n")
  }
  cat("==============================\n")
  cat("Pairwise attribute counts:\n\n")
  pairs <- data.frame(utils::combn(atts, 2))
  for (i in seq_len(ncol(pairs))) {
    vars <- pairs[, i]
    counts <- table(design[[vars[1]]], design[[vars[2]]])
    cat(vars[1], " & ", vars[2], ":\n", sep = "")
    print(counts)
    cat("\n")
  }
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
  cat("==============================\n")
  cat("Counts of attribute overlap:\n")
  cat("(# of questions with N unique levels)\n\n")
  for (i in 1:length(atts)) {
    counts <- tapply(
      design[[atts[i]]], design$obsID,
      FUN = function(x) length(unique(x))
    )
    counts <- as.vector(table(counts))
    names(counts) <- seq(length(counts))
    cat(atts[i], ":\n\n", sep = "")
    print(counts)
    cat("\n")
  }
}

#' Counts of attribute balance
#'
#' This function prints out a summary of the individual and pairwise counts of
#' each level for each attribute across all choice questions in the design.
#' @keywords logitr mnl mxl mixed logit balance overlap
#'
#' @param design A data frame of a survey design.
#' @return Prints the individual and pairwise counts of the number of times
#' each attribute levels in shown in the design.
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
#' # Inspect the design balance
#' cbc_balance(design)
#'
#' # Inspect the design overlap
#' cbc_overlap(design)
cbc_balance <- function(design) {
  atts <- setdiff(
      names(design),
      c("respID", "qID", "altID", "obsID", "profileID")
  )
  # Get counts of each individual attribute
  counts <- lapply(atts, function(x) table(design[[x]]))
  names(counts) <- atts
  # Get pairwise counts matrix for each pair of attributes
  pairs <- data.frame(utils::combn(atts, 2))
  counts_pair <- lapply(pairs, function(x) table(design[[x[1]]], design[[x[2]]]))
  cat("=====================================\n")
  cat("Individual attribute level counts\n\n")
  for (i in 1:length(counts)) {
      cat(names(counts)[i], ":\n", sep = "")
      print(counts[[i]])
      cat("\n")
  }
  cat("=====================================\n")
  cat("Pairwise attribute level counts\n\n")
  for (i in 1:ncol(pairs)) {
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
#' @return Prints the counts of the number of choice questions that contain
#' the unique number of levels for each attribute.
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
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3, # Number of alternatives per question
#'   n_q      = 6 # Number of questions per respondent
#' )
#'
#' # Inspect the design balance
#' cbc_balance(design)
#'
#' # Inspect the design overlap
#' cbc_overlap(design)
cbc_overlap <- function(design) {
  atts <- setdiff(
      names(design),
      c("respID", "qID", "altID", "obsID", "profileID")
  )
  counts <- lapply(atts, function(x) get_att_overlap_counts(x, design))
  cat("==============================\n")
  cat("Counts of attribute overlap:\n")
  cat("(# of questions with N unique levels)\n\n")
  for (i in 1:length(counts)) {
    cat(atts[i], ":\n\n", sep = "")
    print(counts[[i]])
    cat("\n")
  }
}

get_att_overlap_counts <- function(x, design) {
  counts <- tapply(
    design[[x]], design$obsID,
    FUN = function(x) length(unique(x))
  )
  counts <- as.vector(table(counts))
  names(counts) <- seq(length(counts))
  return(counts)
}

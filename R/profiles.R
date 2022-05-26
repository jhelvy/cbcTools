#' Make a data frame of all combinations of attribute levels
#'
#' This function creates a data frame of of all possible combinations of
#' attribute levels.
#' @param ... A series of vectors defining the levels of each attribute. Each
#' argument should be named according to the attribute name, e.g.,
#' `price = c(1, 2, 3)`. Conditional attribute levels can also be included by
#' setting each level of an attribute to a named list that determines the
#' levels of other attributes for that specific level.
#' @return A data frame of all possible combinations of attribute levels.
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
#' # Generate profiles for with conditional levels
#' profiles <- cbc_profiles(
#'   price = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   freshness = c("Excellent", "Average", "Poor"),
#'   type = list(
#'     "Fuji" = list(
#'         price = c(2, 2.5, 3)
#'     ),
#'     "Gala" = list(
#'         price = c(1, 1.5, 2)
#'     ),
#'     "Honeycrisp" = list(
#'         price = c(2.5, 3, 3.5, 4, 4.5, 5),
#'         freshness = c("Excellent", "Average")
#'     )
#'   )
#' )
#'
cbc_profiles <- function(...) {
  levels <- list(...)
  # Check for conditional levels
  cond <- lapply(levels, function(x) names(x))
  cond_check <- unlist(lapply(cond, function(x) is.null(x)))
  if (! all(cond_check)) {
    return(cbc_profiles_conditional(levels, cond_check))
  }
  # No conditional levels, so return full set of combinations
  profiles <- expand.grid(levels)
  profiles <- add_profile_ids(profiles)
  return(profiles)
}

cbc_profiles_conditional <- function(levels, cond_check) {
  # Make all possible profiles
  new_levels <- levels
  cond_indices <- which(cond_check == FALSE)
  for (i in seq_len(length(cond_indices))) {
    att <- levels[[cond_indices[i]]]
    new_levels[[cond_indices[i]]] <- names(att)
  }
  profiles <- expand.grid(new_levels)
  # Now filter out profiles based on conditionals
  for (i in seq_len(length(cond_indices))) {
    att_name <- names(cond_indices[i])
    att <- levels[[cond_indices[i]]]
    for (j in seq_len(length(att))) {
      att_level <- names(att)[[j]]
      conditions <- att[[j]]
      for (k in seq_len(length(conditions))) {
        cond_att <- names(conditions)[k]
        cond_levels <- conditions[[k]]
        indices_att <- which(profiles[[att_name]] == att_level)
        indices_cond <- which(profiles[[cond_att]] %in% cond_levels)
        indices_drop <- setdiff(indices_att, indices_cond)
        profiles <- profiles[-indices_drop,]
      }
    }
  }
  row.names(profiles) <- NULL
  profiles <- add_profile_ids(profiles)
  return(profiles)
}

add_profile_ids <- function(profiles) {
  profiles$profileID <- seq(nrow(profiles))
  varNames <- varNames <- setdiff(names(profiles), "profileID")
  profiles <- profiles[, c("profileID", varNames)]
  return(profiles)
}

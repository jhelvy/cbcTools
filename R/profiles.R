#' Make a data frame of all combinations of attribute levels
#'
#' This function creates a data frame of of all possible combinations of
#' attribute levels.
#' @param ... A series of vectors defining the levels of each attribute. Each
#' argument should be named according to the attribute name, e.g.,
#' `price = c(1, 2, 3)`.
#' @return A data frame of all possible combinations of attribute levels.
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Generate all profiles for a simple conjoint experiment about apples
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
cbc_profiles <- function(...) {
  levels <- list(...)
  profiles <- expand.grid(levels)
  profiles <- add_profile_ids(profiles)
  return(profiles)
}

#' Obtain a restricted set of profiles
#'
#' This function returns a restricted set of profiles as a data frame.
#' @param profiles A data frame in which each row is a possible profile.
#' This can be generated using the `cbc_profiles()` function.
#' @param ... Lists defining pairs of restrictions for attributes and levels.
#' Each restriction should be provided as a named list defining one restriction,
#' e.g. `list(type = 'Fuji', freshness = 'Poor')`.
#' @return A restricted set of profiles as a data frame.
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Generate all profiles for a simple conjoint experiment about apples
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Obtain a restricted subset of profiles
#' df <- cbc_restrictions(
#'     profiles,
#'     list(type = 'Fuji', freshness = 'Poor'),
#'     list(price = 3, type = 'Gala')
#' )
cbc_restrict <- function(profiles, ...) {
    restrictions <- list(...)
    check_inputs_restrict(profiles, restrictions)
    restrict_rows <- unique(unlist(lapply(restrictions, function(x) which(
        (profiles[names(x)[1]] == x[[1]]) & (profiles[names(x)[2]] == x[[2]])
    ))))
    profiles <- profiles[-restrict_rows,]
    profiles <- add_profile_ids(profiles)
    return(profiles)
}

add_profile_ids <- function(profiles) {
  profiles$profileID <- seq(nrow(profiles))
  profiles <- profiles[, c("profileID", setdiff(names(profiles), "profileID"))]
  row.names(profiles) <- NULL
  return(profiles)
}

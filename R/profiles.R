#' Make a data frame of all combinations of attribute levels
#'
#' This function creates a data frame of of all possible combinations of
#' attribute levels.
#' @param ... Any number of named vectors defining each attribute and their levels,
#' e.g. `price = c(1, 2, 3)`. Separate each vector by a comma.
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
  check_inputs_profiles(levels)
  profiles <- expand.grid(levels)
  profiles <- add_profile_ids(profiles)
  return(profiles)
}

#' Obtain a restricted set of profiles
#'
#' This function returns a restricted set of profiles as a data frame.
#' @param profiles A data frame in which each row is a possible profile.
#' This can be generated using the `cbc_profiles()` function.
#' @param ... Any number of restricted pairs of attribute levels, defined as
#' pairs of logical expressions separated by commas. For example, the
#' restriction `type == 'Fuji' & freshness == 'Poor'` will eliminate profiles
#' such that `"Fuji"` type apples will never be shown with `"Poor"` freshness.
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
#' # Obtain a restricted subset of profiles based on pairs of logical
#' # expressions. The example below contains the following restrictions:
#'
#' # - `"Gala"` apples will not be shown with the prices `1.5`, `2.5`, & `3.5`.
#' # - `"Honeycrisp"` apples will not be shown with prices less than `2`.
#' # - `"Honeycrisp"` apples will not be shown with the `"Poor"` freshness.
#' # - `"Fuji"` apples will not be shown with the `"Excellent"` freshness.
#'
#' profiles_restricted <- cbc_restrict(
#'     profiles,
#'     type == "Gala" & price %in% c(1.5, 2.5, 3.5),
#'     type == "Honeycrisp" & price > 2,
#'     type == "Honeycrisp" & freshness == "Poor",
#'     type == "Fuji" & freshness == "Excellent"
#' )
cbc_restrict <- function(profiles, ...) {
    check_inputs_restrict(profiles)
    drop_ids <- unique(unlist(lapply(
        rlang::enquos(...),
        function(x) dplyr::filter(profiles, !!x) |> dplyr::pull(.data$profileID)
    )))
    profiles <- profiles[-drop_ids,]
    profiles <- add_profile_ids(profiles)
    return(profiles)
}

add_profile_ids <- function(profiles) {
  profiles$profileID <- seq(nrow(profiles))
  profiles <- profiles[, c("profileID", setdiff(names(profiles), "profileID"))]
  row.names(profiles) <- NULL
  return(profiles)
}

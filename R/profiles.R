#' Make a data frame of all combinations of attribute levels
#'
#' This function creates a data frame of of all possible combinations of
#' attribute levels.
#' @param ... Any number of named vectors defining each attribute and their levels,
#' e.g. `price = c(1, 2, 3)`. Separate each vector by a comma.
#' @return A data frame of all possible combinations of attribute levels with
#' class `cbc_profiles`.
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

  # Add metadata about the attribute structure
  attr(profiles, "attribute_info") <- get_attribute_metadata(levels)
  attr(profiles, "original_count") <- nrow(profiles)
  class(profiles) <- c("cbc_profiles", "data.frame")
  return(profiles)
}

#' Obtain a restricted set of profiles
#'
#' This function returns a restricted set of profiles as a data frame.
#' @param profiles A data frame of class `cbc_profiles` created using the
#' `cbc_profiles()` function.
#' @param ... Any number of restricted pairs of attribute levels, defined as
#' pairs of logical expressions separated by commas. For example, the
#' restriction `type == 'Fuji' & freshness == 'Poor'` will eliminate profiles
#' such that `"Fuji"` type apples will never be shown with `"Poor"` freshness.
#' @return A restricted set of profiles as a data frame with class `cbc_profiles`.
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
  if (!inherits(profiles, "cbc_profiles")) {
    stop("profiles must be a cbc_profiles object created by cbc_profiles()")
  }

  # Preserve and update metadata
  original_count <- attr(profiles, "original_count")
  attribute_info <- attr(profiles, "attribute_info")

  # Track restrictions for metadata
  restrictions <- rlang::enexprs(...)
  restriction_text <- sapply(restrictions, deparse)

  # Apply restrictions
  drop_ids <- unique(unlist(lapply(restrictions, function(x) {
    subset_ids <- subset(profiles, eval(x), select = c("profileID"))
    as.character(subset_ids$profileID)
  })))

  profiles_removed <- 0
  if (length(drop_ids) > 0) {
    drop_rows <- which(profiles$profileID %in% drop_ids)
    profiles <- profiles[-drop_rows, ]
    profiles_removed <- length(drop_rows)
  }

  profiles <- add_profile_ids(profiles)

  # Update metadata
  restrictions_applied <- c(
    attr(profiles, "restrictions_applied") %||% character(0),
    restriction_text
  )
  total_removed <- (attr(profiles, "total_removed") %||% 0) + profiles_removed

  attr(profiles, "original_count") <- original_count
  attr(profiles, "attribute_info") <- attribute_info
  attr(profiles, "restrictions_applied") <- restrictions_applied
  attr(profiles, "total_removed") <- total_removed
  attr(profiles, "profiles_removed_this_step") <- profiles_removed

  class(profiles) <- c("cbc_profiles", "data.frame")
  return(profiles)
}

add_profile_ids <- function(profiles) {
  profiles$profileID <- seq(nrow(profiles))
  profiles <- profiles[, c("profileID", setdiff(names(profiles), "profileID"))]
  row.names(profiles) <- NULL
  return(profiles)
}

# Helper function to create attribute metadata
get_attribute_metadata <- function(levels) {
  attr_info <- list()
  for (attr in names(levels)) {
    values <- levels[[attr]]

    if (is.numeric(values)) {
      attr_info[[attr]] <- list(
        type = "continuous",
        n_levels = length(values),
        range = range(values),
        summary = sprintf(
          "Continuous (%d levels, range: %.2f-%.2f)",
          length(values),
          min(values),
          max(values)
        )
      )
    } else {
      attr_info[[attr]] <- list(
        type = "categorical",
        n_levels = length(values),
        levels = values,
        summary = sprintf(
          "Categorical (%d levels: %s)",
          length(values),
          paste(values, collapse = ", ")
        )
      )
    }
  }
  return(attr_info)
}

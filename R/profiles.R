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
        summary = sprintf("Continuous (%d levels, range: %.2f-%.2f)",
                          length(values), min(values), max(values))
      )
    } else {
      attr_info[[attr]] <- list(
        type = "categorical",
        n_levels = length(values),
        levels = values,
        summary = sprintf("Categorical (%d levels: %s)",
                          length(values),
                          paste(values, collapse = ", "))
      )
    }
  }
  return(attr_info)
}

# Null-coalescing operator helper
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

#' Print method for cbc_profiles objects
#' @param x A cbc_profiles object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_profiles <- function(x, ...) {
  cat("CBC Profiles\n")
  cat("============\n")

  # Display attribute information
  attr_info <- attr(x, "attribute_info")
  for (attr in names(attr_info)) {
    cat(sprintf("%-12s: %s\n", attr, attr_info[[attr]]$summary))
  }

  # Display profile counts
  original_count <- attr(x, "original_count")
  current_count <- nrow(x)
  total_removed <- attr(x, "total_removed") %||% 0

  cat(sprintf("\nProfiles: %d", current_count))
  if (total_removed > 0) {
    cat(sprintf(" (originally %d, %d removed by restrictions)",
                original_count, total_removed))
  }
  cat("\n")

  # Display restrictions if any
  restrictions <- attr(x, "restrictions_applied")
  if (!is.null(restrictions) && length(restrictions) > 0) {
    cat("\nRestrictions applied:\n")
    for (i in seq_along(restrictions)) {
      cat(sprintf("  %d. %s\n", i, restrictions[i]))
    }
    cat("\n")
  }

  cat("First few rows:\n")
  # Remove the class temporarily to avoid infinite recursion
  class(x) <- "data.frame"
  print(utils::head(x))
  if (nrow(x) > 6) {
    cat(sprintf("... and %d more rows\n", nrow(x) - 6))
  }

  invisible(x)
}

#' Check if object is a cbc_profiles object
#' @param x Object to check
#' @return Logical indicating if x is a cbc_profiles object
#' @export
is.cbc_profiles <- function(x) {
  inherits(x, "cbc_profiles")
}

#' Create prior specifications for CBC models
#'
#' Creates a standardized prior specification object for use in CBC analysis
#' functions like cbc_choices() and cbc_d_error(). Supports both named and unnamed
#' vectors for categorical attributes, where named vectors explicitly map levels
#' to coefficients.
#'
#' @param profiles A data frame of profiles created by cbc_profiles()
#' @param ... Named arguments for each parameter's priors. For continuous variables,
#'   provide a single value. For categorical variables, provide either:
#'   - An unnamed vector of values one less than the number of levels (dummy coding)
#'   - A named vector mapping specific levels to coefficients (remaining level becomes reference)
#' @param sd Optional named list of standard deviations for random parameters
#' @param correlation Optional correlation matrix for random parameters
#' @param distribution Optional named vector specifying distribution type for random
#'   parameters ("normal" or "lognormal")
#' @return A structured prior specification object
#' @export
#' @examples
#' # Create profiles for an example conjoint about apples
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c("Poor", "Average", "Excellent")
#' )
#'
#' # Example 1: Simple fixed parameters with unnamed vectors
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3),          # Dummy-coded categorical
#'   freshness = c(0.4, 0.8)      # Dummy-coded categorical
#' )
#'
#' # Example 2: Using named vectors for categorical variables
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c("Fuji" = 0.2, "Gala" = 0.3),  # Honeycrisp as reference
#'   freshness = c("Poor" = -0.4, "Average" = 0.1)  # Excellent as reference
#' )
#'
#' # Example 3: Mixed approach with random parameters
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c("Fuji" = 0.2, "Gala" = 0.3),
#'   freshness = c(0.4, 0.8),
#'   sd = list(
#'     price = 0.4,
#'     type = c(0.4, 0.4)
#'   )
#' )
cbc_priors <- function(profiles, ..., sd = NULL, correlation = NULL, distribution = NULL) {
  # Validate profiles input
  if (!inherits(profiles, "data.frame") || !"profileID" %in% names(profiles)) {
    stop("'profiles' must be a data frame created by cbc_profiles()")
  }

  # Get attribute information from profiles
  attr_info <- get_attribute_info(profiles)

  # Capture the means
  means <- list(...)

  # Validate attribute names
  check_attribute_names(means, attr_info)

  # Process and validate each mean parameter
  means <- process_mean_parameters(means, attr_info)

  # Validate sd if provided
  if (!is.null(sd)) {
    check_sd_specification(sd, means, attr_info)
  }

  # Validate distribution if provided
  if (!is.null(distribution)) {
    check_distribution_specification(distribution, attr_info)
  }

  # Create the prior specification object
  prior_spec <- list(
    means = means,
    sd = sd,
    correlation = correlation,
    distribution = distribution,
    attr_info = attr_info  # Store attribute info for printing
  )

  # Add class for potential method dispatch
  class(prior_spec) <- c("cbc_priors", "list")

  return(prior_spec)
}

# Helper function to extract attribute information from profiles for cbc_priors
# Helper function to extract attribute information from profiles
get_attribute_info <- function(profiles) {
  # Remove profileID column
  attrs <- profiles[, -which(names(profiles) == "profileID")]

  # Get information for each attribute
  attr_info <- lapply(names(attrs), function(attr) {
    values <- attrs[[attr]]
    is_continuous <- is.numeric(values)

    if (is_continuous) {
      list(
        type = "continuous",
        range = range(values),
        levels = unique(values)  # Store all unique values for continuous attributes
      )
    } else {
      list(
        type = "categorical",
        levels = if (is.factor(values)) levels(values) else unique(values)
      )
    }
  })
  names(attr_info) <- names(attrs)
  return(attr_info)
}

# Helper function to validate attribute names
check_attribute_names <- function(means, attr_info) {
  # Check for missing attributes
  missing_attrs <- setdiff(names(attr_info), names(means))
  if (length(missing_attrs) > 0) {
    stop("Missing prior specifications for attributes: ",
         paste(missing_attrs, collapse = ", "))
  }

  # Check for extra attributes
  extra_attrs <- setdiff(names(means), names(attr_info))
  if (length(extra_attrs) > 0) {
    stop("Prior specifications provided for non-existent attributes: ",
         paste(extra_attrs, collapse = ", "))
  }
}

# Helper function to process and validate mean parameters
process_mean_parameters <- function(means, attr_info) {
  result <- lapply(names(means), function(attr) {
    value <- means[[attr]]
    info <- attr_info[[attr]]

    if (info$type == "continuous") {
      if (!is.numeric(value) || length(value) != 1) {
        stop("Prior for continuous attribute '", attr, "' must be a single numeric value")
      }
      return(value)
    } else {
      # For categorical attributes
      if (is.null(names(value))) {
        # Unnamed vector - validate length
        if (length(value) != length(info$levels) - 1) {
          stop("Prior for categorical attribute '", attr, "' must have ",
               length(info$levels) - 1, " values (one less than number of levels)")
        }
        # Add names based on non-reference levels
        names(value) <- info$levels[-1]
      } else {
        # Named vector - validate names
        invalid_levels <- setdiff(names(value), info$levels)
        if (length(invalid_levels) > 0) {
          stop("Invalid levels specified for attribute '", attr, "': ",
               paste(invalid_levels, collapse = ", "))
        }
      }
      return(value)
    }
  })

  # Add the attribute names to the result list
  names(result) <- names(means)
  return(result)
}

# Helper function to validate sd specification
check_sd_specification <- function(sd, means, attr_info) {
  if (!is.list(sd)) {
    stop("sd must be a named list")
  }

  # Check that all sd parameters correspond to existing means
  invalid_sds <- setdiff(names(sd), names(means))
  if (length(invalid_sds) > 0) {
    stop("SD specified for non-existent parameters: ",
         paste(invalid_sds, collapse = ", "))
  }

  # Check lengths match for each parameter
  for (param in names(sd)) {
    if (attr_info[[param]]$type == "continuous") {
      if (length(sd[[param]]) != 1) {
        stop("SD for continuous attribute '", param, "' must be a single value")
      }
    } else {
      if (length(sd[[param]]) != length(means[[param]])) {
        stop("SD for categorical attribute '", param,
             "' must match length of means specification")
      }
    }
  }
}

#' Display attribute levels and dummy coding for a CBC design
#'
#' Shows how categorical variables will be dummy coded and what each coefficient
#' represents in the utility function.
#'
#' @param design A data frame containing a choice experiment design created
#'   by `cbc_design()`
#' @param exclude Optional character vector of attribute names to exclude
#' @return Invisibly returns a list containing the coding information, but primarily
#'   prints formatted information to the console
#' @export
#' @examples
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price     = seq(1, 5, 0.5),
#'   type      = c('Fuji', 'Gala', 'Honeycrisp'),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Generate design
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_resp = 100,
#'   n_alts = 3,
#'   n_q = 6
#' )
#'
#' # View attribute levels and coding
#' cbc_levels(design)
cbc_levels <- function(design, exclude = NULL) {
  # Get attribute columns (excluding metadata)
  attr_cols <- get_var_names(design)

  if (!is.null(exclude)) {
    attr_cols <- setdiff(attr_cols, exclude)
  }

  # Process each attribute
  attr_info <- list()

  cat("CBC Design Attribute Information:\n")
  cat("===============================\n\n")

  for (attr in attr_cols) {
    values <- design[[attr]]
    if (is.numeric(values)) {
      # Continuous variable
      cat(sprintf("%-12s: Continuous variable\n", attr))
      cat(sprintf("              Range: %.2f to %.2f\n",
                  min(values), max(values)))
      cat("              Coefficient represents effect of one-unit change\n\n")

      attr_info[[attr]] <- list(
        type = "continuous",
        range = range(values)
      )

    } else {
      # Categorical variable
      levels <- unique(sort(as.character(values)))
      n_levels <- length(levels)
      base_level <- levels[1]
      coded_levels <- levels[-1]

      cat(sprintf("%-12s: Categorical variable (%d levels)\n", attr, n_levels))
      cat("              Base level:", base_level, "\n")
      for (i in seq_along(coded_levels)) {
        cat(sprintf("              β%-2d: %s\n",
                    i, coded_levels[i]))
      }
      cat("\n")

      attr_info[[attr]] <- list(
        type = "categorical",
        base_level = base_level,
        coded_levels = coded_levels
      )
    }
  }

  # Example prior specification
  cat("Example prior specification:\n")
  cat("------------------------\n")
  cat("priors <- cbc_priors(\n")

  for (attr in attr_cols) {
    if (attr_info[[attr]]$type == "continuous") {
      cat(sprintf("    %-12s = 0,  # Effect of one-unit change\n", attr))
    } else {
      coefs <- rep("0", length(attr_info[[attr]]$coded_levels))
      cat(sprintf("    %-12s = c(%s),  # vs %s\n",
                  attr,
                  paste(coefs, collapse = ", "),
                  attr_info[[attr]]$base_level))
    }
  }

  cat("    # Add sd = list(...) for random parameters\n")
  cat(")\n")

  invisible(attr_info)
}

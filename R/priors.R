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
  meta_cols <- c("profileID", "respID", "qID", "altID", "obsID", "blockID")
  attr_cols <- setdiff(names(design), meta_cols)

  if (!is.null(exclude)) {
    attr_cols <- setdiff(attr_cols, exclude)
  }

  # Function to determine if variable is continuous
  is_continuous <- function(x) {
    is.numeric(x) && length(unique(x)) > 10
  }

  # Process each attribute
  attr_info <- list()

  cat("CBC Design Attribute Information:\n")
  cat("===============================\n\n")

  for (attr in attr_cols) {
    values <- design[[attr]]
    if (is_continuous(values)) {
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

#' Create prior specifications for CBC models
#'
#' Creates a standardized prior specification object for use in CBC analysis
#' functions like cbc_choices() and cbc_d_error().
#'
#' @param ... Named arguments for each parameter's priors. For continuous variables,
#'   provide a single value. For categorical variables, provide a vector of values
#'   one less than the number of levels (dummy coding).
#' @param sd Optional named list of standard deviations for random parameters
#' @param correlation Optional correlation matrix for random parameters
#' @param distribution Optional named vector specifying distribution type for random
#'   parameters ("normal" or "lognormal")
#' @return A structured prior specification object
#' @export
#' @examples
#' # Example 1: Simple fixed parameters
#' priors <- cbc_priors(
#'   price = -0.5,
#'   type = c(0.2, 0.3),          # Dummy-coded categorical
#'   freshness = c(0.4, 0.8)      # Dummy-coded categorical
#' )
#'
#' # Example 2: Independent random parameters
#' priors <- cbc_priors(
#'   price = -0.5,
#'   type = c(0.2, 0.3),
#'   freshness = c(0.4, 0.8),
#'   sd = list(
#'     price = 0.4,
#'     type = c(0.4, 0.4)
#'   )
#' )
#'
#' # Example 3: Correlated random parameters
#' priors <- cbc_priors(
#'   price = -0.5,
#'   type = c(0.2, 0.3),
#'   freshness = c(0.4, 0.8),
#'   sd = list(
#'     price = 0.4,
#'     type = c(0.4, 0.4),
#'     freshness = c(0.4, 0.4)
#'   ),
#'   correlation = matrix(c(
#'     1.00, 0.30, 0.30, 0.00, 0.00,
#'     0.30, 1.00, 0.30, 0.00, 0.00,
#'     0.30, 0.30, 1.00, 0.00, 0.00,
#'     0.00, 0.00, 0.00, 1.00, 0.30,
#'     0.00, 0.00, 0.00, 0.30, 1.00
#'   ), 5, 5)
#' )
#'
#' # Example 4: Mixed distributions
#' priors <- cbc_priors(
#'   price = -0.5,
#'   type = c(0.2, 0.3),
#'   freshness = c(0.4, 0.8),
#'   sd = list(
#'     price = 0.4,
#'     type = c(0.4, 0.4)
#'   ),
#'   distribution = c(
#'     price = "lognormal",
#'     type = "normal"
#'   )
#' )
cbc_priors <- function(..., sd = NULL, correlation = NULL, distribution = NULL) {
  # Capture the means
  means <- list(...)

  # Validate inputs
  if (!is.null(sd) && !is.list(sd)) {
    stop("sd must be a named list")
  }
  if (!is.null(distribution) && !is.vector(distribution)) {
    stop("distribution must be a named vector")
  }

  # Create the prior specification object
  prior_spec <- list(
    means = means,
    sd = sd,
    correlation = correlation,
    distribution = distribution
  )

  # Add class for potential method dispatch
  class(prior_spec) <- c("cbc_priors", "list")

  return(prior_spec)
}

# Helper function to process priors into matrix form for internal use
process_priors <- function(prior_spec) {
  if (!inherits(prior_spec, "cbc_priors")) {
    stop("Prior specification must be created with cbc_priors()")
  }

  # Extract means and create flattened vector
  means <- unlist(prior_spec$means)
  param_names <- names(means)

  # Process standard deviations
  if (!is.null(prior_spec$sd)) {
    sds <- unlist(prior_spec$sd)
    # Fill in zeros for fixed parameters
    all_sds <- rep(0, length(means))
    names(all_sds) <- names(means)
    all_sds[names(sds)] <- sds
  } else {
    all_sds <- rep(0, length(means))
    names(all_sds) <- names(means)
  }

  # Create covariance matrix
  if (!is.null(prior_spec$correlation)) {
    if (nrow(prior_spec$correlation) != length(means)) {
      stop("Correlation matrix dimensions don't match number of parameters")
    }
    sigma <- diag(all_sds) %*% prior_spec$correlation %*% diag(all_sds)
  } else {
    sigma <- diag(all_sds^2)
  }

  # Process distributions
  if (!is.null(prior_spec$distribution)) {
    distributions <- prior_spec$distribution
    # Validate distribution types
    valid_dist <- c("normal", "lognormal")
    if (!all(distributions %in% valid_dist)) {
      stop("Invalid distribution type. Must be 'normal' or 'lognormal'")
    }
  } else {
    distributions <- rep("normal", length(means))
    names(distributions) <- names(means)
  }

  return(list(
    means = means,
    sigma = sigma,
    param_names = param_names,
    distributions = distributions
  ))
}

# Print method for cbc_priors objects
#' @export
print.cbc_priors <- function(x, ...) {
  cat("CBC Prior Specification:\n\n")

  # Print means
  cat("Means:\n")
  print(x$means)

  # Print SDs if present
  if (!is.null(x$sd)) {
    cat("\nStandard Deviations:\n")
    print(x$sd)
  }

  # Print correlation if present
  if (!is.null(x$correlation)) {
    cat("\nCorrelation Matrix:\n")
    print(x$correlation)
  }

  # Print distributions if present
  if (!is.null(x$distribution)) {
    cat("\nDistributions:\n")
    print(x$distribution)
  }

  invisible(x)
}

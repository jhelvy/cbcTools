#' Compute D-error for a choice experiment design
#'
#' @param design A data frame containing a choice experiment design created
#'   by `cbc_design()`
#' @param priors A numeric vector of prior parameter values, list with
#'   mean/sigma for Bayesian priors, or `NULL` for D0-error (default)
#' @param n_draws Number of draws for DB-error calculation if using Bayesian
#'   priors
#' @param exclude Character vector of attribute names to exclude from D-error
#'   calculation
#' @return The D-error value
#' @details
#' For Bayesian priors (DB-error), you can specify either:
#' * A list with 'mean' and 'sd' vectors for uncorrelated parameters
#' * A list with 'mean' vector and 'sigma' matrix for correlated parameters
#'
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
#' # DB-error with uncorrelated priors
#' db_error1 <- cbc_d_error(design,
#'   priors = list(
#'     mean = c(-0.5, 0.2, 0.3, 0.4, 0.8),
#'     sd = rep(0.4, 5)
#'   ))
#'
#' # DB-error with correlated priors
#' sigma <- matrix(c(
#'   0.16, 0.06, 0.06, 0.00, 0.00,
#'   0.06, 0.16, 0.06, 0.00, 0.00,
#'   0.06, 0.06, 0.16, 0.00, 0.00,
#'   0.00, 0.00, 0.00, 0.16, 0.06,
#'   0.00, 0.00, 0.00, 0.06, 0.16
#' ), 5, 5)
#' db_error2 <- cbc_d_error(design,
#'   priors = list(
#'     mean = c(-0.5, 0.2, 0.3, 0.4, 0.8),
#'     sigma = sigma
#'   ))
#' @export
cbc_d_error <- function(
  design,
  priors  = NULL,
  n_draws = 1000,
  exclude = NULL
) {

  # Validate that input is a cbc_design object
  required_cols <- c("profileID", "respID", "qID", "altID", "obsID")
  if (!all(required_cols %in% names(design))) {
    stop("Design must be created by cbc_design()")
  }

  # Get attribute columns (all columns except the required ones)
  attr_cols <- setdiff(names(design), required_cols)

  # Remove excluded attributes
  if (!is.null(exclude)) {
    attr_cols <- setdiff(attr_cols, exclude)
  }

  if (length(attr_cols) == 0) {
    stop("No attribute columns found in design")
  }

  # Convert design to model matrices by question
  X_list <- design_to_matrices(design, attr_cols)

  # Get number of parameters
  K <- ncol(X_list[[1]])

  # Handle different prior cases
  if (is.null(priors)) {
    # D0-error: Use zero priors
    priors <- rep(0, K)
    d_error <- compute_dp_error(X_list, priors)

  } else if (is.numeric(priors)) {
    # DP-error: Use provided point priors
    if (length(priors) != K) {
      stop(sprintf("Length of priors (%d) must match number of parameters (%d)",
                  length(priors), K))
    }
    d_error <- compute_dp_error(X_list, priors)

  } else if (is.list(priors)) {
    # DB-error: Validate inputs
    if (!("mean" %in% names(priors))) {
      stop("Prior list must contain 'mean' vector")
    }
    if (length(priors$mean) != K) {
      stop("Length of prior means must match number of parameters")
    }

    # Handle both sd vector and sigma matrix cases
    if ("sd" %in% names(priors)) {
      if (length(priors$sd) != K) {
        stop("Length of prior sds must match number of parameters")
      }
      # Convert sd vector to diagonal sigma matrix
      sigma <- diag(priors$sd^2)
    } else if ("sigma" %in% names(priors)) {
      sigma <- priors$sigma
      if (!is.matrix(sigma) || nrow(sigma) != K || ncol(sigma) != K) {
        stop("sigma must be a KxK matrix where K is number of parameters")
      }
      if (!isSymmetric(sigma)) {
        stop("sigma must be a symmetric matrix")
      }
      if (!all(eigen(sigma)$values > 0)) {
        stop("sigma must be positive definite")
      }
    } else {
      stop("Prior list must contain either 'sd' vector or 'sigma' matrix")
    }

    d_error <- compute_db_error(X_list, priors$mean, sigma, n_draws)

  } else {
    stop("priors must be NULL, numeric vector, or list with mean and sd/sigma")
  }

  return(d_error)
}

#' Convert cbc_design to list of model matrices by question
design_to_matrices <- function(design, attr_cols) {
  design <- orderedFactorsToChars(design) # ordered factors cause weird names
  formula <- stats::as.formula(paste0("~ ", paste(attr_cols, collapse = " + ")))
  X <- getDesignMatrix(formula, design)

    # Split design by question - come back here
  questions <- split(design, list(design$respID, design$qID))
  return(X_list)
  return(X)
}

# Ordered factors have strange returned column names when encoding with
# model.matrix, so convert them to characters
orderedFactorsToChars <- function(data) {
  types <- getColumnTypes(data)
  names <- names(types[types == "ordered"])
  if (length(names) > 0) {
    for (i in 1:length(names)) {
      data[,names[i]] <- factor(data[,names[i]], ordered = FALSE)
    }
  }
  return(data)
}

getColumnTypes <- function(data) {
  return(unlist(lapply(lapply(data, class), function(x) x[1])))
}

getDesignMatrix <- function(formula, data) {
  tmp <- stats::model.matrix(formula, data)
  X <- tmp[,-1,drop=FALSE] # Drop intercept
  attr(X, "contrasts") <- attr(tmp, "contrasts")
  attr(X, "assign") <- attr(tmp, "assign")[-1]
  return(X)
}


#' Compute DP-error for a given design and priors
#' @noRxport
compute_dp_error <- function(X_list, priors) {
  # Initialize information matrix
  K <- length(priors)
  M <- matrix(0, K, K)

  # Compute information matrix for each choice set
  for (X in X_list) {
    # Compute utilities and choice probabilities
    utilities <- X %*% priors
    probs <- exp(utilities) / sum(exp(utilities))

    # Add to information matrix
    P <- diag(as.vector(probs)) - tcrossprod(probs)
    M <- M + t(X) %*% P %*% X
  }

  # Compute D-error
  d_error <- det(solve(M))^(1/K)
  return(d_error)
}

#' Compute DB-error using Monte Carlo simulation
#' @noRxport
compute_db_error <- function(X_list, mu, sigma, n_draws) {
  # Generate random draws from multivariate normal distribution
  draws <- MASS::mvrnorm(n = n_draws, mu = mu, Sigma = sigma)

  # Compute DP-error for each draw
  d_errors <- apply(draws, 1, function(priors) {
    compute_dp_error(X_list, priors)
  })

  # DB-error is average of DP-errors
  return(mean(d_errors))
}














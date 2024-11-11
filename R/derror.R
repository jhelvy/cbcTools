#' Compute D-error for a choice experiment design
#'
#' @param design A data frame containing a choice experiment design created
#'   by `cbc_design()`
#' @param priors A numeric vector of prior parameter values, list with
#'   mean/sigma for Bayesian priors, or `NULL` for D0-error (default)
#' @param obsID The name of the column in `design` that identifies each choice
#' observation. Defaults to `"obsID"`.
#' @param n_draws Number of draws for DB-error calculation if using Bayesian
#'   priors. Defaults to `100`.
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
  priors = NULL,
  n_draws = 100,
  exclude = NULL
) {

  # Validate that input is a cbc_design object
  if (!all(get_id_names() %in% names(design))) {
    stop("Design must be created by cbc_design()")
  }

  # Get attribute columns (all columns except the required ones)
  pars <- get_var_names(design)

  # Remove excluded attributes
  if (!is.null(exclude)) {
    pars <- setdiff(pars, exclude)
  }

  if (length(pars) == 0) {
    stop("No attribute columns found in design")
  }

  # Define the prior model
  null_prior <- FALSE
  if (is.null(priors)) {
    priors <- setup_null_priors(design, pars)
    null_prior <- TRUE
  }

  # Get predicted probabilities
  design <- sim_probs_prior(design, model, null_prior)

  # Convert design to model matrices by question
  obsID_vector <- design$obsID
  X_list <- split(as.data.frame(model$data$X), obsID_vector)
  X_list <- lapply(X_list, as.matrix)

  # Convert probabilities into list of vectors
  P_list <- split(design$predicted_prob, obsID_vector)

  # Compute D-error
  d_error <- compute_d_error(X_list, P_list)

  return(d_error)
}

setup_null_priors <- function(design, pars) {
  X <- design[pars]
  n_levels <- apply(X, 2, function(x) length(unique(x)))
  types <- unlist(lapply(lapply(X, class), function(x) x[1]))
  discIDs <- which(types %in% c("character", "factor"))
  priors <- as.list(stats::setNames(rep(0, length(pars)), pars))
  for (i in 1:length(discIDs)) {
    id <- discIDs[i]
    priors[[id]] <- rep(0, n_levels[id] - 1)
  }
  return(priors)
}

sim_probs_prior <- function(design, model, null_prior) {
  if (null_prior) {
    reps <- table(design['obsID'])
    probs <- 1 / reps
    design$predicted_prob <- rep(probs, times = reps)
    return(design)
  }
  result <- stats::predict(
    object     = model,
    newdata    = design,
    obsID      = 'obsID',
    type       = "prob",
    returnData = TRUE
  )
  return(result)
}

compute_d_error <- function(X_list, P_list) {
  # Initialize information matrix
  K <- ncol(X_list[[1]])
  M <- matrix(0, K, K)

  # Compute information matrix for each choice set
  for (i in seq_len(length(X_list))) {
    X <- X_list[[i]]
    P <- P_list[[i]]

    # Add to information matrix
    P <- diag(as.vector(P)) - tcrossprod(P)
    M <- M + t(X) %*% P %*% X
  }

  # Compute D-error
  d_error <- det(M)^(-1/K)
  return(d_error)
}

#' Compute DB-error using Monte Carlo simulation
compute_db_error <- function(X_list, mu, sigma, n_draws) {
  # Generate random draws from multivariate normal distribution
  draws <- MASS::mvrnorm(n = n_draws, mu = mu, Sigma = sigma)

  # Compute DP-error for each draw
  d_errors <- apply(draws, 1, function(priors) {
    compute_d_error(X_list, priors)
  })

  # DB-error is average of DP-errors
  return(mean(d_errors))
}

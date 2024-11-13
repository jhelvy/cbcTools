#' Compute D-error for a choice experiment design
#'
#' @param design A data frame containing a choice experiment design created
#'   by `cbc_design()`
#' @param priors A cbc_priors object created by cbc_priors(), or NULL for D0-error
#' @param exclude Character vector of attribute names to exclude from D-error
#'   calculation
#' @return The D-error value
#' @details
#' Computes either D-error (with fixed priors) or DB-error (with random draws)
#' depending on whether parameter draws are included in the priors object.
#' @export
cbc_d_error <- function(design, priors = NULL, exclude = NULL) {
  # Validate that input is a cbc_design object
  if (!all(get_id_names() %in% names(design))) {
    stop("Design must be created by cbc_design()")
  }

  # Validate priors object and set up objects for random pars
  validate_priors(priors)
  randPars <- get_rand_pars(priors)
  par_draws <- priors$par_draws

  # Get attribute columns (excluding metadata columns)
  atts <- get_var_names(design)

  # Remove excluded attributes
  if (!is.null(exclude)) {
    atts <- setdiff(atts, exclude)
  }
  if (length(atts) == 0) {
    stop("No attribute columns found in design after exclusions")
  }

  # Encode design matrix and split into list by obsID
  obsID <- design$obsID
  reps <- table(obsID)
  codedData <- logitr::recodeData(design, atts, randPars)
  X <- codedData$X
  X_list <- split(as.data.frame(X), obsID)
  X_list <- lapply(X_list, as.matrix)

  # If no draws, then compute standard D error, otherwise use DB error
  if (is.null(par_draws)) {
    # Get predicted probabilities, then compute error
    probs <- compute_probs(obsID, reps, atts, priors, X)
    d_error <- compute_d_error(X_list, probs, obsID)
    return(d_error)
  }
  return(compute_db_error(par_draws, X, X_list, obsID, reps))
}

compute_probs <- function(obsID, reps, atts, priors, X) {
  if (is.null(priors)) {
    return(compute_probs_null(reps))
  }
  return(compute_probs_prior(obsID, reps, atts, priors, X))
}

compute_probs_null <- function(reps) {
  probs <- 1 / reps
  return(rep(probs, times = reps))
}

compute_probs_prior <- function(obsID, reps, atts, priors, X) {
  V <- X %*% priors$pars
  return(logit(V, obsID, reps))
}

# Helper function to predict choice probabilities
logit <- function(V, obsID, reps) {
  expV <- exp(V)
  sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
  return(expV / sumExpV[rep(seq_along(reps), reps),])
}

logit_draws <- function(par_draws, X, obsID, reps) {
  VDraws <- X %*% t(par_draws)
  return(logit(VDraws, obsID, reps))
}

compute_d_error <- function(X_list, probs, obsID) {
  # Convert probabilities into list of vectors
  P_list <- split(probs, obsID)

  # Initialize information matrix
  K <- ncol(X_list[[1]])

  # Compute information matrix components for all observations at once
  M <- Reduce(`+`, Map(function(x, p) {
    p_mat <- diag(p) - tcrossprod(p)
    crossprod(x, p_mat %*% x)
  }, X_list, P_list))

  # Compute D-error
  return(det(M)^(-1/K))
 }

# compute_d_error_slower <- function(X_list, probs, obsID) {
#   # Convert probabilities into list of vectors
#   P_list <- split(probs, obsID)
#
#   # Initialize information matrix
#   K <- ncol(X_list[[1]])
#   M <- matrix(0, K, K)
#
#   # Compute information matrix for each choice set
#   for (i in seq_len(length(X_list))) {
#     X <- X_list[[i]]
#     P <- P_list[[i]]
#
#     # Add to information matrix
#     P <- diag(as.vector(P)) - tcrossprod(P)
#     M <- M + t(X) %*% P %*% X
#   }
#
#   # Compute D-error
#   d_error <- det(M)^(-1/K)
#   return(d_error)
# }

compute_db_error <- function(par_draws, X, X_list, obsID, reps) {
  P_draws <- logit_draws(par_draws, X, obsID, reps)
  n_draws <- ncol(P_draws)

  # Compute DP-error for each draw
  d_errors <- apply(P_draws, 2, function(x) {
    compute_d_error(X_list, x, obsID)
  })

  # DB-error is average of DP-errors
  d_error <- mean(d_errors)
  return(d_error)
}

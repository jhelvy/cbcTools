# Helper functions

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

#' Compute design error metrics for a choice experiment design
#'
#' This function computes design efficiency metrics for choice experiments.
#' By default, it returns only D-error, but can compute multiple optimality
#' criteria including D-, A-, G-, and E-error as well as condition numbers.
#'
#' @param x Either a `cbc_design` object created by `cbc_design()`, a `cbc_survey`
#'   object, or a data frame containing a choice experiment design
#' @param errors Character vector specifying which error metrics to compute.
#'   Options: "d" (D-error), "a" (A-error), "g" (G-error), "e" (E-error),
#'   "all" (comprehensive metrics). Defaults to "d". When priors contain
#'   random parameters, Bayesian versions are automatically computed.
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL` for
#'   null priors. If `x` is a `cbc_design` object, the priors from that object
#'   will be used by default.
#' @param exclude Character vector of attribute names to exclude from error
#'   calculations
#' @return For single error types ("d"), returns the numeric error value.
#'   For multiple error types or "all", returns a list containing requested metrics:
#'   \itemize{
#'     \item d_error: D-optimality criterion (lower is better)
#'     \item a_error: A-optimality criterion (lower is better)
#'     \item g_error: G-optimality criterion (lower is better)
#'     \item e_error: E-optimality criterion (higher is better)
#'   }
#' @details
#' The different error criteria measure different aspects of design efficiency:
#' \itemize{
#'   \item D-error: Minimizes generalized variance (determinant of covariance matrix)
#'   \item A-error: Minimizes average variance (trace of covariance matrix)
#'   \item G-error: Minimizes maximum prediction variance
#'   \item E-error: Maximizes minimum eigenvalue (related to worst-estimated parameter)
#' }
#'
#' **Computational Approach**: The function automatically detects whether priors
#' contain random parameters (created with \code{rand_spec()}). If random parameters
#' are present, all error metrics are computed using a Bayesian approach that
#' averages results across parameter draws. If priors contain only fixed parameters,
#' standard (non-Bayesian) calculations are used.
#' @export
#' @examples
#' # Create profiles and design
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C")
#' )
#'
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # Compute D-error only (default)
#' d_error <- cbc_error(design)
#'
#' # Compute comprehensive design error metrics
#' errors <- cbc_error(design, errors = "all")
#' print(errors)
#'
#' # Compute specific error types
#' multiple_errors <- cbc_error(design, errors = c("d", "a", "g"))
#'
#' # With random priors, Bayesian approach is automatically used
#' priors_random <- cbc_priors(
#'   profiles = profiles,
#'   price = rand_spec(dist = "n", mean = -0.5, sd = 0.2),
#'   type = c(0.2, 0.3)
#' )
#'
#' # This will show message about Bayesian approach
#' bayesian_errors <- cbc_error(design, errors = c("d", "a"), priors = priors_random)
cbc_error <- function(x, errors = "d", priors = NULL, exclude = NULL) {
  UseMethod("cbc_error")
}

#' @rdname cbc_error
#' @export
cbc_error.cbc_survey <- function(x, errors = "d", priors = NULL, exclude = NULL) {
  # Get design reference from survey
  design_ref <- attr(x, "design_ref")

  # Use priors from design object if not specified
  if (is.null(priors)) {
    if (!is.null(design_ref)) {
      priors <- design_ref$priors
    }
  } else {
    # Validate provided priors against survey's profiles
    if (!inherits(priors, "cbc_priors")) {
      stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    if (!is.null(design_ref)) {
      validate_priors_profiles(priors, design_ref$profiles)
    }
  }

  # Call the data frame method
  cbc_error.data.frame(x, errors, priors, exclude)
}

#' @rdname cbc_error
#' @export
cbc_error.cbc_design <- function(x, errors = "d", priors = NULL, exclude = NULL) {
  # Use priors from design object if not specified
  if (is.null(priors)) {
    priors <- attr(x, "priors")
  } else {
    # Validate provided priors against design's profiles
    if (!inherits(priors, "cbc_priors")) {
      stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    validate_priors_profiles(priors, attr(x, "profiles"))
  }

  # Call the data frame method (x is now the data frame directly)
  cbc_error.data.frame(x, errors, priors, exclude)
}

#' @rdname cbc_error
#' @export
cbc_error.data.frame <- function(x, errors = "d", priors = NULL, exclude = NULL) {
  # Validate that input is a proper design data frame
  if (!all(get_id_names() %in% names(x))) {
    stop("Design must be created by cbc_design() or contain the required ID columns: ",
         paste(get_id_names(), collapse = ", "))
  }

  # Validate priors object and set up objects for random pars
  validate_priors(priors)
  randPars <- get_rand_pars(priors)
  par_draws <- if (!is.null(priors)) priors$par_draws else NULL

  # Get attribute columns (excluding metadata columns)
  atts <- get_var_names(x)

  # Remove excluded attributes
  if (!is.null(exclude)) {
    atts <- setdiff(atts, exclude)
  }
  if (length(atts) == 0) {
    stop("No attribute columns found in design after exclusions")
  }

  # Encode design matrix and split into list by obsID
  obsID <- x$obsID
  reps <- table(obsID)
  codedData <- logitr::recodeData(x, atts, randPars)
  X <- codedData$X
  X_list <- split(as.data.frame(X), obsID)
  X_list <- lapply(X_list, as.matrix)

  # Validate errors argument
  valid_errors <- c("d", "a", "g", "e", "all")
  if (!all(errors %in% valid_errors)) {
    invalid <- setdiff(errors, valid_errors)
    stop("Invalid error types: ", paste(invalid, collapse = ", "),
         ". Valid options are: ", paste(valid_errors, collapse = ", "))
  }

  # Inform user about Bayesian approach when random parameters are present
  if (!is.null(par_draws)) {
    message(
      "Using Bayesian approach: Averaging error metrics across ",
      nrow(par_draws), " draws from each of ",
      ncol(par_draws), " random parameters"
    )
  }

  # Compute metrics based on requested error types
  if (is.null(par_draws)) {
    # Get predicted probabilities, then compute metrics
    probs <- compute_probs(obsID, reps, atts, priors, X)

    if ("all" %in% errors) {
      # Return comprehensive metrics
      metrics <- compute_all_efficiency_metrics(X_list, probs, obsID)
      class(metrics) <- c("cbc_errors", "list")
      return(metrics)
    } else {
      # Compute specific metrics
      metrics <- compute_selected_efficiency_metrics(X_list, probs, obsID, errors)
    }
  } else {
    # Use Bayesian approach with parameter draws
    if ("all" %in% errors) {
      metrics <- compute_bayesian_efficiency_metrics(par_draws, X, X_list, obsID, reps)
      class(metrics) <- c("cbc_errors", "list")
      return(metrics)
    } else {
      # Compute Bayesian versions of requested metrics
      metrics <- compute_bayesian_selected_metrics(par_draws, X, X_list, obsID, reps, errors)
    }
  }

  # For single error types, return just the value
  if (length(errors) == 1 && errors[1] == "d") {
    return(metrics[["d_error"]])
  }

  # For multiple error types, return list with class
  class(metrics) <- c("cbc_errors", "list")
  return(metrics)
}

# Helper function to compute selected efficiency metrics
compute_selected_efficiency_metrics <- function(X_list, probs, obsID, errors) {
  # Convert probabilities into list of vectors
  P_list <- split(probs, obsID)

  # Initialize information matrix
  K <- ncol(X_list[[1]])

  # Compute information matrix components for all observations at once
  M <- Reduce(`+`, Map(function(x, p) {
    p_mat <- diag(p) - tcrossprod(p)
    crossprod(x, p_mat %*% x)
  }, X_list, P_list))

  # Initialize results list
  selected <- list()

  # Compute only requested metrics
  for (error_type in errors) {
    if (error_type == "d") {
      # D-optimality: minimize det(Cov) = maximize det(Info)
      det_M <- det(M)
      selected$d_error <- if (det_M > 0) det_M^(-1/K) else Inf

    } else if (error_type == "a") {
      # A-optimality: minimize trace(Cov) - need eigenvalues of inverse
      if (det(M) <= .Machine$double.eps) {
        Minv <- MASS::ginv(M)
      } else {
        Minv <- solve(M)
      }
      eigenvals_cov <- eigen(Minv, only.values = TRUE)$values
      eigenvals_cov <- eigenvals_cov[eigenvals_cov > .Machine$double.eps]
      selected$a_error <- sum(eigenvals_cov)

    } else if (error_type == "e") {
      # E-optimality: maximize min eigenvalue of Info matrix
      eigenvals <- eigen(M, only.values = TRUE)$values
      eigenvals <- eigenvals[eigenvals > .Machine$double.eps]
      selected$e_error <- min(eigenvals)

    } else if (error_type == "g") {
      # G-optimality: requires computing prediction variances
      if (det(M) <= .Machine$double.eps) {
        Minv <- MASS::ginv(M)
      } else {
        Minv <- solve(M)
      }
      selected$g_error <- compute_g_error_approx(X_list, Minv, P_list)
    }
  }

  return(selected)
}

# Helper function for Bayesian selected metrics
compute_bayesian_selected_metrics <- function(par_draws, X, X_list, obsID, reps, errors) {
  P_draws <- logit_draws(par_draws, X, obsID, reps)
  n_draws <- ncol(P_draws)

  # Compute Bayesian versions of requested metrics by averaging across draws
  selected <- list()

  for (error_type in errors) {
    if (error_type == "d") {
      # Use the dedicated DB-error function for D-error (most efficient)
      selected$d_error <- compute_db_error(par_draws, X, X_list, obsID, reps)
    } else {
      # For other metrics, compute them for each draw and average
      metric_values <- apply(P_draws, 2, function(p) {
        result <- compute_selected_efficiency_metrics(X_list, p, obsID, error_type)
        result[[paste0(error_type, "_error")]]
      })
      selected[[paste0(error_type, "_error")]] <- mean(metric_values)
    }
  }

  return(selected)
}

# Helper function to compute all efficiency metrics from information matrix
compute_all_efficiency_metrics <- function(X_list, probs, obsID) {
  # Convert probabilities into list of vectors
  P_list <- split(probs, obsID)

  # Initialize information matrix
  K <- ncol(X_list[[1]])

  # Compute information matrix components for all observations at once
  M <- Reduce(`+`, Map(function(x, p) {
    p_mat <- diag(p) - tcrossprod(p)
    crossprod(x, p_mat %*% x)
  }, X_list, P_list))

  # Check if matrix is invertible
  if (det(M) <= .Machine$double.eps) {
    warning("Information matrix is singular or near-singular. Results may be unreliable.")
    # Use pseudoinverse for singular matrices
    Minv <- MASS::ginv(M)
    det_M <- 0
  } else {
    Minv <- solve(M)
    det_M <- det(M)
  }

  # Compute eigenvalues for various criteria
  eigenvals <- eigen(M, only.values = TRUE)$values
  eigenvals_cov <- eigen(Minv, only.values = TRUE)$values

  # Remove negative eigenvalues (numerical precision issues)
  eigenvals <- eigenvals[eigenvals > .Machine$double.eps]
  eigenvals_cov <- eigenvals_cov[eigenvals_cov > .Machine$double.eps]

  # D-optimality: minimize det(Cov) = maximize det(Info)
  d_error <- if (det_M > 0) det_M^(-1/K) else Inf

  # A-optimality: minimize trace(Cov)
  a_error <- sum(eigenvals_cov)

  # E-optimality: maximize min eigenvalue of Info matrix
  e_error <- min(eigenvals)

  # Condition number: max eigenvalue / min eigenvalue
  condition_number <- if (length(eigenvals) > 0 && min(eigenvals) > 0) {
    max(eigenvals) / min(eigenvals)
  } else {
    Inf
  }

  # G-optimality: requires design matrix computation (approximation)
  # G-optimality minimizes max prediction variance across design points
  g_error <- compute_g_error_approx(X_list, Minv, P_list)

  return(list(
    d_error = d_error,
    a_error = a_error,
    g_error = g_error,
    e_error = e_error,
    condition_number = condition_number,
    det_information = det_M,
    trace_information = sum(eigenvals),
    eigenvalues = eigenvals,
    information_matrix = M
  ))
}

# Helper function for G-optimality approximation
compute_g_error_approx <- function(X_list, Minv, P_list) {
  # G-optimality: max prediction variance over design region
  # For choice experiments, compute max variance over observed design points
  max_var <- 0

  for (i in seq_along(X_list)) {
    X_obs <- X_list[[i]]
    # Prediction variance for each alternative in this choice set
    for (j in seq_len(nrow(X_obs))) {
      x_vec <- X_obs[j, , drop = FALSE]
      pred_var <- x_vec %*% Minv %*% t(x_vec)
      max_var <- max(max_var, pred_var)
    }
  }

  return(as.numeric(max_var))
}

# Bayesian version for random parameters
compute_bayesian_efficiency_metrics <- function(par_draws, X, X_list, obsID, reps) {
  P_draws <- logit_draws(par_draws, X, obsID, reps)
  n_draws <- ncol(P_draws)

  # Compute metrics for each draw
  metrics_draws <- apply(P_draws, 2, function(p) {
    compute_all_efficiency_metrics(X_list, p, obsID)
  })

  # Average across draws
  avg_metrics <- list(
    d_error = mean(sapply(metrics_draws, function(x) x$d_error)),
    a_error = mean(sapply(metrics_draws, function(x) x$a_error)),
    g_error = mean(sapply(metrics_draws, function(x) x$g_error)),
    e_error = mean(sapply(metrics_draws, function(x) x$e_error)),
    condition_number = mean(sapply(metrics_draws, function(x) x$condition_number)),
    det_information = mean(sapply(metrics_draws, function(x) x$det_information)),
    trace_information = mean(sapply(metrics_draws, function(x) x$trace_information)),
    eigenvalues = apply(sapply(metrics_draws, function(x) x$eigenvalues), 1, mean)
  )

  return(avg_metrics)
}

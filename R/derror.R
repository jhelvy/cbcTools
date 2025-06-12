#' Compute D-error for a choice experiment design
#'
#' @param x Either a `cbc_design` object created by `cbc_design()` or a data frame
#'   containing a choice experiment design
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL` for
#'   D0-error. If `x` is a `cbc_design` object, the priors from that object will
#'   be used by default.
#' @param exclude Character vector of attribute names to exclude from D-error
#'   calculation
#' @param ... Additional arguments passed to methods
#' @return The D-error value
#' @details
#' Computes either D-error (with fixed priors) or DB-error (with random draws)
#' depending on whether parameter draws are included in the priors object.
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
#' # Compute D-error using design object
#' cbc_d_error(design)
#'
#' # Create priors and compute with them
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3)
#' )
#'
#' cbc_d_error(design, priors = priors)
cbc_d_error <- function(x, ...) {
  UseMethod("cbc_d_error")
}

#' @rdname cbc_d_error
#' @export
cbc_d_error.cbc_survey <- function(x, priors = NULL, exclude = NULL, ...) {
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
  cbc_d_error.data.frame(x, priors, exclude, ...)
}

#' @rdname cbc_d_error
#' @export
cbc_d_error.cbc_design <- function(x, priors = NULL, exclude = NULL, ...) {
  # Use priors from design object if not specified
  if (is.null(priors)) {
    priors <- x$priors
  } else {
    # Validate provided priors against design's profiles
    if (!inherits(priors, "cbc_priors")) {
      stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    validate_priors_profiles(priors, x$profiles)
  }

  # Call the data frame method
  cbc_d_error.data.frame(x$design, priors, exclude, ...)
}

#' @rdname cbc_d_error
#' @export
cbc_d_error.data.frame <- function(x, priors = NULL, exclude = NULL, ...) {
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

  # If no draws, then compute standard D error, otherwise use DB error
  if (is.null(par_draws)) {
    # Get predicted probabilities, then compute error
    probs <- compute_probs(obsID, reps, atts, priors, X)
    d_error <- compute_d_error(X_list, probs, obsID)
  } else {
    d_error <- compute_db_error(par_draws, X, X_list, obsID, reps)
  }

  return(d_error)
}

#' Compute DB-error for a choice experiment design with random parameters
#'
#' @param x Either a `cbc_design` object created by `cbc_design()` or a data frame
#'   containing a choice experiment design
#' @param priors A `cbc_priors` object created by `cbc_priors()` that includes
#'   random parameters. If `x` is a `cbc_design` object, the priors from that
#'   object will be used by default.
#' @param exclude Character vector of attribute names to exclude from DB-error
#'   calculation
#' @param ... Additional arguments passed to methods
#' @return The DB-error value
#' @details
#' DB-error (Bayesian D-error) accounts for uncertainty in parameter estimates
#' by averaging D-error across multiple parameter draws. This is only meaningful
#' when priors include random parameters with parameter draws.
#' @export
#' @examples
#' # Create profiles with random priors
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C")
#' )
#'
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = rand_spec(dist = "n", mean = -0.5, sd = 0.2),
#'   type = c(0.2, 0.3)
#' )
#'
#' design <- cbc_design(
#'   profiles = profiles,
#'   priors = priors,
#'   n_alts = 2,
#'   n_q = 4,
#'   method = "sequential"
#' )
#'
#' # Compute DB-error
#' cbc_db_error(design)
cbc_db_error <- function(x, ...) {
  UseMethod("cbc_db_error")
}

#' @rdname cbc_db_error
#' @export
cbc_db_error.cbc_survey <- function(x, priors = NULL, exclude = NULL, ...) {
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

  # Check that priors include random parameters
  if (is.null(priors) || is.null(priors$par_draws)) {
    stop("DB-error requires priors with random parameters. Use cbc_d_error() for fixed priors.")
  }

  # Call the data frame method
  cbc_db_error.data.frame(x, priors, exclude, ...)
}

#' @rdname cbc_db_error
#' @export
cbc_db_error.cbc_design <- function(x, priors = NULL, exclude = NULL, ...) {
  # Use priors from design object if not specified
  if (is.null(priors)) {
    priors <- x$priors
  } else {
    # Validate provided priors against design's profiles
    if (!inherits(priors, "cbc_priors")) {
      stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    validate_priors_profiles(priors, x$profiles)
  }

  # Check that priors include random parameters
  if (is.null(priors) || is.null(priors$par_draws)) {
    stop("DB-error requires priors with random parameters. Use cbc_d_error() for fixed priors.")
  }

  # Call the data frame method
  cbc_db_error.data.frame(x$design, priors, exclude, ...)
}

#' @rdname cbc_db_error
#' @export
cbc_db_error.data.frame <- function(x, priors = NULL, exclude = NULL, ...) {
  # Check that priors include random parameters
  if (is.null(priors) || is.null(priors$par_draws)) {
    stop("DB-error requires priors with random parameters. Use cbc_d_error() for fixed priors.")
  }

  # Use the same computation as D-error - it will automatically use DB-error
  # when parameter draws are present
  return(cbc_d_error.data.frame(x, priors, exclude, ...))
}

#' Compare D-errors across multiple designs
#'
#' @param ... Multiple `cbc_design` or `cbc_survey` objects to compare, or named
#'   arguments where each argument is a design or survey object
#' @param priors Optional `cbc_priors` object to use for all designs/surveys. If not
#'   specified, each object's own priors will be used.
#' @param exclude Character vector of attribute names to exclude from D-error
#'   calculations
#' @return A data frame with design names and their corresponding D-errors,
#'   sorted by D-error (best to worst)
#' @export
#' @examples
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C")
#' )
#'
#' # Create different designs
#' design_sequential <- cbc_design(profiles, method = "sequential", n_alts = 2, n_q = 4)
#'
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3)
#' )
#'
#' # Create surveys
#' survey_sequential <- cbc_survey(design_sequential, n_resp = 100)
#' survey_random <- cbc_survey(profiles, n_resp = 100)  # Random survey from profiles
#'
#' # Compare surveys
#' compare_d_errors(
#'   Sequential = survey_sequential,
#'   Random = survey_random,
#'   priors = priors
#' )
compare_d_errors <- function(..., priors = NULL, exclude = NULL) {
  objects <- list(...)

  # Get object names
  object_names <- names(objects)
  if (is.null(object_names)) {
    object_names <- paste0("Object_", seq_along(objects))
  }

  # Validate all inputs are cbc_design or cbc_survey objects
  for (i in seq_along(objects)) {
    if (!inherits(objects[[i]], c("cbc_design", "cbc_survey"))) {
      stop(sprintf("Argument %d (%s) must be a cbc_design or cbc_survey object",
                   i, object_names[i]))
    }
  }

  # Compute D-errors
  d_errors <- sapply(objects, function(obj) {
    cbc_d_error(obj, priors = priors, exclude = exclude)
  })

  # Create result data frame
  result <- data.frame(
    design = object_names,
    d_error = d_errors,
    stringsAsFactors = FALSE
  )

  # Sort by D-error (lower is better)
  result <- result[order(result$d_error), ]

  # Add efficiency relative to best
  result$relative_efficiency <- result$d_error[1] / result$d_error

  # Add ranking
  result$rank <- seq_len(nrow(result))

  # Reorder columns
  result <- result[, c("rank", "design", "d_error", "relative_efficiency")]

  # Reset row names
  rownames(result) <- NULL

  class(result) <- c("cbc_d_error_comparison", "data.frame")
  return(result)
}

#' Print method for D-error comparisons
#' @param x A cbc_d_error_comparison object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_d_error_comparison <- function(x, ...) {
  cat("D-Error Comparison\n")
  cat("==================\n")
  cat("(Lower D-error is better)\n\n")

  # Format the output nicely and remove custom class
  print_df <- x
  print_df$d_error <- sprintf("%.6f", print_df$d_error)
  print_df$relative_efficiency <- sprintf("%.2f", print_df$relative_efficiency)

  # Remove custom class to avoid infinite recursion
  class(print_df) <- "data.frame"
  print(print_df, row.names = FALSE)

  cat(sprintf("\nBest design: %s (D-error = %.6f)\n",
              x$design[1], x$d_error[1]))

  if (nrow(x) > 1) {
    worst_idx <- nrow(x)
    improvement <- (x$d_error[worst_idx] - x$d_error[1]) / x$d_error[worst_idx] * 100
    cat(sprintf("Improvement over worst: %.1f%%\n", improvement))
  }

  invisible(x)
}

# Helper functions (unchanged from original implementation)

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

#' Compute comprehensive design error metrics for a choice experiment design
#'
#' This function computes multiple optimality criteria including D-, A-, G-, and
#' E-error as well as condition numbers to provide a comprehensive evaluation
#' of design efficiency.
#'
#' @param x Either a `cbc_design` object created by `cbc_design()`, a `cbc_survey`
#'   object, or a data frame containing a choice experiment design
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL` for
#'   null priors. If `x` is a `cbc_design` object, the priors from that object
#'   will be used by default.
#' @param exclude Character vector of attribute names to exclude from error
#'   calculations
#' @return A list containing:
#'   \itemize{
#'     \item d_error: D-optimality criterion (lower is better)
#'     \item a_error: A-optimality criterion (lower is better)
#'     \item g_error: G-optimality criterion (lower is better)
#'     \item e_error: E-optimality criterion (higher is better)
#'     \item condition_number: Condition number of information matrix (lower is better)
#'     \item det_information: Determinant of information matrix
#'     \item trace_information: Trace of information matrix
#'     \item eigenvalues: Eigenvalues of information matrix
#'   }
#' @details
#' The different error criteria measure different aspects of design efficiency:
#' \itemize{
#'   \item D-error: Minimizes generalized variance (determinant of covariance matrix)
#'   \item A-error: Minimizes average variance (trace of covariance matrix)
#'   \item G-error: Minimizes maximum prediction variance
#'   \item E-error: Maximizes minimum eigenvalue (related to worst-estimated parameter)
#'   \item Condition number: Ratio of largest to smallest eigenvalue (numerical stability)
#' }
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
#' # Compute comprehensive design error metrics
#' errors <- cbc_error(design)
#' print(errors)
cbc_error <- function(x, priors = NULL, exclude = NULL) {
  UseMethod("cbc_error")
}

#' @rdname cbc_error
#' @export
cbc_error.cbc_survey <- function(x, priors = NULL, exclude = NULL) {
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
  cbc_error.data.frame(x, priors, exclude)
}

#' @rdname cbc_error
#' @export
cbc_error.cbc_design <- function(x, priors = NULL, exclude = NULL) {
  # Use priors from design object if not specified
  if (is.null(priors)) {
    priors <- x$priors
  } else {
    # Validate provided priors against design's profiles
    if (!inherits(priors, "cbc_priors")) {
      stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    validate_priors_profiles(priors, x$profiles)
  }

  # Call the data frame method
  cbc_error.data.frame(x$design, priors, exclude)
}

#' @rdname cbc_error
#' @export
cbc_error.data.frame <- function(x, priors = NULL, exclude = NULL) {
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

  # Compute all efficiency metrics
  if (is.null(par_draws)) {
    # Get predicted probabilities, then compute metrics
    probs <- compute_probs(obsID, reps, atts, priors, X)
    metrics <- compute_all_efficiency_metrics(X_list, probs, obsID)
  } else {
    # Use Bayesian approach with parameter draws
    metrics <- compute_bayesian_efficiency_metrics(par_draws, X, X_list, obsID, reps)
  }

  # Add class for printing
  class(metrics) <- c("cbc_errors", "list")
  return(metrics)
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

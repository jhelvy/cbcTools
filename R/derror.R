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
#' @param ... Multiple `cbc_design` objects to compare, or named arguments where
#'   each argument is a `cbc_design` object
#' @param priors Optional `cbc_priors` object to use for all designs. If not
#'   specified, each design's own priors will be used.
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
#' design_random <- cbc_design(profiles, n_alts = 2, n_q = 4, method = "random")
#'
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3)
#' )
#'
#' design_sequential <- cbc_design(
#'   profiles, priors = priors, n_alts = 2, n_q = 4, method = "sequential"
#' )
#'
#' # Compare designs
#' compare_d_errors(
#'   Random = design_random,
#'   Sequential = design_sequential,
#'   priors = priors
#' )
compare_d_errors <- function(..., priors = NULL, exclude = NULL) {
  designs <- list(...)

  # Get design names
  design_names <- names(designs)
  if (is.null(design_names)) {
    design_names <- paste0("Design_", seq_along(designs))
  }

  # Validate all inputs are cbc_design objects
  for (i in seq_along(designs)) {
    if (!inherits(designs[[i]], "cbc_design")) {
      stop(sprintf("Argument %d (%s) must be a cbc_design object",
                   i, design_names[i]))
    }
  }

  # Compute D-errors
  d_errors <- sapply(designs, function(design) {
    cbc_d_error(design, priors = priors, exclude = exclude)
  })

  # Create result data frame
  result <- data.frame(
    design = design_names,
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

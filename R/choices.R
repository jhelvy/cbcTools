#' Simulate choices for a survey design
#'
#' Simulate choices for a survey design, either randomly or according to a
#' utility model defined by user-provided prior parameters. All choices are
#' simulated using the 'logitr' package. For more details see the JSS article
#' on the 'logitr' package (Helveston, 2023).
#' @keywords logitr mnl mxl mixed logit simulation
#'
#' @param x Either a `cbc_survey` object created by `cbc_survey()` or a data frame
#'   containing a survey design
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL` for
#'   random choices. If `x` is a `cbc_survey` object, the priors from that
#'   object will be used by default.
#' @param n_draws The number of Halton draws to use for simulated choices
#'   for mixed logit models. Defaults to `100`.
#' @param ... Additional arguments passed to methods
#' @references
#' Helveston, J. P. (2023). logitr: Fast Estimation of Multinomial and Mixed Logit Models with Preference Space and Willingness-to-Pay Space Utility Parameterizations. Journal of Statistical Software, 105(10), 1–37,
#' \doi{10.18637/jss.v105.i10}
#' @return Returns the input data with an additional `choice` column
#' identifying the simulated choices.
#' @export
#' @examples
#' library(cbcTools)
#'
#' # A simple conjoint experiment about apples
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Make a survey design from all possible profiles
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Create survey and simulate choices
#' survey <- cbc_survey(design, n_resp = 100)
#' data_with_choices <- cbc_choices(survey)
#'
#' # Simulate choices according to a prior utility model
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price     = -0.1,
#'   type      = c(0.1, 0.2),
#'   freshness = c(0.1, 0.2)
#' )
#'
#' data <- cbc_choices(survey, priors = priors)
#'
#' # Simulate choices according to a prior utility model with random parameters
#' priors_random <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.1,
#'   type = rand_spec(mean = c(0.1, 0.2), sd = c(0.5, 0.8)),
#'   freshness = c(0.1, 0.2)
#' )
#'
#' data <- cbc_choices(survey, priors = priors_random)
cbc_choices <- function(x, ...) {
  UseMethod("cbc_choices")
}

#' @rdname cbc_choices
#' @export
cbc_choices.cbc_survey <- function(x, priors = NULL, n_draws = 100, ...) {
  # Get design reference from survey
  design_ref <- attr(x, "design_ref")

  # Use priors from design object if not specified
  if (is.null(priors)) {
    if (!is.null(design_ref)) {
      priors <- design_ref$priors
    }
    if (is.null(priors)) {
      message("No priors specified. Simulating random choices.")
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
  result <- cbc_choices.data.frame(x, priors, n_draws, ...)

  # Add metadata about the choice simulation
  survey_info <- attr(x, "survey_info")
  attr(result, "choice_info") <- list(
    design_method = if (!is.null(design_ref)) design_ref$method else "unknown",
    d_error = if (!is.null(design_ref)) design_ref$d_error else NA,
    n_respondents = survey_info$n_resp,
    priors_used = !is.null(priors),
    simulation_method = if (is.null(priors)) "random" else "utility_based",
    simulated_at = Sys.time()
  )

  class(result) <- c("cbc_choices", "data.frame")
  return(result)
}

#' @rdname cbc_choices
#' @export
cbc_choices.data.frame <- function(x, priors = NULL, n_draws = 100, ...) {
  # Validate that input has required columns
  if (!"obsID" %in% names(x)) {
    stop("Data frame must contain an 'obsID' column. Use cbc_survey() to create proper survey data.")
  }

  if (is.null(priors)) {
    result <- sim_choices_rand(x)
  } else {
    # Validate priors object
    if (!inherits(priors, "cbc_priors")) {
      stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    result <- sim_choices_prior(x, priors, n_draws)
  }

  # Add metadata about the choice simulation
  attr(result, "choice_info") <- list(
    design_method = "unknown",
    d_error = NA,
    priors_used = !is.null(priors),
    simulation_method = if (is.null(priors)) "random" else "utility_based",
    simulated_at = Sys.time()
  )

  class(result) <- c("cbc_choices", "data.frame")
  return(result)
}

# Core simulation functions

sim_choices_rand <- function(design) {
  if (!"obsID" %in% names(design)) {
    stop("Design must contain 'obsID' column")
  }

  nrows <- table(design['obsID'])
  choices <- list()
  for (i in seq_len(length(nrows))) {
    n <- nrows[i]
    choice <- rep(0, n)
    choice[sample(seq(n), 1)] <- 1
    choices[[i]] <- choice
  }
  design$choice <- unlist(choices)
  return(design)
}

sim_choices_prior <- function(survey, priors, n_draws) {
  if (!"obsID" %in% names(survey)) {
    stop("Survey must contain 'obsID' column")
  }

  # Get variable names for the model
  var_names <- get_var_names(survey)

  # Set up random parameters if any
  randPars <- get_rand_pars(priors)

  # Encode the design matrix
  codedData <- logitr::recodeData(survey, var_names, randPars)
  X <- codedData$X

  # Calculate utilities
  if (!is.null(priors$par_draws)) {
    # For random parameters, use draws
    n_draws_actual <- min(n_draws, nrow(priors$par_draws))
    # Sample from the draws
    draw_indices <- sample(nrow(priors$par_draws), n_draws_actual, replace = TRUE)
    selected_draws <- priors$par_draws[draw_indices, , drop = FALSE]

    # Calculate utilities for each draw and average
    utilities <- matrix(0, nrow = nrow(X), ncol = n_draws_actual)
    for (i in 1:n_draws_actual) {
      utilities[, i] <- X %*% selected_draws[i, ]
    }
    # Average utilities across draws
    V <- rowMeans(utilities)
  } else {
    # For fixed parameters
    V <- X %*% priors$pars
  }

  # Convert utilities to probabilities using logit
  obsID <- survey$obsID
  reps <- table(obsID)
  probs <- logit_choice_probs(V, obsID, reps)

  # Simulate choices based on probabilities
  survey$choice <- simulate_choices_from_probs(probs, obsID, reps)

  return(survey)
}

# Helper function to compute logit probabilities
logit_choice_probs <- function(V, obsID, reps) {
  expV <- exp(V)
  sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
  return(expV / sumExpV[rep(seq_along(reps), reps), ])
}

# Helper function to simulate choices from probabilities
simulate_choices_from_probs <- function(probs, obsID, reps) {
  choices <- numeric(length(probs))

  for (obs in unique(obsID)) {
    obs_rows <- which(obsID == obs)
    obs_probs <- probs[obs_rows]

    # Simulate choice based on probabilities
    chosen_alt <- sample(length(obs_probs), 1, prob = obs_probs)
    choices[obs_rows[chosen_alt]] <- 1
  }

  return(choices)
}

drop_interactions <- function(parNames) {
  ints <- grepl("\\*", parNames)
  if (any(ints)) {
    return(parNames[ints == FALSE])
  }
  return(parNames)
}

#' Simulate choices for a survey design
#'
#' Simulate choices for a survey design, either randomly or according to a
#' utility model defined by user-provided prior parameters.
#' @keywords logitr mnl mxl mixed logit simulation
#'
#' @param design A data frame of a survey design.
#' @param obsID The name of the column in `design` that identifies each choice
#' observation. Defaults to `"obsID"`.
#' @param priors A list of one or more prior parameters that define a prior
#' (assumed) utility model used to simulate choices for the `survey` data frame.
#' If `NULL` (the default), choices will be randomly assigned.
#' @param n_draws The number of Halton draws to use for simulated choices
#' for mixed logit models. Defaults to `100`.
#' @return Returns the `design` data frame with an additional `choice` column
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
#' # Make a randomized survey design
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3, # Number of alternatives per question
#'   n_q      = 6 # Number of questions per respondent
#' )
#'
#' # Simulate random choices
#' data <- cbc_choices(
#'   design = design,
#'   obsID  = "obsID"
#' )
#'
#' # Simulate choices according to a prior utility model
#' data <- cbc_choices(
#'   design = design,
#'   obsID = "obsID",
#'   priors = list(
#'     price     = 0.1,
#'     type      = c(0.1, 0.2),
#'     freshness = c(0.1, 0.2)
#'   )
#' )
#'
#' # Simulate choices according to a prior model with interactions
#' data <- cbc_choices(
#'   design = design,
#'   obsID = "obsID",
#'   priors = list(
#'     price     = 0.1,
#'     type      = c(0.1, 0.2),
#'     freshness = c(0.1, 0.2),
#'     `price*type` = c(0.1, 0.5)
#'   )
#' )
#'
#' # Simulate choices according to a prior utility model with random parameters
#' data <- cbc_choices(
#'   design = design,
#'   obsID = "obsID",
#'   priors = list(
#'     price = 0.1,
#'     type = randN(mean = c(0.1, 0.2), sd = c(1, 2)),
#'     freshness = c(0.1, 0.2)
#'   )
#' )
cbc_choices <- function(
  design,
  obsID = "obsID",
  priors = NULL,
  n_draws = 100
) {
  if (is.null(priors)) {
    return(sim_choices_rand(design, obsID))
  }
  return(sim_choices_prior(design, obsID, priors, n_draws))
}

sim_choices_rand <- function(design, obsID) {
  nrows <- table(design[obsID])
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

sim_choices_prior <- function(design, obsID, priors, n_draws) {
  model <- def_model_prior(design, priors, n_draws)
  result <- stats::predict(
    object     = model,
    newdata    = design,
    obsID      = obsID,
    type       = "outcome",
    returnData = TRUE
  )
  result$choice <- result$predicted_outcome # Rename choice column
  result$predicted_outcome <- NULL
  # Revert variable order to that of the original design
  result <- result[c(names(design), "choice")]
  return(result)
}

def_model_prior <- function(design, priors, n_draws) {
  parNamesFull <- names(priors)
  parNames <- drop_interactions(names(priors))
  # Separate out random and fixed parameters
  parNamesRand <- names(priors[lapply(priors, class) == "list"])
  parNamesFixed <- parNames[!parNames %in% parNamesRand]
  # Make sure continuous vars are numeric
  cNames <- get_continuous_names(design, parNames)
  if (length(cNames) > 0) {
    design[, cNames] <- lapply(design[cNames], as.numeric)
  }
  # Define all other model objects
  randPars <- unlist(lapply(priors[parNamesRand], function(x) x$type))
  codedData <- logitr::recodeData(design, parNamesFull, randPars)
  parNamesCoded <- codedData$pars
  randParsCoded <- codedData$randPars
  parSetup <- get_parSetup(parNamesCoded, randParsCoded)
  parIDs <- get_parIDs(parSetup)
  coefs <- get_coefs(priors, parNamesCoded, randPars, randParsCoded)
  return(structure(list(
    coefficients = coefs,
    modelType    = ifelse(length(parNamesRand) > 0, "mxl", "mnl"),
    modelSpace   = "pref",
    parSetup     = parSetup,
    parIDs       = parIDs,
    standardDraws = getStandardDraws(parIDs, n_draws),
    # Create data object
    data = list(factorLevels = codedData$factorLevels),
    # Create n object, which stores counts of various variables
    n = list(
      vars       = length(parSetup),
      parsFixed  = length(which(parSetup == "f")),
      parsRandom = length(which(parSetup != "f")),
      draws      = n_draws,
      pars       = length(coefs),
      multiStarts = 1
    ),
    inputs = list(
      pars       = parNamesFull,
      price      = NULL,
      randPars   = randPars,
      numDraws   = n_draws,
      numMultiStarts = 1,
      correlation = FALSE
    )
  ), class = "logitr"))
}

drop_interactions <- function(parNames) {
  ints <- grepl("\\*", parNames)
  if (any(ints)) {
    return(parNames[ints == FALSE])
  }
  return(parNames)
}

get_continuous_names <- function(design, parNames) {
  levels <- lapply(design[parNames], function(x) unique(x))
  type_numeric <- unlist(lapply(levels, is.numeric))
  return(names(type_numeric[type_numeric]))
}

# Modified from {logitr}
get_parSetup <- function(parNames, randPars) {
  parSetup <- rep("f", length(parNames))
  for (i in seq_len(length(parNames))) {
    name <- parNames[i]
    if (name %in% names(randPars)) {
      parSetup[i] <- randPars[name]
    }
  }
  names(parSetup) <- parNames
  return(parSetup)
}

# Modified from {logitr}
get_parIDs <- function(parSetup) {
  return(list(
    f  = which(parSetup == "f"),
    r  = which(parSetup != "f"),
    n  = which(parSetup == "n"),
    ln = which(parSetup == "ln"),
    cn = which(parSetup == "cn")
  ))
}

get_coefs <- function(pars, parNamesCoded, randPars, randParsCoded) {
  # Define random parameter names
  parNamesRand <- names(randPars)
  parNamesRandCoded <- names(randParsCoded)
  # Get all fixed parameters
  parsFixed <- unlist(pars[!names(pars) %in% parNamesRand])
  names(parsFixed) <- parNamesCoded[!parNamesCoded %in% parNamesRandCoded]
  if (length(randPars) == 0) {
    return(parsFixed)
  }
  # Get all the random parameters
  parsRand_mean <- unlist(lapply(pars[parNamesRand], function(x) x$pars$mean))
  names(parsRand_mean) <- parNamesRandCoded
  parsRand_sd <- unlist(lapply(pars[parNamesRand], function(x) x$pars$sd))
  names(parsRand_sd) <- paste0("sd_", parNamesRandCoded)
  # Order and rename the coefficients
  coefs <- c(parsFixed, parsRand_mean)
  coefs <- coefs[parNamesCoded]
  # Add the sigma coefficients
  coefs <- c(coefs, parsRand_sd)
  return(coefs)
}

# Modified from {logitr}
getStandardDraws <- function(parIDs, numDraws) {
    numBetas <- length(parIDs$f) + length(parIDs$r)
    draws <- as.matrix(randtoolbox::halton(numDraws, numBetas, normal = TRUE))
    draws[, parIDs$f] <- 0 * draws[, parIDs$f]
    return(draws)
}

#' Define a prior (assumed) model parameter as normally-distributed.
#'
#' Define a prior (assumed) model parameter as normally-distributed.
#' Used in the `cbc_choices()` function.
#'
#' @param mean Vector of means, defaults to `0`.
#' @param sd Vector of standard deviations, defaults to `1`.
#' @return A list defining normally-distributed parameters of the prior
#' (assumed) utility model used to simulate choices in the `cbc_choices()`
#' function.
#' @export
#' @examples
#' # Insert example
randN <- function(mean = 0, sd = 1) {
  return(list(pars = list(mean = mean, sd = sd), type = "n"))
}

#' Define prior (assumed) model parameter as log-normally-distributed.
#'
#' Define prior (assumed) model parameter as log-normally-distributed.
#' Used in the `cbc_choices()` function.
#'
#' @param mean Mean of the distribution on the log scale, defaults to `0`.
#' @param sd Standard deviation of the distribution on the log scale,
#' defaults to `1`.
#' @return A list defining log-normally-distributed parameters of the prior
#' (assumed) utility model used to simulate choices in the `cbc_choices()`
#' function.
#' @export
#' @examples
#' # Insert example
randLN <- function(mean = 0, sd = 1) {
  return(list(pars = list(mean = mean, sd = sd), type = "ln"))
}

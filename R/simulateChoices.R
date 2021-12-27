#' Simulate choices for a given survey
#'
#' Simulate choices for a given `survey` data frame, either randomly or
#' according to a utility model defined by user-provided parameters.
#'
#' @param survey A survey data frame exported from the `makeSurvey()`
#' function.
#' @param obsID The name of the column that identifies each choice observation.
#' Defaults to `"obsID"`.
#' @param pars A list of one or more parameters separated by commas that define
#' the "true" utility model used to simulate choices for the `survey` data
#' frame. If no parameters are included, choices will be randomly assigned.
#' Defaults to `NULL`.
#' @param numDraws The number of Halton draws to use for simulated choices
#' based on mixed logit models. Defaults to `100`.
#' @return Returns the `survey` data frame with an additional `choice` column
#' identifying the simulated choices.
#' @export
#' @examples
#' library(conjointTools)
#'
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a full-factorial design of experiment
#' doe <- makeDoe(levels)
#'
#' # Re-code levels
#' doe <- recodeDoe(doe, levels)
#'
#' # Make the conjoint survey by randomly sampling from the doe
#' survey <- makeSurvey(
#'   doe       = doe,  # Design of experiment
#'   nResp     = 2000, # Total number of respondents (upper bound)
#'   nAltsPerQ = 3,    # Number of alternatives per question
#'   nQPerResp = 6     # Number of questions per respondent
#' )
#'
#' # Simulate random choices for the survey
#' data_random <- simulateChoices(
#'     survey = survey,
#'     obsID  = "obsID"
#' )
#'
#' # Simulate choices based on a utility model with the following parameters:
#' #   - 1 continuous "price" parameter
#' #   - 4 discrete parameters for "type"
#' #   - 2 discrete parameters for "freshness"
#' data_mnl <- simulateChoices(
#'     survey = survey,
#'     obsID  = "obsID",
#'     pars = list(
#'         price     = 0.1,
#'         type      = c(0.1, 0.2, 0.3, 0.4),
#'         freshness = c(0.1, -0.1))
#' )
#'
#' # Simulate choices based on a utility model with the following parameters:
#' #   - 1 continuous "price" parameter
#' #   - 4 discrete parameters for "type"
#' #   - 2 random normal discrete parameters for "freshness"
#' #   - 2 interaction parameters between "price" and "freshness"
#' data_mxl <- simulateChoices(
#'     survey = survey,
#'     obsID  = "obsID",
#'     pars = list(
#'         price     = 0.1,
#'         type      = c(0.1, 0.2, 0.3, 0.4),
#'         freshness = randN(mu = c(0.1, -0.1), sigma = c(1, 2)),
#'         `price*freshness` = c(1, 2))
#' )
simulateChoices = function(
  survey,
  obsID = "obsID",
  pars = NULL,
  numDraws = 100
) {
    if (is.null(pars)) { return(simulateRandomChoices(survey, obsID)) }
    return(simulateUtilityChoices(survey, obsID, pars, numDraws))
}

simulateRandomChoices <- function(survey, obsID) {
    nrows <- table(survey[obsID])
    choices <- list()
    for (i in seq_len(length(nrows))) {
        n <- nrows[i]
        choice <- rep(0, n)
        choice[sample(seq(n), 1)] <- 1
        choices[[i]] <- choice
    }
    survey$choice <- unlist(choices)
    return(survey)
}

simulateUtilityChoices <- function(survey, obsID, pars, numDraws) {
    model <- defineTrueModel(survey, pars, numDraws)
    result <- stats::predict(
      object = model,
      newdata = survey,
      obsID = obsID,
      type = "outcome",
      returnData = TRUE)
    result$choice <- result$predicted_outcome # Rename choice column
    result$predicted_outcome <- NULL
    # Revert variable order to that of the original survey
    result <- result[c(names(survey), "choice")]
    return(result)
}

defineTrueModel <- function(survey, pars, numDraws) {
    parNamesFull <- names(pars)
    parNames <- dropInteractions(names(pars))
    # Separate out random and fixed parameters
    parNamesRand <- names(pars[lapply(pars, class) == "list"])
    parNamesFixed <- parNames[! parNames %in% parNamesRand]
    # Make sure continuous survey vars are numeric
    cNames <- getContinuousParNames(survey, parNames)
    if (length(cNames) > 0) {
      survey[,cNames] <- lapply(survey[cNames], as.numeric)
    }
    # Define all other model objects
    randPars <- unlist(lapply(pars[parNamesRand], function(x) x$type))
    codedData <- logitr::recodeData(survey, parNamesFull, randPars)
    parNamesCoded <- codedData$pars
    randParsCoded <- codedData$randPars
    parSetup <- getParSetup(parNamesCoded, randParsCoded)
    parIDs <- getParIDs(parSetup)
    coefs <- getCoefficients(pars, parNamesCoded, randPars, randParsCoded)
    return(structure(list(
      coefficients = coefs,
      modelType = ifelse(length(parNamesRand) > 0, "mxl", "mnl"),
      parSetup  = parSetup,
      parIDs    = parIDs,
      inputs = list(
        pars     = parNamesFull,
        price    = NULL,
        randPars = randPars,
        numDraws = numDraws,
        modelSpace = "pref"
      )), class = "logitr")
    )
}

dropInteractions <- function(parNames) {
    ints <- grepl("\\*", parNames)
    if (any(ints)) {
      return(parNames[ints == FALSE])
    }
    return(parNames)
}

getContinuousParNames <- function(survey, parNames) {
    levels <- lapply(survey[parNames], function(x) unique(x))
    type_numeric <- unlist(lapply(levels, is.numeric))
    return(names(type_numeric[type_numeric]))
}

# Modified from {logitr}
getParSetup <- function(parNames, randPars) {
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
getParIDs <- function(parSetup) {
  return(list(
    fixed     = which(parSetup == "f"),
    random    = which(parSetup != "f"),
    normal    = which(parSetup == "n"),
    logNormal = which(parSetup == "ln")
  ))
}

getCoefficients <- function(pars, parNamesCoded, randPars, randParsCoded) {
  # Define random parameter names
  parNamesRand <- names(randPars)
  parNamesRandCoded <- names(randParsCoded)
  # Get all fixed parameters
  parsFixed <- unlist(pars[! names(pars) %in% parNamesRand])
  names(parsFixed) <- parNamesCoded[! parNamesCoded %in% parNamesRandCoded]
  if (length(randPars) == 0) {
    return(parsFixed)
  }
  # Get all the random parameters
  parsRand_mu <- unlist(lapply(pars[parNamesRand], function(x) x$pars$mu))
  names(parsRand_mu) <- parNamesRandCoded
  parsRand_sigma <- unlist(
    lapply(pars[parNamesRand], function(x) x$pars$sigma))
  names(parsRand_sigma) <- paste(parNamesRandCoded, "sigma", sep = "_")
  # Order and rename the coefficients
  coefs <- c(parsFixed, parsRand_mu)
  coefs <- coefs[parNamesCoded]
  newNames <- parNamesCoded
  newNames[which(newNames %in% parNamesRandCoded)] <- paste(
    parNamesRandCoded, "mu", sep = "_")
  names(coefs) <- newNames
  # Add the sigma coefficients
  coefs <- c(coefs, parsRand_sigma)
  return(coefs)
}

#' Define "true" model parameters as normally-distributed.
#'
#' Define "true" model parameters as normally-distributed. Used in the
#' `simulateChoices()` function.
#'
#' @param mu Vector of means, defaults to `0`.
#' @param sigma Vector of standard deviations, defaults to `1`.
#' @return A list defining normally-distributed parameters of the "true"
#' utility model used to simulate choices in the `simulateChoices()` function.
#' @export
#' @examples
#' library(conjointTools)
#'
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a full-factorial design of experiment
#' doe <- makeDoe(levels)
#'
#' # Re-code levels
#' doe <- recodeDoe(doe, levels)
#'
#' # Make the conjoint survey by randomly sampling from the doe
#' survey <- makeSurvey(
#'   doe       = doe,  # Design of experiment
#'   nResp     = 2000, # Total number of respondents (upper bound)
#'   nAltsPerQ = 3,    # Number of alternatives per question
#'   nQPerResp = 6     # Number of questions per respondent
#' )
#'
#' # Simulate choices based on a utility model with the following parameters:
#' #   - 1 continuous "price" parameter
#' #   - 4 discrete parameters for "type"
#' #   - 2 random normal discrete parameters for "freshness"
#' data_mxl <- simulateChoices(
#'     survey = survey,
#'     obsID  = "obsID",
#'     pars = list(
#'         price     = 0.1,
#'         type      = c(0.1, 0.2, 0.3, 0.4),
#'         freshness = randN(mu = c(0.1, -0.1), sigma = c(1, 2)))
#' )
randN <- function(mu = 0, sigma = 1) {
    return(list(pars = list(mu = mu, sigma = sigma), type = "n"))
}

#' Define "true" model parameters as normally-distributed.
#'
#' Define "true" model parameters as normally-distributed. Used in the
#' `simulateChoices()` function.
#'
#' @param mu Mean of the distribution on the log scale, defaults to `0`.
#' @param sigma Standard deviation of the distribution on the log scale,
#' defaults to `1`.
#' @return A list defining log-normally-distributed parameters of the "true"
#' utility model used to simulate choices in the `simulateChoices()` function.
#' @export
#' @examples
#' library(conjointTools)
#'
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a full-factorial design of experiment
#' doe <- makeDoe(levels)
#'
#' # Re-code levels
#' doe <- recodeDoe(doe, levels)
#'
#' # Make the conjoint survey by randomly sampling from the doe
#' survey <- makeSurvey(
#'   doe       = doe,  # Design of experiment
#'   nResp     = 2000, # Total number of respondents (upper bound)
#'   nAltsPerQ = 3,    # Number of alternatives per question
#'   nQPerResp = 6     # Number of questions per respondent
#' )
#'
#' # Simulate choices based on a utility model with the following parameters:
#' #   - 1 continuous "price" parameter
#' #   - 4 discrete parameters for "type"
#' #   - 2 random log-normal discrete parameters for "freshness"
#' data_mxl <- simulateChoices(
#'     survey = survey,
#'     obsID  = "obsID",
#'     pars = list(
#'         price     = 0.1,
#'         type      = c(0.1, 0.2, 0.3, 0.4),
#'         freshness = randLN(mu = c(0.1, 0.2), sigma = c(0.1, 0.2)))
#' )
randLN <- function(mu = 0, sigma = 1) {
    return(list(pars = list(mu = mu, sigma = sigma), type = "ln"))
}

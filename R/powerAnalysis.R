#' Conduct a power analysis for a given survey design
#'
#' For a given survey, this function simulates choices (either randomly or
#' according to a utility model defined by user-provided parameters), and then
#' estimates the same user-specified model multiple times using different size
#' subsets of the simulated choice data. The number of models to run is set by
#' the `nbreaks` argument, which breaks up the data into groups of increasing
#' sample sizes. The function returns a data frame of the estimated
#' coefficients and standard errors for each parameter in each model.
#' All models are estimated models using the {logitr} package.
#' @keywords logitr, mnl, mxl, logit, sample size, power analysis
#' @param nbreaks The number of different sample size groups.
#' @param survey A survey data frame exported from the `makeSurvey()`
#' function.
#' @param obsID The name of the column in `survey` that identifies each
#' choice observation.
#' @param pars The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `survey` argument.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal or `'ln'` for log-normal.
#' Defaults to `NULL`.
#' @param truePars A list of one or more parameters separated by commas that
#' define the "true" (assumed) utility model used to simulate choices for the
#' `survey` data frame. If no parameters are included, choices will be randomly
#' assigned. Defaults to `NULL`.
#' @param numDraws The number of Halton draws to use for simulated choices
#' based on mixed logit models. Defaults to `100`.
#' @return A data frame of the estimated coefficients and standard errors for
#' each parameter in each model.
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
#' # Make a full-factorial design of experiment and recode the levels
#' doe <- makeDoe(levels)
#'
#' # Make the survey
#' survey <- makeSurvey(
#'     doe       = doe,  # Design of experiment
#'     nResp     = 2000, # Total number of respondents (upper bound)
#'     nAltsPerQ = 3,    # Number of alternatives per question
#'     nQPerResp = 6     # Number of questions per respondent
#' )
#'
#' # Conduct the power analysis
#' results <- powerAnalysis(
#'     nbreaks = 10,
#'     survey  = survey,
#'     pars    = c("price", "type", "freshness"),
#'     obsID   = "obsID"
#' )
#'
#' # Visualize results
#' library(ggplot2)
#' ggplot(results) +
#'   geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
#'   geom_point(aes(x = sampleSize, y = se, color = coef)) +
#'   expand_limits(y = 0) +
#'   theme_bw()
powerAnalysis <- function(
  nbreaks = 10,
  survey,
  obsID,
  pars,
  randPars = NULL,
  truePars = NULL,
  numDraws = 100
) {

  # Simulate random choices for the survey
  data <- cbc_choices(
     survey = survey,
     obsID  = obsID,
     truePars = truePars,
     numDraws = numDraws
  )

  # Estimate models with different sample sizes
  models <- estimateModels(
      nbreaks = 10,
      data    = data,
      pars    = pars,
      outcome = "choice",
      obsID   = obsID
  )

  # Extract coefficients and standard errors from models
  return(getModelResults(models))
}

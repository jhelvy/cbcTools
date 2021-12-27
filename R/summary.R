#' Extract the coefficients and standard errors from a data frame of estimated models
#'
#' Returns a data frame of the estimated coefficients and standard errors for
#' each parameter in each model in a data frame of estimated models.
#' @keywords conjointTools, coefficient, standard error
#' @param models A data frame containing estimated models for a series of
#' different sample sizes, obtained using the `estimateModels()` function.
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
#' doe <- recodeDoe(doe, levels)
#'
#' # Make the survey
#' survey <- makeSurvey(
#'     doe       = doe,  # Design of experiment
#'     nResp     = 2000, # Total number of respondents (upper bound)
#'     nAltsPerQ = 3,    # Number of alternatives per question
#'     nQPerResp = 6     # Number of questions per respondent
#' )
#'
#' # Simulate random choices for the survey
#' data <- simulateChoices(
#'     survey = survey,
#'     obsID  = "obsID"
#' )
#'
#' # Estimate models with different sample sizes
#' models <- estimateModels(
#'     nbreaks = 10,
#'     data    = data,
#'     pars    = c("price", "type", "freshness"),
#'     outcome = "choice",
#'     obsID   = "obsID"
#' )
#'
#' # Extract coefficients and standard errors from models
#' results <- getModelResults(models)
getModelResults <- function(models) {
    est <- lapply(models, function(x) stats::coef(x))
    se <- lapply(models, function(x) logitr::se(x))
    names <- lapply(est, names)
    results <- data.frame(
        sampleSize = rep(as.numeric(names(models)), each = length(est[[1]])),
        coef = do.call(c, names),
        est = do.call(c, est),
        se = do.call(c, se)
    )
    row.names(results) <- NULL
    return(results)
}

#' Simulate choices for a survey design
#'
#' Simulate choices for a survey design, either randomly or according to a
#' utility model defined by user-provided prior parameters. All choices are
#' simulated using the 'logitr' package. For more details see the JSS article
#' on the 'logitr' package (Helveston, 2023).
#' @keywords logitr mnl mxl mixed logit simulation
#'
#' @param design A data frame of a survey design.
#' @param priors A list of one or more prior parameters that define a prior
#' (assumed) utility model used to simulate choices for the `survey` data frame.
#' If `NULL` (the default), choices will be randomly assigned.
#' @param n_draws The number of Halton draws to use for simulated choices
#' for mixed logit models. Defaults to `100`.
#' @references
#' Helveston, J. P. (2023). logitr: Fast Estimation of Multinomial and Mixed Logit Models with Preference Space and Willingness-to-Pay Space Utility Parameterizations. Journal of Statistical Software, 105(10), 1–37,
#' \doi{10.18637/jss.v105.i10}
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
#' # Make a survey design from all possible profiles
#' # (This is the default setting where method = 'full' for "full factorial")
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
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
  priors = NULL,
  n_draws = 100
) {
  if (is.null(priors)) {
    return(sim_choices_rand(design))
  }
  return(sim_choices_prior(design, priors, n_draws))
}

sim_choices_rand <- function(design) {
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

sim_choices_prior <- function(design, priors, n_draws) {
  result <- stats::predict(
    object     = priors$model,
    newdata    = design,
    obsID      = "obsID",
    type       = "outcome",
    returnData = TRUE
  )
  result$choice <- result$predicted_outcome # Rename choice column
  result$predicted_outcome <- NULL
  # Revert variable order to that of the original design
  result <- result[c(names(design), "choice")]
  return(result)
}

drop_interactions <- function(parNames) {
  ints <- grepl("\\*", parNames)
  if (any(ints)) {
    return(parNames[ints == FALSE])
  }
  return(parNames)
}

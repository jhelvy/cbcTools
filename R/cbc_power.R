#' Estimate the same model on different size subsets of data
#'
#' This function estimates the same model multiple times using different size
#' subsets of a set of choice data. The number of models to run is set by the
#' `nbreaks` argument, which breaks up the data into groups of increasing
#' sample sizes. All models are estimated models using the {logitr} package.
#' @keywords logitr, mnl, mxl, logit, sample size, power
#'
#' @param nbreaks The number of different sample size groups.
#' @param n_q Number of questions per respondent. Defaults to `1` if not
#' specified.
#' @param data The data, formatted as a `data.frame` object.
#' @param outcome The name of the column that identifies the outcome variable,
#' which should be coded with a `1` for `TRUE` and `0` for `FALSE`.
#' @param obsID The name of the column that identifies each observation.
#' @param pars The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal or `'ln'` for log-normal.
#' Defaults to `NULL`.
#' @param returnModels If `TRUE`, a list of all estimated models is returned.
#' This can be useful if you want to extract other outputs from each model,
#' such as the variance-covariance matrix, etc. Defaults to `FALSE`.
#' @param panelID The name of the column that identifies the individual (for
#' panel data where multiple observations are recorded for each individual).
#' Defaults to `NULL`.
#' @param clusterID The name of the column that identifies the cluster
#' groups to be used in model estimation. Defaults to `NULL`.
#' @param robust Determines whether or not a robust covariance matrix is
#' estimated. Defaults to `FALSE`. Specification of a `clusterID` will override
#' the user setting and set this to `TRUE' (a warning will be displayed in this
#' case). Replicates the functionality of Stata's cmcmmixlogit.
#' @param predict If `TRUE`, predicted probabilities, fitted values, and
#' residuals are also included in the returned model objects. Defaults to
#' `FALSE`.
#' @param ... Other arguments that are passed to `logitr::logitr()` for model
#' estimation. See the `logitr` documentation for details about other
#' available arguments.
#' @export
#' @examples
#' # Insert examples
cbc_power <- function(
  nbreaks = 10,
  n_q = 1,
  data,
  outcome,
  obsID,
  pars,
  randPars  = NULL,
  returnModels = FALSE,
  panelID   = NULL,
  clusterID = NULL,
  robust    = FALSE,
  predict   = FALSE,
  ...
) {
    dataList <- make_data_list(data, obsID, nbreaks, n_q)
    suppressMessages(
      models <- lapply(
        dataList,
        logitr::logitr,
        outcome   = outcome,
        obsID     = obsID,
        pars      = pars,
        randPars  = randPars,
        panelID   = panelID,
        clusterID = clusterID,
        robust    = robust,
        predict   = predict,
        ... = ...
    ))
    # Add sample size to each model
    sizes <- names(models)
    for (i in seq_len(length(models))) {
      models[[i]]$sampleSize <- as.numeric(sizes[i])
    }
    if (returnModels) {
      return(models)
    }
    return(cbc_extract_error(models))
}

make_data_list <- function(data, obsID, nbreaks, n_q) {
    maxObs <- max(data[obsID])
    nObs <- ceiling(seq(ceiling(maxObs/nbreaks), maxObs, length.out = nbreaks))
    dataList <- list()
    for (i in 1:nbreaks) {
      temp <- data[which(data[,obsID] %in% seq(nObs[i])),]
      temp$sampleSize <- round(nObs[i] / n_q)
      dataList[[i]] <- temp
    }
    sampleSizes <- unlist(lapply(dataList, function(x) unique(x$sampleSize)))
    names(dataList) <- sampleSizes
    return(dataList)
}

cbc_extract_error <- function(models) {
    sampleSize <- unlist(lapply(models, function(x) x$sampleSize))
    est <- lapply(models, function(x) stats::coef(x))
    se <- lapply(models, function(x) logitr::se(x))
    names <- lapply(est, names)
    results <- data.frame(
        sampleSize = rep(sampleSize, each = length(est[[1]])),
        coef = do.call(c, names),
        est = do.call(c, est),
        se = do.call(c, se)
    )
    row.names(results) <- NULL
    return(results)
}

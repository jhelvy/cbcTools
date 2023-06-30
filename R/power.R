#' Estimate the same model on different size subsets of data
#'
#' This function estimates the same model multiple times using different size
#' subsets of a set of choice data and then returns a data frame of the
#' estimated model coefficients and standard errors for each sample size. This
#' is useful for determining the required sample size for obtaining a desired
#' level of statistical power on each coefficient. The number of models to
#' estimate is set by the `nbreaks` argument, which breaks up the data into
#' groups of increasing sample sizes. All models are estimated models using
#' the 'logitr' package. For more details see the JSS article on the 'logitr'
#' package (Helveston, 2023).
#' @keywords logitr mnl mxl mixed logit sample size power
#'
#' @param data The data, formatted as a `data.frame` object.
#' @param outcome The name of the column that identifies the outcome variable,
#' which should be coded with a `1` for `TRUE` and `0` for `FALSE`.
#' @param obsID The name of the column that identifies each observation.
#' @param pars The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal or `'ln'` for log-normal.
#' Defaults to `NULL`.
#' @param nbreaks The number of different sample size groups.
#' @param n_q Number of questions per respondent. Defaults to `1` if not
#' specified.
#' @param return_models If `TRUE`, a list of all estimated models is returned.
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
#' @param n_cores The number of cores to use for parallel processing.
#' Set to `1` to run serially Defaults to `NULL`, in which case the number of
#' cores is set to `parallel::detectCores() - 1`. Max cores allowed is capped
#' at `parallel::detectCores()`.
#' @param ... Other arguments that are passed to `logitr::logitr()` for model
#' estimation. See the {logitr} documentation for details about other
#' available arguments.
#' @references
#' Helveston, J. P. (2023). logitr: Fast Estimation of Multinomial and Mixed Logit Models with Preference Space and Willingness-to-Pay Space Utility Parameterizations. Journal of Statistical Software, 105(10), 1–37,
#' \doi{10.18637/jss.v105.i10}
#' @return Returns a data frame of estimated model coefficients and standard
#' errors for the same model estimated on subsets of the `data` with increasing
#' sample sizes.
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
#' # Conduct a power analysis
#' power <- cbc_power(
#'   data    = data,
#'   pars    = c("price", "type", "freshness"),
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   nbreaks = 10,
#'   n_q     = 6,
#'   n_cores = 2
#' )
cbc_power <- function(
  data,
  outcome,
  obsID,
  pars,
  randPars  = NULL,
  nbreaks = 10,
  n_q = 1,
  return_models = FALSE,
  panelID   = NULL,
  clusterID = NULL,
  robust    = FALSE,
  predict   = FALSE,
  n_cores   = NULL,
  ...
) {
    n_cores <- set_num_cores(n_cores)
    dataList <- make_data_list(data, obsID, nbreaks, n_q)
    if (n_cores == 1) {
      message("Estimating models...")
      suppressMessages(suppressWarnings(
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
        )
      ))
    } else {
      message("Estimating models using ", n_cores, " cores...")
      if (Sys.info()[['sysname']] == 'Windows') {
        cl <- parallel::makeCluster(n_cores, "PSOCK")
        suppressMessages(suppressWarnings(
          models <- parallel::parLapply(
            cl = cl,
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
          )
        ))
        parallel::stopCluster(cl)
      } else {
        suppressMessages(suppressWarnings(
          models <- parallel::mclapply(
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
            ... = ...,
            mc.cores = n_cores
          )
        ))
      }
    }
    message("done!")
    # Add sample size to each model
    sizes <- names(models)
    for (i in seq_len(length(models))) {
      models[[i]]$sampleSize <- as.numeric(sizes[i])
    }
    if (return_models) {
      class(models) <- c("cbc_models", "list")
      return(models)
    }
    errors <- extract_errors(models)
    class(errors) <- c("cbc_errors", "data.frame")
    return(errors)
}

set_num_cores <- function(n_cores) {
  cores_available <- parallel::detectCores()
  max_cores <- cores_available - 1
  # CRAN checks limits you to 2 cores, see this SO issue:
  # https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(chk) && (chk != "false")) {
    # use 2 cores in CRAN/Travis/AppVeyor
    return(2L)
  }
  if (is.null(n_cores)) {
    return(max_cores)
  } else if (!is.numeric(n_cores)) {
    warning(
      "Non-numeric value provided for n_cores...setting n_cores to ",
      max_cores
    )
    return(max_cores)
  } else if (n_cores > cores_available) {
    warning(
      "Cannot use ", n_cores, " cores because your machine only has ",
      cores_available, " available...setting n_cores to ", max_cores
    )
    return(max_cores)
  }
  return(n_cores)
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

extract_errors <- function(models) {
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

#' Plot a comparison of different design powers
#'
#' This function creates a ggplot2 object comparing the power curves of
#' different designs. Each design is color coded and each facet (sub plot)
#' is a model coefficient.
#' @param ... Any number of data frame containing power results obtained from
#' the `cbc_power()` function, separated by commas.
#' @return A plot comparing the power curves of different designs.
#' @importFrom ggplot2 ggplot aes geom_hline geom_point expand_limits theme_bw
#' theme element_blank labs facet_wrap
#' @importFrom rlang .data
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Make designs to compare: full factorial vs bayesian d-efficient
#' design_full <- cbc_design(
#'   profiles = profiles,
#'   n_resp = 300, n_alts = 3, n_q = 6
#' )
#' # Same priors will be used in bayesian design and simulated choices
#' priors <- list(
#'   price     = -0.1,
#'   type      = c(0.1, 0.2),
#'   freshness = c(0.1, 0.2)
#' )
#' design_bayesian <- cbc_design(
#'   profiles  = profiles,
#'   n_resp = 300, n_alts = 3, n_q = 6, n_start = 1, method = "CEA",
#'   priors = priors, parallel = FALSE
#' )
#'
#' # Obtain power for each design by simulating choices
#' power_full <- design_full |>
#' cbc_choices(obsID = "obsID", priors = priors) |>
#'   cbc_power(
#'     pars = c("price", "type", "freshness"),
#'     outcome = "choice", obsID = "obsID", nbreaks = 10, n_q = 6, n_cores = 2
#'   )
#' power_bayesian <- design_bayesian |>
#'   cbc_choices(obsID = "obsID", priors = priors) |>
#'   cbc_power(
#'     pars = c("price", "type", "freshness"),
#'     outcome = "choice", obsID = "obsID", nbreaks = 10, n_q = 6, n_cores = 2
#'   )
#'
#' # Compare power of each design
#' plot_compare_power(power_bayesian, power_full)
plot_compare_power <- function(...) {
  power <- list(...)
  design_names <- unlist(lapply(as.list(match.call())[-1], deparse))
  names(power) <- design_names
  for (i in 1:length(power)) {
    power[[i]]$design <- names(power)[i]
  }
  power <- do.call(rbind, power)
  ggplot2::ggplot(power) +
    geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
    geom_point(
      aes(x = .data$sampleSize, y = .data$se, color = .data$design),
      size = 1.8
    ) +
    facet_wrap(~.data$coef) +
    expand_limits(y = 0) +
    theme_bw(base_size = 14) +
    theme(panel.grid.minor = element_blank()) +
    labs(
      color = "Design",
      x = "Sample size",
      y = "Standard error"
    )
}

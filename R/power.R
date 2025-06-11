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
#' @param data Either a `cbc_choices` object created by `cbc_choices()` or a
#'   data frame containing choice data with the required columns.
#' @param outcome The name of the column that identifies the outcome variable,
#'   which should be coded with a `1` for `TRUE` and `0` for `FALSE`. Defaults
#'   to `"choice"` for `cbc_choices` objects.
#' @param obsID The name of the column that identifies each observation.
#'   Defaults to `"obsID"` for `cbc_choices` objects.
#' @param pars The names of the parameters to be estimated in the model.
#'   Must be the same as the column names in the `data` argument. For
#'   `cbc_choices` objects, this will be automatically determined from the
#'   choice data if not specified.
#' @param randPars A named vector whose names are the random parameters and
#'   values the distribution: `'n'` for normal or `'ln'` for log-normal.
#'   Defaults to `NULL`.
#' @param nbreaks The number of different sample size groups. Defaults to `10`.
#' @param n_q Number of questions per respondent. For `cbc_choices` objects,
#'   this will be automatically determined if not specified.
#' @param return_models If `TRUE`, a list of all estimated models is returned.
#'   This can be useful if you want to extract other outputs from each model,
#'   such as the variance-covariance matrix, etc. Defaults to `FALSE`.
#' @param panelID The name of the column that identifies the individual (for
#'   panel data where multiple observations are recorded for each individual).
#'   Defaults to `"respID"` for `cbc_choices` objects with multiple respondents.
#' @param clusterID The name of the column that identifies the cluster
#'   groups to be used in model estimation. Defaults to `NULL`.
#' @param robust Determines whether or not a robust covariance matrix is
#'   estimated. Defaults to `FALSE`. Specification of a `clusterID` will override
#'   the user setting and set this to `TRUE' (a warning will be displayed in this
#'   case). Replicates the functionality of Stata's cmcmmixlogit.
#' @param predict If `TRUE`, predicted probabilities, fitted values, and
#'   residuals are also included in the returned model objects. Defaults to
#'   `FALSE`.
#' @param n_cores The number of cores to use for parallel processing.
#'   Set to `1` to run serially Defaults to `NULL`, in which case the number of
#'   cores is set to `parallel::detectCores() - 1`. Max cores allowed is capped
#'   at `parallel::detectCores()`.
#' @param ... Other arguments that are passed to `logitr::logitr()` for model
#'   estimation. See the {logitr} documentation for details about other
#'   available arguments.
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
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Create survey and simulate choices
#' survey <- cbc_survey(design, n_resp = 300)
#' choices <- cbc_choices(survey)
#'
#' # Conduct a power analysis using cbc_choices object
#' power <- cbc_power(
#'   data = choices,
#'   nbreaks = 10,
#'   n_cores = 2
#' )
#'
#' # Visualize the results
#' plot(power)
cbc_power <- function(data, ...) {
  UseMethod("cbc_power")
}

#' @rdname cbc_power
#' @export
cbc_power.cbc_choices <- function(
  data,
  outcome = "choice",
  obsID = "obsID",
  pars = NULL,
  randPars = NULL,
  nbreaks = 10,
  n_q = NULL,
  return_models = FALSE,
  panelID = NULL,
  clusterID = NULL,
  robust = FALSE,
  predict = FALSE,
  n_cores = NULL,
  ...
) {
  # Auto-detect parameters if not specified
  if (is.null(pars)) {
    # Get attribute columns from the choice data, excluding only the choice column
    all_vars <- get_var_names(data)
    pars <- setdiff(all_vars, "choice")

    if (length(pars) == 0) {
      stop("Could not auto-detect parameters. Please specify 'pars' argument.")
    }

    message("Auto-detected parameters: ", paste(pars, collapse = ", "))
  }

  # Auto-detect n_q if not specified
  if (is.null(n_q)) {
    if ("qID" %in% names(data)) {
      n_q <- max(data$qID, na.rm = TRUE)
    } else {
      n_q <- 1
      warning("Could not determine n_q from data. Setting n_q = 1.")
    }
  }

  # Auto-detect panelID if not specified and we have multiple respondents
  if (is.null(panelID) && "respID" %in% names(data)) {
    n_resp <- max(data$respID, na.rm = TRUE)
    if (n_resp > 1) {
      panelID <- "respID"
      message("Multiple respondents detected. Using 'respID' as panelID for panel data estimation.")
    }
  }

  # Get choice info for better error messages and reporting
  choice_info <- attr(data, "choice_info")

  # Call the data frame method
  result <- cbc_power.data.frame(
    data = data,
    outcome = outcome,
    obsID = obsID,
    pars = pars,
    randPars = randPars,
    nbreaks = nbreaks,
    n_q = n_q,
    return_models = return_models,
    panelID = panelID,
    clusterID = clusterID,
    robust = robust,
    predict = predict,
    n_cores = n_cores,
    ...
  )

  # Add choice simulation info to the result
  if (!is.null(choice_info)) {
    attr(result, "choice_info") <- choice_info
  }

  return(result)
}

#' @rdname cbc_power
#' @export
cbc_power.data.frame <- function(
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

#' Enhanced print method for cbc_errors objects
#' @param x A cbc_errors object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_errors <- function(x, ...) {
  cat("CBC Power Analysis Results\n")
  cat("==========================\n")

  # Get choice info if available
  choice_info <- attr(x, "choice_info")
  if (!is.null(choice_info)) {
    cat(sprintf("Simulation method: %s\n", choice_info$simulation_method))
    if (!is.na(choice_info$d_error)) {
      cat(sprintf("Design D-error: %.6f\n", choice_info$d_error))
    }
    if (!is.null(choice_info$n_respondents)) {
      cat(sprintf("Data from: %d respondents\n", choice_info$n_respondents))
    }
    cat("\n")
  }

  # Basic statistics
  n_breaks <- length(unique(x$sampleSize))
  sample_range <- range(x$sampleSize)
  n_pars <- length(unique(x$coef))

  cat(sprintf("Sample sizes: %d to %d (%d breaks)\n",
              sample_range[1], sample_range[2], n_breaks))
  cat(sprintf("Parameters: %d (%s)\n",
              n_pars, paste(unique(x$coef), collapse = ", ")))
  cat("\n")

  # Summary of results at different sample sizes
  cat("Standard errors by sample size:\n")
  cat("(Showing every few sample sizes)\n\n")

  # Show results for a few sample sizes
  sample_sizes <- unique(x$sampleSize)
  show_sizes <- sample_sizes[seq(1, length(sample_sizes), length.out = min(5, length(sample_sizes)))]

  for (size in show_sizes) {
    subset_data <- x[x$sampleSize == size, ]
    cat(sprintf("n = %d:\n", size))
    for (i in seq_len(nrow(subset_data))) {
      cat(sprintf("  %-12s: SE = %.4f\n",
                  subset_data$coef[i], subset_data$se[i]))
    }
    cat("\n")
  }

  cat("Use plot() to visualize power curves.\n")

  invisible(x)
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
#' \dontrun{
#' library(cbcTools)
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Make designs to compare: random vs sequential
#' design_random <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 3, n_q = 6, method = "random"
#' )
#'
#' # Same priors will be used in sequential design and simulated choices
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price     = -0.1,
#'   type      = c(0.1, 0.2),
#'   freshness = c(0.1, 0.2)
#' )
#'
#' design_sequential <- cbc_design(
#'   profiles  = profiles,
#'   priors = priors,
#'   n_alts = 3, n_q = 6, method = "sequential"
#' )
#'
#' # Create surveys and simulate choices
#' survey_random <- cbc_survey(design_random, n_resp = 100)
#' choices_random <- cbc_choices(survey_random, priors = priors)
#'
#' survey_sequential <- cbc_survey(design_sequential, n_resp = 100)
#' choices_sequential <- cbc_choices(survey_sequential, priors = priors)
#'
#' # Obtain power for each design
#' power_random <- cbc_power(choices_random, nbreaks = 5, n_cores = 2)
#' power_sequential <- cbc_power(choices_sequential, nbreaks = 5, n_cores = 2)
#'
#' # Compare power of each design
#' plot_compare_power(power_random, power_sequential)
#' }
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

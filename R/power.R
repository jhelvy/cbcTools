#' Estimate power analysis for choice experiment designs
#'
#' This function estimates the same model multiple times using different sample
#' sizes to assess statistical power. It returns both the estimated models and
#' a summary of coefficient estimates, standard errors, and power statistics.
#'
#' @param data A data frame containing choice data. Can be a `cbc_choices` object
#'   or any data frame with the required columns.
#' @param outcome Name of the outcome variable column (1 for chosen, 0 for not).
#'   Defaults to "choice".
#' @param obsID Name of the observation ID column. Defaults to "obsID".
#' @param pars Names of the parameters to estimate. If NULL (default), will
#'   auto-detect from column names for `cbc_choices` objects.
#' @param randPars Named vector of random parameters and their distributions
#'   ('n' for normal, 'ln' for log-normal). Defaults to NULL.
#' @param n_breaks Number of sample size groups to test. Defaults to 10.
#' @param n_q Number of questions per respondent. Auto-detected for `cbc_choices`
#'   objects if not specified.
#' @param panelID Name of the panel ID column for panel data. Auto-detected
#'   as "respID" for multi-respondent `cbc_choices` objects.
#' @param alpha Significance level for power calculations. Defaults to 0.05.
#' @param return_models If TRUE, includes full model objects in returned list.
#'   Defaults to FALSE.
#' @param n_cores Number of cores for parallel processing. Defaults to
#'   `parallel::detectCores() - 1`.
#' @param ... Additional arguments passed to `logitr::logitr()`.
#'
#' @return A `cbc_power` object containing:
#'   - `power_summary`: Data frame with sample sizes, coefficients, estimates,
#'     standard errors, t-statistics, and power
#'   - `models`: List of estimated models (if `return_models = TRUE`)
#'   - `sample_sizes`: Vector of sample sizes tested
#'   - `n_breaks`: Number of breaks used
#'   - `alpha`: Significance level used
#'
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles and design
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C"),
#'   quality = c("Low", "High")
#' )
#'
#' design <- cbc_design(profiles, n_alts = 2, n_q = 6)
#'
#' # Simulate choices
#' priors <- cbc_priors(profiles, price = -0.1, type = c(0.5, 0.2), quality = 0.3)
#' choices <- cbc_choices(design, priors)
#'
#' # Run power analysis
#' power_results <- cbc_power(choices, n_breaks = 8)
#'
#' # View results
#' print(power_results)
#' plot(power_results)
cbc_power <- function(
    data,
    outcome = "choice",
    obsID = "obsID",
    pars = NULL,
    randPars = NULL,
    n_breaks = 10,
    n_q = NULL,
    panelID = NULL,
    alpha = 0.05,
    return_models = FALSE,
    n_cores = NULL,
    ...
) {
    # Validate inputs
    if (!is.data.frame(data)) {
        stop("data must be a data frame")
    }

    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
        stop("alpha must be a single numeric value between 0 and 1")
    }

    if (!is.numeric(n_breaks) || length(n_breaks) != 1 || n_breaks < 2) {
        stop("n_breaks must be a single numeric value >= 2")
    }

    # Auto-detect parameters for cbc_choices objects
    if (is.null(pars) && !inherits(data, "cbc_choices")) {
        stop(
            "Could not auto-detect parameters. Please specify 'pars' argument."
        )
    }

    if (inherits(data, "cbc_choices")) {
        if (is.null(pars)) {
            all_vars <- get_var_names(data)
            pars <- setdiff(all_vars, outcome)
            if (length(pars) == 0) {
                stop(
                    "Could not auto-detect parameters. Please specify 'pars' argument."
                )
            }
            message("Auto-detected parameters: ", paste(pars, collapse = ", "))
        }

        if (is.null(n_q) && "qID" %in% names(data)) {
            n_q <- max(data$qID, na.rm = TRUE)
        }

        if (is.null(panelID) && "respID" %in% names(data)) {
            n_resp <- max(data$respID, na.rm = TRUE)
            if (n_resp > 1) {
                panelID <- "respID"
                message("Using 'respID' as panelID for panel data estimation.")
            }
        }
    }

    # Set defaults
    if (is.null(n_q)) {
        n_q <- 1
    }
    if (is.null(n_cores)) {
        n_cores <- set_num_cores(NULL)
    }

    # Validate required columns exist
    required_cols <- c(outcome, obsID, pars)
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Create sample size groups
    max_obs <- max(data[[obsID]], na.rm = TRUE)
    sample_sizes <- create_sample_sizes(max_obs, n_q, n_breaks)

    # Create data subsets
    data_list <- create_data_subsets(data, obsID, sample_sizes, n_q)

    # Estimate models
    message(
        "Estimating models using ",
        min(n_cores, length(data_list)),
        " cores..."
    )
    models <- estimate_models_parallel(
        data_list,
        outcome,
        obsID,
        pars,
        randPars,
        panelID,
        n_cores,
        ...
    )
    message("Model estimation complete!")

    # Extract power summary
    power_summary <- extract_power_summary(models, sample_sizes, alpha)

    # Create return object
    result <- list(
        power_summary = power_summary,
        sample_sizes = sample_sizes,
        n_breaks = n_breaks,
        alpha = alpha,
        choice_info = attr(data, "choice_info")
    )

    if (return_models) {
        result$models <- models
    }

    class(result) <- c("cbc_power", "list")
    return(result)
}

# Helper Functions ----

create_sample_sizes <- function(max_obs, n_q, n_breaks) {
    min_obs <- ceiling(max_obs / n_breaks)
    obs_sequence <- ceiling(seq(min_obs, max_obs, length.out = n_breaks))
    sample_sizes <- round(obs_sequence / n_q)
    return(unique(sample_sizes)) # Remove duplicates
}

create_data_subsets <- function(data, obsID, sample_sizes, n_q) {
    data_list <- list()

    for (i in seq_along(sample_sizes)) {
        sample_size <- sample_sizes[i]
        max_obs_for_size <- sample_size * n_q

        subset_data <- data[data[[obsID]] <= max_obs_for_size, ]
        subset_data$sample_size <- sample_size

        data_list[[i]] <- subset_data
    }

    names(data_list) <- as.character(sample_sizes)
    return(data_list)
}

estimate_models_parallel <- function(
    data_list,
    outcome,
    obsID,
    pars,
    randPars,
    panelID,
    n_cores,
    ...
) {
    n_cores <- min(n_cores, length(data_list))

    if (n_cores == 1) {
        models <- estimate_models_sequential(
            data_list,
            outcome,
            obsID,
            pars,
            randPars,
            panelID,
            ...
        )
    } else {
        models <- estimate_models_parallel_internal(
            data_list,
            outcome,
            obsID,
            pars,
            randPars,
            panelID,
            n_cores,
            ...
        )
    }

    return(models)
}

estimate_models_sequential <- function(
    data_list,
    outcome,
    obsID,
    pars,
    randPars,
    panelID,
    ...
) {
    models <- list()

    for (i in seq_along(data_list)) {
        tryCatch(
            {
                models[[i]] <- logitr::logitr(
                    data = data_list[[i]],
                    outcome = outcome,
                    obsID = obsID,
                    pars = pars,
                    randPars = randPars,
                    panelID = panelID,
                    ...
                )
                models[[i]]$sample_size <- unique(data_list[[i]]$sample_size)
            },
            error = function(e) {
                warning("Model ", i, " failed to converge: ", e$message)
                models[[i]] <- NULL
            }
        )
    }

    # Remove failed models
    models <- models[!sapply(models, is.null)]
    names(models) <- sapply(models, function(x) as.character(x$sample_size))

    return(models)
}

estimate_models_parallel_internal <- function(
    data_list,
    outcome,
    obsID,
    pars,
    randPars,
    panelID,
    n_cores,
    ...
) {
    estimate_single_model <- function(data_subset) {
        tryCatch(
            {
                model <- logitr::logitr(
                    data = data_subset,
                    outcome = outcome,
                    obsID = obsID,
                    pars = pars,
                    randPars = randPars,
                    panelID = panelID,
                    ...
                )
                model$sample_size <- unique(data_subset$sample_size)
                return(model)
            },
            error = function(e) {
                return(NULL)
            }
        )
    }

    if (Sys.info()[['sysname']] == 'Windows') {
        cl <- parallel::makeCluster(n_cores, "PSOCK")
        on.exit(parallel::stopCluster(cl))

        suppressMessages(suppressWarnings({
            models <- parallel::parLapply(cl, data_list, estimate_single_model)
        }))
    } else {
        suppressMessages(suppressWarnings({
            models <- parallel::mclapply(
                data_list,
                estimate_single_model,
                mc.cores = n_cores
            )
        }))
    }

    # Remove failed models
    models <- models[!sapply(models, is.null)]
    names(models) <- sapply(models, function(x) as.character(x$sample_size))

    return(models)
}

extract_power_summary <- function(models, sample_sizes, alpha) {
    if (length(models) == 0) {
        stop("No models successfully estimated")
    }

    # Extract coefficient information from each model
    results_list <- list()

    for (i in seq_along(models)) {
        model <- models[[i]]
        sample_size <- model$sample_size

        coefficients <- stats::coef(model)
        std_errors <- logitr::se(model)

        # Calculate t-statistics and power
        t_stats <- abs(coefficients / std_errors)
        critical_value <- stats::qt(1 - alpha / 2, df = Inf) # Using normal approximation
        power <- stats::pnorm(t_stats - critical_value) +
            stats::pnorm(-t_stats - critical_value)

        # Create data frame for this model
        model_results <- data.frame(
            sample_size = sample_size,
            parameter = names(coefficients),
            estimate = as.numeric(coefficients),
            std_error = as.numeric(std_errors),
            t_statistic = as.numeric(t_stats),
            power = as.numeric(power),
            stringsAsFactors = FALSE
        )

        results_list[[i]] <- model_results
    }

    # Combine all results
    power_summary <- do.call(rbind, results_list)
    rownames(power_summary) <- NULL

    return(power_summary)
}

#' Compare power across multiple designs
#' @param ... Named cbc_power objects to compare
#' @param type Type of plot: "power" for power curves or "se" for standard error curves
#' @param power_threshold Power threshold for horizontal reference line (only for power plots). Defaults to 0.8
#' @return A ggplot object comparing power curves
#' @export
plot_compare_power <- function(..., type = "power", power_threshold = 0.8) {
    power <- design <- std_error <- sample_size <- NULL

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "Package 'ggplot2' is required for plotting. Please install it with install.packages('ggplot2')"
        )
    }

    if (!type %in% c("power", "se")) {
        stop("type must be 'power' or 'se'")
    }

    power_objects <- list(...)

    if (length(power_objects) < 2) {
        stop("Need at least 2 power objects to compare")
    }

    # Get names from the call
    object_names <- as.character(substitute(list(...)))[-1]
    if (is.null(names(power_objects))) {
        names(power_objects) <- object_names
    }

    # Combine data
    combined_data <- list()
    for (i in seq_along(power_objects)) {
        obj_data <- power_objects[[i]]$power_summary
        obj_data$design <- names(power_objects)[i]
        combined_data[[i]] <- obj_data
    }

    plot_data <- do.call(rbind, combined_data)

    # Create comparison plot
    if (type == "power") {
        p <- ggplot2::ggplot(
            plot_data,
            ggplot2::aes(x = sample_size, y = power)
        ) +
            ggplot2::geom_hline(
                yintercept = power_threshold,
                color = "red",
                linetype = "dashed",
                alpha = 0.7
            ) +
            ggplot2::geom_line(ggplot2::aes(color = design), linewidth = 1) +
            ggplot2::geom_point(ggplot2::aes(color = design), size = 2) +
            ggplot2::facet_wrap(~parameter) +
            ggplot2::theme_bw() +
            ggplot2::labs(
                x = "Sample Size (number of respondents)",
                y = "Statistical Power",
                title = "Power Comparison Across Designs",
                subtitle = sprintf(
                    "Dashed line shows %.0f%% power threshold",
                    power_threshold * 100
                ),
                color = "Design"
            ) +
            ggplot2::ylim(0, 1)
    } else {
        p <- ggplot2::ggplot(
            plot_data,
            ggplot2::aes(x = sample_size, y = std_error)
        ) +
            ggplot2::geom_line(ggplot2::aes(color = design), linewidth = 1) +
            ggplot2::geom_point(ggplot2::aes(color = design), size = 2) +
            ggplot2::facet_wrap(~parameter) +
            ggplot2::theme_bw() +
            ggplot2::labs(
                x = "Sample Size (number of respondents)",
                y = "Standard Error",
                title = "Standard Error Comparison Across Designs",
                color = "Design"
            )
    }

    return(p)
}

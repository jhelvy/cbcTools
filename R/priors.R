#' Create prior specifications for CBC models
#'
#' Creates a standardized prior specification object for use in CBC analysis
#' functions like `cbc_choices()` and `cbc_design()`. Supports both fixed and random
#' parameters, with flexible specification of categorical variable levels.
#'
#' @param profiles A data frame of profiles created by `cbc_profiles()`
#' @param no_choice Prior specification for no-choice alternative. Can be:
#'   - A single numeric value for fixed no-choice utility
#'   - A `rand_spec()` object for random no-choice utility
#'   - `NULL` if no no-choice option (default)
#' @param n_draws Number of draws for DB-error calculation if using Bayesian
#'   priors. Defaults to `100`
#' @param draw_type Specify the draw type as a character: `"halton"`
#'   (the default) or `"sobol"` (recommended for models with more than 5
#'   random parameters).
#' @param ... Named arguments specifying priors for each attribute:
#'   - For fixed parameters:
#'     - Continuous variables: provide a single numeric value
#'     - Categorical variables: provide either:
#'       - An unnamed vector of values one less than the number of levels (dummy coding)
#'       - A named vector mapping levels to coefficients (remaining level becomes reference)
#'   - For random parameters: use `rand_spec()` to specify distribution, parameters, and correlations
#' @return A structured prior specification object including parameter draws for
#'   random coefficients
#' @export
cbc_priors <- function(
    profiles,
    no_choice = NULL,
    n_draws = 100,
    draw_type = "halton",
    ...
) {
    # Validate input class
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }

    # Get attribute information and combine with parameter specifications
    params <- list(...)
    if (!is.null(no_choice)) params$no_choice <- no_choice

    attrs <- get_combined_attr_info(profiles, params, include_no_choice = !is.null(no_choice))
    processed_params <- process_parameters(attrs)

    # Create coded data structure for parameter ordering
    model_data <- prepare_profiles_for_model(profiles, processed_params$means, attrs)
    profile_attrs <- names(attrs)[names(attrs) != "no_choice"]
    profile_randPars <- get_random_pars_for_logitr(processed_params$random, attrs, profile_attrs)

    codedData <- logitr::recodeData(model_data, profile_attrs, profile_randPars)
    codedParNames <- if (!is.null(no_choice)) c(codedData$pars, "no_choice") else codedData$pars

    # Generate parameter draws if we have random parameters
    par_draws <- NULL
    cor_mat <- NULL
    if (length(processed_params$random) > 0) {
        cor_mat <- build_correlation_matrix(attrs, processed_params$random)
        par_draws <- generate_parameter_draws(
            codedParNames, attrs, processed_params, cor_mat, n_draws, draw_type
        )
    }

    # Build parameter vectors in coded order
    if (!is.null(par_draws)) {
        # Use actual expected values from parameter draws
        # This correctly handles log-normal, censored normal, and correlated parameters
        pars_mean <- colMeans(par_draws)
        names(pars_mean) <- colnames(par_draws)
    } else {
        # No random parameters, use the input specification means
        pars_mean <- build_parameter_vector(codedParNames, attrs, processed_params$means)
    }

    # Create return object with metadata
    result <- list(
        profiles_metadata = create_profiles_metadata(profiles),
        attrs = attrs,
        pars = pars_mean,
        correlation = cor_mat,
        par_draws = par_draws,
        has_no_choice = !is.null(no_choice),
        created_at = Sys.time()
    )

    class(result) <- c("cbc_priors", "list")
    return(result)
}

#' Create a random parameter specification
#'
#' @param dist Character. Distribution type: "n" for normal, "ln" for log-normal,
#'   or "cn" for censored normal
#' @param mean Numeric. Mean parameter value(s)
#' @param sd Numeric. Standard deviation parameter value(s)
#' @param correlations List of correlation specifications created by cor_spec()
#' @return A random parameter specification list
#' @export
rand_spec <- function(dist = "n", mean, sd, correlations = NULL) {
    if (!dist %in% c("n", "ln", "cn")) {
        stop('dist must be one of "n" (normal), "ln" (log-normal), or "cn" (censored normal)')
    }

    if (!is.null(correlations)) {
        validate_correlations(correlations)
    }

    structure(
        list(dist = dist, mean = mean, sd = sd, correlations = correlations),
        class = "cbc_random_par"
    )
}

#' Create a correlation specification for random parameters
#'
#' @param with Character. Name of attribute to correlate with
#' @param value Numeric. Correlation value between -1 and 1
#' @param level Character. For categorical variables, specific level to correlate from
#' @param with_level Character. For categorical variables, specific level to correlate with
#' @return A correlation specification list
#' @export
cor_spec <- function(with, value, level = NULL, with_level = NULL) {
    if (!is.numeric(value) || value < -1 || value > 1) {
        stop("Correlation value must be between -1 and 1")
    }

    structure(
        list(attr = with, value = value, level = level, with_level = with_level),
        class = "cbc_correlation"
    )
}

#' Validate that priors are compatible with profiles
#'
#' This function checks if priors were created for the same profiles structure
#' @param priors A cbc_priors object
#' @param profiles A cbc_profiles object
#' @return Invisibly returns TRUE if compatible, throws error or warning if not
#' @export
validate_priors_profiles <- function(priors, profiles) {
    if (!inherits(priors, "cbc_priors")) {
        stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }

    priors_meta <- priors$profiles_metadata
    current_hash <- digest_profiles(profiles)

    # Check if profiles structure has changed
    if (priors_meta$profile_hash != current_hash) {
        current_attr_info <- attr(profiles, "attribute_info")

        if (!identical(priors_meta$attribute_info, current_attr_info)) {
            warning(
                "Priors were created for different profile attributes or levels. ",
                "Consider recreating priors with cbc_priors().",
                call. = FALSE
            )
        } else if (priors_meta$n_profiles != nrow(profiles)) {
            message(
                "Priors were created for profiles with ", priors_meta$n_profiles,
                " rows, but current profiles have ", nrow(profiles), " rows. ",
                "This is typically fine if you've applied restrictions."
            )
        }
    }

    invisible(TRUE)
}

# Helper Functions ----

# Validate correlations list
validate_correlations <- function(correlations) {
    if (!is.list(correlations)) {
        stop("correlations must be a list of correlation specifications created by cor_spec()")
    }
    if (!all(sapply(correlations, inherits, "cbc_correlation"))) {
        stop("all correlations must be created using cor_spec()")
    }
}

# Create profiles metadata for validation
create_profiles_metadata <- function(profiles) {
    list(
        attribute_info = attr(profiles, "attribute_info"),
        n_profiles = nrow(profiles),
        profile_hash = digest_profiles(profiles)
    )
}

# Create a simple hash of profiles structure
digest_profiles <- function(profiles) {
    attr_info <- attr(profiles, "attribute_info")
    structure_string <- paste(
        names(attr_info),
        sapply(attr_info, function(x) paste(x$type, x$n_levels, collapse = "_")),
        collapse = "|"
    )
    abs(sum(utf8ToInt(structure_string)))
}

# Get combined attribute information with parameter specifications
get_combined_attr_info <- function(profiles, params, include_no_choice = FALSE) {
    profile_attrs <- profiles[, -which(names(profiles) == "profileID")]

    # Validate parameter specifications
    validate_param_coverage(profile_attrs, params, include_no_choice)

    # Process each profile attribute
    attrs <- lapply(names(profile_attrs), function(attr) {
        create_attr_info(profile_attrs[[attr]], params[[attr]], attr)
    })
    names(attrs) <- names(profile_attrs)

    # Add no_choice attribute if specified
    if (include_no_choice && "no_choice" %in% names(params)) {
        attrs$no_choice <- create_no_choice_info(params$no_choice)
    }

    return(attrs)
}

# Validate parameter coverage
validate_param_coverage <- function(profile_attrs, params, include_no_choice) {
    missing_attrs <- setdiff(names(profile_attrs), names(params))
    if (length(missing_attrs) > 0) {
        stop("Missing prior specifications for attributes: ",
             paste(missing_attrs, collapse = ", "))
    }

    valid_params <- c(names(profile_attrs), if(include_no_choice) "no_choice" else NULL)
    extra_attrs <- setdiff(names(params), valid_params)
    if (length(extra_attrs) > 0) {
        stop("Prior specifications provided for non-existent attributes: ",
             paste(extra_attrs, collapse = ", "))
    }
}

# Create attribute information structure
create_attr_info <- function(values, param, attr_name) {
    all_levels <- get_all_levels(values)
    is_continuous <- is.numeric(values)
    is_random <- inherits(param, "cbc_random_par")

    info <- list(
        continuous = is_continuous,
        levels = all_levels,
        random = is_random
    )

    if (is_continuous) {
        info$range <- range(values)
    }

    # Process parameter values
    if (is_random) {
        info <- c(info, list(
            dist = param$dist,
            correlations = param$correlations
        ))
        info <- c(info, process_param_values(param$mean, param$sd, all_levels, is_continuous, attr_name, TRUE))
    } else {
        info <- c(info, process_param_values(param, NULL, all_levels, is_continuous, attr_name, FALSE))
    }

    return(info)
}

# Create no-choice attribute information
create_no_choice_info <- function(no_choice_param) {
    info <- list(
        continuous = TRUE,
        levels = NULL,
        random = inherits(no_choice_param, "cbc_random_par")
    )

    if (info$random) {
        info$dist <- no_choice_param$dist
        info$mean <- no_choice_param$mean
        info$sd <- no_choice_param$sd
        info$correlations <- no_choice_param$correlations
    } else {
        info$mean <- no_choice_param
    }

    return(info)
}

# Get all levels for an attribute
get_all_levels <- function(values) {
    if (is.numeric(values)) {
        sort(unique(values))
    } else {
        if (is.factor(values)) levels(values) else unique(values)
    }
}

# Process parameter values (handles both fixed and random)
process_param_values <- function(mean_param, sd_param, all_levels, is_continuous, attr_name, is_random) {
    if (is_continuous) {
        result <- list(mean = mean_param)
        if (is_random) result$sd <- sd_param
        return(result)
    }

    # Categorical variable processing
    if (!is.null(names(mean_param))) {
        # Named vector case
        validate_categorical_levels(names(mean_param), all_levels, attr_name)
        if (is_random && !identical(names(mean_param), names(sd_param))) {
            stop(sprintf("Names for mean and sd must match for random parameter '%s'", attr_name))
        }
        result <- list(mean = mean_param)
        if (is_random) result$sd <- sd_param
        return(result)
    } else {
        # Unnamed vector case - assign names based on remaining levels
        remaining_levels <- all_levels[-1]  # All but first level
        if (length(mean_param) != length(remaining_levels)) {
            stop(sprintf(
                "Incorrect number of values for '%s'. Expected %d values (one less than the number of levels)",
                attr_name, length(remaining_levels)
            ))
        }
        names(mean_param) <- remaining_levels
        result <- list(mean = mean_param)
        if (is_random) {
            names(sd_param) <- remaining_levels
            result$sd <- sd_param
        }
        return(result)
    }
}

# Validate categorical levels
validate_categorical_levels <- function(provided_levels, all_levels, attr_name) {
    invalid_levels <- setdiff(provided_levels, all_levels)
    if (length(invalid_levels) > 0) {
        stop(sprintf(
            "Invalid level(s) provided for '%s': %s\nValid levels are: %s",
            attr_name,
            paste(invalid_levels, collapse = ", "),
            paste(all_levels, collapse = ", ")
        ))
    }
}

# Process parameters into fixed and random components
process_parameters <- function(attrs) {
    fixed <- list()
    random <- list()
    means <- list()

    for (attr in names(attrs)) {
        info <- attrs[[attr]]
        means[[attr]] <- info$mean

        if (info$random) {
            random[[attr]] <- list(
                dist = info$dist,
                mean = info$mean,
                sd = info$sd,
                correlations = info$correlations
            )
        } else {
            fixed[[attr]] <- info$mean
        }
    }

    return(list(fixed = fixed, random = random, means = means))
}

# Get random parameters formatted for logitr
get_random_pars_for_logitr <- function(random_params, attrs, profile_attrs) {
    randPars <- list()
    for (attr in names(random_params)) {
        if (attr %in% profile_attrs) {  # Only profile attributes, not no_choice
            randPars[[attr]] <- random_params[[attr]]$dist
        }
    }
    return(if (length(randPars) > 0) randPars else NULL)
}

# Build parameter vector in coded order
build_parameter_vector <- function(codedParNames, attrs, means) {
    pars_mean <- numeric(length(codedParNames))
    names(pars_mean) <- codedParNames

    for (param_name in codedParNames) {
        original_attr <- find_original_attribute(param_name, attrs)
        pars_mean[param_name] <- extract_parameter_value(param_name, original_attr, attrs, means)
    }

    return(pars_mean)
}

# Find which original attribute a coded parameter belongs to
find_original_attribute <- function(param_name, attrs) {
    for (attr in names(attrs)) {
        if (attr == "no_choice" && param_name == "no_choice") {
            return(attr)
        } else if (attrs[[attr]]$continuous && param_name == attr) {
            return(attr)
        } else if (!attrs[[attr]]$continuous && startsWith(param_name, attr)) {
            return(attr)
        }
    }
    return(NULL)
}

# Extract parameter value for a specific coded parameter
extract_parameter_value <- function(param_name, original_attr, attrs, means) {
    if (is.null(original_attr) || !original_attr %in% names(means)) {
        return(0)  # Default value
    }

    attr_mean <- means[[original_attr]]

    if (attrs[[original_attr]]$continuous || original_attr == "no_choice") {
        return(attr_mean)
    } else {
        # Extract level name from coded parameter name
        level_name <- gsub(paste0("^", original_attr), "", param_name)
        return(attr_mean[level_name])
    }
}

# Prepare profiles for model creation
prepare_profiles_for_model <- function(profiles, means, attrs) {
    model_data <- profiles

    for (attr in names(attrs)) {
        if (attr != "no_choice" && !attrs[[attr]]$continuous) {
            levels_order <- determine_level_order(attr, means, attrs)
            model_data[[attr]] <- factor(profiles[[attr]], levels = levels_order)
        }
    }

    return(model_data)
}

# Determine level order for categorical variables
determine_level_order <- function(attr, means, attrs) {
    attr_mean <- means[[attr]]
    all_levels <- attrs[[attr]]$levels

    if (!is.null(names(attr_mean))) {
        # Named vector case
        coef_levels <- names(attr_mean)
        ref_level <- setdiff(all_levels, coef_levels)[1]
        return(c(ref_level, coef_levels))
    } else {
        # Unnamed vector case
        return(all_levels)
    }
}

# Build correlation matrix from random parameter specifications
build_correlation_matrix <- function(attrs, random_params) {
    par_names <- get_parameter_names_for_correlation(attrs, random_params)
    n_pars <- length(par_names)

    if (n_pars == 0) return(NULL)

    cor_mat <- diag(n_pars)
    rownames(cor_mat) <- colnames(cor_mat) <- par_names

    # Process correlations from each random parameter
    for (attr1 in names(random_params)) {
        param <- random_params[[attr1]]
        if (!is.null(param$correlations)) {
            tryCatch({
                cor_mat <- process_parameter_correlations(attr1, param$correlations, attrs, random_params, cor_mat)
            }, error = function(e) {
                stop(sprintf("Error processing correlations for attribute '%s': %s", attr1, e$message))
            })
        }
    }

    # Validate correlation matrix is positive definite
    eigenvals <- eigen(cor_mat)$values
    if (!all(eigenvals > 1e-10)) {
        stop("Specified correlations result in an invalid correlation matrix. Try specifying fewer or smaller correlations.")
    }

    return(cor_mat)
}

# Get parameter names for correlation matrix
get_parameter_names_for_correlation <- function(attrs, random_params) {
    par_names <- c()
    for (attr in names(random_params)) {
        if (attrs[[attr]]$continuous) {
            par_names <- c(par_names, attr)
        } else {
            categorical_names <- names(attrs[[attr]]$mean)
            # No "." separator to match logitr parameter naming
            par_names <- c(par_names, paste0(attr, categorical_names))
        }
    }
    return(par_names)
}

# Process correlations for a single parameter
process_parameter_correlations <- function(attr1, correlations, attrs, random_params, cor_mat) {
    # Group correlations by target attribute to handle general + specific overrides
    corr_by_attr <- list()

    for (cor in correlations) {
        attr2 <- cor$attr

        if (!attr2 %in% names(corr_by_attr)) {
            corr_by_attr[[attr2]] <- list()
        }

        corr_by_attr[[attr2]][[length(corr_by_attr[[attr2]]) + 1]] <- cor
    }

    # Process each target attribute
    for (attr2 in names(corr_by_attr)) {
        # Validate that target attribute exists and is random
        if (!(attr2 %in% names(random_params))) {
            stop(sprintf(
                "Cannot correlate with '%s' - it must be a random parameter specified using rand_spec()",
                attr2
            ))
        }

        attr_correlations <- corr_by_attr[[attr2]]

        # Find general correlation (no level specified) and specific correlations
        general_cor <- NULL
        specific_cors <- list()

        for (cor in attr_correlations) {
            if (is.null(cor$level) && is.null(cor$with_level)) {
                if (!is.null(general_cor)) {
                    warning(sprintf(
                        "Multiple general correlations specified between '%s' and '%s'. Using the last one.",
                        attr1, attr2
                    ))
                }
                general_cor <- cor
            } else {
                specific_cors[[length(specific_cors) + 1]] <- cor
            }
        }

        # Apply correlations - this modifies cor_mat in place and returns it
        cor_mat <- apply_correlations_between_attributes(attr1, attr2, general_cor, specific_cors, attrs, cor_mat)
    }

    return(cor_mat)
}

# Apply correlations between two attributes
apply_correlations_between_attributes <- function(attr1, attr2, general_cor, specific_cors, attrs, cor_mat) {

    # Get parameter names for both attributes
    attr1_params <- get_attribute_parameter_names(attr1, attrs)
    attr2_params <- get_attribute_parameter_names(attr2, attrs)

    # Apply general correlation first (if specified)
    if (!is.null(general_cor)) {
        for (param1 in attr1_params) {
            for (param2 in attr2_params) {
                # Check if parameters exist in matrix
                if (!param1 %in% rownames(cor_mat) || !param2 %in% colnames(cor_mat)) {
                    next
                }

                if (param1 != param2) {
                    cor_mat[param1, param2] <- general_cor$value
                    cor_mat[param2, param1] <- general_cor$value
                }
            }
        }
    }

    # Apply specific correlations (these override general correlations)
    for (cor in specific_cors) {
        # Determine source parameters
        if (!is.null(cor$level)) {
            # Source level specified
            if (attrs[[attr1]]$continuous) {
                stop(sprintf("Cannot specify level '%s' for continuous attribute '%s'", cor$level, attr1))
            }
            source_params <- get_specific_parameter_names(attr1, cor$level, attrs)
        } else {
            source_params <- attr1_params
        }

        # Determine target parameters
        if (!is.null(cor$with_level)) {
            # Target level specified
            if (attrs[[attr2]]$continuous) {
                stop(sprintf("Cannot specify with_level '%s' for continuous attribute '%s'", cor$with_level, attr2))
            }
            target_params <- get_specific_parameter_names(attr2, cor$with_level, attrs)
        } else {
            target_params <- attr2_params
        }

        # Set correlations
        for (param1 in source_params) {
            for (param2 in target_params) {
                # Check if parameters exist in matrix
                if (!param1 %in% rownames(cor_mat) || !param2 %in% colnames(cor_mat)) {
                    next
                }

                if (param1 != param2) {
                    cor_mat[param1, param2] <- cor$value
                    cor_mat[param2, param1] <- cor$value
                }
            }
        }
    }

    return(cor_mat)
}

get_parameter_names_for_correlation <- function(attrs, random_params) {
    par_names <- c()
    for (attr in names(random_params)) {
        if (attrs[[attr]]$continuous) {
            par_names <- c(par_names, attr)
        } else {
            categorical_names <- names(attrs[[attr]]$mean)
            # Fixed: remove the "." to match your correction
            par_names <- c(par_names, paste0(attr, categorical_names))
        }
    }
    return(par_names)
}

# Get parameter names for a specific level of an attribute
get_specific_parameter_names <- function(attr, level, attrs) {
    if (attrs[[attr]]$continuous) {
        stop(sprintf("Cannot specify level for continuous attribute '%s'", attr))
    }

    # Validate level exists
    if (!level %in% names(attrs[[attr]]$mean)) {
        stop(sprintf("Invalid level '%s' for attribute '%s'", level, attr))
    }

    # No "." separator to match logitr parameter naming
    return(paste0(attr, level))
}

get_attribute_parameter_names <- function(attr, attrs) {
    if (attrs[[attr]]$continuous) {
        return(attr)
    } else {
        # Categorical: return parameter names for all levels
        level_names <- names(attrs[[attr]]$mean)
        # No "." separator to match logitr parameter naming
        return(paste0(attr, level_names))
    }
}

# Determine which matrix indices should be correlated
determine_correlation_indices <- function(attr1, attr2, cor, attrs) {
    rows <- get_correlation_parameter_names(attr1, cor$level, attrs)
    cols <- get_correlation_parameter_names(attr2, cor$with_level, attrs)
    return(list(rows = rows, cols = cols))
}

# Get parameter names for correlation (handles continuous vs categorical)
get_correlation_parameter_names <- function(attr, level, attrs) {
    if (attrs[[attr]]$continuous) {
        if (!is.null(level)) {
            stop(sprintf("Cannot specify level for continuous attribute '%s'", attr))
        }
        return(attr)
    } else {
        if (!is.null(level)) {
            if (!level %in% names(attrs[[attr]]$mean)) {
                stop(sprintf("Invalid level '%s' for attribute '%s'", level, attr))
            }
            return(paste0(attr, ".", level))
        } else {
            return(paste0(attr, ".", names(attrs[[attr]]$mean)))
        }
    }
}

# Set correlation values in matrix
set_correlation_values <- function(cor_mat, rows, cols, value) {
    for (row in rows) {
        for (col in cols) {
            if (row != col) {  # Don't set correlation with self
                cor_mat[row, col] <<- value
                cor_mat[col, row] <<- value
            }
        }
    }
}

# Generate parameter draws for Bayesian analysis
generate_parameter_draws <- function(codedParNames, attrs, processed_params, cor_mat, n_draws, draw_type) {
    # Build parameter setup for logitr functions
    all_randPars <- get_all_random_pars(codedParNames, attrs, processed_params$random)
    parSetup <- get_parSetup(codedParNames, all_randPars)
    parIDs <- get_parIDs(parSetup)

    # Build standard deviation vector
    pars_sd <- build_sd_vector(codedParNames, attrs, processed_params$random)

    # Set up n list for makeBetaDraws
    n <- list(
        vars = length(parSetup),
        parsFixed = length(which(parSetup == "f")),
        parsRandom = length(which(parSetup != "f")),
        draws = n_draws,
        pars = length(codedParNames)
    )

    # Generate parameter draws
    standardDraws <- getStandardDraws(parIDs, n$draws, draw_type)
    pars_mean <- build_parameter_vector(codedParNames, attrs, processed_params$means)

    return(makeBetaDraws(pars_mean, pars_sd, parIDs, n, standardDraws, cor_mat))
}

# Get all random parameters mapped to coded names
get_all_random_pars <- function(codedParNames, attrs, random_params) {
    all_randPars <- list()

    for (param_name in codedParNames) {
        original_attr <- find_original_attribute(param_name, attrs)
        if (!is.null(original_attr) && original_attr %in% names(random_params)) {
            all_randPars[[param_name]] <- random_params[[original_attr]]$dist
        }
    }

    return(all_randPars)
}

# Build standard deviation vector
build_sd_vector <- function(codedParNames, attrs, random_params) {
    pars_sd <- c()
    pars_sd_names <- c()

    for (param_name in codedParNames) {
        original_attr <- find_original_attribute(param_name, attrs)
        if (!is.null(original_attr) && original_attr %in% names(random_params)) {
            sd_value <- extract_sd_value(param_name, original_attr, attrs, random_params)
            pars_sd <- c(pars_sd, sd_value)
            pars_sd_names <- c(pars_sd_names, param_name)
        }
    }

    names(pars_sd) <- pars_sd_names
    return(pars_sd)
}

# Extract standard deviation value for a parameter
extract_sd_value <- function(param_name, original_attr, attrs, random_params) {
    random_param <- random_params[[original_attr]]

    if (attrs[[original_attr]]$continuous || original_attr == "no_choice") {
        return(random_param$sd)
    } else {
        level_name <- gsub(paste0("^", original_attr), "", param_name)
        return(random_param$sd[level_name])
    }
}

# Modified from {logitr} - these functions remain unchanged for compatibility
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

get_parIDs <- function(parSetup) {
    return(list(
        f  = which(parSetup == "f"),
        r  = which(parSetup != "f"),
        n  = which(parSetup == "n"),
        ln = which(parSetup == "ln"),
        cn = which(parSetup == "cn")
    ))
}

makeBetaDraws <- function(pars_mean, pars_sd, parIDs, n, standardDraws, cor_mat) {
    # First scale the draws according to the covariance matrix
    lowerMat <- cor_mat^2
    diag(lowerMat) <- pars_sd
    lowerMat[!lower.tri(lowerMat, diag = TRUE)] <- 0
    scaledDraws <- standardDraws
    scaledDraws[,parIDs$r] <- scaledDraws[,parIDs$r] %*% lowerMat

    # Now shift the draws according to the means
    meanMat <- matrix(rep(pars_mean, n$draws), ncol = n$vars, byrow = TRUE)
    betaDraws <- meanMat + scaledDraws

    # log-normal draws: Exponentiate
    if (length(parIDs$ln) > 0) {
        betaDraws[, parIDs$ln] <- exp(betaDraws[, parIDs$ln])
    }

    # Censored normal draws: Censor
    if (length(parIDs$cn) > 0) {
        betaDraws[, parIDs$cn] <- pmax(betaDraws[, parIDs$cn], 0)
    }
    return(betaDraws)
}

getStandardDraws <- function(parIDs, numDraws, draw_type) {
    numBetas <- length(parIDs$f) + length(parIDs$r)
    if (draw_type == 'sobol') {
        draws <- as.matrix(randtoolbox::sobol(
            numDraws, numBetas, normal = TRUE
        ))
    } else {
        draws <- as.matrix(randtoolbox::halton(
            numDraws, numBetas, normal = TRUE
        ))
    }
    draws[, parIDs$f] <- 0 * draws[, parIDs$f]
    return(draws)
}

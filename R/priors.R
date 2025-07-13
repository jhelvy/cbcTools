#' Create prior specifications for CBC models
#'
#' Creates a standardized prior specification object for use in CBC analysis
#' functions like `cbc_choices()` and `cbc_design()`. Supports both fixed and random
#' parameters, with flexible specification of categorical variable levels and
#' interaction terms between fixed parameters.
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
#' @param interactions A list of interaction specifications created by `int_spec()`.
#'   Only interactions between fixed (non-random) parameters are supported.
#'   Each interaction must specify the appropriate level(s) for categorical variables.
#'   Defaults to `NULL` (no interactions).
#' @param ... Named arguments specifying priors for each attribute:
#'   - For fixed parameters:
#'     - Continuous variables: provide a single numeric value
#'     - Categorical variables: provide either:
#'       - An unnamed vector of values one less than the number of levels (dummy coding)
#'       - A named vector mapping levels to coefficients (remaining level becomes reference)
#'   - For random parameters: use `rand_spec()` to specify distribution, parameters, and correlations
#'
#' @details
#' ## Fixed vs Random Parameters
#'
#' **Fixed parameters** assume all respondents have the same preference coefficients.
#' Specify these as simple numeric values.
#'
#' **Random parameters** assume preference coefficients vary across respondents
#' according to a specified distribution. Use `rand_spec()` to define the
#' distribution type, mean, and standard deviation.
#'
#' ## Categorical Variable Specification
#'
#' For categorical variables, you can specify priors in two ways:
#'
#' 1. **Unnamed vector**: Provide coefficients for all levels except the first
#'    (which becomes the reference level). Order matters and should match the
#'    natural order of levels.
#'
#' 2. **Named vector**: Explicitly map coefficient values to specific levels.
#'    Any level not specified becomes the reference level.
#'
#' ## Interaction Terms
#'
#' Use the `interactions` parameter with `int_spec()` to include interaction
#' effects between attributes. Only interactions between fixed parameters are
#' supported. For categorical variables involved in interactions, you must
#' specify the relevant levels.
#'
#' ## No-Choice Options
#'
#' When including a no-choice alternative, provide a `no_choice` parameter.
#' This can be either a fixed numeric value or a `rand_spec()` for random
#' no-choice utility.
#'
#' @return A structured prior specification object including parameter draws for
#'   random coefficients and interaction terms. This object contains:
#'   - `pars`: Vector of mean parameter values
#'   - `par_draws`: Matrix of parameter draws (if random parameters specified)
#'   - `correlation`: Correlation matrix for random parameters (if applicable)
#'   - `interactions`: List of interaction specifications
#'   - `attrs`: Detailed attribute information
#'   - Additional metadata for validation and compatibility checking
#'
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles for examples
#' profiles <- cbc_profiles(
#'   price = c(1, 1.5, 2, 2.5, 3),
#'   type = c('Fuji', 'Gala', 'Honeycrisp'),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Example 1: Simple fixed priors
#' priors_fixed <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.25, # Negative = prefer lower prices
#'   type = c(0.5, 1.0), # "Fuji" is reference level
#'   freshness = c(0.6, 1.2) # "Poor" reference level
#' )
#'
#' # Example 2: Named categorical priors (more explicit)
#' priors_named <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.25,
#'   type = c("Gala" = 0.5, "Honeycrisp" = 1.0),  # "Fuji" is reference
#'   freshness = c("Average" = 0.6, "Excellent" = 1.2)  # "Poor" is reference
#' )
#'
#' # Example 3: Random parameters - normal distributions for "price" and "freshness"
#' priors_random <- cbc_priors(
#'   profiles = profiles,
#'   price = rand_spec(
#'     dist = "n",
#'     mean = -0.25,
#'     sd = 0.1
#'   ),
#'   type = c(0.5, 1.0),
#'   freshness = rand_spec(
#'     dist = "n",
#'     mean = c(0.6, 1.2),
#'     sd = c(0.1, 0.1)
#'   )
#' )
#'
#' # Example 4: Correlated random parameters
#' priors_correlated <- cbc_priors(
#'   profiles = profiles,
#'   price = rand_spec(
#'     dist = "n",
#'     mean = -0.1,
#'     sd = 0.05,
#'     correlations = list(
#'       cor_spec(
#'         with = "type",
#'         with_level = "Honeycrisp",
#'         value = 0.3
#'       )
#'     )
#'   ),
#'   type = rand_spec(
#'     dist = "n",
#'     mean = c("Gala" = 0.1, "Honeycrisp" = 0.2),
#'     sd = c("Gala" = 0.05, "Honeycrisp" = 0.1)
#'   ),
#'   freshness = c(0.1, 0.2)
#' )
#'
#' # Example 5: With interaction terms
#' priors_interactions <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.25,
#'   type = c("Fuji" = 0.5, "Honeycrisp" = 1.0),
#'   freshness = c("Average" = 0.6, "Excellent" = 1.2),
#'   interactions = list(
#'     # Price sensitivity varies by apple type
#'     int_spec(
#'       between = c("price", "type"),
#'       with_level = "Fuji",
#'       value = 0.1
#'     ),
#'     int_spec(
#'       between = c("price", "type"),
#'       with_level = "Honeycrisp",
#'       value = 0.2
#'     ),
#'     # Type preferences vary by freshness
#'     int_spec(
#'       between = c("type", "freshness"),
#'       level = "Honeycrisp",
#'       with_level = "Excellent",
#'       value = 0.3
#'     )
#'   )
#' )
#'
#' # Example 6: Including no-choice option
#' priors_nochoice_fixed <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.25,
#'   type = c(0.5, 1.0),
#'   freshness = c(0.6, 1.2),
#'   no_choice = -0.5 # Negative values make no-choice less attractive
#' )
#'
#' # Example 7: Random no-choice
#' priors_nochoice_random <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.25,
#'   type = c(0.5, 1.0),
#'   freshness = c(0.6, 1.2),
#'   no_choice = rand_spec(dist = "n", mean = -0.5, sd = 0.2)
#' )
#'
#' # View the priors
#' priors_fixed
#' priors_random
cbc_priors <- function(
    profiles,
    no_choice = NULL,
    n_draws = 100,
    draw_type = "halton",
    interactions = NULL, # New parameter
    ...
) {
    # Validate input class
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }

    # Get attribute information and combine with parameter specifications
    params <- list(...)
    if (!is.null(no_choice)) {
        params$no_choice <- no_choice
    }

    attrs <- get_combined_attr_info(
        profiles,
        params,
        include_no_choice = !is.null(no_choice)
    )

    # Validate interactions (only for fixed parameters)
    if (!is.null(interactions)) {
        validate_interactions(interactions, attrs)
    }

    processed_params <- process_parameters(attrs)

    # Create coded data structure for parameter ordering
    model_data <- prepare_profiles_for_model(
        profiles,
        processed_params$means,
        attrs
    )
    profile_attrs <- names(attrs)[names(attrs) != "no_choice"]
    profile_randPars <- get_random_pars_for_logitr(
        processed_params$random,
        attrs,
        profile_attrs
    )

    # Include interaction terms in variable names for logitr
    var_names_with_interactions <- get_var_names_with_interactions(
        profile_attrs,
        interactions
    )

    codedData <- logitr::recodeData(
        model_data,
        var_names_with_interactions,
        profile_randPars
    )
    codedParNames <- if (!is.null(no_choice)) {
        c(codedData$pars, "no_choice")
    } else {
        codedData$pars
    }

    # Generate parameter draws if we have random parameters
    par_draws <- NULL
    cor_mat <- NULL
    if (length(processed_params$random) > 0) {
        cor_mat <- build_correlation_matrix(attrs, processed_params$random)
        par_draws <- generate_parameter_draws(
            codedParNames,
            attrs,
            processed_params,
            cor_mat,
            n_draws,
            draw_type
        )
    }

    # Build parameter vectors in coded order (including interactions)
    if (!is.null(par_draws)) {
        # Use actual expected values from parameter draws for random parameters
        pars_mean <- colMeans(par_draws)
        names(pars_mean) <- codedParNames
        # But override interaction values since they're always fixed
        pars_mean <- set_interaction_values(
            pars_mean,
            codedParNames,
            interactions,
            attrs
        )
    } else {
        # No random parameters, use the input specification means
        pars_mean <- build_parameter_vector_with_interactions(
            codedParNames,
            attrs,
            processed_params$means,
            interactions
        )
    }

    # Create return object with metadata
    result <- list(
        profiles_metadata = create_profiles_metadata(profiles),
        attrs = attrs,
        interactions = interactions, # Store interactions
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
        stop(
            'dist must be one of "n" (normal), "ln" (log-normal), or "cn" (censored normal)'
        )
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
        list(
            attr = with,
            value = value,
            level = level,
            with_level = with_level
        ),
        class = "cbc_correlation"
    )
}

#' Create an interaction specification for fixed parameters
#'
#' @param between Character vector of length 2 specifying the two attributes to interact
#' @param value Numeric. Interaction coefficient value
#' @param level Character. For categorical variables, specific level of first attribute
#' @param with_level Character. For categorical variables, specific level of second attribute
#' @return An interaction specification list
#' @export
#' @examples
#' # Continuous * continuous interaction
#' int_spec(between = c("price", "weight"), value = 0.1)
#'
#' # Continuous * categorical interactions (must specify categorical level)
#' int_spec(between = c("price", "type"), with_level = "Fuji", value = 0.15)
#' int_spec(between = c("price", "type"), with_level = "Gala", value = 0.05)
#'
#' # Categorical * categorical interactions (must specify both levels)
#' int_spec(between = c("type", "freshness"),
#'          level = "Fuji", with_level = "Poor", value = -0.2)
int_spec <- function(between, value, level = NULL, with_level = NULL) {
    if (length(between) != 2) {
        stop("'between' must specify exactly 2 attributes")
    }

    if (!is.numeric(value) || length(value) != 1) {
        stop("'value' must be a single numeric value")
    }

    structure(
        list(
            attr1 = between[1],
            attr2 = between[2],
            value = value,
            level = level,
            with_level = with_level
        ),
        class = "cbc_interaction"
    )
}

# Helper Functions ----

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
        sapply(attr_info, function(x) {
            paste(x$type, x$n_levels, collapse = "_")
        }),
        collapse = "|"
    )
    abs(sum(utf8ToInt(structure_string)))
}

# Get combined attribute information with parameter specifications
get_combined_attr_info <- function(
    profiles,
    params,
    include_no_choice = FALSE
) {
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
        stop(
            "Missing prior specifications for attributes: ",
            paste(missing_attrs, collapse = ", ")
        )
    }

    valid_params <- c(
        names(profile_attrs),
        if (include_no_choice) "no_choice" else NULL
    )
    extra_attrs <- setdiff(names(params), valid_params)
    if (length(extra_attrs) > 0) {
        stop(
            "Prior specifications provided for non-existent attributes: ",
            paste(extra_attrs, collapse = ", ")
        )
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
        info <- c(
            info,
            list(
                dist = param$dist,
                correlations = param$correlations
            )
        )
        info <- c(
            info,
            process_param_values(
                param$mean,
                param$sd,
                all_levels,
                is_continuous,
                attr_name,
                TRUE
            )
        )
    } else {
        info <- c(
            info,
            process_param_values(
                param,
                NULL,
                all_levels,
                is_continuous,
                attr_name,
                FALSE
            )
        )
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
process_param_values <- function(
    mean_param,
    sd_param,
    all_levels,
    is_continuous,
    attr_name,
    is_random
) {
    if (is_continuous) {
        result <- list(mean = mean_param)
        if (is_random) {
            result$sd <- sd_param
        }
        return(result)
    }

    # Categorical variable processing
    if (!is.null(names(mean_param))) {
        # Named vector case
        validate_categorical_levels(names(mean_param), all_levels, attr_name)
        if (is_random && !identical(names(mean_param), names(sd_param))) {
            stop(sprintf(
                "Names for mean and sd must match for random parameter '%s'",
                attr_name
            ))
        }
        result <- list(mean = mean_param)
        if (is_random) {
            result$sd <- sd_param
        }
        return(result)
    } else {
        # Unnamed vector case - assign names based on remaining levels
        remaining_levels <- all_levels[-1] # All but first level
        if (length(mean_param) != length(remaining_levels)) {
            stop(sprintf(
                "Incorrect number of values for '%s'. Expected %d values (one less than the number of levels)",
                attr_name,
                length(remaining_levels)
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
validate_categorical_levels <- function(
    provided_levels,
    all_levels,
    attr_name
) {
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
        if (attr %in% profile_attrs) {
            # Only profile attributes, not no_choice
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
        pars_mean[param_name] <- extract_parameter_value(
            param_name,
            original_attr,
            attrs,
            means
        )
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
        return(0) # Default value
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
            model_data[[attr]] <- factor(
                profiles[[attr]],
                levels = levels_order
            )
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

    if (n_pars == 0) {
        return(NULL)
    }

    cor_mat <- diag(n_pars)
    rownames(cor_mat) <- colnames(cor_mat) <- par_names

    # Process correlations from each random parameter
    for (attr1 in names(random_params)) {
        param <- random_params[[attr1]]
        if (!is.null(param$correlations)) {
            tryCatch(
                {
                    cor_mat <- process_parameter_correlations(
                        attr1,
                        param$correlations,
                        attrs,
                        random_params,
                        cor_mat
                    )
                },
                error = function(e) {
                    stop(sprintf(
                        "Error processing correlations for attribute '%s': %s",
                        attr1,
                        e$message
                    ))
                }
            )
        }
    }

    # Validate correlation matrix is positive definite
    eigenvals <- eigen(cor_mat)$values
    if (!all(eigenvals > 1e-10)) {
        stop(
            "Specified correlations result in an invalid correlation matrix. Try specifying fewer or smaller correlations."
        )
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
process_parameter_correlations <- function(
    attr1,
    correlations,
    attrs,
    random_params,
    cor_mat
) {
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
                        attr1,
                        attr2
                    ))
                }
                general_cor <- cor
            } else {
                specific_cors[[length(specific_cors) + 1]] <- cor
            }
        }

        # Apply correlations - this modifies cor_mat in place and returns it
        cor_mat <- apply_correlations_between_attributes(
            attr1,
            attr2,
            general_cor,
            specific_cors,
            attrs,
            cor_mat
        )
    }

    return(cor_mat)
}

# Apply correlations between two attributes
apply_correlations_between_attributes <- function(
    attr1,
    attr2,
    general_cor,
    specific_cors,
    attrs,
    cor_mat
) {
    # Get parameter names for both attributes
    attr1_params <- get_attribute_parameter_names(attr1, attrs)
    attr2_params <- get_attribute_parameter_names(attr2, attrs)

    # Apply general correlation first (if specified)
    if (!is.null(general_cor)) {
        for (param1 in attr1_params) {
            for (param2 in attr2_params) {
                # Check if parameters exist in matrix
                if (
                    !param1 %in% rownames(cor_mat) ||
                        !param2 %in% colnames(cor_mat)
                ) {
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
                stop(sprintf(
                    "Cannot specify level '%s' for continuous attribute '%s'",
                    cor$level,
                    attr1
                ))
            }
            source_params <- get_specific_parameter_names(
                attr1,
                cor$level,
                attrs
            )
        } else {
            source_params <- attr1_params
        }

        # Determine target parameters
        if (!is.null(cor$with_level)) {
            # Target level specified
            if (attrs[[attr2]]$continuous) {
                stop(sprintf(
                    "Cannot specify with_level '%s' for continuous attribute '%s'",
                    cor$with_level,
                    attr2
                ))
            }
            target_params <- get_specific_parameter_names(
                attr2,
                cor$with_level,
                attrs
            )
        } else {
            target_params <- attr2_params
        }

        # Set correlations
        for (param1 in source_params) {
            for (param2 in target_params) {
                # Check if parameters exist in matrix
                if (
                    !param1 %in% rownames(cor_mat) ||
                        !param2 %in% colnames(cor_mat)
                ) {
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
        stop(sprintf(
            "Cannot specify level for continuous attribute '%s'",
            attr
        ))
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

# Generate parameter draws for Bayesian analysis
generate_parameter_draws <- function(
    codedParNames,
    attrs,
    processed_params,
    cor_mat,
    n_draws,
    draw_type
) {
    # Build parameter setup for logitr functions
    all_randPars <- get_all_random_pars(
        codedParNames,
        attrs,
        processed_params$random
    )
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
    pars_mean <- build_parameter_vector(
        codedParNames,
        attrs,
        processed_params$means
    )

    return(makeBetaDraws(pars_mean, pars_sd, parIDs, n, standardDraws, cor_mat))
}

# Get all random parameters mapped to coded names
get_all_random_pars <- function(codedParNames, attrs, random_params) {
    all_randPars <- list()

    for (param_name in codedParNames) {
        original_attr <- find_original_attribute(param_name, attrs)
        if (
            !is.null(original_attr) && original_attr %in% names(random_params)
        ) {
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
        if (
            !is.null(original_attr) && original_attr %in% names(random_params)
        ) {
            sd_value <- extract_sd_value(
                param_name,
                original_attr,
                attrs,
                random_params
            )
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
        f = which(parSetup == "f"),
        r = which(parSetup != "f"),
        n = which(parSetup == "n"),
        ln = which(parSetup == "ln"),
        cn = which(parSetup == "cn")
    ))
}

makeBetaDraws <- function(
    pars_mean,
    pars_sd,
    parIDs,
    n,
    standardDraws,
    cor_mat
) {
    # First scale the draws according to the covariance matrix
    lowerMat <- cor_mat^2
    diag(lowerMat) <- pars_sd
    lowerMat[!lower.tri(lowerMat, diag = TRUE)] <- 0
    scaledDraws <- standardDraws
    scaledDraws[, parIDs$r] <- scaledDraws[, parIDs$r] %*% lowerMat

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

    colnames(betaDraws) <- names(pars_mean)
    return(betaDraws)
}

getStandardDraws <- function(parIDs, numDraws, draw_type) {
    numBetas <- length(parIDs$f) + length(parIDs$r)
    if (draw_type == 'sobol') {
        draws <- as.matrix(randtoolbox::sobol(
            numDraws,
            numBetas,
            normal = TRUE
        ))
    } else {
        draws <- as.matrix(randtoolbox::halton(
            numDraws,
            numBetas,
            normal = TRUE
        ))
    }
    draws[, parIDs$f] <- 0 * draws[, parIDs$f]
    return(draws)
}

# Interactions ----

# Validate interaction specifications
validate_interactions <- function(interactions, attrs) {
    if (!is.list(interactions)) {
        stop(
            "interactions must be a list of interaction specifications created by int_spec()"
        )
    }

    if (!all(sapply(interactions, inherits, "cbc_interaction"))) {
        stop("all interactions must be created using int_spec()")
    }

    for (int in interactions) {
        # Check that both attributes exist
        missing_attrs <- setdiff(c(int$attr1, int$attr2), names(attrs))
        if (length(missing_attrs) > 0) {
            stop(sprintf(
                "Interaction references non-existent attributes: %s",
                paste(missing_attrs, collapse = ", ")
            ))
        }

        # Check that neither attribute is random
        if (attrs[[int$attr1]]$random || attrs[[int$attr2]]$random) {
            stop(sprintf(
                "Interactions with random parameters not supported. Attributes '%s' and '%s' must be fixed parameters.",
                int$attr1,
                int$attr2
            ))
        }

        # Validate level specifications
        validate_interaction_levels(int, attrs)
    }

    invisible(TRUE)
}

# Validate level specifications for interactions
validate_interaction_levels <- function(int, attrs) {
    attr1_continuous <- attrs[[int$attr1]]$continuous
    attr2_continuous <- attrs[[int$attr2]]$continuous

    # For continuous * categorical interactions, must specify the categorical level
    if (attr1_continuous && !attr2_continuous) {
        # price * type case: must specify with_level for the categorical variable
        if (is.null(int$with_level)) {
            stop(sprintf(
                "For interaction between continuous '%s' and categorical '%s', must specify with_level",
                int$attr1,
                int$attr2
            ))
        }
        if (!is.null(int$level)) {
            stop(sprintf(
                "Cannot specify level for continuous attribute '%s'",
                int$attr1
            ))
        }
    }

    if (!attr1_continuous && attr2_continuous) {
        # type * price case: must specify level for the categorical variable
        if (is.null(int$level)) {
            stop(sprintf(
                "For interaction between categorical '%s' and continuous '%s', must specify level",
                int$attr1,
                int$attr2
            ))
        }
        if (!is.null(int$with_level)) {
            stop(sprintf(
                "Cannot specify with_level for continuous attribute '%s'",
                int$attr2
            ))
        }
    }

    # For categorical * categorical, both level and with_level should be specified
    if (!attr1_continuous && !attr2_continuous) {
        if (is.null(int$level) || is.null(int$with_level)) {
            stop(sprintf(
                "For interaction between categorical '%s' and categorical '%s', must specify both level and with_level",
                int$attr1,
                int$attr2
            ))
        }
    }

    # For continuous * continuous, neither level nor with_level should be specified
    if (attr1_continuous && attr2_continuous) {
        if (!is.null(int$level) || !is.null(int$with_level)) {
            stop(sprintf(
                "Cannot specify level or with_level for interaction between continuous attributes '%s' and '%s'",
                int$attr1,
                int$attr2
            ))
        }
    }

    # Validate that specified levels exist
    if (!is.null(int$level)) {
        if (!int$level %in% names(attrs[[int$attr1]]$mean)) {
            stop(sprintf(
                "Invalid level '%s' for attribute '%s'",
                int$level,
                int$attr1
            ))
        }
    }

    if (!is.null(int$with_level)) {
        if (!int$with_level %in% names(attrs[[int$attr2]]$mean)) {
            stop(sprintf(
                "Invalid with_level '%s' for attribute '%s'",
                int$with_level,
                int$attr2
            ))
        }
    }

    invisible(TRUE)
}

# Get variable names including interaction terms for logitr
get_var_names_with_interactions <- function(
    profile_attrs,
    interactions = NULL
) {
    if (is.null(interactions)) {
        return(profile_attrs)
    }

    # Add interaction terms in logitr format
    interaction_terms <- build_interaction_terms(interactions)
    return(c(profile_attrs, interaction_terms))
}

# Build interaction terms in logitr format
build_interaction_terms <- function(interactions) {
    # For logitr, we only need to specify the general interaction terms
    # like "price*type". logitr will automatically create all the
    # specific level interactions like "price:typeGala"

    unique_pairs <- unique(sapply(interactions, function(int) {
        paste(sort(c(int$attr1, int$attr2)), collapse = "*")
    }))

    return(unique_pairs)
}

# Build parameter vector including interaction values
build_parameter_vector_with_interactions <- function(
    codedParNames,
    attrs,
    means,
    interactions
) {
    # Start with the original function
    pars_mean <- build_parameter_vector(codedParNames, attrs, means)

    # Add interaction values
    pars_mean <- set_interaction_values(
        pars_mean,
        codedParNames,
        interactions,
        attrs
    )

    return(pars_mean)
}

# Set interaction parameter values in the parameter vector
set_interaction_values <- function(
    pars_mean,
    codedParNames,
    interactions,
    attrs
) {
    if (is.null(interactions)) {
        return(pars_mean)
    }

    for (param_name in codedParNames) {
        if (is_interaction_parameter(param_name)) {
            int_value <- extract_interaction_value(
                param_name,
                interactions,
                attrs
            )
            if (!is.na(int_value)) {
                pars_mean[param_name] <- int_value
            }
        }
    }

    return(pars_mean)
}

# Check if a parameter name represents an interaction
is_interaction_parameter <- function(param_name) {
    # logitr uses ":" for interactions, e.g., "price:typeGala"
    grepl(":", param_name, fixed = TRUE)
}

# Extract interaction value for a specific coded parameter
extract_interaction_value <- function(param_name, interactions, attrs) {
    if (is.null(interactions)) {
        return(NA)
    }

    # Parse the interaction parameter name
    # Format: "attr1:attr2level" or "attr1level:attr2level" etc.
    parts <- strsplit(param_name, ":", fixed = TRUE)[[1]]

    if (length(parts) != 2) {
        return(NA)
    }

    # Try to match this parameter to one of our interaction specifications
    for (int in interactions) {
        if (matches_interaction_spec(param_name, parts, int, attrs)) {
            return(int$value)
        }
    }

    return(0) # Default value for unspecified interactions
}

# Check if a coded parameter matches an interaction specification
matches_interaction_spec <- function(param_name, param_parts, int_spec, attrs) {
    # This function needs to handle the complex matching between
    # logitr's parameter naming (e.g., "price:typeGala") and our
    # interaction specifications

    attr1 <- int_spec$attr1
    attr2 <- int_spec$attr2
    level <- int_spec$level
    with_level <- int_spec$with_level

    # Handle different cases based on parameter structure

    # Case 1: continuous * categorical (e.g., "price:typeGala")
    if (attrs[[attr1]]$continuous && !attrs[[attr2]]$continuous) {
        expected_pattern <- paste0(
            attr1,
            ":",
            attr2,
            if (!is.null(with_level)) with_level else ""
        )
        if (is.null(with_level)) {
            # General interaction - match any level
            return(startsWith(param_name, paste0(attr1, ":", attr2)))
        } else {
            # Specific level interaction
            return(param_name == paste0(attr1, ":", attr2, with_level))
        }
    }

    # Case 2: categorical * continuous (e.g., "typeGala:price")
    if (!attrs[[attr1]]$continuous && attrs[[attr2]]$continuous) {
        if (is.null(level)) {
            # General interaction - match any level
            return(grepl(paste0(attr1, ".*:", attr2), param_name))
        } else {
            # Specific level interaction
            return(param_name == paste0(attr1, level, ":", attr2))
        }
    }

    # Case 3: categorical * categorical (e.g., "typeGala:freshnessAverage")
    if (!attrs[[attr1]]$continuous && !attrs[[attr2]]$continuous) {
        if (is.null(level) && is.null(with_level)) {
            # General interaction - this is complex for categorical*categorical
            # Would need to match any combination of levels
            return(
                grepl(paste0(attr1, ".*:", attr2), param_name) ||
                    grepl(paste0(attr2, ".*:", attr1), param_name)
            )
        } else if (!is.null(level) && !is.null(with_level)) {
            # Specific level interaction
            return(
                param_name == paste0(attr1, level, ":", attr2, with_level) ||
                    param_name == paste0(attr2, with_level, ":", attr1, level)
            )
        }
    }

    # Case 4: continuous * continuous - straightforward
    if (attrs[[attr1]]$continuous && attrs[[attr2]]$continuous) {
        return(
            param_name == paste0(attr1, ":", attr2) ||
                param_name == paste0(attr2, ":", attr1)
        )
    }

    return(FALSE)
}

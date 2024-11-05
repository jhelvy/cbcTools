#' Create prior specifications for CBC models
#'
#' Creates a standardized prior specification object for use in CBC analysis
#' functions like cbc_choices() and cbc_d_error(). Supports both fixed and random
#' parameters, with flexible specification of categorical variable levels.
#'
#' @param profiles A data frame of profiles created by cbc_profiles()
#' @param ... Named arguments specifying priors for each attribute:
#'   - For fixed parameters:
#'     - Continuous variables: provide a single numeric value
#'     - Categorical variables: provide either:
#'       - An unnamed vector of values one less than the number of levels (dummy coding)
#'       - A named vector mapping levels to coefficients (remaining level becomes reference)
#'   - For random parameters: use rand() to specify distribution and parameters
#' @param correlation Optional correlation matrix for random parameters
#' @return A structured prior specification object including parameter draws for
#'   random coefficients
#' @export
#' @examples
#' # Create profiles for an example conjoint about apples
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c("Poor", "Average", "Excellent")
#' )
#'
#' # Example 1: Simple fixed parameters with unnamed vectors
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c(0.2, 0.3),          # Dummy-coded categorical (Honeycrisp reference)
#'   freshness = c(0.4, 0.8)      # Dummy-coded categorical (Poor reference)
#' )
#'
#' # Example 2: Using named vectors for categorical variables
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c("Fuji" = 0.2, "Gala" = 0.3),  # Honeycrisp as reference
#'   freshness = c("Poor" = -0.4, "Average" = 0.1)  # Excellent as reference
#' )
#'
#' # Example 3: Mixed fixed and random parameters
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,  # Fixed parameter
#'   type = rand(   # Random parameter - normal distribution
#'     dist = "n",
#'     mean = c("Fuji" = 0.2, "Gala" = 0.3),  # Honeycrisp reference
#'     sd = c("Fuji" = 0.4, "Gala" = 0.4)
#'   ),
#'   freshness = c(0.4, 0.8)  # Fixed parameter
#' )
#'
#' # Example 4: Log-normal distribution for price coefficient
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = rand(
#'     dist = "ln",    # Log-normal distribution for price
#'     mean = -0.5,    # Single value for continuous variable
#'     sd = 0.4
#'   ),
#'   type = c(0.2, 0.3),
#'   freshness = c(0.4, 0.8)
#' )
#'
#' # Example 5: Using correlation matrix with multiple random parameters
#' correlation <- matrix(
#'   c(1.0, 0.3,
#'     0.3, 1.0),
#'   nrow = 2
#' )
#'
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = rand(
#'     dist = "n",
#'     mean = -0.5,
#'     sd = 0.4
#'   ),
#'   type = rand(
#'     dist = "n",
#'     mean = c("Fuji" = 0.2, "Gala" = 0.3),
#'     sd = c("Fuji" = 0.4, "Gala" = 0.4)
#'   ),
#'   freshness = c(0.4, 0.8),
#'   correlation = correlation
#' )
cbc_priors <- function(profiles, ..., correlation = NULL) {
    # Validate profiles input
    if (!inherits(profiles, "data.frame") || !"profileID" %in% names(profiles)) {
        stop("'profiles' must be a data frame created by cbc_profiles()")
    }

    # Get attribute information
    attr_info <- get_attribute_info(profiles)

    # Process provided parameters
    params <- list(...)

    # Validate all attributes are specified
    check_attribute_names(params, attr_info)

    # Process parameters into standardized format
    processed_params <- process_parameters(params, attr_info)

    # Separate fixed and random parameters
    fixed_params <- processed_params$fixed
    random_params <- processed_params$random

    # Store parameter specifications in a structured format for printing
    param_specs <- list()

    # Process fixed parameters
    for (attr in names(fixed_params)) {
        param_specs[[attr]] <- list(
            type = "fixed",
            values = fixed_params[[attr]]
        )
    }

    # Process random parameters
    for (attr in names(random_params)) {
        param_specs[[attr]] <- list(
            type = "random",
            dist = random_params[[attr]]$dist,
            mean = random_params[[attr]]$mean,
            sd = random_params[[attr]]$sd
        )
    }

    # Create parameter draws for random parameters if any exist
    if (length(random_params) > 0) {
        # Set up random parameters specification for logitr
        randPars <- lapply(random_params, function(x) x$dist)

        # Get parameter names
        parNames <- c(names(fixed_params), names(random_params))

        # Create coded data structure
        model_data <- prepare_profiles_for_model(
            profiles, c(
                fixed_params,
                lapply(random_params, function(x) x$mean)),
            attr_info
        )
        codedData <- logitr::recodeData(model_data, parNames, randPars)

        # Set up parameter structure
        parSetup <- get_parSetup(codedData$pars, randPars)
        parIDs <- get_parIDs(parSetup)

        # Combine all parameters for draws
        all_means <- c(
            unlist(fixed_params),
            unlist(lapply(random_params, function(x) x$mean))
        )
        all_sds <- unlist(lapply(random_params, function(x) x$sd))

        # Set up n list for makeBetaDraws
        n <- list(
            vars = length(parSetup),
            parsFixed = length(which(parSetup == "f")),
            parsRandom = length(which(parSetup != "f")),
            draws = 100,  # Default number of draws
            pars = length(all_means) + length(all_sds)
        )

        # Generate parameter draws
        standardDraws <- getStandardDraws(parIDs, n$draws, "halton")
        par_draws <- makeBetaDraws(
            c(all_means, all_sds),
            parIDs,
            n,
            standardDraws,
            correlation
        )
    } else {
        par_draws <- NULL
    }

    # Create return object
    result <- list(
        pars = c(fixed_params, lapply(random_params, function(x) x$mean)),
        par_draws = par_draws,
        correlation = correlation,
        attr_info = attr_info,
        param_specs = param_specs  # Add structured parameter specifications
    )

    class(result) <- c("cbc_priors", "list")
    return(result)
}

# Helper function to create random parameter specifications
rand <- function(dist = "n", mean, sd) {
    if (!dist %in% c("n", "ln", "cn")) {
        stop('dist must be one of "n" (normal), "ln" (log-normal), or "cn" (censored normal)')
    }

    structure(
        list(
            dist = dist,
            mean = mean,
            sd = sd
        ),
        class = "cbc_random_par"
    )
}

# Helper function to process parameters into standardized format
process_parameters <- function(params, attr_info) {
    fixed <- list()
    random <- list()

    for (attr in names(params)) {
        param <- params[[attr]]

        if (inherits(param, "cbc_random_par")) {
            # Random parameter
            random[[attr]] <- param
        } else {
            # Fixed parameter
            if (attr_info[[attr]]$type == "continuous") {
                if (!is.numeric(param) || length(param) != 1) {
                    stop("Prior for continuous attribute '", attr,
                         "' must be a single numeric value")
                }
                fixed[[attr]] <- param
            } else {
                # Categorical attribute
                if (!is.null(names(param))) {
                    # Validate named levels
                    invalid_levels <- setdiff(names(param), attr_info[[attr]]$levels)
                    if (length(invalid_levels) > 0) {
                        stop("Invalid levels for '", attr, "': ",
                             paste(invalid_levels, collapse = ", "))
                    }
                } else {
                    # Validate length for unnamed vector
                    if (length(param) != length(attr_info[[attr]]$levels) - 1) {
                        stop("Prior for categorical attribute '", attr,
                             "' must have ", length(attr_info[[attr]]$levels) - 1,
                             " values")
                    }
                    # Add names based on non-reference levels
                    names(param) <- attr_info[[attr]]$levels[-1]
                }
                fixed[[attr]] <- param
            }
        }
    }

    return(list(fixed = fixed, random = random))
}

# Helper function to extract attribute information from profiles
get_attribute_info <- function(profiles) {
    # Remove profileID column
    attrs <- profiles[, -which(names(profiles) == "profileID")]

    # Get information for each attribute
    attr_info <- lapply(names(attrs), function(attr) {
        values <- attrs[[attr]]
        is_continuous <- is.numeric(values)

        if (is_continuous) {
            list(
                type = "continuous",
                range = range(values),
                levels = unique(values)  # Store all unique values for continuous attributes
            )
        } else {
            list(
                type = "categorical",
                levels = if (is.factor(values)) levels(values) else unique(values)
            )
        }
    })
    names(attr_info) <- names(attrs)
    return(attr_info)
}

# Helper function to validate attribute names
check_attribute_names <- function(means, attr_info) {
    # Check for missing attributes
    missing_attrs <- setdiff(names(attr_info), names(means))
    if (length(missing_attrs) > 0) {
        stop("Missing prior specifications for attributes: ",
             paste(missing_attrs, collapse = ", "))
    }

    # Check for extra attributes
    extra_attrs <- setdiff(names(means), names(attr_info))
    if (length(extra_attrs) > 0) {
        stop("Prior specifications provided for non-existent attributes: ",
             paste(extra_attrs, collapse = ", "))
    }
}

# Helper function to prepare profiles for model creation
prepare_profiles_for_model <- function(profiles, means, attr_info) {
    model_data <- profiles

    # Process each attribute
    for (attr in names(attr_info)) {
        if (attr_info[[attr]]$type == "categorical") {
            # Get reference level based on provided priors
            if (!is.null(names(means[[attr]]))) {
                # Named vector case
                all_levels <- attr_info[[attr]]$levels
                coef_levels <- names(means[[attr]])
                ref_level <- setdiff(all_levels, coef_levels)[1]
                levels_order <- c(ref_level, coef_levels)
            } else {
                # Unnamed vector case
                levels_order <- attr_info[[attr]]$levels
            }

            # Convert to factor with specified level order
            model_data[[attr]] <- factor(
                profiles[[attr]],
                levels = levels_order
            )
        }
    }

    return(model_data)
}

# Modified from {logitr}
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

# Modified from {logitr}
get_parIDs <- function(parSetup) {
    return(list(
        f  = which(parSetup == "f"),
        r  = which(parSetup != "f"),
        n  = which(parSetup == "n"),
        ln = which(parSetup == "ln"),
        cn = which(parSetup == "cn")
    ))
}

# Copied from {logitr}
makeBetaDraws <- function(pars, parIDs, n_vars, standardDraws, correlation) {
    pars_mean <- pars[seq_len(n$vars)]
    pars_sd <- pars[(n$vars + 1):n$pars]
    # First scale the draws according to the covariance matrix
    if (correlation) {
        lowerMat <- matrix(0, n$parsRandom, n$parsRandom)
        lowerMat[lower.tri(lowerMat, diag = TRUE)] <- pars_sd
    } else {
        lowerMat <- diag(pars_sd, ncol = length(pars_sd))
    }
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

getStandardDraws <- function(parIDs, numDraws, drawType) {
    numBetas <- length(parIDs$f) + length(parIDs$r)
    if (drawType == 'sobol') {
        draws <- as.matrix(randtoolbox::sobol(numDraws, numBetas, normal = TRUE))
    } else {
        draws <- as.matrix(randtoolbox::halton(numDraws, numBetas, normal = TRUE))
    }
    draws[, parIDs$f] <- 0 * draws[, parIDs$f]
    return(draws)
}

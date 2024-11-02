#' Create prior specifications for CBC models
#'
#' Creates a standardized prior specification object for use in CBC analysis
#' functions like cbc_choices() and cbc_d_error(). Supports both named and unnamed
#' vectors for categorical attributes, where named vectors explicitly map levels
#' to coefficients.
#'
#' @param profiles A data frame of profiles created by cbc_profiles()
#' @param ... Named arguments for each parameter's priors. For continuous variables,
#'   provide a single value. For categorical variables, provide either:
#'   - An unnamed vector of values one less than the number of levels (dummy coding)
#'   - A named vector mapping specific levels to coefficients (remaining level becomes reference)
#' @param sd Optional named list of standard deviations for random parameters
#' @param correlation Optional correlation matrix for random parameters
#' @param distribution Optional named vector specifying distribution type for random
#'   parameters ("normal" or "lognormal")
#' @return A structured prior specification object including a logitr model object
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
#'   type = c(0.2, 0.3),          # Dummy-coded categorical
#'   freshness = c(0.4, 0.8)      # Dummy-coded categorical
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
#' # Example 3: Mixed approach with random parameters
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.5,
#'   type = c("Fuji" = 0.2, "Gala" = 0.3),
#'   freshness = c(0.4, 0.8),
#'   sd = list(
#'     price = 0.4,
#'     type = c(0.4, 0.4)
#'   )
#' )
cbc_priors <- function(
    profiles,
    ...,
    sd = NULL,
    correlation = NULL,
    distribution = NULL,
    n_draws = 100
) {
    # Validate profiles input
    if (!inherits(profiles, "data.frame") || !"profileID" %in% names(profiles)) {
        stop("'profiles' must be a data frame created by cbc_profiles()")
    }

    # Get attribute information from profiles
    attr_info <- get_attribute_info(profiles)

    # Capture the means
    means <- list(...)

    # Validate attribute names
    check_attribute_names(means, attr_info)

    # Process and validate each mean parameter
    means <- process_mean_parameters(means, attr_info)

    # Validate sd if provided
    if (!is.null(sd)) {
        check_sd_specification(sd, means, attr_info)
    }

    # Create model specification
    model <- create_model_spec(profiles, means, sd, n_draws, attr_info)

    # Create the prior specification object
    prior_spec <- list(
        means = means,
        sd = sd,
        correlation = correlation,
        distribution = distribution,
        attr_info = attr_info,
        model = model
    )

    # Add class for potential method dispatch
    class(prior_spec) <- c("cbc_priors", "list")

    return(prior_spec)
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

# Helper function to create model specification
create_model_spec <- function(profiles, means, sd, n_draws, attr_info) {
    # Create properly formatted data for recodeData
    model_data <- prepare_profiles_for_model(profiles, means, attr_info)

    # Set up random parameters if any
    randPars <- NULL
    if (!is.null(sd)) {
        randPars <- get_random_parameters(sd)
    }

    # Get parameter names including interactions
    parNames <- names(means)

    # Recode data with proper factor levels
    codedData <- logitr::recodeData(model_data, parNames, randPars)

    # Define parameter setup
    parSetup <- get_parSetup(codedData$pars, codedData$randPars)
    parIDs <- get_parIDs(parSetup)

    # Get coefficients in proper order
    coefs <- get_model_coefs(means, sd, codedData$pars, codedData$randPars)

    # Create model object
    model <- structure(list(
        coefficients = coefs,
        modelType = ifelse(length(randPars) > 0, "mxl", "mnl"),
        modelSpace = "pref",
        parSetup = parSetup,
        parIDs = parIDs,
        standardDraws = getStandardDraws(parIDs, n_draws),
        data = list(
            factorLevels = codedData$factorLevels,
            X = codedData$X
        ),
        n = list(
            vars = length(parSetup),
            parsFixed = length(which(parSetup == "f")),
            parsRandom = length(which(parSetup != "f")),
            draws = n_draws,
            pars = length(coefs),
            multiStarts = 1
        ),
        inputs = list(
            pars = parNames,
            price = NULL,
            randPars = randPars,
            numDraws = n_draws,
            numMultiStarts = 1,
            correlation = FALSE
        )
    ), class = "logitr")

    return(model)
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

# Helper function to get random parameters specification
get_random_parameters <- function(sd) {
    randPars <- list()
    for (param in names(sd)) {
        randPars[[param]] <- "n"  # Default to normal distribution
    }
    return(randPars)
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

# Modified from {logitr}
getStandardDraws <- function(parIDs, numDraws) {
    numBetas <- length(parIDs$f) + length(parIDs$r)
    draws <- as.matrix(randtoolbox::halton(numDraws, numBetas, normal = TRUE))
    draws[, parIDs$f] <- 0 * draws[, parIDs$f]
    return(draws)
}

# Helper function to handle coefficient extraction with prior structure
# created using cbc_priors()
get_model_coefs <- function(means, sds, parNamesCoded, randParsCoded) {
    # Get names of random parameters
    parNamesRand <- if (!is.null(sds)) names(sds) else character(0)
    parNamesRandCoded <- names(randParsCoded)

    # Handle fixed parameters
    parsFixed <- unlist(means[!names(means) %in% parNamesRand])
    if (!is.null(parsFixed)) {
        names(parsFixed) <- parNamesCoded[!parNamesCoded %in% parNamesRandCoded]
    }

    # If no random parameters, return fixed parameters
    if (length(parNamesRand) == 0) {
        return(parsFixed)
    }

    # Handle random parameters
    parsRand_mean <- unlist(lapply(means[parNamesRand], function(x) {
        if (is.numeric(x) && !is.null(names(x))) {
            return(x)  # Named vector for categorical
        } else {
            return(as.numeric(x))  # Single value or unnamed vector
        }
    }))
    names(parsRand_mean) <- parNamesRandCoded

    # Handle standard deviations
    parsRand_sd <- unlist(lapply(sds[parNamesRand], function(x) {
        if (is.numeric(x) && !is.null(names(x))) {
            return(x)  # Named vector for categorical
        } else {
            return(as.numeric(x))  # Single value or unnamed vector
        }
    }))
    names(parsRand_sd) <- paste0("sd_", parNamesRandCoded)

    # Combine and order coefficients
    coefs <- c(parsFixed, parsRand_mean)
    coefs <- coefs[parNamesCoded]
    coefs <- c(coefs, parsRand_sd)

    return(coefs)
}

# Helper function to process and validate mean parameters
process_mean_parameters <- function(means, attr_info) {
  result <- lapply(names(means), function(attr) {
    value <- means[[attr]]
    info <- attr_info[[attr]]

    if (info$type == "continuous") {
      if (!is.numeric(value) || length(value) != 1) {
        stop("Prior for continuous attribute '", attr, "' must be a single numeric value")
      }
      return(value)
    } else {
      # For categorical attributes
      if (is.null(names(value))) {
        # Unnamed vector - validate length
        if (length(value) != length(info$levels) - 1) {
          stop("Prior for categorical attribute '", attr, "' must have ",
               length(info$levels) - 1, " values (one less than number of levels)")
        }
        # Add names based on non-reference levels
        names(value) <- info$levels[-1]
      } else {
        # Named vector - validate names
        invalid_levels <- setdiff(names(value), info$levels)
        if (length(invalid_levels) > 0) {
          stop("Invalid levels specified for attribute '", attr, "': ",
               paste(invalid_levels, collapse = ", "))
        }
      }
      return(value)
    }
  })

  # Add the attribute names to the result list
  names(result) <- names(means)
  return(result)
}

# Helper function to validate sd specification
check_sd_specification <- function(sd, means, attr_info) {
  if (!is.list(sd)) {
    stop("sd must be a named list")
  }

  # Check that all sd parameters correspond to existing means
  invalid_sds <- setdiff(names(sd), names(means))
  if (length(invalid_sds) > 0) {
    stop("SD specified for non-existent parameters: ",
         paste(invalid_sds, collapse = ", "))
  }

  # Check lengths match for each parameter
  for (param in names(sd)) {
    if (attr_info[[param]]$type == "continuous") {
      if (length(sd[[param]]) != 1) {
        stop("SD for continuous attribute '", param, "' must be a single value")
      }
    } else {
      if (length(sd[[param]]) != length(means[[param]])) {
        stop("SD for categorical attribute '", param,
             "' must match length of means specification")
      }
    }
  }
}

#' Display attribute levels and dummy coding for a CBC design
#'
#' Shows how categorical variables will be dummy coded and what each coefficient
#' represents in the utility function.
#'
#' @param design A data frame containing a choice experiment design created
#'   by `cbc_design()`
#' @param exclude Optional character vector of attribute names to exclude
#' @return Invisibly returns a list containing the coding information, but primarily
#'   prints formatted information to the console
#' @export
#' @examples
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price     = seq(1, 5, 0.5),
#'   type      = c('Fuji', 'Gala', 'Honeycrisp'),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Generate design
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_resp = 100,
#'   n_alts = 3,
#'   n_q = 6
#' )
#'
#' # View attribute levels and coding
#' cbc_levels(design)
cbc_levels <- function(design, exclude = NULL) {
  # Get attribute columns (excluding metadata)
  attr_cols <- get_var_names(design)

  if (!is.null(exclude)) {
    attr_cols <- setdiff(attr_cols, exclude)
  }

  # Process each attribute
  attr_info <- list()

  cat("CBC Design Attribute Information:\n")
  cat("===============================\n\n")

  for (attr in attr_cols) {
    values <- design[[attr]]
    if (is.numeric(values)) {
      # Continuous variable
      cat(sprintf("%-12s: Continuous variable\n", attr))
      cat(sprintf("              Range: %.2f to %.2f\n",
                  min(values), max(values)))
      cat("              Coefficient represents effect of one-unit change\n\n")

      attr_info[[attr]] <- list(
        type = "continuous",
        range = range(values)
      )

    } else {
      # Categorical variable
      levels <- unique(sort(as.character(values)))
      n_levels <- length(levels)
      base_level <- levels[1]
      coded_levels <- levels[-1]

      cat(sprintf("%-12s: Categorical variable (%d levels)\n", attr, n_levels))
      cat("              Base level:", base_level, "\n")
      for (i in seq_along(coded_levels)) {
        cat(sprintf("              β%-2d: %s\n",
                    i, coded_levels[i]))
      }
      cat("\n")

      attr_info[[attr]] <- list(
        type = "categorical",
        base_level = base_level,
        coded_levels = coded_levels
      )
    }
  }

  # Example prior specification
  cat("Example prior specification:\n")
  cat("------------------------\n")
  cat("priors <- cbc_priors(\n")

  for (attr in attr_cols) {
    if (attr_info[[attr]]$type == "continuous") {
      cat(sprintf("    %-12s = 0,  # Effect of one-unit change\n", attr))
    } else {
      coefs <- rep("0", length(attr_info[[attr]]$coded_levels))
      cat(sprintf("    %-12s = c(%s),  # vs %s\n",
                  attr,
                  paste(coefs, collapse = ", "),
                  attr_info[[attr]]$base_level))
    }
  }

  cat("    # Add sd = list(...) for random parameters\n")
  cat(")\n")

  invisible(attr_info)
}

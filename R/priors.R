#' Create prior specifications for CBC models
#'
#' Creates a standardized prior specification object for use in CBC analysis
#' functions like `cbc_choices()` and `cbc_design()`. Supports both fixed and random
#' parameters, with flexible specification of categorical variable levels.
#'
#' @param profiles A data frame of profiles created by `cbc_profiles()`
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
#'   type = c(0.2, 0.3),      # Dummy-coded categorical (Honeycrisp reference)
#'   freshness = c(0.4, 0.8)  # Dummy-coded categorical (Poor reference)
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
#'   type = rand_spec(  # Random parameter - normal distribution
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
#'   price = rand_spec(
#'     dist = "ln",    # Log-normal distribution for price
#'     mean = -0.5,    # Single value for continuous variable
#'     sd = 0.4
#'   ),
#'   type = c(0.2, 0.3),
#'   freshness = c(0.4, 0.8)
#' )
#'
#' # Example 5: Random parameters with correlations
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = rand_spec(
#'     dist = "ln",
#'     mean = -0.5,
#'     sd = 0.4,
#'     correlations = list(
#'       # Correlate with all type levels
#'       cor_spec(with = "type", value = 0.3),
#'       # Correlate with specific level
#'       cor_spec(with = "type", level = "Fuji", value = 0.4)
#'     )
#'   ),
#'   type = rand_spec(
#'     dist = "n",
#'     mean = c("Fuji" = 0.2, "Gala" = 0.3),
#'     sd = c("Fuji" = 0.4, "Gala" = 0.4),
#'     correlations = list(
#'       # Correlate between levels
#'       cor_spec(
#'         with = "type",
#'         level = "Fuji",
#'         with_level = "Gala",
#'         value = 0.5
#'       )
#'     )
#'   ),
#'   freshness = c(0.4, 0.8)
#' )
cbc_priors <- function(
  profiles,
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
  attrs <- get_combined_attr_info(profiles, params)

  # Process parameters into standardized format
  processed_params <- process_parameters(attrs)

  # Separate fixed and random parameters
  fixed_params <- processed_params$fixed
  random_params <- processed_params$random

  # Set up random parameters specification for logitr
  randPars <- lapply(random_params, function(x) x$dist)

  # Get parameter names
  parNames <- c(names(fixed_params), names(random_params))

  # Create coded data structure
  means <- c(fixed_params, lapply(random_params, function(x) x$mean))
  model_data <- prepare_profiles_for_model(profiles, means, attrs)
  codedData <- logitr::recodeData(model_data, parNames, randPars)
  codedParNames <- codedData$pars
  pars_mean <- c(
    unlist(fixed_params),
    unlist(lapply(random_params, function(x) x$mean))
  )
  names(pars_mean) <- codedParNames

  # Generate parameter draws if we have random parameters
  if (length(random_params) > 0) {

    # Build correlation matrix from specifications
    cor_mat <- build_correlation_matrix(attrs, random_params)

    # Set up parameter structure
    parSetup <- get_parSetup(codedParNames, codedData$randPars)
    parIDs <- get_parIDs(parSetup)

    # Combine all parameters for draws
    pars_sd <- unlist(lapply(random_params, function(x) x$sd))

    names(pars_sd) <- names(codedData$randPars)

    # Set up n list for makeBetaDraws
    n <- list(
      vars = length(parSetup),
      parsFixed = length(which(parSetup == "f")),
      parsRandom = length(which(parSetup != "f")),
      draws = n_draws,
      pars = length(pars_mean) + length(pars_sd)
    )

    # Generate parameter draws
    standardDraws <- getStandardDraws(parIDs, n$draws, draw_type)
    par_draws <- makeBetaDraws(
      pars_mean, pars_sd, parIDs, n, standardDraws, cor_mat
    )
  } else {
    par_draws <- NULL
    cor_mat <- NULL
  }

  # Store profiles reference and metadata for validation
  profiles_metadata <- list(
    attribute_info = attr(profiles, "attribute_info"),
    n_profiles = nrow(profiles),
    profile_hash = digest_profiles(profiles)
  )

  # Create return object
  result <- list(
    profiles_metadata = profiles_metadata,  # For validation
    attrs = attrs,
    pars = pars_mean,
    correlation = cor_mat,
    par_draws = par_draws,
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
#' @examples
#' # For a continuous parameter
#' rand_spec(dist = "ln", mean = -0.5, sd = 0.4)
#'
#' # For a categorical parameter with correlations
#' rand_spec(
#'   dist = "n",
#'   mean = c("Fuji" = 0.2, "Gala" = 0.3),
#'   sd = c("Fuji" = 0.4, "Gala" = 0.4),
#'   correlations = list(
#'     cor_spec(with = "price", value = 0.3),
#'     cor_spec(with = "type", level = "Fuji", with_level = "Gala", value = 0.5)
#'   )
#' )
rand_spec <- function(dist = "n", mean, sd, correlations = NULL) {
  if (!dist %in% c("n", "ln", "cn")) {
    stop('dist must be one of "n" (normal), "ln" (log-normal), or "cn" (censored normal)')
  }

  if (!is.null(correlations)) {
    if (!is.list(correlations)) {
      stop("correlations must be a list of correlation specifications created by cor_spec()")
    }
    if (!all(sapply(correlations, inherits, "cbc_correlation"))) {
      stop("all correlations must be created using cor_spec()")
    }
  }

  structure(
    list(
      dist = dist,
      mean = mean,
      sd = sd,
      correlations = correlations
    ),
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
#' @examples
#' # Correlate with entire attribute
#' cor_spec(with = "price", value = 0.3)
#'
#' # Correlate with specific level
#' cor_spec(with = "type", level = "Fuji", value = 0.3)
#'
#' # Correlate between levels
#' cor_spec(with = "type", level = "Fuji", with_level = "Gala", value = 0.5)
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

# Helper function to build correlation matrix from random parameter specifications
build_correlation_matrix <- function(attrs, random_params) {
  # Get list of all random parameters and their levels
  par_names <- c()
  for (attr in names(random_params)) {
    if (attrs[[attr]]$continuous) {
      par_names <- c(par_names, attr)
    } else {
      # For categorical variables, add a name for each coefficient
      categorical_names <- names(attrs[[attr]]$mean)
      par_names <- c(par_names, paste0(attr, ".", categorical_names))
    }
  }

  # Initialize correlation matrix
  n_pars <- length(par_names)
  cor_mat <- diag(n_pars)
  rownames(cor_mat) <- colnames(cor_mat) <- par_names

  # Process correlations from each random parameter
  for (attr1 in names(random_params)) {
    param <- random_params[[attr1]]
    if (!is.null(param$correlations)) {
      for (cor in param$correlations) {
        attr2 <- cor$attr

        # Validate that correlated attribute exists and is random
        if (!(attr2 %in% names(random_params))) {
          stop(sprintf(
            "Cannot correlate with '%s' - it must be a random parameter specified using rand_spec()",
            attr2
          ))
        }

        # Handle different correlation specifications
        if (attrs[[attr1]]$continuous) {
          if (!is.null(cor$level)) {
            # Validate level exists
            if (!cor$level %in% names(attrs[[attr2]]$mean)) {
              stop(sprintf(
                "Invalid level '%s' for attribute '%s'",
                cor$level, attr2
              ))
            }
            # Continuous with specific level of categorical
            rows <- attr1
            cols <- paste0(attr2, ".", cor$level)
          } else {
            if (attrs[[attr2]]$continuous) {
              # Continuous with continuous
              rows <- attr1
              cols <- attr2
            } else {
              # Continuous with all levels of categorical
              rows <- attr1
              cols <- paste0(attr2, ".", names(attrs[[attr2]]$mean))
            }
          }
        } else {
          if (!is.null(cor$with_level)) {
            # Validate levels exist
            if (!cor$level %in% names(attrs[[attr1]]$mean)) {
              stop(sprintf(
                "Invalid level '%s' for attribute '%s'",
                cor$level, attr1
              ))
            }
            if (!cor$with_level %in% names(attrs[[attr2]]$mean)) {
              stop(sprintf(
                "Invalid level '%s' for attribute '%s'",
                cor$with_level, attr2
              ))
            }
            # Between specific levels
            rows <- paste0(attr1, ".", cor$level)
            cols <- paste0(attr2, ".", cor$with_level)
          } else if (!is.null(cor$level)) {
            if (!cor$level %in% names(attrs[[attr1]]$mean)) {
              stop(sprintf(
                "Invalid level '%s' for attribute '%s'",
                cor$level, attr1
              ))
            }
            if (attrs[[attr2]]$continuous) {
              # Between level and continuous variable
              rows <- paste0(attr1, ".", cor$level)
              cols <- attr2
            } else {
              # Between level and all levels of other categorical
              rows <- paste0(attr1, ".", cor$level)
              cols <- paste0(attr2, ".", names(attrs[[attr2]]$mean))
            }
          } else {
            if (attrs[[attr2]]$continuous) {
              # Between all levels and continuous variable
              rows <- paste0(attr1, ".", names(attrs[[attr1]]$mean))
              cols <- attr2
            } else {
              # Between all levels of both categoricals
              rows <- paste0(attr1, ".", names(attrs[[attr1]]$mean))
              cols <- paste0(attr2, ".", names(attrs[[attr2]]$mean))
            }
          }
        }

        # Set correlations for all combinations
        for (row in rows) {
          for (col in cols) {
            # Verify row and column exist in matrix
            if (!row %in% rownames(cor_mat)) {
              stop(sprintf("Parameter '%s' not found in correlation matrix", row))
            }
            if (!col %in% colnames(cor_mat)) {
              stop(sprintf("Parameter '%s' not found in correlation matrix", col))
            }

            # Don't set correlation with self
            if (row != col) {
              cor_mat[row, col] <- cor$value
              cor_mat[col, row] <- cor$value
            }
          }
        }
      }
    }
  }

  # Validate correlation matrix is positive definite
  if (!all(eigen(cor_mat)$values > 0)) {
    stop("Specified correlations result in an invalid correlation matrix. Try specifying fewer or smaller correlations.")
  }

  return(cor_mat)
}

# Helper function to combine attribute info with parameter specifications
get_combined_attr_info <- function(profiles, params) {
  # Remove profileID column
  profile_attrs <- profiles[, -which(names(profiles) == "profileID")]

  # Validate all attributes are specified
  missing_attrs <- setdiff(names(profile_attrs), names(params))
  if (length(missing_attrs) > 0) {
    stop("Missing prior specifications for attributes: ",
         paste(missing_attrs, collapse = ", "))
  }

  # Check for extra attributes
  extra_attrs <- setdiff(names(params), names(profile_attrs))
  if (length(extra_attrs) > 0) {
    stop("Prior specifications provided for non-existent attributes: ",
         paste(extra_attrs, collapse = ", "))
  }

  # Create combined information for each attribute
  attrs <- lapply(names(profile_attrs), function(attr) {
    values <- profile_attrs[[attr]]
    param <- params[[attr]]

    # Get all levels from profiles
    all_levels <- if (is.numeric(values)) {
      sort(unique(values))
    } else {
      if (is.factor(values)) levels(values) else unique(values)
    }

    # Base structure
    info <- list(
      continuous = is.numeric(values),
      levels = all_levels,
      random = inherits(param, "cbc_random_par")
    )

    # Add range for continuous variables
    if (info$continuous) {
      info$range <- range(values)
    }

    # Validate and process parameters
    if (info$random) {
      info$dist <- param$dist
      info$correlations <- param$correlations  # Preserve correlations
      if (!info$continuous) {
        if (!is.null(names(param$mean))) {
          # Validate named vector levels
          invalid_levels <- setdiff(names(param$mean), all_levels)
          if (length(invalid_levels) > 0) {
            stop(sprintf(
              "Invalid level(s) provided for '%s': %s\nValid levels are: %s",
              attr,
              paste(invalid_levels, collapse = ", "),
              paste(all_levels, collapse = ", ")
            ))
          }
          # Validate that sd has same names as mean
          if (!identical(names(param$mean), names(param$sd))) {
            stop(sprintf(
              "Names for mean and sd must match for random parameter '%s'",
              attr
            ))
          }
          info$mean <- param$mean
          info$sd <- param$sd
        } else {
          # Unnamed vector case - assign names based on remaining levels
          remaining_levels <- all_levels[-1]  # All but first level
          if (length(param$mean) != length(remaining_levels)) {
            stop(sprintf(
              "Incorrect number of values for '%s'. Expected %d values (one less than the number of levels)",
              attr, length(remaining_levels)
            ))
          }
          names(param$mean) <- remaining_levels
          names(param$sd) <- remaining_levels
          info$mean <- param$mean
          info$sd <- param$sd
        }
      } else {
        info$mean <- param$mean
        info$sd <- param$sd
      }
    } else {
      # Fixed parameters
      if (!info$continuous) {
        if (!is.null(names(param))) {
          # Validate named vector levels
          invalid_levels <- setdiff(names(param), all_levels)
          if (length(invalid_levels) > 0) {
            stop(sprintf(
              "Invalid level(s) provided for '%s': %s\nValid levels are: %s",
              attr,
              paste(invalid_levels, collapse = ", "),
              paste(all_levels, collapse = ", ")
            ))
          }
          info$mean <- param
        } else {
          # Unnamed vector case - assign names based on remaining levels
          remaining_levels <- all_levels[-1]  # All but first level
          if (length(param) != length(remaining_levels)) {
            stop(sprintf(
              "Incorrect number of values for '%s'. Expected %d values (one less than the number of levels)",
              attr, length(remaining_levels)
            ))
          }
          names(param) <- remaining_levels
          info$mean <- param
        }
      } else {
        info$mean <- param
      }
    }

    return(info)
  })
  names(attrs) <- names(profile_attrs)
  return(attrs)
}

# Helper function to process parameters based on combined attribute info
process_parameters <- function(attrs) {
  fixed <- list()
  random <- list()

  for (attr in names(attrs)) {
    info <- attrs[[attr]]

    if (info$random) {
      random[[attr]] <- list(
        dist = info$dist,
        mean = info$mean,
        sd = info$sd,
        correlations = info$correlations  # Preserve correlations
      )
    } else {
      fixed[[attr]] <- info$mean
    }
  }

  return(list(fixed = fixed, random = random))
}

# Helper function to prepare profiles for model creation
prepare_profiles_for_model <- function(profiles, means, attrs) {
  model_data <- profiles

  # Process each attribute
  for (attr in names(attrs)) {
    if (!attrs[[attr]]$continuous) {
      # Get reference level based on provided priors
      if (!is.null(names(means[[attr]]))) {
        # Named vector case
        all_levels <- attrs[[attr]]$levels
        coef_levels <- names(means[[attr]])
        ref_level <- setdiff(all_levels, coef_levels)[1]
        levels_order <- c(ref_level, coef_levels)
      } else {
        # Unnamed vector case
        levels_order <- attrs[[attr]]$levels
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

# Returns shifted normal draws for each parameter
makeBetaDraws <- function(
    pars_mean, pars_sd, parIDs, n, standardDraws, cor_mat
) {

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

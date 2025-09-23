#' Simulate choices for a survey design
#'
#' Simulate choices for a survey design, either randomly or according to a
#' utility model defined by user-provided prior parameters. When priors are
#' provided, choices are simulated using the same probability computation
#' framework as used in cbc_design() for consistency.
#'
#' @param design A `cbc_design` object created by `cbc_design()`
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or `NULL`
#'   (default) for random choices.

#' @return Returns the input design with an additional `choice` column
#'   identifying the simulated choices.
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
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # Simulate random choices (default)
#' choices_random <- cbc_choices(design)
#'
#' # Create priors and simulate utility-based choices
#' priors <- cbc_priors(
#'   profiles = profiles,
#'   price = -0.1,
#'   type = c(0.5, 0.2),  # vs reference level
#'   quality = 0.3
#' )
#'
#' choices_utility <- cbc_choices(design, priors = priors)
cbc_choices <- function(design, priors = NULL) {
    # Validate input
    if (!inherits(design, "cbc_design")) {
        stop("design must be a cbc_design object created by cbc_design()")
    }

    if (is.null(priors)) {
        # Simulate random choices
        result <- simulate_random_choices(design)
        simulation_method <- "random"
        priors_used <- FALSE
    } else {
        # Validate priors
        if (!inherits(priors, "cbc_priors")) {
            stop("priors must be a cbc_priors object created by cbc_priors()")
        }

        # Check if different priors were used in design optimization
        check_priors_consistency(priors, design)

        # Simulate utility-based choices
        result <- simulate_utility_based_choices(design, priors)
        simulation_method <- "utility_based"
        priors_used <- TRUE
    }

    # Preserve encoding attributes
    attr(result, "is_dummy_coded") <- attr(design, "is_dummy_coded")
    attr(result, "categorical_structure") <- attr(
        design,
        "categorical_structure"
    )

    # Add choice simulation metadata
    design_params <- attr(design, "design_params")
    attr(result, "choice_info") <- list(
        simulation_method = simulation_method,
        d_error = design_params$d_error_prior %||%
            design_params$d_error_null %||%
            NA,
        n_respondents = if ("respID" %in% names(result)) {
            max(result$respID, na.rm = TRUE)
        } else {
            1
        },
        priors_used = priors_used,
        simulated_at = Sys.time()
    )

    class(result) <- c("cbc_choices", "data.frame")
    return(result)
}

# Helper functions ----

# Simulate random choices
simulate_random_choices <- function(design) {
    choices <- rep(0, nrow(design))

    # Get unique observation IDs
    unique_obs <- unique(design$obsID)

    for (obs in unique_obs) {
        obs_rows <- which(design$obsID == obs)
        # Randomly select one alternative
        chosen_row <- sample(obs_rows, 1)
        choices[chosen_row] <- 1
    }

    design$choice <- choices
    return(design)
}

# Check if different priors were used in design vs choice simulation
check_priors_consistency <- function(choice_priors, design) {
    design_priors <- attr(design, "priors")

    if (!is.null(design_priors)) {
        # Simple check: compare parameter vectors
        if (!identical(choice_priors$pars, design_priors$pars)) {
            warning(
                "Different priors used for choice simulation than for design optimization. ",
                "This may not be the intended behavior. Consider using the same priors ",
                "for both design creation and choice simulation.",
                call. = FALSE
            )
        }

        # Also check parameter draws if both have them
        if (
            !is.null(choice_priors$par_draws) &&
                !is.null(design_priors$par_draws)
        ) {
            if (!identical(choice_priors$par_draws, design_priors$par_draws)) {
                warning(
                    "Different parameter draws used for choice simulation than for design optimization.",
                    call. = FALSE
                )
            }
        }
    }
}

# Simulate utility-based choices using design infrastructure
simulate_utility_based_choices <- function(design, priors) {
    # Extract information from design object
    design_params <- attr(design, "design_params")
    profiles <- attr(design, "profiles")

    if (is.null(profiles)) {
        stop(
            "Design object missing required profile information for choice simulation"
        )
    }

    # Create optimization environment using the existing function
    opt_env <- setup_optimization_environment(
        profiles = profiles,
        method = "random", # Hard-code this so that the obsID vectors are correct
        time_start = Sys.time(), # Not important for choice simulation
        n_alts = design_params$n_alts,
        n_q = design_params$n_q,
        n_resp = design_params$n_resp,
        n_blocks = design_params$n_blocks,
        n_cores = 1, # Not used for choice simulation
        n_start = 1, # Not used for choice simulation
        max_iter = 1, # Not used for choice simulation
        priors = priors, # The new priors for choice simulation
        no_choice = design_params$no_choice,
        label = design_params$label,
        balance_by = NULL, # Not used for choice simulation
        remove_dominant = FALSE, # Not needed for choice simulation
        dominance_types = NULL, # Not needed for choice simulation
        dominance_threshold = 0.8, # Not needed for choice simulation
        max_dominance_attempts = 1, # Not needed for choice simulation
        randomize_questions = TRUE, # Not used for choice simulation
        randomize_alts = TRUE, # Not used for choice simulation
        include_probs = FALSE, # Not used for choice simulation
        use_idefix = FALSE # Not used for choice simulation
    )

    # Get design matrix from the design object
    design_matrix <- get_design_matrix_from_design_object(design, opt_env)

    # Compute probabilities using existing functions
    probs <- get_probs(design_matrix, opt_env)

    # Handle Bayesian case
    if (opt_env$is_bayesian) {
        # probs is a matrix of probability draws, use average probabilities
        probs <- rowMeans(probs)
    }

    # Simulate choices based on probabilities
    design$choice <- simulate_choices_from_probabilities(probs, opt_env$obsID)

    return(design)
}

# Get design matrix from design object (use stored matrix if available)
get_design_matrix_from_design_object <- function(design, opt_env) {
    # Get the regular profiles (excluding no-choice if present)
    regular_design <- design
    if (opt_env$no_choice) {
        regular_design <- design[design$profileID != 0, ]
    }

    # Determine matrix dimensions
    n_questions <- max(regular_design$obsID)
    n_alts <- opt_env$n$alts

    # Initialize design matrix
    design_matrix <- matrix(0, nrow = n_questions, ncol = n_alts)

    # Fill matrix from profileID data
    for (obs in 1:n_questions) {
        obs_rows <- regular_design[regular_design$obsID == obs, ]
        obs_rows <- obs_rows[order(obs_rows$altID), ] # Ensure proper order

        if (nrow(obs_rows) == n_alts) {
            design_matrix[obs, ] <- obs_rows$profileID
        } else {
            stop(sprintf(
                "Inconsistent number of alternatives in observation %d",
                obs
            ))
        }
    }

    return(design_matrix)
}

# Simulate choices from computed probabilities
simulate_choices_from_probabilities <- function(probs, obsID) {
    choices <- rep(0, length(probs))

    # Group by observation and simulate choice for each
    unique_obs <- unique(obsID)

    for (obs in unique_obs) {
        obs_rows <- which(obsID == obs)
        obs_probs <- probs[obs_rows]

        # Normalize probabilities (in case of numerical issues)
        obs_probs <- pmax(obs_probs, 1e-10) # Avoid zero probabilities
        obs_probs <- obs_probs / sum(obs_probs)

        # Sample one alternative based on probabilities
        chosen_alt <- sample(length(obs_probs), 1, prob = obs_probs)
        choices[obs_rows[chosen_alt]] <- 1
    }

    return(choices)
}

# Note: This implementation reuses the following functions from design.R:
# - setup_optimization_environment()
# - get_probs()
# - logit_regular()
# - logit_draws()
# - logit()
# - get_design_vector()
# - design_matrix_no_choice()
#
# And from util.R:
# - `%||%` operator

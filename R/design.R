#' Generate survey designs for choice experiments (Updated Implementation)
#'
#' This function creates experimental designs for choice-based conjoint experiments
#' using multiple design approaches including optimization and frequency-based methods.
#'
#' @param profiles A data frame of class `cbc_profiles` created using `cbc_profiles()`
#' @param method Choose the design method: "random", "shortcut", "minoverlap", "balanced", "stochastic", "modfed", or "cea". Defaults to "random"
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or NULL for random/shortcut designs
#' @param n_alts Number of alternatives per choice question
#' @param n_q Number of questions per respondent (or per block)
#' @param n_resp Number of respondents (for random/shortcut designs) or 1 (for optimized designs that get repeated)
#' @param n_blocks Number of blocks in the design. Defaults to 1
#' @param n_cores Number of cores to use for parallel processing in the design search.
#' Defaults to NULL, in which case it is set to the number of available cores minus 1.
#' @param no_choice Include a "no choice" option? Defaults to FALSE
#' @param label The name of the variable to use in a "labeled" design. Defaults to NULL
#' @param randomize_questions Randomize question order for each respondent? Defaults to TRUE (optimized methods only)
#' @param randomize_alts Randomize alternative order within questions? Defaults to TRUE (optimized methods only)
#' @param remove_dominant Remove choice sets with dominant alternatives? Defaults to FALSE
#' @param dominance_types Types of dominance to check: "total" and/or "partial"
#' @param dominance_threshold Threshold for total dominance detection. Defaults to 0.8
#' @param max_dominance_attempts Maximum attempts to replace dominant choice sets. Defaults to 50.
#' @param max_iter Maximum iterations for optimized designs. Defaults to 50
#' @param n_start Number of random starts for optimized designs. Defaults to 5
#' @param include_probs Include predicted probabilities in resulting design? Requires `priors`. Defaults to `FALSE`
#' @param use_idefix If `TRUE` (the default), the idefix package will be used to find optimal designs, which is faster.
#' Only valid with `"cea"` and `"modfed"` methods.
#'
#' @details
#' ## Design Methods
#'
#' The `method` argument determines the design approach used:
#'
#' - `"random"`: Creates designs by randomly sampling profiles for each respondent independently
#' - `"shortcut"`: Frequency-based greedy algorithm that balances attribute level usage
#' - `"minoverlap"`: Greedy algorithm that minimizes attribute overlap within choice sets
#' - `"balanced"`: Greedy algorithm that maximizes overall attribute balance across the design
#' - `"stochastic"`: Stochastic profile swapping with D-error optimization (first improvement found)
#' - `"modfed"`: Modified Fedorov algorithm with exhaustive profile swapping for D-error optimization
#' - `"cea"`: Coordinate Exchange Algorithm with attribute-by-attribute D-error optimization
#'
#' ## Method Compatibility
#'
#' The table below summarizes method compatibility with design features:
#'
#' | Method       | No choice? | Labeled designs? | Restricted profiles? | Blocking? | Interactions? | Dominance removal? |
#' |--------------|------------|------------------|---------------------|-----------|---------------|-------------------|
#' | "random"     | Yes        | Yes              | Yes                 | No        | Yes           | Yes               |
#' | "shortcut"   | Yes        | Yes              | Yes                 | No        | No            | Yes               |
#' | "minoverlap" | Yes        | Yes              | Yes                 | No        | No            | Yes               |
#' | "balanced"   | Yes        | Yes              | Yes                 | No        | No            | Yes               |
#' | "stochastic" | Yes        | Yes              | Yes                 | Yes       | Yes           | Yes               |
#' | "modfed"     | Yes        | Yes              | Yes                 | Yes       | Yes           | Yes               |
#' | "cea"        | Yes        | Yes              | No                  | Yes       | Yes           | Yes               |
#'
#' ## Design Quality Assurance
#'
#' All methods ensure the following criteria are met:
#'
#' 1. No duplicate profiles within any choice set
#' 2. No duplicate choice sets within any respondent
#' 3. If `remove_dominant = TRUE`, choice sets with dominant alternatives are eliminated (optimization methods only)
#'
#' ## Method Details
#'
#' ### Random Method
#'
#' Creates designs where each respondent sees completely independent, randomly generated choice sets.
#'
#' ### Greedy Methods (shortcut, minoverlap, balanced)
#'
#' These methods use frequency-based algorithms that make locally optimal choices:
#' - **Shortcut**: Balances attribute level usage within questions and across the overall design
#' - **Minoverlap**: Minimizes attribute overlap within choice sets while allowing some overlap for balance
#' - **Balanced**: Maximizes overall attribute balance, prioritizing level distribution over overlap reduction
#'
#' These methods provide good level balance without requiring priors or D-error calculations and offer fast execution suitable for large designs.
#'
#' ### D-Error Optimization Methods (stochastic, modfed, cea)
#'
#' These methods minimize D-error to create statistically efficient designs:
#' - **Stochastic**: Random profile sampling with first improvement acceptance
#' - **Modfed**: Exhaustive profile testing for best improvement (slower but thorough)
#' - **CEA**: Coordinate exchange testing attribute levels individually (requires full factorial profiles)
#'
#' ## idefix Integration
#'
#' When `use_idefix = TRUE` (the default), the function leverages the highly optimized
#' algorithms from the idefix package for 'cea' and 'modfed' design generation methods.
#' This can provide significant speed improvements, especially for larger
#' problems.
#'
#' Key benefits of idefix integration:
#'
#' - Faster optimization algorithms with C++ implementation
#' - Better handling of large candidate sets
#' - Optimized parallel processing
#' - Advanced blocking capabilities for multi-block designs
#'
#' @return A `cbc_design` object containing the experimental design
#' @export
cbc_design <- function(
    profiles,
    method = "random",
    priors = NULL,
    n_alts,
    n_q,
    n_resp = 100,
    n_blocks = 1,
    n_cores = NULL,
    no_choice = FALSE,
    label = NULL,
    randomize_questions = TRUE,
    randomize_alts = TRUE,
    remove_dominant = FALSE,
    dominance_types = c("total", "partial"),
    dominance_threshold = 0.8,
    max_dominance_attempts = 50,
    max_iter = 50,
    n_start = 5,
    include_probs = FALSE,
    use_idefix = TRUE
) {
    time_start <- Sys.time()

    # Override use_idefix if method doesn't support it
    if (use_idefix && !method %in% c("cea", "modfed")) {
        use_idefix <- FALSE
    }

    # Validate inputs
    validate_design_inputs(
        profiles,
        method,
        priors,
        n_alts,
        n_q,
        n_resp,
        n_blocks,
        no_choice,
        label,
        randomize_questions,
        randomize_alts,
        remove_dominant,
        dominance_types,
        dominance_threshold,
        max_dominance_attempts,
        include_probs,
        use_idefix
    )

    # Set up the optimization environment
    opt_env <- setup_optimization_environment(
        profiles,
        method,
        time_start,
        n_alts,
        n_q,
        n_resp,
        n_blocks,
        n_cores,
        n_start,
        max_iter,
        priors,
        no_choice,
        label,
        remove_dominant,
        dominance_types,
        dominance_threshold,
        max_dominance_attempts,
        randomize_questions,
        randomize_alts,
        include_probs,
        use_idefix
    )

    design_result <- generate_design(opt_env)

    # Construct the final design as a dummy-coded data frame
    final_design <- construct_final_design(
        design_result$design_matrix,
        opt_env
    )

    # Add metadata and return
    final <- finalize_design_object(
        final_design,
        design_result,
        opt_env
    )

    return(final)
}

# Set up the optimization environment with pre-computed utilities and proper factor alignment
setup_optimization_environment <- function(
    profiles,
    method,
    time_start,
    n_alts,
    n_q,
    n_resp,
    n_blocks,
    n_cores,
    n_start,
    max_iter,
    priors,
    no_choice,
    label,
    remove_dominant,
    dominance_types,
    dominance_threshold,
    max_dominance_attempts,
    randomize_questions,
    randomize_alts,
    include_probs,
    use_idefix
) {
    # Auto-generate zero priors for CEA/Modfed with idefix if none provided
    if (
        is.null(priors) &&
            use_idefix &&
            method %in% c("cea", "modfed")
    ) {
        message(
            "No priors specified for ",
            method,
            " method with idefix. Using zero priors for all parameters."
        )
        priors <- create_zero_priors(profiles, no_choice)
    }

    # Method logic vars
    method_random <- method == "random"
    method_greedy <- method %in% get_methods_greedy()
    method_optimal <- method %in% get_methods_optimal()

    # OVERRIDES

    # Override randomize_alts for labeled designs
    if (!is.null(label) && randomize_alts) {
        message(
            "Note: randomize_alts set to FALSE for labeled designs to preserve label structure"
        )
        randomize_alts <- FALSE
    }

    if (!method_optimal) {
        # Blocks only apply to D-optimal designs
        if (n_blocks != 1) {
            message(
                "For '",
                method,
                "' designs, n_blocks is ignored and set to 1"
            )
            n_blocks <- 1
        }
    }

    # Get attribute names (excluding profileID)
    attr_names <- setdiff(names(profiles), "profileID")

    # Check if we have interactions
    has_interactions <- !is.null(priors) &&
        !is.null(priors$interactions) &&
        length(priors$interactions) > 0

    # Align factor levels with priors BEFORE encoding
    profiles_aligned <- align_profiles_with_priors(profiles, priors, attr_names)

    # Get random parameters for encoding
    randPars <- get_rand_pars(priors)

    # ALWAYS create the main X_matrix (without interactions) for D-error calculation
    X_matrix <- logitr::recodeData(profiles_aligned, attr_names, randPars)$X

    # Create separate interaction-augmented matrix for utility calculations if needed
    X_matrix_utility <- X_matrix
    if (has_interactions) {
        var_names_with_interactions <- c(
            attr_names,
            build_interaction_terms(priors$interactions)
        )
        X_matrix_utility <- logitr::recodeData(
            profiles_aligned,
            var_names_with_interactions,
            randPars
        )$X
    }

    # Defaults
    is_bayesian <- FALSE
    n_draws <- NULL
    partial_utilities <- NULL
    exp_utilities_draws <- NULL
    exp_utilities <- NULL
    label_constraints <- NULL
    has_priors <- !is.null(priors)

    # Compute utilities if we have priors
    if (has_priors) {
        if (!is.null(priors$par_draws)) {
            is_bayesian <- TRUE
        }

        # Handle no_choice parameter extraction
        if (no_choice) {
            no_choice_index <- which(names(priors$pars) == "no_choice")
            exp_no_choice <- exp(priors$pars[no_choice_index])
            # Extract priors parameters (including interactions) excluding no_choice
            priors_pars <- priors$pars[-no_choice_index]
        } else {
            priors_pars <- priors$pars
        }

        # Validate parameter dimensions if we have interactions
        if (has_interactions) {
            expected_params <- ncol(X_matrix_utility)
            actual_params <- length(priors_pars)
            if (expected_params != actual_params) {
                stop(sprintf(
                    "Parameter dimension mismatch: Utility matrix expects %d parameters but priors provides %d. Check interaction specifications.",
                    expected_params,
                    actual_params
                ))
            }
        }

        # Compute utilities for each profile using the interaction-aware matrix
        if (is_bayesian) {
            # Use draws for Bayesian calculation if exist
            n_draws <- nrow(priors$par_draws)

            # Handle no_choice draws
            if (no_choice) {
                exp_no_choice_draws <- exp(priors$par_draws[, no_choice_index])
                priors_par_draws <- priors$par_draws[, -no_choice_index]
            } else {
                priors_par_draws <- priors$par_draws
            }

            # Bayesian case with parameter draws (including interactions)
            exp_utilities_draws <- array(dim = c(nrow(profiles), n_draws))

            for (i in 1:n_draws) {
                utilities_i <- X_matrix_utility %*% priors_par_draws[i, ]
                exp_utilities_draws[, i] <- exp(utilities_i)
            }
            if (no_choice) {
                exp_utilities_draws <- rbind(
                    exp_utilities_draws,
                    exp_no_choice_draws
                )
            }
        } else {
            # Fixed parameters case - use interaction-aware matrix for utilities
            utilities <- X_matrix_utility %*% priors_pars
            exp_utilities <- exp(utilities)
            if (no_choice) {
                exp_utilities <- rbind(exp_utilities, exp_no_choice)
            }
        }

        # Pre-compute partial utilities for dominance checking if needed
        if (remove_dominant && "partial" %in% dominance_types) {
            if (has_interactions) {
                # Use interaction-aware partial utilities computation
                partial_utilities <- compute_partial_utilities_with_interactions(
                    X_matrix_utility,
                    priors
                )
            } else {
                partial_utilities <- compute_partial_utilities(X_matrix, priors)
            }
        }
    } else {
        # No priors - equal probabilities
        exp_utilities <- rep(1, nrow(profiles))

        # Handle no-choice with no priors
        if (no_choice) {
            # Default no-choice utility of 0 (exp(0) = 1)
            exp_utilities <- c(exp_utilities, 1)
        }
    }

    # Set up label constraints if specified
    if (!is.null(label)) {
        label_constraints <- setup_label_constraints(profiles, label, n_alts)
    }

    # Setup n parameter
    n <- list(
        q = n_q,
        alts = n_alts,
        resp = n_resp,
        blocks = n_blocks,
        start = n_start,
        cores = set_num_cores(n_cores),
        questions = ifelse(
            method %in% get_methods_optimal(),
            n_q * n_blocks,
            n_q * n_resp
        ),
        params = ncol(X_matrix), # Use main matrix for parameter count
        draws = n_draws,
        profiles = nrow(profiles),
        max_iter = max_iter,
        max_attempts = max_dominance_attempts
    )

    # Precompute all ID vectors
    n_alts_total <- ifelse(no_choice, n_alts + 1, n_alts)
    n$alts_total <- n_alts_total
    reps <- rep(n_alts_total, n$questions)
    respID <- rep(1:n_resp, each = n_alts_total * n_q)
    obsID_partials <- rep(1:n$questions, each = n_alts)
    obsID <- rep(1:n$questions, each = n_alts_total)
    altID <- rep(1:n_alts_total, n$questions)

    return(list(
        profiles = profiles_aligned,
        priors = priors,
        method = method,
        # Store method-specific logic vars
        method_random = method_random,
        method_greedy = method_greedy,
        method_optimal = method_optimal,
        time_start = time_start,
        has_priors = has_priors,
        has_interactions = has_interactions,
        attr_names = attr_names,
        exp_utilities = exp_utilities,
        exp_utilities_draws = exp_utilities_draws,
        partial_utilities = partial_utilities,
        X_matrix = X_matrix, # Main matrix for D-error calculations
        X_matrix_utility = X_matrix_utility, # Interaction-aware matrix for utilities
        no_choice = no_choice,
        label = label,
        label_constraints = label_constraints,
        is_bayesian = is_bayesian,
        n = n,
        reps = reps,
        respID = respID,
        obsID = obsID,
        obsID_partials = obsID_partials,
        altID = altID,
        remove_dominant = remove_dominant,
        dominance_types = dominance_types,
        dominance_threshold = dominance_threshold,
        available_profile_ids = profiles$profileID,
        randomize_questions = randomize_questions,
        randomize_alts = randomize_alts,
        include_probs = include_probs,
        use_idefix = use_idefix
    ))
}

create_zero_priors <- function(profiles, no_choice = FALSE) {
    # Get attribute names (excluding profileID)
    attr_names <- setdiff(names(profiles), "profileID")

    # Create zero priors for each attribute
    zero_params <- list()

    for (attr in attr_names) {
        values <- profiles[[attr]]

        if (is.numeric(values)) {
            # Continuous attribute: single zero value
            zero_params[[attr]] <- 0
        } else {
            # Categorical attribute: zero for all non-reference levels
            unique_vals <- if (is.factor(values)) {
                levels(values)
            } else {
                unique(values)
            }
            n_levels <- length(unique_vals)

            if (n_levels > 1) {
                # Create zero vector for non-reference levels (all but first)
                zero_params[[attr]] <- rep(0, n_levels - 1)
            } else {
                # Single level - still need one parameter
                zero_params[[attr]] <- 0
            }
        }
    }

    # Add no-choice parameter if requested
    if (no_choice) {
        zero_params$no_choice <- 0
    }

    # Create the cbc_priors object using the existing function
    do.call(cbc_priors, c(list(profiles = profiles), zero_params))
}

build_interaction_terms <- function(interactions) {
    # For logitr, we only need to specify the general interaction terms
    # like "price*type". logitr will automatically create all the
    # specific level interactions like "price:typeGala"

    unique_pairs <- unique(sapply(interactions, function(int) {
        paste(sort(c(int$attr1, int$attr2)), collapse = "*")
    }))

    return(unique_pairs)
}

align_profiles_with_priors <- function(profiles, priors, attr_names) {
    if (is.null(priors)) {
        return(profiles) # No alignment needed
    }

    profiles_aligned <- profiles

    for (attr in attr_names) {
        if (attr %in% names(profiles) && attr %in% names(priors$attrs)) {
            attr_info <- priors$attrs[[attr]]

            # Only process categorical variables
            if (!attr_info$continuous) {
                values <- profiles[[attr]]

                # Determine the intended level order from priors
                if (!is.null(names(attr_info$mean))) {
                    # Named priors: use the names to determine order
                    coef_levels <- names(attr_info$mean)
                    all_unique_values <- unique(values)

                    # Reference level is the one not in the coefficient names
                    ref_level <- setdiff(all_unique_values, coef_levels)
                    if (length(ref_level) != 1) {
                        stop(sprintf(
                            "Cannot determine reference level for attribute '%s'. ",
                            "Named priors should specify all levels except one.",
                            attr
                        ))
                    }

                    # Order: reference level first, then coefficient levels
                    level_order <- c(ref_level, coef_levels)
                } else {
                    # Unnamed priors: use natural order, first level is reference
                    if (is.factor(values)) {
                        level_order <- levels(values)
                    } else {
                        level_order <- unique(values)
                    }

                    # Validate that we have the right number of priors
                    expected_coefs <- length(level_order) - 1
                    actual_coefs <- length(attr_info$mean)
                    if (expected_coefs != actual_coefs) {
                        stop(sprintf(
                            "Attribute '%s' has %d levels but %d prior values provided. ",
                            "Expected %d values (one less than number of levels).",
                            attr,
                            length(level_order),
                            actual_coefs,
                            expected_coefs
                        ))
                    }
                }

                # Set as factor with proper level order
                profiles_aligned[[attr]] <- factor(values, levels = level_order)
            }
        }
    }

    return(profiles_aligned)
}

generate_design <- function(opt_env) {
    if (opt_env$method_random) {
        return(generate_random_design(opt_env))
    } else if (opt_env$method_greedy) {
        return(generate_greedy_design(opt_env))
    } else if (opt_env$use_idefix) {
        # Try idefix first, fallback to cbcTools if it fails
        return(generate_idefix_design_with_fallback(opt_env))
    }
    return(generate_optimized_design(opt_env))
}

# Generate a random design using profileID sampling with matrix operations
generate_random_design <- function(opt_env) {
    # Start with a completely random design matrix
    design_matrix <- generate_initial_random_matrix(opt_env)

    # Iteratively fix problems
    attempts <- 0
    max_attempts <- opt_env$n$max_attempts
    while (attempts < max_attempts) {
        attempts <- attempts + 1

        # Find all problematic questions
        problem_questions <- find_problematic_questions(design_matrix, opt_env)

        if (length(problem_questions) == 0) {
            break # Design is valid
        }

        # Replace problematic questions
        for (q in problem_questions) {
            design_matrix[q, ] <- sample_question_profiles(opt_env)
        }
    }

    # Find all remaining problematic questions (if any)
    if (length(find_problematic_questions(design_matrix, opt_env)) > 0) {
        warning(sprintf(
            "Could not generate fully valid design after %d attempts",
            max_attempts
        ))
    }

    return(list(
        design_matrix = design_matrix,
        total_attempts = attempts
    ))
}

# Generate initial random design matrix
generate_initial_random_matrix <- function(
    opt_env,
    n_questions = NULL,
    n_alts = NULL
) {
    n <- opt_env$n
    if (is.null(n_questions)) {
        n_questions <- n$questions
    }
    if (is.null(n_alts)) {
        n_alts <- n$alts
    }
    design_matrix <- matrix(0, nrow = n_questions, ncol = n_alts)

    if (!is.null(opt_env$label_constraints)) {
        # Labeled design: sample one from each label group for each question
        for (q in 1:n_questions) {
            design_matrix[q, ] <- sample_labeled_profiles(opt_env)
        }
    } else {
        # Regular design: sample without replacement for each question
        for (q in 1:n_questions) {
            design_matrix[q, ] <- sample(
                opt_env$available_profile_ids,
                n$alts,
                replace = FALSE
            )
        }
    }

    return(design_matrix)
}

# Find all questions that have problems
find_problematic_questions <- function(design_matrix, opt_env) {
    n <- opt_env$n
    problematic <- logical(n$questions)

    # Check for within-question duplicates
    problematic <- check_problem_question_dupes(
        design_matrix,
        opt_env,
        problematic
    )

    # Check for duplicate questions within each respondent
    problematic <- check_problem_resp_dupes(design_matrix, opt_env, problematic)

    # Check for dominance if required
    problematic <- check_problem_dominance(design_matrix, opt_env, problematic)

    return(which(problematic))
}

check_problem_question_dupes <- function(design_matrix, opt_env, problematic) {
    for (q in 1:opt_env$n$questions) {
        question_profiles <- design_matrix[q, ]
        if (length(unique(question_profiles)) != length(question_profiles)) {
            problematic[q] <- TRUE
        }
    }
    return(problematic)
}

check_problem_resp_dupes <- function(design_matrix, opt_env, problematic) {
    n <- opt_env$n
    n_iter <- opt_env$n$blocks
    if (opt_env$method == 'random') {
        n_iter <- opt_env$n$resp
    }
    for (resp in 1:n_iter) {
        # Get question indices for this respondent
        resp_start <- (resp - 1) * n$q + 1
        resp_end <- resp * n$q
        resp_questions <- resp_start:resp_end

        # Check for duplicates within this respondent's questions
        for (i in 1:length(resp_questions)) {
            q1 <- resp_questions[i]
            if (problematic[q1]) {
                next
            } # Already marked as problematic

            current_sorted <- sort(design_matrix[q1, ])

            for (j in 1:length(resp_questions)) {
                if (i == j) {
                    next
                } # Don't compare question to itself

                q2 <- resp_questions[j]
                other_sorted <- sort(design_matrix[q2, ])

                if (identical(current_sorted, other_sorted)) {
                    problematic[q1] <- TRUE
                    break
                }
            }
        }
    }
    return(problematic)
}

check_problem_dominance <- function(design_matrix, opt_env, problematic) {
    if (opt_env$remove_dominant) {
        # Total dominance check
        if ("total" %in% opt_env$dominance_types) {
            total_bad <- get_total_bad(design_matrix, opt_env)
            problematic[total_bad] <- TRUE
        }

        # Partial dominance check
        if ("partial" %in% opt_env$dominance_types) {
            partial_bad <- get_partial_bad(design_matrix, opt_env)
            problematic[partial_bad] <- TRUE
        }
    }
    return(problematic)
}

get_total_bad <- function(design_matrix, opt_env) {
    probs <- get_probs(design_matrix, opt_env)
    if (opt_env$is_bayesian) {
        # here probs is a matrix of prob draws
        probs <- rowMeans(probs)
    }
    total_bad <- opt_env$obsID[which(probs > opt_env$dominance_threshold)]
    return(total_bad)
}

get_partial_bad <- function(design_matrix, opt_env) {
    design_vector <- get_design_vector(design_matrix)
    partials <- opt_env$partial_utilities[design_vector, ]
    partial_bad <- find_dominant_rows_by_group(partials, opt_env$obsID_partials)
    return(opt_env$obsID_partials[partial_bad])
}

# Function to find dominant rows within groups
find_dominant_rows_by_group <- function(partials_matrix, group_ids) {
    # Split row indices by group
    group_splits <- split(1:nrow(partials_matrix), group_ids)

    # Find dominant rows within each group
    dominant_indices <- c()

    for (group_rows in group_splits) {
        if (length(group_rows) > 1) {
            # Only check if group has multiple rows
            # Extract submatrix for this group
            group_matrix <- partials_matrix[group_rows, , drop = FALSE]

            # Find dominant rows within this group
            local_dominant <- find_best_rows(group_matrix)

            # Convert local indices back to global indices
            if (length(local_dominant) > 0) {
                global_dominant <- group_rows[local_dominant]
                dominant_indices <- c(dominant_indices, global_dominant)
            }
        }
    }

    return(sort(dominant_indices))
}

find_best_rows <- function(mat) {
    # Check if each row has the maximum value in every column
    row_indices <- 1:nrow(mat)
    best_rows <- row_indices[apply(mat, 1, function(row) {
        all(sapply(1:ncol(mat), function(col) row[col] == max(mat[, col])))
    })]

    return(best_rows)
}

# Sample profileIDs for a single question
sample_question_profiles <- function(opt_env) {
    n_alts <- opt_env$n$alts
    if (!is.null(opt_env$label_constraints)) {
        # Labeled design: sample one from each label group
        return(sample_labeled_profiles(opt_env))
    } else {
        # Regular design: sample without replacement from all available profiles
        return(sample(opt_env$available_profile_ids, n_alts, replace = FALSE))
    }
}

# Sample profiles for labeled design
sample_labeled_profiles <- function(opt_env) {
    label_constraints <- opt_env$label_constraints
    n_alts <- opt_env$n$alts
    if (length(label_constraints$groups) != n_alts) {
        stop("Number of alternatives must match number of label groups")
    }
    profiles <- numeric(n_alts)
    for (i in 1:n_alts) {
        group_profiles <- label_constraints$groups[[i]]
        profiles[i] <- sample(group_profiles, 1)
    }
    return(profiles)
}

# Convert profileID design matrix to full design data frame using existing encoded matrix
construct_final_design <- function(design_matrix, opt_env) {
    n <- opt_env$n
    if (opt_env$no_choice) {
        design_matrix <- design_matrix_no_choice(design_matrix, opt_env)
    }

    # Initialize design data frame
    design <- data.frame(profileID = as.vector(t(design_matrix)))
    if (!opt_env$method_optimal) {
        design <- add_metadata(design, n$resp, n$alts_total, n$q)
        id_cols <- c("profileID", "respID", "qID", "altID", "obsID")
    } else {
        design <- add_metadata_optimal(design, n$blocks, n$alts_total, n$q)
        id_cols <- c("profileID", "blockID", "qID", "altID", "obsID")
    }

    # Create lookup table from the MAIN X_matrix (without interactions for the final design)
    X_df <- as.data.frame(opt_env$X_matrix) # This is the main matrix
    X_df$profileID <- opt_env$profiles$profileID

    # Handle no-choice option if present
    if (opt_env$no_choice) {
        # Add no-choice row (all parameters = 0)
        no_choice_row <- as.data.frame(matrix(
            0,
            nrow = 1,
            ncol = ncol(opt_env$X_matrix)
        ))
        names(no_choice_row) <- names(X_df)[1:ncol(opt_env$X_matrix)]
        no_choice_row$profileID <- n$profiles + 1
        no_choice_row$no_choice <- 1 # Add no_choice indicator

        # Add no_choice column to regular profiles (set to 0)
        X_df$no_choice <- 0

        # Combine
        X_df <- rbind(X_df, no_choice_row)
    }

    # Join design with encoded attributes (main effects only, no interactions in final design)
    design <- merge(design, X_df, by = "profileID", sort = FALSE)

    # Reorder columns and rows
    attr_cols <- setdiff(names(design), id_cols)
    design <- design[, c(id_cols, attr_cols)]
    design <- design[order(design$obsID, design$altID), ]
    row.names(design) <- NULL

    # Compute and add probabilities if desired
    if (opt_env$include_probs) {
        design$prob <- get_probs(design_matrix, opt_env)
    }

    # Store categorical structure for potential decoding
    categorical_structure <- get_categorical_structure_from_profiles(
        opt_env$profiles,
        opt_env$attr_names
    )
    attr(design, "categorical_structure") <- categorical_structure
    attr(design, "is_dummy_coded") <- TRUE

    if (opt_env$method_optimal) {
        # Repeat and randomize across respondents
        design <- repeat_design_across_respondents(design, opt_env)
    }

    return(design)
}

add_metadata <- function(design, n_resp, n_alts, n_q) {
    design$respID <- rep(seq(n_resp), each = n_alts * n_q)
    design$qID <- rep(rep(seq(n_q), each = n_alts), n_resp)
    design$altID <- rep(seq(n_alts), n_resp * n_q)
    design$obsID <- rep(seq(n_resp * n_q), each = n_alts)
    return(design)
}

add_metadata_optimal <- function(design, n_block, n_alts, n_q) {
    design$blockID <- rep(seq(n_block), each = n_alts * n_q)
    design$qID <- rep(rep(seq(n_q), each = n_alts), n_block)
    design$altID <- rep(seq(n_alts), n_block * n_q)
    design$obsID <- rep(seq(n_block * n_q), each = n_alts)
    return(design)
}

# Get categorical structure information from original profiles
get_categorical_structure_from_profiles <- function(profiles, attr_names) {
    categorical_info <- list()

    for (attr in attr_names) {
        if (attr %in% names(profiles)) {
            values <- profiles[[attr]]
            if (!is.numeric(values)) {
                # This is a categorical variable
                if (is.factor(values)) {
                    levels_order <- levels(values)
                } else {
                    levels_order <- unique(values)
                }

                categorical_info[[attr]] <- list(
                    is_categorical = TRUE,
                    levels = levels_order,
                    reference_level = levels_order[1] # First level is reference
                )
            } else {
                # Numeric variable
                categorical_info[[attr]] <- list(
                    is_categorical = FALSE
                )
            }
        }
    }

    return(categorical_info)
}

compute_design_efficiency_metrics <- function(design) {
    # Balance metrics
    balance_result <- compute_balance_metrics_internal(design)

    # Overlap metrics
    overlap_result <- compute_overlap_metrics_internal(design)

    return(list(
        balance_score = balance_result$overall_balance,
        balance_details = balance_result$balance_metrics,
        overlap_score = overlap_result$overall_overlap,
        overlap_details = overlap_result$overlap_metrics,
        profiles_used = length(unique(design$profileID[design$profileID != 0])),
        profiles_available = max(design$profileID, na.rm = TRUE)
    ))
}

# Internal function for balance computation
compute_balance_metrics_internal <- function(design) {
    # Get attribute columns
    atts <- setdiff(
        names(design),
        c("respID", "qID", "altID", "obsID", "profileID", "blockID")
    )

    # Get counts of each individual attribute
    counts <- lapply(atts, function(attr) table(design[[attr]]))
    names(counts) <- atts

    # Calculate balance metrics for each attribute
    balance_metrics <- calculate_balance_metrics(counts)

    # Calculate overall balance score
    overall_balance <- mean(sapply(balance_metrics, function(x) {
        x$balance_score
    }))

    return(list(
        individual_counts = counts,
        balance_metrics = balance_metrics,
        overall_balance = overall_balance
    ))
}

# Internal function for overlap computation
compute_overlap_metrics_internal <- function(design) {
    # Get attribute columns
    atts <- setdiff(
        names(design),
        c("respID", "qID", "altID", "obsID", "profileID", "blockID")
    )

    # Calculate overlap for each attribute
    overlap_counts <- lapply(atts, function(attr) {
        get_att_overlap_counts(attr, design)
    })
    names(overlap_counts) <- atts

    # Calculate overlap metrics
    overlap_metrics <- calculate_overlap_metrics(overlap_counts, design)

    # Calculate overall overlap score (average of complete overlap rates)
    overall_overlap <- mean(sapply(overlap_metrics, function(x) {
        x$complete_overlap_rate
    }))

    return(list(
        overlap_counts = overlap_counts,
        overlap_metrics = overlap_metrics,
        overall_overlap = overall_overlap
    ))
}

# Finalize the design object with metadata including both D-errors
finalize_design_object <- function(design, design_result, opt_env) {
    method <- opt_env$method

    # Update profileID to 0 for no_choice (if present)
    if (opt_env$no_choice) {
        maxID <- max(design$profileID)
        design$profileID[which(design$profileID == maxID)] <- 0
    }

    # Store the design matrix for later use in cbc_choices
    # This ensures exact consistency between design optimization and choice simulation
    attr(design, "design_matrix") <- design_result$design_matrix

    # Build design_params list
    n <- opt_env$n
    design_params <- list(
        method = method,
        n_q = n$q,
        n_alts = n$alts,
        n_alts_total = n$alts_total,
        n_resp = n$resp,
        n_blocks = n$blocks,
        no_choice = opt_env$no_choice,
        label = opt_env$label,
        randomize_questions = if (method == "random") {
            NA
        } else {
            opt_env$randomize_questions
        },
        randomize_alts = if (method == "random") NA else opt_env$randomize_alts,
        created_at = Sys.time(),
        has_interactions = opt_env$has_interactions,
        n_interactions = if (opt_env$has_interactions) {
            length(opt_env$priors$interactions)
        } else {
            0
        },
        remove_dominant = opt_env$remove_dominant,
        dominance_types = if (opt_env$remove_dominant) {
            opt_env$dominance_types
        } else {
            NULL
        },
        dummy_coded = TRUE
    )

    # Add D-error information (both null and prior-based)

    if (opt_env$method_optimal) {
        # For optimized designs, compute D-errors

        # Always compute null D-error (no priors, equal probabilities)
        design_params$d_error_null <- compute_design_d_error_null(
            design_result$design_matrix,
            opt_env
        )

        # Compute prior-based D-error if priors are available
        design_params$d_error_prior <- NULL
        if (opt_env$has_priors) {
            design_params$d_error_prior <- compute_design_d_error(
                design_result$design_matrix,
                opt_env
            )
        }
    }

    # Pre-compute efficiency metrics
    efficiency_metrics <- compute_design_efficiency_metrics(design)

    # Store metadata including the optimization environment
    # This allows cbc_choices to recreate the exact same utility calculations
    attr(design, "profiles") <- opt_env$profiles
    attr(design, "priors") <- opt_env$priors
    attr(design, "optimization_environment") <- list(
        has_interactions = opt_env$has_interactions,
        attr_names = opt_env$attr_names,
        no_choice = opt_env$no_choice,
        available_profile_ids = opt_env$available_profile_ids,
        # Store the utility matrices for reuse
        X_matrix = opt_env$X_matrix,
        X_matrix_utility = opt_env$X_matrix_utility,
        exp_utilities = opt_env$exp_utilities,
        exp_utilities_draws = opt_env$exp_utilities_draws,
        is_bayesian = opt_env$is_bayesian
    )

    time_stop <- Sys.time()
    design_params$time_elapsed_sec <- as.numeric(time_stop - opt_env$time_start)
    attr(design, "design_params") <- design_params

    # Calculate summary statistics
    n_profiles_used <- length(unique(design$profileID[design$profileID != 0]))
    if (opt_env$no_choice) {
        n_profiles_used <- n_profiles_used - 1
    }

    attr(design, "design_summary") <- list(
        n_profiles_available = n$profiles,
        n_profiles_used = n_profiles_used,
        profile_usage_rate = n_profiles_used / n$profiles,
        n_choice_sets = n$blocks * n$q * n$resp,
        optimization_attempts = design_result$total_attempts,
        efficiency = efficiency_metrics
    )

    class(design) <- c("cbc_design", "data.frame")
    return(design)
}

# Helper functions ----

# Get random parameters from priors object
get_rand_pars <- function(priors) {
    if (is.null(priors)) {
        return(NULL)
    }

    # Extract random parameter names from priors structure
    random_attrs <- names(which(sapply(priors$attrs, function(x) x$random)))
    if (length(random_attrs) == 0) {
        return(NULL)
    }

    # Return in format expected by logitr::recodeData
    rand_pars <- list()
    for (attr in random_attrs) {
        rand_pars[[attr]] <- priors$attrs[[attr]]$dist
    }

    return(rand_pars)
}

compute_partial_utilities_with_interactions <- function(X_matrix, priors) {
    n_profiles <- nrow(X_matrix)

    # Extract parameters excluding no_choice if present
    if (priors$has_no_choice) {
        no_choice_index <- which(names(priors$pars) == "no_choice")
        pars_without_no_choice <- priors$pars[-no_choice_index]
    } else {
        pars_without_no_choice <- priors$pars
    }

    # Compute mean partial utility across par draws if exist
    par_draws <- priors$par_draws
    if (!is.null(par_draws)) {
        # For Bayesian case, exclude no_choice from draws too
        if (priors$has_no_choice) {
            par_draws_without_no_choice <- par_draws[, -no_choice_index]
        } else {
            par_draws_without_no_choice <- par_draws
        }

        n_draws <- nrow(par_draws_without_no_choice)
        pars <- par_draws_without_no_choice[1, ]
        partials <- compute_partial_utility_single(X_matrix, pars, n_profiles)
        for (i in 2:n_draws) {
            pars <- par_draws_without_no_choice[i, ]
            partials <- partials +
                compute_partial_utility_single(X_matrix, pars, n_profiles)
        }
        return(partials / n_draws)
    }
    # Otherwise compute partial utility using priors (now including interactions)
    return(compute_partial_utility_single(
        X_matrix,
        pars_without_no_choice,
        n_profiles
    ))
}

# Compute partial utilities for dominance checking
compute_partial_utilities <- function(X_matrix, priors) {
    n_profiles <- nrow(X_matrix)

    # Compute mean partial utility across par draws if exist
    par_draws <- priors$par_draws
    n_draws <- nrow(par_draws)
    if (!is.null(par_draws)) {
        pars <- par_draws[1, ]
        partials <- compute_partial_utility_single(X_matrix, pars, n_profiles)
        for (i in 2:n_draws) {
            pars <- par_draws[i, ]
            partials <- partials +
                compute_partial_utility_single(X_matrix, pars, n_profiles)
        }
        return(partials / n_draws)
    }
    # Otherwise just compute direct partial utility using prior pars
    return(compute_partial_utility_single(X_matrix, priors$pars, n_profiles))
}

compute_partial_utility_single <- function(X_matrix, pars, n_profiles) {
    par_mat <- matrix(
        rep(pars, n_profiles),
        nrow = n_profiles,
        byrow = TRUE
    )
    return(X_matrix * par_mat)
}

# Set up label constraints for labeled designs
setup_label_constraints <- function(profiles, label, n_alts) {
    # Split profiles by label values
    label_values <- unique(profiles[[label]])

    if (length(label_values) != n_alts) {
        stop(sprintf(
            "Number of label values (%d) must match n_alts (%d)",
            length(label_values),
            n_alts
        ))
    }

    groups <- list()
    for (i in seq_along(label_values)) {
        groups[[i]] <- profiles$profileID[profiles[[label]] == label_values[i]]
    }

    return(list(
        values = label_values,
        groups = groups
    ))
}

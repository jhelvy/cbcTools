#' Generate survey designs for choice experiments (New Implementation)
#'
#' This function creates experimental designs for choice-based conjoint experiments
#' using a new profileID-based optimization approach for improved efficiency.
#'
#' @param profiles A data frame of class `cbc_profiles` created using `cbc_profiles()`
#' @param method Choose the design method: "random" or "sequential". Defaults to "random"
#' @param priors A `cbc_priors` object created by `cbc_priors()`, or NULL for random designs
#' @param n_alts Number of alternatives per choice question
#' @param n_q Number of questions per respondent (or per block)
#' @param n_resp Number of respondents (for random designs) or 1 (for sequential designs that get repeated)
#' @param n_blocks Number of blocks in the design. Defaults to 1
#' @param no_choice Include a "no choice" option? Defaults to FALSE
#' @param label The name of the variable to use in a "labeled" design. Defaults to NULL
#' @param randomize_questions Randomize question order for each respondent? Defaults to TRUE (sequential only)
#' @param randomize_alts Randomize alternative order within questions? Defaults to TRUE (sequential only)
#' @param remove_dominant Remove choice sets with dominant alternatives? Defaults to FALSE
#' @param dominance_types Types of dominance to check: "total" and/or "partial"
#' @param dominance_threshold Threshold for total dominance detection. Defaults to 0.8
#' @param max_dominance_attempts Maximum attempts to replace dominant choice sets. Defaults to 50.
#' @param max_iter Maximum iterations for sequential optimization. Defaults to 50
#' @param n_start Number of random starts for sequential designs. Defaults to 5
#' @return A `cbc_design` object containing the experimental design
#' @export
cbc_design <- function(
        profiles,
        method = "random",
        priors = NULL,
        n_alts,
        n_q,
        n_resp = 1,
        n_blocks = 1,
        no_choice = FALSE,
        label = NULL,
        randomize_questions = TRUE,
        randomize_alts = TRUE,
        remove_dominant = FALSE,
        dominance_types = c("total", "partial"),
        dominance_threshold = 0.8,
        max_dominance_attempts = 50,
        max_iter = 50,
        n_start = 5
) {

    # Validate inputs
    validate_design_inputs(profiles, method, priors, n_alts, n_q, n_resp, n_blocks,
                           no_choice, label, randomize_questions, randomize_alts,
                           remove_dominant, dominance_types, dominance_threshold,
                           max_dominance_attempts)

    # Set up the optimization environment
    opt_env <- setup_optimization_environment(
        profiles, priors, no_choice, label, remove_dominant,
        dominance_types, dominance_threshold
    )

    # Generate design based on method
    if (method == "random") {
        # Random designs: generate completely random design for all respondents
        design_result <- generate_random_design(
            opt_env, n_alts, n_q, n_resp, n_blocks, max_dominance_attempts
        )

        # Convert profileID matrix back to full design
        final_design <- construct_final_design(
            design_result$design_matrix, opt_env, n_alts, n_q, n_resp, n_blocks
        )

    } else if (method == "sequential") {
        # Sequential designs: create optimized base design, then repeat across respondents
        base_design_result <- generate_sequential_design(
            opt_env, n_alts, n_q, n_blocks, max_iter, n_start, max_dominance_attempts
        )

        # Convert base design to full design
        base_design <- construct_final_design(
            base_design_result$design_matrix, opt_env, n_alts, n_q, 1, n_blocks
        )

        # Repeat and randomize across respondents
        final_design <- repeat_design_across_respondents(
            base_design, n_resp, n_alts, n_q, n_blocks,
            randomize_questions, randomize_alts
        )

        # Update design result metadata
        design_result <- base_design_result
        design_result$repeated_across_respondents <- TRUE
    }

    # Add metadata and return
    finalize_design_object(final_design, opt_env, design_result, method,
                           n_alts, n_q, n_resp, n_blocks, no_choice, label,
                           randomize_questions, randomize_alts)
}

#' Set up the optimization environment with pre-computed utilities
#'
#' @param profiles A cbc_profiles object
#' @param priors A cbc_priors object or NULL
#' @param no_choice Logical, include no-choice option
#' @param label Character, label variable name or NULL
#' @param remove_dominant Logical, remove dominant choice sets
#' @param dominance_types Character vector of dominance types
#' @param dominance_threshold Numeric threshold for dominance
#' @return A list containing the optimization environment
setup_optimization_environment <- function(
    profiles, priors, no_choice, label, remove_dominant,
    dominance_types, dominance_threshold
) {

    # Get attribute names (excluding profileID)
    attr_names <- setdiff(names(profiles), "profileID")

    # Get random parameters for encoding
    randPars <- get_rand_pars(priors)

    # Encode the profiles
    coded_data <- logitr::recodeData(profiles, attr_names, randPars)
    X_matrix <- coded_data$X

    # Defaults for no choice
    exp_no_choice <- NULL
    exp_no_choice_draws <- NULL

    # Encode profiles using logitr if we have priors
    if (!is.null(priors)) {

        if (no_choice) {
            prior_no_choice <- priors$pars["no_choice"]
            no_choice_index <- which(names(priors$pars) == "no_choice")
            priors$pars <- priors$pars[-no_choice_index]
        }

        # Compute utilities for each profile
        if (!is.null(priors$par_draws)) {

            if (no_choice) {
                prior_no_choice_draws <- priors$par_draws[, no_choice_index]
                priors$par_draws <- priors$par_draws[, -no_choice_index]
            }

            # Bayesian case with parameter draws
            n_draws <- nrow(priors$par_draws)
            exp_utilities_draws <- array(dim = c(nrow(profiles), n_draws))

            for (i in 1:n_draws) {
                utilities_i <- X_matrix %*% priors$par_draws[i, ]
                exp_utilities_draws[, i] <- exp(utilities_i)
            }
            exp_utilities <- NULL
        } else {
            # Fixed parameters case
            utilities <- X_matrix %*% priors$pars
            exp_utilities <- exp(utilities)
            exp_utilities_draws <- NULL
        }

        # Pre-compute partial utilities for dominance checking if needed
        partial_utilities <- NULL
        if (remove_dominant && "partial" %in% dominance_types) {
            partial_utilities <- compute_partial_utilities(X_matrix, priors, attr_names)
        }

    } else {
        # No priors - equal probabilities
        exp_utilities <- rep(1, nrow(profiles))
        exp_utilities_draws <- NULL
        partial_utilities <- NULL
    }

    # Handle no-choice option
    exp_no_choice <- NULL
    exp_no_choice_draws <- NULL
    if (no_choice) {
        if (!is.null(priors)) {
            exp_no_choice <- exp(prior_no_choice)
        } else {
            exp_no_choice <- 1  # Default no-choice utility of 0 (exp(0) = 1)
        }

        if (!is.null(exp_utilities_draws)) {
            # Add no-choice utilities for each draw
            if (!is.null(priors)) {
                exp_no_choice_draws <- exp(prior_no_choice_draws)
            } else {
                exp_no_choice_draws <- rep(1, ncol(exp_utilities_draws))
            }
        }
    }

    # Set up label constraints if specified
    label_constraints <- NULL
    if (!is.null(label)) {
        label_constraints <- setup_label_constraints(profiles, label, n_alts)
    }

    return(list(
        profiles = profiles,
        priors = priors,
        attr_names = attr_names,
        exp_no_choice = exp_no_choice,
        exp_no_choice_draws = exp_no_choice_draws,
        exp_utilities = exp_utilities,
        exp_utilities_draws = exp_utilities_draws,
        partial_utilities = partial_utilities,
        X_matrix = X_matrix,
        no_choice = no_choice,
        label = label,
        label_constraints = label_constraints,
        remove_dominant = remove_dominant,
        dominance_types = dominance_types,
        dominance_threshold = dominance_threshold,
        available_profile_ids = profiles$profileID
    ))
}

#' Generate a random design using profileID sampling with matrix operations
#'
#' For random designs, generates a completely random design for ALL respondents.
#' Each respondent gets their own random questions.
#'
#' @param opt_env Optimization environment from setup_optimization_environment
#' @param n_alts Number of alternatives per question
#' @param n_q Number of questions per respondent
#' @param n_resp Number of respondents
#' @param n_blocks Number of blocks
#' @param max_attempts Maximum attempts to find valid design
#' @return A list with design_matrix and metadata
generate_random_design <- function(opt_env, n_alts, n_q, n_resp, n_blocks, max_attempts) {

    # For random designs: total questions = n_q * n_resp * n_blocks
    total_questions <- n_q * n_resp * n_blocks

    # Start with a completely random design matrix
    design_matrix <- generate_initial_random_matrix(opt_env, n_alts, total_questions)

    # Iteratively fix problems
    attempts <- 0
    while (attempts < max_attempts) {
        attempts <- attempts + 1

        # Find all problematic questions
        problem_questions <- find_problematic_questions(design_matrix, opt_env, n_q, n_resp)

        if (length(problem_questions) == 0) {
            break  # Design is valid
        }

        # Replace problematic questions
        for (q in problem_questions) {
            design_matrix[q, ] <- sample_question_profiles(opt_env, n_alts)
        }
    }

    if (length(find_problematic_questions(design_matrix, opt_env, n_q, n_resp)) > 0) {
        warning(sprintf("Could not generate fully valid design after %d attempts", max_attempts))
    }

    # Compute final D-error
    d_error <- compute_design_d_error(design_matrix, opt_env)

    return(list(
        design_matrix = design_matrix,
        d_error = d_error,
        method = "random",
        total_attempts = attempts,
        total_questions = total_questions
    ))
}

#' Generate initial random design matrix
#'
#' @param opt_env Optimization environment
#' @param n_alts Number of alternatives per question
#' @param total_questions Total number of questions
#' @return Matrix of profileIDs
generate_initial_random_matrix <- function(opt_env, n_alts, total_questions) {

    design_matrix <- matrix(0, nrow = total_questions, ncol = n_alts)

    if (!is.null(opt_env$label_constraints)) {
        # Labeled design: sample one from each label group for each question
        for (q in 1:total_questions) {
            design_matrix[q, ] <- sample_labeled_profiles(opt_env$label_constraints, n_alts)
        }
    } else {
        # Regular design: sample without replacement for each question
        for (q in 1:total_questions) {
            design_matrix[q, ] <- sample(opt_env$available_profile_ids, n_alts, replace = FALSE)
        }
    }

    return(design_matrix)
}

#' Find all questions that have problems (vectorized approach)
#'
#' For random designs, checks for duplicates within each respondent's question set
#'
#' @param design_matrix Current design matrix
#' @param opt_env Optimization environment
#' @param n_q Number of questions per respondent
#' @param n_resp Number of respondents
#' @return Vector of question indices that have problems
find_problematic_questions <- function(design_matrix, opt_env, n_q, n_resp) {

    n_questions <- nrow(design_matrix)
    problematic <- logical(n_questions)

    # Check for within-question duplicates
    for (q in 1:n_questions) {
        question_profiles <- design_matrix[q, ]
        if (length(unique(question_profiles)) != length(question_profiles)) {
            problematic[q] <- TRUE
        }
    }

    # Check for duplicate questions within each respondent
    for (resp in 1:n_resp) {
        # Get question indices for this respondent
        resp_start <- (resp - 1) * n_q + 1
        resp_end <- resp * n_q
        resp_questions <- resp_start:resp_end

        # Check for duplicates within this respondent's questions
        for (i in 1:length(resp_questions)) {
            q1 <- resp_questions[i]
            if (problematic[q1]) next  # Already marked as problematic

            current_sorted <- sort(design_matrix[q1, ])

            for (j in 1:length(resp_questions)) {
                if (i == j) next  # Don't compare question to itself

                q2 <- resp_questions[j]
                other_sorted <- sort(design_matrix[q2, ])

                if (identical(current_sorted, other_sorted)) {
                    problematic[q1] <- TRUE
                    break
                }
            }
        }
    }

    # Check for dominance if required
    if (opt_env$remove_dominant) {
        for (q in 1:n_questions) {
            if (problematic[q]) next  # Already marked as problematic

            question_profiles <- design_matrix[q, ]
            if (has_dominant_alternative(question_profiles, opt_env)) {
                problematic[q] <- TRUE
            }
        }
    }

    return(which(problematic))
}

#' Sample profileIDs for a single question
#'
#' @param opt_env Optimization environment
#' @param n_alts Number of alternatives to sample
#' @return Vector of profileIDs
sample_question_profiles <- function(opt_env, n_alts) {

    if (!is.null(opt_env$label_constraints)) {
        # Labeled design: sample one from each label group
        return(sample_labeled_profiles(opt_env$label_constraints, n_alts))
    } else {
        # Regular design: sample without replacement from all available profiles
        return(sample(opt_env$available_profile_ids, n_alts, replace = FALSE))
    }
}

#' Sample profiles for labeled design
#'
#' @param label_constraints Label constraint structure
#' @param n_alts Number of alternatives (should match number of label groups)
#' @return Vector of profileIDs, one from each label group
sample_labeled_profiles <- function(label_constraints, n_alts) {

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

#' Check if a question has a dominant alternative
#'
#' @param question_profiles Vector of profileIDs
#' @param opt_env Optimization environment
#' @return Logical indicating if dominance exists
has_dominant_alternative <- function(question_profiles, opt_env) {

    # Total dominance check
    if ("total" %in% opt_env$dominance_types) {
        probs <- compute_question_probabilities(question_profiles, opt_env)
        if (max(probs) > opt_env$dominance_threshold) {
            return(TRUE)
        }
    }

    # Partial dominance check
    if ("partial" %in% opt_env$dominance_types && !is.null(opt_env$partial_utilities)) {
        if (has_partial_dominance(question_profiles, opt_env)) {
            return(TRUE)
        }
    }

    return(FALSE)
}

#' Compute choice probabilities for a question
#'
#' @param question_profiles Vector of profileIDs
#' @param opt_env Optimization environment
#' @return Vector of choice probabilities
compute_question_probabilities <- function(question_profiles, opt_env) {

    if (!is.null(opt_env$exp_utilities_draws)) {
        # Bayesian case: average probabilities across draws
        n_draws <- ncol(opt_env$exp_utilities_draws)
        prob_matrix <- matrix(0, nrow = length(question_profiles), ncol = n_draws)

        for (i in 1:n_draws) {
            exp_utils <- opt_env$exp_utilities_draws[question_profiles, i]
            prob_matrix[, i] <- exp_utils / sum(exp_utils)
        }

        return(rowMeans(prob_matrix))

    } else {
        # Fixed parameters case
        exp_utils <- opt_env$exp_utilities[question_profiles]
        return(exp_utils / sum(exp_utils))
    }
}

#' Compute D-error for entire design using profileID matrix
#'
#' Reconstructs X matrix from profileIDs and computes full information matrix
#'
#' @param design_matrix Matrix of profileIDs (questions x alternatives)
#' @param opt_env Optimization environment
#' @return Numeric D-error value
compute_design_d_error <- function(design_matrix, opt_env) {

    n_questions <- nrow(design_matrix)
    n_alts <- ncol(design_matrix)

    # Reconstruct X matrix for entire design by slicing the encoded profiles
    design_profiles <- as.vector(t(design_matrix))

    # Build X matrix for this design using the encoded profiles matrix
    n_params <- ncol(opt_env$X_matrix)
    X_design <- opt_env$X_matrix[design_profiles, ]

    # Create obsID and altID for regular alternatives
    obsID <- rep(1:n_questions, each = n_alts)
    altID <- rep(1:n_alts, n_questions)

    # If no-choice is enabled, append no-choice rows
    if (opt_env$no_choice) {
        # Add no-choice rows (all zeros)
        no_choice_rows <- matrix(0, nrow = n_questions, ncol = n_params)
        X_design <- rbind(X_design, no_choice_rows)

        # Add obsID and altID for no-choice alternatives
        obsID <- c(obsID, 1:n_questions)  # Same obsID as questions
        altID <- c(altID, rep(n_alts + 1, n_questions))  # altID = n_alts + 1

        # Sort by obsID then altID to get proper interleaving
        sort_order <- order(obsID, altID)
        X_design <- X_design[sort_order, ]
        obsID <- obsID[sort_order]
        altID <- altID[sort_order]

        n_alts_final <- n_alts + 1
    } else {
        n_alts_final <- n_alts
    }

    # Compute probabilities for each question
    if (!is.null(opt_env$exp_utilities_draws)) {
        # Bayesian case: average D-error across draws
        n_draws <- ncol(opt_env$exp_utilities_draws)
        d_errors <- numeric(n_draws)

        for (draw in 1:n_draws) {
            probs <- compute_design_probabilities_with_nochoice(design_matrix, opt_env, draw)
            d_errors[draw] <- compute_d_error_from_probs(X_design, probs, obsID)
        }

        return(mean(d_errors))

    } else {
        # Fixed parameters case or no priors case
        probs <- compute_design_probabilities_with_nochoice(design_matrix, opt_env, NULL)
        return(compute_d_error_from_probs(X_design, probs, obsID))
    }
}

# Helper function to compute probabilities including no-choice
compute_design_probabilities_with_nochoice <- function(design_matrix, opt_env, draw_index = NULL) {

    n_questions <- nrow(design_matrix)
    n_alts <- ncol(design_matrix)

    if (opt_env$no_choice) {
        # Compute probabilities for each question including no-choice
        all_probs <- numeric(n_questions * (n_alts + 1))

        for (q in 1:n_questions) {
            question_profiles <- design_matrix[q, ]

            # Get utilities for regular alternatives
            if (!is.null(draw_index)) {
                # Bayesian case
                exp_utils_regular <- opt_env$exp_utilities_draws[question_profiles, draw_index]
                exp_util_nochoice <- opt_env$exp_no_choice_draws[draw_index]
            } else {
                # Fixed case or no priors
                if (!is.null(opt_env$exp_utilities)) {
                    exp_utils_regular <- opt_env$exp_utilities[question_profiles]
                } else {
                    exp_utils_regular <- rep(1, n_alts)  # No priors case
                }
                exp_util_nochoice <- opt_env$exp_no_choice
            }

            # Combine utilities and compute probabilities
            all_exp_utils <- c(exp_utils_regular, exp_util_nochoice)
            question_probs <- all_exp_utils / sum(all_exp_utils)

            # Store probabilities in correct positions (before sorting)
            regular_indices <- (q - 1) * (n_alts + 1) + 1:n_alts
            nochoice_index <- q * (n_alts + 1)

            all_probs[regular_indices] <- question_probs[1:n_alts]
            all_probs[nochoice_index] <- question_probs[n_alts + 1]
        }

        # Now sort probabilities to match the sorted X_design
        obsID_prob <- rep(1:n_questions, each = n_alts + 1)
        altID_prob <- rep(1:(n_alts + 1), n_questions)
        sort_order <- order(obsID_prob, altID_prob)
        all_probs <- all_probs[sort_order]

    } else {
        # No no-choice case - use existing function
        all_probs <- compute_design_probabilities_fixed(design_matrix, opt_env)
    }

    return(all_probs)
}

#' Compute probabilities for entire design (fixed parameters)
#'
#' @param design_matrix Matrix of profileIDs
#' @param opt_env Optimization environment
#' @return Vector of probabilities for all alternatives
compute_design_probabilities_fixed <- function(design_matrix, opt_env) {

    n_questions <- nrow(design_matrix)
    n_alts <- ncol(design_matrix)
    probs <- numeric(n_questions * n_alts)

    for (q in 1:n_questions) {
        question_profiles <- design_matrix[q, ]
        question_probs <- compute_question_probabilities(question_profiles, opt_env)

        start_idx <- (q - 1) * n_alts + 1
        end_idx <- q * n_alts
        probs[start_idx:end_idx] <- question_probs
    }

    return(probs)
}

#' Compute probabilities for entire design (single draw)
#'
#' @param design_matrix Matrix of profileIDs
#' @param opt_env Optimization environment
#' @param draw_index Which parameter draw to use
#' @return Vector of probabilities for all alternatives
compute_design_probabilities_draw <- function(design_matrix, opt_env, draw_index) {

    n_questions <- nrow(design_matrix)
    n_alts <- ncol(design_matrix)
    probs <- numeric(n_questions * n_alts)

    for (q in 1:n_questions) {
        question_profiles <- design_matrix[q, ]

        # Get exp utilities for this draw
        exp_utils <- opt_env$exp_utilities_draws[as.character(question_profiles), draw_index]
        question_probs <- exp_utils / sum(exp_utils)

        start_idx <- (q - 1) * n_alts + 1
        end_idx <- q * n_alts
        probs[start_idx:end_idx] <- question_probs
    }

    return(probs)
}

#' Compute D-error from X matrix and probabilities
#'
#' @param X_design Design matrix (all alternatives x parameters)
#' @param probs Vector of choice probabilities
#' @param obsID Vector indicating which observation each row belongs to
#' @return D-error value
compute_d_error_from_probs <- function(X_design, probs, obsID) {

    # Split X and probs by observation
    X_list <- split(as.data.frame(X_design), obsID)
    P_list <- split(probs, obsID)

    # Convert back to matrices
    X_list <- lapply(X_list, as.matrix)

    # Compute information matrix
    info_result <- compute_info_matrix(X_list, P_list)

    # Compute D-error
    K <- ncol(X_design)
    det_M <- det(info_result$M_total)

    if (det_M <= 0) {
        return(Inf)
    }

    return(det_M^(-1/K))
}

#' Compute information matrix from X and probability lists
#'
#' @param X_list List of X matrices (one per choice question)
#' @param P_list List of probability vectors (one per choice question)
#' @return List with M_list and M_total
compute_info_matrix <- function(X_list, P_list) {
    # Pre-compute information matrices for each choice set
    M_list <- mapply(function(X, P) {
        P_matrix <- diag(as.vector(P)) - tcrossprod(P)
        t(X) %*% P_matrix %*% X
    }, X_list, P_list, SIMPLIFY = FALSE)

    # Compute total information matrix
    M_total <- Reduce("+", M_list)

    # Return both matrices
    return(list(
        M_list = M_list,
        M_total = M_total
    ))
}

#' Convert profileID design matrix to full design data frame
#'
#' @param design_matrix Matrix of profileIDs
#' @param opt_env Optimization environment
#' @param n_alts Number of alternatives per question
#' @param n_q Number of questions per block
#' @param n_blocks Number of blocks
#' @return Data frame with full design
construct_final_design <- function(design_matrix, opt_env, n_alts, n_q, n_resp, n_blocks) {

    if (opt_env$no_choice) {
        design_matrix <- add_no_choice_to_design(design_matrix, ncol(design_matrix))
    }

    total_questions <- nrow(design_matrix)
    actual_n_alts <- ncol(design_matrix)  # May be n_alts + 1 if no-choice added
    total_rows <- total_questions * actual_n_alts

    # Initialize design data frame
    design <- data.frame(
        profileID = as.vector(t(design_matrix)),
        blockID = rep(rep(1:n_blocks, each = n_q), n_resp * actual_n_alts),
        respID = rep(rep(1:n_resp, each = n_q * n_blocks), each = actual_n_alts),
        qID = rep(rep(1:n_q, n_resp * n_blocks), each = actual_n_alts),
        altID = rep(1:actual_n_alts, total_questions),
        obsID = rep(1:total_questions, each = actual_n_alts)
    )

    # Add attribute values by joining with profiles
    if (opt_env$no_choice) {
        # Handle no-choice rows
        no_choice_rows <- design$profileID == 0

        # Create no-choice profile
        no_choice_profile <- opt_env$profiles[1, ]  # Copy structure
        no_choice_profile$profileID <- 0
        no_choice_profile[opt_env$attr_names] <- 0  # Set all attributes to 0
        no_choice_profile$no_choice <- 1  # Add no_choice indicator

        # Join regular profiles
        regular_profiles <- opt_env$profiles
        regular_profiles$no_choice <- 0

        # Combine profiles
        all_profiles <- rbind(no_choice_profile, regular_profiles)

    } else {
        all_profiles <- opt_env$profiles
    }

    # Join with profiles to get attribute values
    design <- merge(design, all_profiles, by = "profileID", sort = FALSE)

    # Reorder columns and rows
    id_cols <- c("profileID", "blockID", "qID", "altID", "obsID")
    attr_cols <- setdiff(names(design), id_cols)
    design <- design[, c(id_cols, attr_cols)]
    design <- design[order(design$obsID, design$altID), ]
    row.names(design) <- NULL

    return(design)
}

#' Finalize the design object with metadata and class
#'
#' @param design The design data frame
#' @param opt_env Optimization environment
#' @param design_result Result from design generation
#' @param method Design method used
#' @param n_alts Number of alternatives
#' @param n_q Number of questions per respondent
#' @param n_resp Number of respondents
#' @param n_blocks Number of blocks
#' @param no_choice No-choice option included
#' @param label Label variable used
#' @param randomize_questions Question randomization setting
#' @param randomize_alts Alternative randomization setting
#' @return cbc_design object
finalize_design_object <- function(design, opt_env, design_result, method,
                                   n_alts, n_q, n_resp, n_blocks, no_choice, label,
                                   randomize_questions, randomize_alts) {

    # Store metadata
    attr(design, "profiles") <- opt_env$profiles
    attr(design, "priors") <- opt_env$priors

    attr(design, "design_params") <- list(
        method = method,
        n_q = n_q,
        n_alts = n_alts,
        n_resp = n_resp,
        n_blocks = n_blocks,
        no_choice = no_choice,
        label = label,
        randomize_questions = if (method == "sequential") randomize_questions else NA,
        randomize_alts = if (method == "sequential") randomize_alts else NA,
        d_error = design_result$d_error,
        created_at = Sys.time(),
        remove_dominant = opt_env$remove_dominant,
        dominance_types = if (opt_env$remove_dominant) opt_env$dominance_types else NULL
    )

    # Calculate summary statistics
    n_profiles_available <- nrow(opt_env$profiles)
    n_profiles_used <- length(unique(design$profileID[design$profileID != 0]))

    attr(design, "design_summary") <- list(
        n_profiles_available = n_profiles_available,
        n_profiles_used = n_profiles_used,
        profile_usage_rate = n_profiles_used / n_profiles_available,
        n_choice_sets = n_blocks * n_q * n_resp,
        method_specific = if (method == "random") {
            list(
                total_questions_generated = design_result$total_questions,
                optimization_attempts = design_result$total_attempts
            )
        } else {
            list(
                base_design_optimized = TRUE,
                repeated_across_respondents = design_result$repeated_across_respondents %||% FALSE
            )
        }
    )

    class(design) <- c("cbc_design", "data.frame")
    return(design)
}

# Helper functions that will need to be implemented

#' Basic input validation for cbc_design
#'
#' Validates all inputs to ensure they meet requirements for design generation
#'
#' @param profiles cbc_profiles object
#' @param method Design method ("random" or "sequential")
#' @param priors cbc_priors object or NULL
#' @param n_alts Number of alternatives per question
#' @param n_q Number of questions per respondent
#' @param n_resp Number of respondents
#' @param n_blocks Number of blocks
#' @param no_choice Include no-choice option
#' @param label Label variable for labeled designs
#' @param randomize_questions Randomize question order (sequential only)
#' @param randomize_alts Randomize alternative order (sequential only)
#' @param remove_dominant Remove dominant choice sets
#' @param dominance_types Types of dominance to check
#' @param dominance_threshold Threshold for dominance detection
#' @param max_dominance_attempts Maximum attempts to avoid dominance
validate_design_inputs <- function(
    profiles, method, priors, n_alts, n_q, n_resp, n_blocks,
    no_choice, label, randomize_questions, randomize_alts,
    remove_dominant, dominance_types, dominance_threshold,
    max_dominance_attempts
) {

    # Validate profiles
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }

    if (nrow(profiles) == 0) {
        stop("profiles must contain at least one profile")
    }

    # Validate method
    if (!method %in% c("random", "sequential")) {
        stop("method must be 'random' or 'sequential'")
    }

    # Validate priors
    if (!is.null(priors) && !inherits(priors, "cbc_priors")) {
        stop("priors must be a cbc_priors object created by cbc_priors() or NULL")
    }

    # Validate numeric inputs
    if (!is.numeric(n_alts) || n_alts < 2) {
        stop("n_alts must be a numeric value >= 2")
    }

    if (!is.numeric(n_q) || n_q < 1) {
        stop("n_q must be a numeric value >= 1")
    }

    if (!is.numeric(n_resp) || n_resp < 1) {
        stop("n_resp must be a numeric value >= 1")
    }

    if (!is.numeric(n_blocks) || n_blocks < 1) {
        stop("n_blocks must be a numeric value >= 1")
    }

    # Method-specific validation
    if (method == "random") {
        if (n_resp == 1) {
            warning("For random designs with n_resp = 1, consider using method = 'sequential' for better efficiency")
        }
    } else if (method == "sequential") {
        if (n_resp > 1) {
            message(sprintf("Sequential design will be optimized for 1 respondent, then repeated across %d respondents", n_resp))
        }
    }

    # Check feasibility constraints
    if (n_alts > nrow(profiles)) {
        stop(sprintf(
            "n_alts (%d) cannot be larger than the number of available profiles (%d)",
            n_alts, nrow(profiles)
        ))
    }

    # Validate label constraints
    if (!is.null(label)) {
        if (!label %in% names(profiles)) {
            stop(sprintf("label variable '%s' not found in profiles", label))
        }

        n_label_levels <- length(unique(profiles[[label]]))
        if (n_label_levels != n_alts) {
            stop(sprintf(
                "For labeled designs, number of label levels (%d) must equal n_alts (%d)",
                n_label_levels, n_alts
            ))
        }
    }

    # Validate dominance parameters
    if (remove_dominant) {
        if (is.null(priors)) {
            stop("remove_dominant = TRUE requires priors to be specified")
        }

        valid_dominance_types <- c("total", "partial")
        if (!all(dominance_types %in% valid_dominance_types)) {
            stop(sprintf(
                "dominance_types must be one or more of: %s",
                paste(valid_dominance_types, collapse = ", ")
            ))
        }

        if (!is.numeric(dominance_threshold) || dominance_threshold <= 0 || dominance_threshold >= 1) {
            stop("dominance_threshold must be a numeric value between 0 and 1")
        }

        if (!is.numeric(max_dominance_attempts) || max_dominance_attempts < 1) {
            stop("max_dominance_attempts must be a numeric value >= 1")
        }
    }

    # Check for sufficient combinations
    if (method == "random") {
        if (!is.null(label)) {
            # For labeled designs, check each label group has enough profiles
            label_counts <- table(profiles[[label]])
            min_label_count <- min(label_counts)

            if (min_label_count < n_q) {
                warning(sprintf(
                    "Label group with fewest profiles (%d) has fewer profiles than questions per respondent (%d). May have difficulty generating unique questions.",
                    min_label_count, n_q
                ))
            }
        } else {
            # For random designs, only check n_q (each respondent independent)
            max_possible_questions <- choose(nrow(profiles), n_alts)

            if (n_q > max_possible_questions) {
                stop(sprintf(
                    "Requested %d questions per respondent but only %d unique combinations possible with %d profiles and %d alternatives per question",
                    n_q, max_possible_questions, nrow(profiles), n_alts
                ))
            }
        }
    } else if (method == "sequential") {
        # For sequential designs, check n_q * n_blocks (base design size)
        max_possible_questions <- choose(nrow(profiles), n_alts)
        base_design_questions <- n_q * n_blocks

        if (base_design_questions > max_possible_questions) {
            stop(sprintf(
                "Requested %d questions in base design but only %d unique combinations possible with %d profiles and %d alternatives per question",
                base_design_questions, max_possible_questions, nrow(profiles), n_alts
            ))
        }
    }

    # Validate no-choice with priors
    if (no_choice && !is.null(priors) && !priors$has_no_choice) {
        stop("no_choice = TRUE requires priors to include a no_choice parameter. Use cbc_priors(..., no_choice = value)")
    }
    if (!is.null(priors) && priors$has_no_choice) {
        if (!no_choice) {
            stop("Since priors has a no_choice value, must set no_choice = TRUE in cbc_design()")
        }
    }

    invisible(TRUE)
}

#' Get random parameters from priors object
get_rand_pars <- function(priors) {
    if (is.null(priors)) return(NULL)

    # Extract random parameter names from priors structure
    random_attrs <- names(which(sapply(priors$attrs, function(x) x$random)))
    if (length(random_attrs) == 0) return(NULL)

    # Return in format expected by logitr::recodeData
    rand_pars <- list()
    for (attr in random_attrs) {
        rand_pars[[attr]] <- priors$attrs[[attr]]$dist
    }

    return(rand_pars)
}

#' Compute partial utilities for dominance checking
#'
#' Pre-computes partial utility contribution of each attribute for each profile
#' This allows fast dominance checking by simple matrix lookups and comparisons
#'
#' @param X_matrix Encoded design matrix from logitr::recodeData
#' @param priors cbc_priors object
#' @param attr_names Original attribute names
#' @return Matrix with profiles as rows and attributes as columns
compute_partial_utilities <- function(X_matrix, priors, attr_names) {

    if (is.null(priors)) {
        stop("Priors required for partial dominance checking")
    }

    n_profiles <- nrow(X_matrix)
    partial_utils <- matrix(0, nrow = n_profiles, ncol = length(attr_names))
    colnames(partial_utils) <- attr_names

    # Get parameter names and their mapping to original attributes
    par_names <- names(priors$pars)

    # For each original attribute, sum up the relevant parameter contributions
    for (attr in attr_names) {
        # Find parameters that belong to this attribute
        attr_pars <- par_names[grepl(paste0("^", attr), par_names)]

        if (length(attr_pars) > 0) {
            # Get the column indices in X_matrix for these parameters
            attr_cols_X <- which(colnames(X_matrix) %in% attr_pars)

            if (length(attr_cols_X) > 0) {
                # Compute partial utility for each profile
                X_attr <- X_matrix[, attr_cols_X, drop = FALSE]
                pars_attr <- priors$pars[attr_cols_X]
                partial_utils[, attr] <- X_attr %*% pars_attr
            }
        }
    }

    return(partial_utils)
}

#' Set up label constraints for labeled designs
setup_label_constraints <- function(profiles, label, n_alts) {
    # Split profiles by label values
    label_values <- unique(profiles[[label]])

    if (length(label_values) != n_alts) {
        stop(sprintf("Number of label values (%d) must match n_alts (%d)",
                     length(label_values), n_alts))
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

#' Check for partial dominance between alternatives
#'
#' Uses pre-computed partial utilities matrix for fast dominance checking
#' Checks if any alternative has higher partial utility for ALL attributes
#'
#' @param question_profiles Vector of profileIDs in this question
#' @param opt_env Optimization environment containing partial_utilities matrix
#' @return Logical indicating if partial dominance exists
has_partial_dominance <- function(question_profiles, opt_env) {

    if (is.null(opt_env$partial_utilities)) {
        return(FALSE)
    }

    # Get partial utilities for these profiles
    # Convert profileIDs to row indices (assuming 1-indexed profiles)
    profile_indices <- question_profiles[question_profiles > 0]  # Exclude no-choice

    if (length(profile_indices) < 2) {
        return(FALSE)
    }

    partial_utils <- opt_env$partial_utilities[profile_indices, , drop = FALSE]

    # Check if any alternative dominates on ALL attributes
    n_alts <- nrow(partial_utils)
    n_attrs <- ncol(partial_utils)

    for (i in 1:n_alts) {
        is_best_for_all <- TRUE
        for (j in 1:n_attrs) {
            if (partial_utils[i, j] < max(partial_utils[, j])) {
                is_best_for_all <- FALSE
                break
            }
        }
        if (is_best_for_all) {
            return(TRUE)
        }
    }

    return(FALSE)
}

generate_sequential_design <- function(opt_env, n_alts, n_q, n_blocks, max_iter, n_start, max_attempts) {

    # Set up parallel processing
    n_cores <- set_num_cores(NULL)
    message("Running ", n_start, " design searches using ", n_cores, " cores...")

    # Create list of different random starting designs (base designs only)
    start_designs <- lapply(1:n_start, function(i) {
        generate_initial_random_matrix(opt_env, n_alts, n_q * n_blocks)
    })

    # Run optimization in parallel
    if (Sys.info()[['sysname']] == 'Windows') {
        cl <- parallel::makeCluster(n_cores, "PSOCK")
        # Export necessary functions to cluster
        parallel::clusterExport(cl, c(
            "optimize_design_profileid", "compute_design_d_error_with_nochoice",
            "sample_question_profiles", "add_no_choice_to_design_temp"
        ), envir = environment())

        results <- suppressMessages(suppressWarnings(
            parallel::parLapply(
                cl = cl,
                seq_along(start_designs),
                function(i) {
                    result <- optimize_design_profileid(
                        start_designs[[i]], opt_env, n_alts, n_q, n_blocks, max_iter
                    )
                    result$start_number <- i
                    return(result)
                }
            )
        ))
        parallel::stopCluster(cl)
    } else {
        results <- suppressMessages(suppressWarnings(
            parallel::mclapply(
                seq_along(start_designs),
                function(i) {
                    result <- optimize_design_profileid(
                        start_designs[[i]], opt_env, n_alts, n_q, n_blocks, max_iter
                    )
                    result$start_number <- i
                    return(result)
                },
                mc.cores = n_cores
            )
        ))
    }

    # Find best design based on D-error
    d_errors <- sapply(results, function(x) x$d_error)
    best_index <- which.min(d_errors)
    best_result <- results[[best_index]]

    # Print summary of all starts
    message("\nD-error results from all starts:")
    sorted_results <- sort(d_errors)
    for (i in seq_along(sorted_results)) {
        idx <- which(d_errors == sorted_results[i])[1]
        message(sprintf(
            "Start %d: %.6f %s",
            results[[idx]]$start_number,
            sorted_results[i],
            if(idx == best_index) "  (Best)" else ""
        ))
    }

    return(best_result)
}

optimize_design_profileid <- function(design_matrix, opt_env, n_alts, n_q, n_blocks, max_iter) {

    # Compute initial D-error (temporarily adding no-choice if needed)
    current_d_error <- compute_design_d_error_with_nochoice(design_matrix, opt_env)

    total_questions <- n_q * n_blocks

    for (iter in 1:max_iter) {
        message("Iteration ", iter, ": D-error = ", round(current_d_error, 6))
        improved <- FALSE
        start_d_error <- current_d_error

        # Try improving each position in the design
        for (q in 1:total_questions) {
            for (alt in 1:n_alts) {

                current_profile <- design_matrix[q, alt]
                best_profile <- current_profile
                best_d_error <- current_d_error

                # Try multiple alternative profiles for this position
                n_attempts <- 20  # Number of profiles to try
                for (attempt in 1:n_attempts) {

                    # Sample a new profile using the same constraints as random design
                    if (!is.null(opt_env$label_constraints)) {
                        # For labeled designs, we need to maintain the label structure
                        # Get which label group this alternative position should be
                        label_group_index <- alt
                        if (label_group_index <= length(opt_env$label_constraints$groups)) {
                            eligible_profiles <- opt_env$label_constraints$groups[[label_group_index]]
                            new_profile <- sample(eligible_profiles, 1)
                        } else {
                            next  # Skip if label group doesn't exist
                        }
                    } else {
                        # Regular design: sample any profile
                        new_profile <- sample(opt_env$available_profile_ids, 1)
                    }

                    if (new_profile == current_profile) next

                    # Create test design
                    test_design <- design_matrix
                    test_design[q, alt] <- new_profile

                    # Check basic constraints (no duplicates within question)
                    question_profiles <- test_design[q, ]
                    if (length(unique(question_profiles)) != length(question_profiles)) {
                        next  # Skip if duplicates within question
                    }

                    # Check dominance if required
                    if (opt_env$remove_dominant) {
                        if (has_dominant_alternative(question_profiles, opt_env)) {
                            next  # Skip if dominant alternatives
                        }
                    }

                    # Compute D-error for test design (temporarily adding no-choice)
                    test_d_error <- compute_design_d_error_with_nochoice(test_design, opt_env)

                    # Accept if improvement
                    if (test_d_error < best_d_error) {
                        best_profile <- new_profile
                        best_d_error <- test_d_error
                    }
                }

                # Update design if we found an improvement
                if (best_profile != current_profile) {
                    design_matrix[q, alt] <- best_profile
                    current_d_error <- best_d_error
                    improved <- TRUE
                }
            }
        }

        if (!improved) {
            message("No improvement found, stopping optimization")
            break
        }
    }

    return(list(
        design_matrix = design_matrix,
        d_error = current_d_error,
        method = "sequential",
        iterations = iter
    ))
}

# Helper function to compute D-error with temporary no-choice insertion
compute_design_d_error_with_nochoice <- function(design_matrix, opt_env) {

    if (opt_env$no_choice) {
        # Temporarily add no-choice to compute D-error
        temp_design <- add_no_choice_to_design(design_matrix, ncol(design_matrix))
        return(compute_design_d_error(temp_design, opt_env))
    } else {
        # No no-choice needed
        return(compute_design_d_error(design_matrix, opt_env))
    }
}

#' Add no-choice option to design matrix
#'
#' Adds a no-choice alternative (profileID = 0) as the last alternative in each question
#'
#' @param design_matrix Matrix of profileIDs (questions x alternatives)
#' @param n_alts Current number of alternatives per question
#' @return Updated design matrix with no-choice alternatives added
add_no_choice_to_design <- function(design_matrix, n_alts) {

    n_questions <- nrow(design_matrix)

    # Create new matrix with additional column for no-choice
    new_design_matrix <- matrix(0, nrow = n_questions, ncol = n_alts + 1)

    # Copy existing alternatives
    new_design_matrix[, 1:n_alts] <- design_matrix

    # Add no-choice (profileID = 0) as last alternative
    new_design_matrix[, n_alts + 1] <- 0

    return(new_design_matrix)
}

# Helper function from earlier (already defined)
set_num_cores <- function(n_cores) {
    cores_available <- parallel::detectCores()
    max_cores <- cores_available - 1
    # CRAN checks limits you to 2 cores
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && (chk != "false")) {
        return(2L)
    }
    if (is.null(n_cores)) {
        return(max_cores)
    } else if (!is.numeric(n_cores)) {
        warning("Non-numeric value provided for n_cores...setting n_cores to ", max_cores)
        return(max_cores)
    } else if (n_cores > cores_available) {
        warning("Cannot use ", n_cores, " cores because your machine only has ",
                cores_available, " available...setting n_cores to ", max_cores)
        return(max_cores)
    }
    return(n_cores)
}

#' Repeat base design across respondents with optional randomization
#'
#' Used for sequential designs to take the optimized base design and repeat it
#' across multiple respondents with optional question/alternative randomization
#'
#' @param base_design Base design data frame (for 1 respondent)
#' @param n_resp Number of respondents
#' @param n_alts Number of alternatives per question
#' @param n_q Number of questions per respondent
#' @param n_blocks Number of blocks
#' @param randomize_questions Randomize question order for each respondent
#' @param randomize_alts Randomize alternative order within questions
#' @return Full design data frame for all respondents
repeat_design_across_respondents <- function(
        base_design, n_resp, n_alts, n_q, n_blocks,
        randomize_questions, randomize_alts
) {

    if (n_resp == 1) {
        return(base_design)  # No need to repeat
    }

    # Replicate base design across respondents
    full_design_list <- list()

    for (resp in 1:n_resp) {
        resp_design <- base_design
        resp_design$respID <- resp

        # Randomize question order if requested
        if (randomize_questions) {
            resp_design <- randomize_question_order(resp_design, n_q, n_alts)
        }

        # Randomize alternative order if requested
        if (randomize_alts) {
            resp_design <- randomize_alternative_order(resp_design, n_q, n_alts)
        }

        full_design_list[[resp]] <- resp_design
    }

    # Combine all respondents
    full_design <- do.call(rbind, full_design_list)

    # Update obsID to be unique across all respondents
    total_questions <- n_q * n_resp * n_blocks
    full_design$obsID <- rep(1:total_questions, each = n_alts)

    # Reorder by respID, qID, altID
    full_design <- full_design[order(full_design$respID, full_design$qID, full_design$altID), ]
    row.names(full_design) <- NULL

    return(full_design)
}

#' Randomize question order within a respondent's design
#'
#' @param resp_design Design for one respondent
#' @param n_q Number of questions per respondent
#' @param n_alts Number of alternatives per question
#' @return Design with randomized question order
randomize_question_order <- function(resp_design, n_q, n_alts) {

    # Create new random question order
    new_q_order <- sample(1:n_q)

    # Create mapping from old to new question IDs
    q_map <- data.frame(
        old_qID = 1:n_q,
        new_qID = new_q_order
    )

    # Update question IDs
    resp_design$qID <- q_map$new_qID[match(resp_design$qID, q_map$old_qID)]

    return(resp_design)
}

#' Randomize alternative order within questions
#'
#' @param resp_design Design for one respondent
#' @param n_q Number of questions per respondent
#' @param n_alts Number of alternatives per question
#' @return Design with randomized alternative order
randomize_alternative_order <- function(resp_design, n_q, n_alts) {

    # For each question, randomize alternative order
    for (q in 1:n_q) {
        q_rows <- which(resp_design$qID == q)
        new_alt_order <- sample(1:n_alts)
        resp_design$altID[q_rows] <- new_alt_order
    }

    return(resp_design)
}

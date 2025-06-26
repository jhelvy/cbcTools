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

    time_start <- Sys.time()

    # Validate inputs
    validate_design_inputs(
        profiles, method, priors, n_alts, n_q, n_resp, n_blocks,
        no_choice, label, randomize_questions, randomize_alts,
        remove_dominant, dominance_types, dominance_threshold,
        max_dominance_attempts
    )

    # Override randomize_alts for labeled designs
    if (!is.null(label) && randomize_alts) {
        message("Note: randomize_alts set to FALSE for labeled designs to preserve label structure")
        randomize_alts <- FALSE
    }

    # Set up the optimization environment
    opt_env <- setup_optimization_environment(
        profiles, n_alts, n_q, n_resp, n_blocks, priors, no_choice, label,
        remove_dominant, dominance_types, dominance_threshold,
        max_dominance_attempts
    )

    # Generate design based on method
    if (method == "random") {
        # Override any provided blocks - it's always 1 for random method
        if (n_blocks != 1) {
            message("For 'random' designs, n_blocks is ignored and set to 1")
            n_blocks <- 1
        }

        # Random designs: generate completely random design for all respondents
        design_result <- generate_random_design(opt_env)

        # Convert profileID matrix back to full design
        final_design <- construct_final_design(design_result$design_matrix, opt_env)

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

    # # Move no_choice to the end
    # if (no_choice) {
    #     # Reorder columns to put no_choice at the end
    #     other_cols <- setdiff(names(final_design), "no_choice")
    #     final_design <- final_design[, c(other_cols, "no_choice")]
    # }

    # Add metadata and return
    final <- finalize_design_object(
        final_design, design_result, opt_env, method,
        randomize_questions, randomize_alts, time_start
    )

    return(final)
}

# Set up the optimization environment with pre-computed utilities and proper factor alignment
setup_optimization_environment <- function(
    profiles, n_alts, n_q, n_resp, n_blocks, priors, no_choice, label,
    remove_dominant, dominance_types, dominance_threshold, max_dominance_attempts
) {

    # Get attribute names (excluding profileID)
    attr_names <- setdiff(names(profiles), "profileID")

    # Align factor levels with priors BEFORE encoding
    profiles_aligned <- align_profiles_with_priors(profiles, priors, attr_names)

    # Get random parameters for encoding
    randPars <- get_rand_pars(priors)

    # Encode the profiles using aligned profiles
    coded_data <- logitr::recodeData(profiles_aligned, attr_names, randPars)
    X_matrix <- coded_data$X

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
            priors$pars <- priors$pars[-no_choice_index]
        }

        # Compute utilities for each profile
        if (is_bayesian) {
            # Use draws for Bayesian calculation if exist
            n_draws <- nrow(priors$par_draws)

            # Handle no_choice draws
            if (no_choice) {
                exp_no_choice_draws <- exp(priors$par_draws[, no_choice_index])
                priors$par_draws <- priors$par_draws[, -no_choice_index]
            }

            # Bayesian case with parameter draws
            exp_utilities_draws <- array(dim = c(nrow(profiles), n_draws))

            for (i in 1:n_draws) {
                utilities_i <- X_matrix %*% priors$par_draws[i, ]
                exp_utilities_draws[, i] <- exp(utilities_i)
            }
            if (no_choice) {
                exp_utilities_draws <- rbind(exp_utilities_draws, exp_no_choice_draws)
            }

        } else {
            # Fixed parameters case
            utilities <- X_matrix %*% priors$pars
            exp_utilities <- exp(utilities)
            if (no_choice) {
                exp_utilities <- rbind(exp_utilities, exp_no_choice)
            }
        }

        # Pre-compute partial utilities for dominance checking if needed
        if (remove_dominant && "partial" %in% dominance_types) {
            partial_utilities <- compute_partial_utilities(X_matrix, priors)
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
        questions =n_q*n_resp,
        params = ncol(X_matrix),
        draws = n_draws,
        profiles = nrow(profiles),
        max_attempts = max_dominance_attempts
    )

    # Precompute all ID vectors
    n_alts_total <- ifelse(no_choice, n_alts + 1, n_alts)
    n$alts_total <- n_alts_total
    reps <- rep(n_alts_total, n$questions)
    respID <- rep(1:n_resp, each = n_alts_total*n_q)
    obsID_partials <- rep(1:n$questions, each = n_alts)
    obsID <- rep(1:n$questions, each = n_alts_total)
    altID <- rep(1:n_alts_total, n$questions)

    return(list(
        profiles = profiles_aligned, # Use aligned profiles
        priors = priors,
        has_priors = has_priors,
        attr_names = attr_names,
        exp_utilities = exp_utilities,
        exp_utilities_draws = exp_utilities_draws,
        partial_utilities = partial_utilities,
        X_matrix = X_matrix,
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
        available_profile_ids = profiles$profileID
    ))
}

#' Align profile factor levels with priors before encoding
#'
#' This ensures that logitr::recodeData creates dummy variables that match
#' the prior specifications exactly.
#'
#' @param profiles Original profiles data frame
#' @param priors cbc_priors object or NULL
#' @param attr_names Attribute names to process
#' @return profiles data frame with properly aligned factor levels
align_profiles_with_priors <- function(profiles, priors, attr_names) {
    if (is.null(priors)) {
        return(profiles)  # No alignment needed
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
                            attr, length(level_order), actual_coefs, expected_coefs
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

# Generate a random design using profileID sampling with matrix operations
generate_random_design <- function(opt_env) {

    # Start with a completely random design matrix
    design_matrix <- generate_initial_random_matrix(opt_env)

    # Iteratively fix problems
    attempts <- 0
    while (attempts < opt_env$n$max_attempts) {
        attempts <- attempts + 1

        # Find all problematic questions
        problem_questions <- find_problematic_questions(design_matrix, opt_env)

        if (length(problem_questions) == 0) {
            break  # Design is valid
        }

        # Replace problematic questions
        for (q in problem_questions) {
            design_matrix[q, ] <- sample_question_profiles(opt_env)
        }
    }

    # Find all remaining problematic questions (if any)
    if (length(find_problematic_questions(design_matrix, opt_env)) > 0) {
        warning(sprintf("Could not generate fully valid design after %d attempts", max_attempts))
    }

    return(list(
        design_matrix = design_matrix,
        total_attempts = attempts
    ))
}

#' Generate initial random design matrix
generate_initial_random_matrix <- function(opt_env) {

    n <- opt_env$n
    design_matrix <- matrix(0, nrow = n$questions, ncol = n$alts)

    if (!is.null(opt_env$label_constraints)) {
        # Labeled design: sample one from each label group for each question
        for (q in 1:n$questions) {
            design_matrix[q, ] <- sample_labeled_profiles(opt_env)
        }
    } else {
        # Regular design: sample without replacement for each question
        for (q in 1:n$questions) {
            design_matrix[q, ] <- sample(opt_env$available_profile_ids, n$alts, replace = FALSE)
        }
    }

    return(design_matrix)
}

#' Find all questions that have problems
find_problematic_questions <- function(design_matrix, opt_env) {

    n <- opt_env$n
    problematic <- logical(n$questions)

    # Check for within-question duplicates
    for (q in 1:n$questions) {
        question_profiles <- design_matrix[q, ]
        if (length(unique(question_profiles)) != length(question_profiles)) {
            problematic[q] <- TRUE
        }
    }

    # Check for duplicate questions within each respondent
    for (resp in 1:n$resp) {
        # Get question indices for this respondent
        resp_start <- (resp - 1) * n$q + 1
        resp_end <- resp * n$q
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

    return(which(problematic))
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
    partials <- opt_env$partial_utilities[design_vector,]
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
        if (length(group_rows) > 1) {  # Only check if group has multiple rows
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

get_probs <- function(design_matrix, opt_env) {
    if (opt_env$is_bayesian) {
        return(logit_draws(design_matrix, opt_env))
    }
    return(logit_regular(design_matrix, opt_env))
}

# Helper functions to compute choice probabilities
logit <- function(expV, obsID, reps) {
    sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
    return(expV / sumExpV[rep(seq_along(reps), reps),])
}

get_design_vector <- function(design_matrix) {
    return(as.vector(t(design_matrix)))
}

design_matrix_no_choice <- function(design_matrix, opt_env) {
    return(cbind(
        design_matrix,
        matrix(opt_env$n$profiles + 1, opt_env$n$questions)
    ))
}

logit_regular <- function(design_matrix, opt_env) {
    if (opt_env$no_choice) {
        design_matrix <- design_matrix_no_choice(design_matrix, opt_env)
    }
    design_vector <- get_design_vector(design_matrix)
    expV <- opt_env$exp_utilities[design_vector,]
    return(logit(expV, opt_env$obsID, opt_env$reps))
}

logit_draws <- function(design_matrix, opt_env) {
    if (opt_env$no_choice) {
        design_matrix <- design_matrix_no_choice(design_matrix, opt_env)
    }
    design_vector <- get_design_vector(design_matrix)
    expVDraws <- opt_env$exp_utilities_draws[design_vector,]
    logitDraws <- logit(expVDraws, opt_env$obsID, opt_env$reps)
    return(logitDraws)
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

# Compute D-error for entire design using profileID matrix
compute_design_d_error <- function(design_matrix, opt_env) {
    X_list <- make_X_list(design_matrix, opt_env)
    probs <- get_probs(design_matrix, opt_env)
    if (opt_env$is_bayesian) {
        # here probs is a matrix of prob draws
        return(compute_db_error(X_list, probs, opt_env))
    }
    return(compute_d_error(X_list, probs, opt_env))
}

# Compute D-error for entire design using profileID matrix
compute_design_d_error_null <- function(design_matrix, opt_env) {
    n <- opt_env$n
    X_list <- make_X_list(design_matrix, opt_env)
    probs <- rep(1/n$alts_total, n$questions*n$alts_total)
    return(compute_d_error(X_list, probs, opt_env))
}

make_X_list <- function(design_matrix, opt_env) {
    X_design <- opt_env$X_matrix
    if (opt_env$no_choice) {
        design_matrix <- design_matrix_no_choice(design_matrix, opt_env)
        X_design <- rbind(X_design, X_design[1,]*0)
    }
    design_vector <- get_design_vector(design_matrix)
    X_design <- X_design[design_vector, ]
    X_list <- split(as.data.frame(X_design), opt_env$obsID)
    X_list <- lapply(X_list, as.matrix)
    return(X_list)
}

compute_d_error <- function(X_list, probs, opt_env) {
    P_list <- split(probs, opt_env$obsID)

    # Compute information matrix
    M <- compute_info_matrix(X_list, P_list)

    # Compute D-error
    det_M <- det(M)
    if (det_M <= 0) {
        return(Inf)
    }

    K <- opt_env$n$params
    return(det_M^(-1/K))
}

compute_info_matrix <- function(X_list, P_list) {
    M <- Reduce(`+`, Map(function(x, p) {
        p_mat <- diag(p) - tcrossprod(p)
        crossprod(x, p_mat %*% x)
    }, X_list, P_list))
    return(M)
}

compute_db_error <- function(X_list, probs, opt_env) {
    n_draws <- opt_env$n$draws

    # Compute D-error for each draw
    d_errors <- apply(probs, 2, function(x) {
        compute_d_error(X_list, x, opt_env)
    })

    # DB-error is average of DP-errors
    d_error <- mean(d_errors)
    return(d_error)
}

# Convert profileID design matrix to full design data frame using existing encoded matrix
construct_final_design <- function(design_matrix, opt_env) {

    n <- opt_env$n

    if (opt_env$no_choice) {
        design_matrix <- design_matrix_no_choice(design_matrix, opt_env)
    }

    # Initialize design data frame with ID columns
    design <- data.frame(
        profileID = as.vector(t(design_matrix)),
        blockID = rep(rep(1:n$blocks, each = n$q), n$resp * n$alts_total),
        respID = rep(rep(1:n$resp, each = n$q * n$blocks), each = n$alts_total),
        qID = rep(rep(1:n$q, n$resp * n$blocks), each = n$alts_total),
        altID = rep(1:n$alts_total, n$questions),
        obsID = rep(1:n$questions, each = n$alts_total)
    )

    # Create lookup table from X_matrix (which is already encoded)
    # X_matrix rows correspond to profiles$profileID order
    X_df <- as.data.frame(opt_env$X_matrix)
    X_df$profileID <- opt_env$profiles$profileID

    # Handle no-choice option if present
    if (opt_env$no_choice) {
        # Add no-choice row (all parameters = 0)
        no_choice_row <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(opt_env$X_matrix)))
        names(no_choice_row) <- names(X_df)[1:ncol(opt_env$X_matrix)]
        no_choice_row$profileID <- n$profiles + 1
        no_choice_row$no_choice <- 1  # Add no_choice indicator

        # Add no_choice column to regular profiles (set to 0)
        X_df$no_choice <- 0

        # Combine
        X_df <- rbind(X_df, no_choice_row)
    }

    # Join design with encoded attributes
    design <- merge(design, X_df, by = "profileID", sort = FALSE)

    # Reorder columns and rows
    id_cols <- c("profileID", "blockID", "qID", "altID", "obsID")
    attr_cols <- setdiff(names(design), id_cols)
    design <- design[, c(id_cols, attr_cols)]
    design <- design[order(design$obsID, design$altID), ]
    row.names(design) <- NULL

    # Store categorical structure for potential decoding
    categorical_structure <- get_categorical_structure_from_profiles(opt_env$profiles, opt_env$attr_names)
    attr(design, "categorical_structure") <- categorical_structure
    attr(design, "is_dummy_coded") <- TRUE

    return(design)
}

#' Get categorical structure information from original profiles
#'
#' @param profiles Original profiles data frame
#' @param attr_names Attribute names
#' @return List containing information about categorical variables
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
                    reference_level = levels_order[1]  # First level is reference
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

# Finalize the design object with metadata including both D-errors
finalize_design_object <- function(
    design, design_result, opt_env, method,
    randomize_questions, randomize_alts, time_start
) {

    design_matrix <- design_result$design_matrix

    # Build design_params list
    n <- opt_env$n
    design_params <- list(
        method = method,
        n_q = n$q,
        n_alts = n$alts,
        n_resp = n$resp,
        n_blocks = n$blocks,
        no_choice = opt_env$no_choice,
        label = opt_env$label,
        randomize_questions = if (method == "sequential") randomize_questions else NA,
        randomize_alts = if (method == "sequential") randomize_alts else NA,
        created_at = Sys.time(),
        remove_dominant = opt_env$remove_dominant,
        dominance_types = if (opt_env$remove_dominant) opt_env$dominance_types else NULL,
        dummy_coded = TRUE
    )

    # Add D-error information (both null and prior-based)
    if (method == "sequential" && design_result$repeated_across_respondents) {
        # For sequential designs, compute both D-errors for base and full survey

        # Base design D-errors (from optimization)
        base_d_errors <- compute_both_d_errors(design_result$design_matrix, opt_env)

        # Full survey D-errors
        full_d_errors <- compute_both_d_errors_from_survey(design, opt_env)

        # Store all D-error information
        design_params$d_error_base_null <- base_d_errors$null_d_error
        design_params$d_error_full_null <- full_d_errors$null_d_error

        if (base_d_errors$has_priors) {
            design_params$d_error_base_prior <- base_d_errors$prior_d_error
            design_params$d_error_full_prior <- full_d_errors$prior_d_error
            design_params$d_error <- full_d_errors$prior_d_error  # Primary D-error (prior-based)
        } else {
            design_params$d_error <- full_d_errors$null_d_error   # Primary D-error (null)
        }

    } else {
        # For random designs, compute both D-errors
        d_errors <- compute_both_d_errors(design_matrix, opt_env)

        design_params$d_error_null <- d_errors$d_error_null

        if (opt_env$has_priors) {
            design_params$d_error_prior <- d_errors$d_error_prior
            design_params$d_error <- d_errors$d_error_prior  # Primary D-error (prior-based)
        } else {
            design_params$d_error <- d_errors$d_error_null   # Primary D-error (null)
        }
    }

    # Store metadata
    attr(design, "profiles") <- opt_env$profiles
    attr(design, "priors") <- opt_env$priors
    time_stop <- Sys.time()
    design_params$time_elapsed_sec <- as.numeric(time_stop - time_start)
    attr(design, "design_params") <- design_params

    # Calculate summary statistics
    n_profiles_used <- length(unique(design$profileID[design$profileID != 0]))

    attr(design, "design_summary") <- list(
        n_profiles_available = n$profiles,
        n_profiles_used = n_profiles_used,
        profile_usage_rate = n_profiles_used / n$profiles,
        n_choice_sets = n$blocks * n$q * n$resp,
        method_specific = if (method == "random") {
            list(
                total_questions_generated = n$questions,
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

# Helper functions ----

# Validates all inputs to ensure they meet requirements for design generation
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
#' @return Matrix with profiles as rows and attributes as columns
compute_partial_utilities <- function(X_matrix, priors) {

    n_profiles <- nrow(X_matrix)

    # Compute mean partial utility across par draws if exist
    par_draws <- priors$par_draws
    n_draws <- nrow(par_draws)
    if (!is.null(par_draws)) {
        pars <- par_draws[1,]
        partials <- compute_partial_utility_single(X_matrix, pars, n_profiles)
        for (i in 2:n_draws) {
            pars <- par_draws[i,]
            partials <- partials + compute_partial_utility_single(X_matrix, pars, n_profiles)
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
    return(X_matrix*par_mat)
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
            "optimize_design_profileid", "sample_question_profiles"
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
    current_d_error <- compute_design_d_error(design_matrix, opt_env)

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
                    test_d_error <- compute_design_d_error(test_design, opt_env)

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

#' Repeat base design across respondents with optional randomization (Fixed for no-choice)
#'
#' Used for sequential designs to take the optimized base design and repeat it
#' across multiple respondents with optional question/alternative randomization
#'
#' @param base_design Base design data frame (for 1 respondent)
#' @param n_resp Number of respondents
#' @param n_alts Number of regular alternatives per question (excluding no-choice)
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

    # Determine actual number of alternatives in the base design
    actual_n_alts <- max(base_design$altID)
    has_no_choice <- "no_choice" %in% names(base_design)

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
            # Pass the original n_alts (excluding no-choice)
            resp_design <- randomize_alternative_order(resp_design, n_q, n_alts)
        }

        full_design_list[[resp]] <- resp_design
    }

    # Combine all respondents
    full_design <- do.call(rbind, full_design_list)

    # Update obsID to be unique across all respondents
    # Calculate total questions based on actual alternatives in base design
    total_questions <- n_q * n_resp * n_blocks
    full_design$obsID <- rep(1:total_questions, each = actual_n_alts)

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

#' Randomize alternative order within questions (Fixed for no-choice)
#'
#' @param resp_design Design for one respondent
#' @param n_q Number of questions per respondent
#' @param n_alts Number of regular alternatives per question (excluding no-choice)
#' @return Design with randomized alternative order
randomize_alternative_order <- function(resp_design, n_q, n_alts) {

    # Check if design has no-choice option
    has_no_choice <- "no_choice" %in% names(resp_design)

    # For each question, randomize alternative order
    for (q in 1:n_q) {
        q_rows <- which(resp_design$qID == q)

        if (has_no_choice) {
            # Separate regular alternatives from no-choice
            regular_rows <- q_rows[resp_design$altID[q_rows] <= n_alts]
            nochoice_rows <- q_rows[resp_design$altID[q_rows] > n_alts]

            # Only randomize the regular alternatives
            if (length(regular_rows) > 0) {
                new_alt_order <- sample(1:n_alts)
                resp_design$altID[regular_rows] <- new_alt_order
            }

            # Keep no-choice alternative in last position
            if (length(nochoice_rows) > 0) {
                resp_design$altID[nochoice_rows] <- n_alts + 1
            }
        } else {
            # No no-choice: randomize all alternatives
            new_alt_order <- sample(1:n_alts)
            resp_design$altID[q_rows] <- new_alt_order
        }
    }

    return(resp_design)
}

#' Convert dummy-coded design back to categorical format
#'
#' This function converts a dummy-coded CBC design back to its original
#' categorical format. This is useful for displaying choice questions to
#' respondents or for analysis that requires categorical variables. Only
#' works for designs without no-choice options, as no-choice designs cannot
#' be meaningfully converted back to categorical format.
#'
#' @param design A `cbc_design` object with dummy-coded categorical variables
#' @return A `cbc_design` object with categorical variables restored to their original format
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles with categorical variables
#' profiles <- cbc_profiles(
#'   price = c(10, 20, 30),
#'   quality = c("Low", "Medium", "High"),
#'   brand = c("A", "B")
#' )
#'
#' # Create design (will be dummy-coded by default)
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # View dummy-coded design
#' head(design)
#'
#' # Convert back to categorical format
#' design_categorical <- cbc_decode_design(design)
#' head(design_categorical)
cbc_decode_design <- function(design) {
    # Check input class
    if (!inherits(design, "cbc_design")) {
        stop("Input must be a cbc_design object created by cbc_design()")
    }

    # Check if design has no-choice option
    design_params <- attr(design, "design_params")
    if (!is.null(design_params) && design_params$no_choice) {
        stop(
            "Cannot convert designs with no-choice option back to categorical format.\n",
            "No-choice designs must remain dummy-coded to maintain consistency."
        )
    }

    # Check if design is dummy-coded
    if (!is_dummy_coded(design)) {
        message("Design is already in categorical format.")
        return(design)
    }

    # Get categorical structure information
    categorical_structure <- attr(design, "categorical_structure")
    if (is.null(categorical_structure)) {
        warning("No categorical structure information found. Design may already be decoded or created with an older version.")
        return(design)
    }

    # Apply decoding
    decoded_design <- decode_categorical_variables(design, categorical_structure)

    # Update attributes
    attr(decoded_design, "is_dummy_coded") <- FALSE

    # Copy over all other design attributes
    design_attrs <- names(attributes(design))
    design_attrs <- setdiff(design_attrs, c("names", "class", "row.names", "is_dummy_coded"))

    for (attr_name in design_attrs) {
        attr(decoded_design, attr_name) <- attr(design, attr_name)
    }

    class(decoded_design) <- class(design)
    return(decoded_design)
}

# Helper functions needed for cbc_decode_design

#' Check if design is dummy-coded
#' @param design Design object to check
#' @return Logical indicating if design is dummy-coded
is_dummy_coded <- function(design) {
    is_coded <- attr(design, "is_dummy_coded")
    if (is.null(is_coded)) {
        # If no attribute, try to infer from column names
        # Look for column names that suggest dummy coding (e.g., "qualityHigh", "brandB")
        categorical_structure <- attr(design, "categorical_structure")
        if (!is.null(categorical_structure)) {
            categorical_attrs <- names(categorical_structure)[
                sapply(categorical_structure, function(x) x$is_categorical)
            ]

            # Check if any original categorical column names are missing
            missing_categoricals <- setdiff(categorical_attrs, names(design))
            if (length(missing_categoricals) > 0) {
                return(TRUE)  # Likely dummy-coded if original categorical columns are missing
            }
        }
        return(FALSE)
    }
    return(is_coded)
}

#' Decode categorical variables from dummy coding
#' @param design Design data frame with dummy-coded variables
#' @param categorical_structure Information about original categorical structure
#' @return Design data frame with categorical variables restored
decode_categorical_variables <- function(design, categorical_structure) {

    decoded_design <- design

    # Get categorical attributes
    categorical_attrs <- names(categorical_structure)[
        sapply(categorical_structure, function(x) x$is_categorical)
    ]

    for (attr in categorical_attrs) {
        levels_info <- categorical_structure[[attr]]
        levels_order <- levels_info$levels
        reference_level <- levels_info$reference_level
        non_ref_levels <- setdiff(levels_order, reference_level)

        # Find dummy columns for this attribute
        dummy_cols <- paste0(attr, non_ref_levels)
        existing_dummy_cols <- intersect(dummy_cols, names(design))

        if (length(existing_dummy_cols) > 0) {
            # Reconstruct categorical variable
            n_rows <- nrow(design)
            categorical_values <- rep(reference_level, n_rows)

            # Set non-reference levels based on dummy variables
            for (dummy_col in existing_dummy_cols) {
                level_name <- gsub(paste0("^", attr), "", dummy_col)
                is_this_level <- design[[dummy_col]] == 1
                categorical_values[is_this_level] <- level_name
            }

            # Add reconstructed categorical variable
            decoded_design[[attr]] <- factor(categorical_values, levels = levels_order)

            # Remove dummy columns
            decoded_design[existing_dummy_cols] <- NULL
        }
    }

    return(decoded_design)
}

#' Compute D-error from a full survey design data frame
#'
#' This is a wrapper function that converts a full survey design back to
#' design matrix format and then computes the D-error. Useful for calculating
#' D-error of the full survey after it's been constructed.
#'
#' @param survey_design Full survey design data frame with profileID, obsID, altID columns
#' @param opt_env Optimization environment containing X_matrix and other parameters
#' @return Numeric D-error value for the full survey
compute_design_d_error_from_survey <- function(survey_design, opt_env) {
    # Get regular (non-no-choice) rows
    if ("no_choice" %in% names(survey_design)) {
        regular_rows <- survey_design[survey_design$profileID != 0, ]
    } else {
        regular_rows <- survey_design
    }

    # Get unique observation IDs and determine matrix dimensions
    unique_obs <- sort(unique(regular_rows$obsID))
    n_alts <- max(regular_rows$altID[regular_rows$profileID != 0])

    # Rebuild design matrix from survey data
    design_matrix <- matrix(0, nrow = length(unique_obs), ncol = n_alts)

    for (i in seq_along(unique_obs)) {
        obs_data <- regular_rows[regular_rows$obsID == unique_obs[i], ]
        obs_data <- obs_data[order(obs_data$altID), ]  # Ensure proper order
        design_matrix[i, ] <- obs_data$profileID[1:n_alts]
    }

    # Use existing compute_design_d_error function
    return(compute_design_d_error(design_matrix, opt_env))
}

# Compute both null and prior-based D-errors for a design matrix
compute_both_d_errors <- function(design_matrix, opt_env) {

    # Always compute null D-error (no priors, equal probabilities)
    d_error_null <- compute_design_d_error_null(design_matrix, opt_env)

    # Compute prior-based D-error if priors are available
    d_error_prior <- NULL
    if (opt_env$has_priors) {
        d_error_prior <- compute_design_d_error(design_matrix, opt_env)
    }

    return(list(
        d_error_null = d_error_null,
        d_error_prior = d_error_prior
    ))
}

#' Compute both D-errors from a full survey design data frame
#'
#' @param survey_design Full survey design data frame
#' @param opt_env Optimization environment
#' @return List with null_d_error and prior_d_error (if available)
compute_both_d_errors_from_survey <- function(survey_design, opt_env) {

    # Get regular (non-no-choice) rows
    if ("no_choice" %in% names(survey_design)) {
        regular_rows <- survey_design[survey_design$profileID != 0, ]
    } else {
        regular_rows <- survey_design
    }

    # Get unique observation IDs and determine matrix dimensions
    unique_obs <- sort(unique(regular_rows$obsID))
    n_alts <- max(regular_rows$altID[regular_rows$profileID != 0])

    # Rebuild design matrix from survey data
    design_matrix <- matrix(0, nrow = length(unique_obs), ncol = n_alts)

    for (i in seq_along(unique_obs)) {
        obs_data <- regular_rows[regular_rows$obsID == unique_obs[i], ]
        obs_data <- obs_data[order(obs_data$altID), ]  # Ensure proper order
        design_matrix[i, ] <- obs_data$profileID[1:n_alts]
    }

    # Compute both D-errors
    return(compute_both_d_errors(design_matrix, opt_env))
}

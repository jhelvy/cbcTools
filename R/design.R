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
#' @param n_cores Number of cores to use for parallel processing in the design search.
#' Defaults to NULL, in which case it is set to the number of available cores minus 1.
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
        profiles, method, time_start,
        n_alts, n_q, n_resp, n_blocks, n_cores, n_start, max_iter,
        priors, no_choice, label,
        remove_dominant, dominance_types, dominance_threshold,
        max_dominance_attempts, randomize_questions, randomize_alts
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
        design_matrix <- design_result$design_matrix

        # Convert profileID matrix back to full design
        final_design <- construct_final_design(design_matrix, opt_env)

    } else if (method == "sequential") {
        # Sequential designs: create optimized base design, then repeat across respondents
        design_result <- generate_sequential_design(opt_env)
        design_matrix <- design_result$design_matrix

        # Convert base design to full design
        base_design <- construct_final_design(design_matrix, opt_env)

        # Repeat and randomize across respondents
        final_design <- repeat_design_across_respondents(base_design, opt_env)
    }

    # Add metadata and return
    final <- finalize_design_object(final_design, design_result, opt_env)

    return(final)
}

# Set up the optimization environment with pre-computed utilities and proper factor alignment
setup_optimization_environment <- function(
    profiles, method, time_start,
    n_alts, n_q, n_resp, n_blocks, n_cores, n_start, max_iter,
    priors, no_choice, label,
    remove_dominant, dominance_types, dominance_threshold,
    max_dominance_attempts, randomize_questions, randomize_alts
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
        start = n_start,
        cores = set_num_cores(n_cores),
        questions = ifelse(method == 'random', n_q*n_resp, n_q*n_blocks),
        params = ncol(X_matrix),
        draws = n_draws,
        profiles = nrow(profiles),
        max_iter = max_iter,
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
        method = method,
        time_start = time_start,
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
        available_profile_ids = profiles$profileID,
        randomize_questions = randomize_questions,
        randomize_alts = randomize_alts
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

# Generate initial random design matrix
generate_initial_random_matrix <- function(
    opt_env, n_questions = NULL, n_alts = NULL
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
            design_matrix[q, ] <- sample(opt_env$available_profile_ids, n$alts, replace = FALSE)
        }
    }

    return(design_matrix)
}

# Find all questions that have problems
find_problematic_questions <- function(design_matrix, opt_env) {

    n <- opt_env$n
    problematic <- logical(n$questions)

    # Check for within-question duplicates
    problematic <- check_problem_question_dupes(design_matrix, opt_env, problematic)

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
        matrix(opt_env$n$profiles + 1, nrow(design_matrix))
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

# Compute D-error for entire design using profileID matrix
compute_design_d_error <- function(design_matrix, opt_env) {
    if (!opt_env$has_priors) {
        return(compute_design_d_error_null(design_matrix, opt_env))
    }
    X_list <- make_X_list(design_matrix, opt_env)
    probs <- get_probs(design_matrix, opt_env)
    if (opt_env$is_bayesian) {
        # here probs is a matrix of prob draws
        return(compute_db_error(X_list, probs, opt_env$obsID))
    }
    return(compute_d_error(X_list, probs, opt_env$obsID))
}

compute_d_error <- function(X_list, probs, obsID) {
    P_list <- split(probs, obsID)

    # Compute information matrix
    M <- compute_info_matrix(X_list, P_list)

    # Compute D-error
    det_M <- det(M)
    if (det_M <= 0) {
        return(Inf)
    }

    K <- ncol(X_list[[1]])
    return(det_M^(-1/K))
}

compute_info_matrix <- function(X_list, P_list) {
    M <- Reduce(`+`, Map(function(x, p) {
        p_mat <- diag(p) - tcrossprod(p)
        crossprod(x, p_mat %*% x)
    }, X_list, P_list))
    return(M)
}

# Compute D-error for entire design using profileID matrix
compute_design_d_error_null <- function(design_matrix, opt_env) {
    n <- opt_env$n
    X_list <- make_X_list(design_matrix, opt_env)
    probs <- rep(1/n$alts_total, nrow(design_matrix)*n$alts_total)
    return(compute_d_error(X_list, probs, opt_env$obsID))
}

compute_db_error <- function(X_list, probs, obsID) {
    # Compute D-error for each draw
    d_errors <- apply(probs, 2, function(x) {
        compute_d_error(X_list, x, obsID)
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

    # Initialize design data frame
    design <- data.frame(profileID = as.vector(t(design_matrix)))
    if (opt_env$method == 'random') {
        design <- add_metadata_random(design, n$resp, n$alts_total, n$q)
        id_cols <- c("profileID", "respID", "qID", "altID", "obsID")
    } else {
        design <- add_metadata_other(design, n$blocks, n$alts_total, n$q)
        id_cols <- c("profileID", "blockID", "qID", "altID", "obsID")
    }

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

add_metadata_random <- function(design, n_resp, n_alts, n_q) {
    design$respID <- rep(seq(n_resp), each = n_alts * n_q)
    design$qID     <- rep(rep(seq(n_q), each = n_alts), n_resp)
    design$altID   <- rep(seq(n_alts), n_resp * n_q)
    design$obsID   <- rep(seq(n_resp * n_q), each = n_alts)
    return(design)
}

add_metadata_other <- function(design, n_block, n_alts, n_q) {
    design$blockID <- rep(seq(n_block), each = n_alts * n_q)
    design$qID     <- rep(rep(seq(n_q), each = n_alts), n_block)
    design$altID   <- rep(seq(n_alts), n_block * n_q)
    design$obsID   <- rep(seq(n_block * n_q), each = n_alts)
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
    overall_balance <- mean(sapply(balance_metrics, function(x) x$balance_score))

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
    overlap_counts <- lapply(atts, function(attr) get_att_overlap_counts(attr, design))
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
        randomize_questions = if (method == "random") NA else opt_env$randomize_questions,
        randomize_alts = if (method == "random") NA else opt_env$randomize_alts,
        created_at = Sys.time(),
        remove_dominant = opt_env$remove_dominant,
        dominance_types = if (opt_env$remove_dominant) opt_env$dominance_types else NULL,
        dummy_coded = TRUE
    )

    # Add D-error information (both null and prior-based)

    if (method == "sequential") {
        # For sequential designs, compute both D-errors for base and full survey

        # Always compute null D-error (no priors, equal probabilities)
        design_params$d_error_null <- compute_design_d_error_null(
            design_result$design_matrix, opt_env
        )

        # Compute prior-based D-error if priors are available
        design_params$d_error_prior <- NULL
        if (opt_env$has_priors) {
            design_params$d_error_prior <- compute_design_d_error(
                design_result$design_matrix, opt_env
            )
        }
    }

    # Pre-compute efficiency metrics
    efficiency_metrics <- compute_design_efficiency_metrics(design)

    # Store metadata
    attr(design, "profiles") <- opt_env$profiles
    attr(design, "priors") <- opt_env$priors
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

generate_sequential_design <- function(opt_env) {

    # Set up parallel processing
    n <- opt_env$n
    message("Running ", n$start, " design searches using ", n$cores, " cores...")

    # Create list of different random starting designs (base designs only)
    start_designs <- lapply(1:n$start, function(i) {
        generate_initial_random_matrix(opt_env)
    })

    # Run optimization in parallel
    if (Sys.info()[['sysname']] == 'Windows') {
        cl <- parallel::makeCluster(n$cores, "PSOCK")
        # Export necessary functions to cluster
        parallel::clusterExport(cl, c(
            "optimize_design", "sample_question_profiles",
            "compute_design_d_error", "find_problematic_questions"
        ), envir = environment())

        results <- suppressMessages(suppressWarnings(
            parallel::parLapply(
                cl = cl,
                seq_along(start_designs),
                function(i) {
                    result <- optimize_design(start_designs[[i]], opt_env)
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
                    result <- optimize_design(start_designs[[i]], opt_env)
                    result$start_number <- i
                    return(result)
                },
                mc.cores = n$cores
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

optimize_design <- function(design_matrix, opt_env) {

    # Compute initial D-error
    current_d_error <- compute_design_d_error(
        design_matrix, opt_env
    )

    n <- opt_env$n
    for (iter in 1:n$max_iter) {
        message("Iteration ", iter, ": D-error = ", round(current_d_error, 6))
        improved <- FALSE
        start_d_error <- current_d_error

        # Try improving each position in the design
        for (q in 1:n$questions) {
            for (alt in 1:n$alts) {

                current_profile <- design_matrix[q, alt]
                best_profile <- current_profile
                best_d_error <- current_d_error

                # Try multiple alternative profiles for this position
                n_attempts <- length(opt_env$available_profile_ids)  # Number of profiles to try
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

                    # Check all constraints
                    problem_questions <- find_problematic_questions(test_design, opt_env)
                    if (length(problem_questions) > 0) {
                        if (q %in% problem_questions) {
                            next
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
        total_attempts = iter
    ))
}

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

# Repeat base design across respondents with block allocation (Fixed)
# Used for sequential designs to take the optimized base design and repeat it
# across multiple respondents with proper block allocation and optional randomization
repeat_design_across_respondents <- function(base_design, opt_env) {
    n <- opt_env$n
    if (n$resp == 1) {
        return(base_design)  # No need to repeat
    }

    # For multi-block designs, allocate respondents to blocks
    if (n$blocks > 1) {
        # Split base design by block
        block_designs <- split(base_design, base_design$blockID)
        # Allocate respondents proportionally across blocks
        resp_per_block <- allocate_respondents_to_blocks(n$resp, n$blocks)
        full_design_list <- list()
        resp_counter <- 1

        for (block_id in 1:n$blocks) {
            block_design <- block_designs[[as.character(block_id)]]
            n_resp_this_block <- resp_per_block[block_id]

            # Assign respondents to this block
            for (resp_in_block in 1:n_resp_this_block) {
                resp_design <- block_design
                resp_design$respID <- resp_counter
                # Keep blockID to track which design block this respondent saw
                resp_design$blockID <- block_id

                # Randomize question order if requested
                if (opt_env$randomize_questions) {
                    resp_design <- randomize_question_order(resp_design, n$q, n$alts_total)
                }

                # Randomize alternative order if requested
                if (opt_env$randomize_alts) {
                    # Pass the original n_alts (excluding no-choice)
                    resp_design <- randomize_alternative_order(resp_design, opt_env$no_choice, n$q, n$alts)
                }

                full_design_list[[resp_counter]] <- resp_design
                resp_counter <- resp_counter + 1
            }
        }
    } else {
        # Single block case (original logic)
        full_design_list <- list()

        for (resp in 1:n$resp) {
            resp_design <- base_design
            resp_design$respID <- resp

            # Randomize question order if requested
            if (opt_env$randomize_questions) {
                resp_design <- randomize_question_order(resp_design, n$q, n$alts_total)
            }

            # Randomize alternative order if requested
            if (opt_env$randomize_alts) {
                # Pass the original n_alts (excluding no-choice)
                resp_design <- randomize_alternative_order(resp_design, opt_env$no_choice, n$q, n$alts)
            }

            full_design_list[[resp]] <- resp_design
        }
    }

    # Combine all respondents
    full_design <- do.call(rbind, full_design_list)

    # Now re-sort for potentially randomized row order
    full_design <- full_design[order(full_design$respID, full_design$qID, full_design$altID), ]
    row.names(full_design) <- NULL

    # Regenerate IDs based on current (potentially randomized) row order
    total_questions <- n$q * n$resp
    full_design$obsID <- rep(1:total_questions, each = n$alts_total)

    # Reorder columns
    full_design <- full_design[,c(get_id_names(), get_var_names(full_design))]

    return(full_design)
}

# Helper function to allocate respondents proportionally across blocks
allocate_respondents_to_blocks <- function(n_resp, n_blocks) {
    # Calculate base allocation
    base_per_block <- floor(n_resp / n_blocks)
    remainder <- n_resp %% n_blocks

    # Allocate respondents
    resp_per_block <- rep(base_per_block, n_blocks)

    # Distribute remainder randomly across blocks
    if (remainder > 0) {
        extra_blocks <- sample(1:n_blocks, remainder)
        resp_per_block[extra_blocks] <- resp_per_block[extra_blocks] + 1
    }

    return(resp_per_block)
}

# Randomize question order within a respondent's design
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

# Randomize alternative order within questions (Fixed for no-choice)
randomize_alternative_order <- function(resp_design, no_choice, n_q, n_alts) {

    # For each question, randomize alternative order
    for (q in 1:n_q) {
        q_rows <- which(resp_design$qID == q)

        if (no_choice) {
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

get_design_matrix_from_survey <- function(survey_design, opt_env) {

    # Get regular (non-no-choice) rows
    if (opt_env$no_choice) {
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

    return(design_matrix)
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

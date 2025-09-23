# ===== MAIN GREEDY DESIGN GENERATION =====

# Generate greedy design (shortcut, minoverlap, or balanced)
generate_greedy_design <- function(opt_env) {
    n <- opt_env$n
    method <- opt_env$method # "shortcut", "minoverlap", or "balanced"
    attr_names <- opt_env$attr_names
    profiles <- opt_env$profiles

    message(
        "Generating ",
        method,
        " design for ",
        n$resp,
        " respondents using ",
        n$cores,
        " cores..."
    )

    # Pre-compute encoded profiles and tracker systems
    encoded_profiles <- get_encoded_profiles(profiles)
    trackers_init <- initialize_greedy_trackers_matrix(
        attr_names,
        profiles,
        encoded_profiles,
        method
    )

    # Generate designs and track final overall state
    if (n$cores == 1 || n$resp == 1) {
        # Sequential execution - track cumulative state
        resp_results <- list()
        overall_trackers <- trackers_init

        for (resp in 1:n$resp) {
            result <- generate_greedy_for_respondent_with_tracking(
                resp,
                overall_trackers,
                opt_env,
                encoded_profiles
            )
            resp_results[[resp]] <- result
            overall_trackers <- result$final_trackers
        }

        final_overall_trackers <- overall_trackers
        resp_designs <- lapply(resp_results, function(x) x$design)
    } else {
        # Parallel execution - each worker returns design + tracker state
        if (Sys.info()[['sysname']] == 'Windows') {
            cl <- parallel::makeCluster(min(n$cores, n$resp), "PSOCK")
            # Export necessary functions and objects to cluster
            parallel::clusterExport(
                cl,
                c(
                    "generate_greedy_for_respondent_with_tracking",
                    "initialize_greedy_trackers_matrix",
                    "update_greedy_trackers_matrix",
                    "select_profile_greedy",
                    "calculate_greedy_score_matrix",
                    "get_pairwise_score_matrix",
                    "get_overlap_score_matrix",
                    "get_frequency_score_matrix",
                    "initialize_frequency_tracker_matrix",
                    "update_frequency_tracker_matrix",
                    "initialize_pairwise_tracker_matrix",
                    "initialize_overlap_tracker_matrix",
                    "update_pairwise_tracker_matrix",
                    "update_overlap_tracker_matrix",
                    "get_eligible_profiles_list_greedy",
                    "get_encoded_profiles",
                    "reset_question_trackers_matrix"
                ),
                envir = environment()
            )

            resp_results <- suppressMessages(suppressWarnings(
                parallel::parLapply(cl, 1:n$resp, function(resp) {
                    generate_greedy_for_respondent_with_tracking(
                        resp,
                        trackers_init,
                        opt_env,
                        encoded_profiles
                    )
                })
            ))
            parallel::stopCluster(cl)
        } else {
            resp_results <- suppressMessages(suppressWarnings(
                parallel::mclapply(
                    1:n$resp,
                    function(resp) {
                        generate_greedy_for_respondent_with_tracking(
                            resp,
                            trackers_init,
                            opt_env,
                            encoded_profiles
                        )
                    },
                    mc.cores = min(n$cores, n$resp)
                )
            ))
        }

        # Combine all tracker states to get final overall state
        final_overall_trackers <- combine_tracker_states(resp_results, method)
        resp_designs <- lapply(resp_results, function(x) x$design)
    }

    # Combine all respondent designs into a single matrix
    design_matrix <- do.call(rbind, resp_designs)

    # Post-process for dominance removal if requested
    if (opt_env$remove_dominant && opt_env$has_priors) {
        design_matrix <- remove_dominant_questions_greedy(
            design_matrix,
            opt_env,
            encoded_profiles,
            final_overall_trackers # Always have the final tracker state now
        )
    }

    return(list(
        design_matrix = design_matrix,
        total_attempts = 1,
        method = method
    ))
}

# Generate greedy design for a single respondent with overall tracker updates
generate_greedy_for_respondent_with_tracking <- function(
    resp_id,
    overall_trackers,
    opt_env,
    encoded_profiles
) {
    n <- opt_env$n
    method <- opt_env$method

    # Create design matrix for this respondent
    resp_design <- matrix(0, nrow = n$q, ncol = n$alts)

    # Set eligible profiles (pre-computed for speed)
    eligible_profiles_list <- get_eligible_profiles_list_greedy(opt_env)

    # Initialize local trackers from overall state
    trackers <- overall_trackers

    # Generate each question for this respondent
    for (q in 1:n$q) {
        # Reset question-level trackers
        trackers <- reset_question_trackers_matrix(trackers, method)

        # Generate each alternative in this question
        for (alt in 1:n$alts) {
            eligible_profiles <- eligible_profiles_list[[alt]]

            # Select best profile using greedy algorithm
            selected_profile <- select_profile_greedy(
                eligible_profiles,
                trackers,
                resp_design,
                q,
                alt,
                opt_env,
                encoded_profiles
            )

            # Store selected profile
            resp_design[q, alt] <- selected_profile

            # Update trackers
            trackers <- update_greedy_trackers_matrix(
                trackers,
                selected_profile,
                resp_design,
                q,
                alt,
                method
            )
        }
    }

    return(list(
        design = resp_design,
        final_trackers = trackers
    ))
}

# Combine tracker states from parallel execution
combine_tracker_states <- function(resp_results, method) {
    # Initialize combined trackers from first result
    combined_trackers <- resp_results[[1]]$final_trackers

    # Add all other respondent tracker states to get total counts
    for (i in 2:length(resp_results)) {
        resp_trackers <- resp_results[[i]]$final_trackers

        # Combine overall frequency trackers
        combined_trackers$overall_freq$current_counts <-
            combined_trackers$overall_freq$current_counts +
            resp_trackers$overall_freq$current_counts

        # Combine advanced trackers if needed
        if (method %in% c("minoverlap", "balanced")) {
            combined_trackers$overall_pairs$current_counts <-
                combined_trackers$overall_pairs$current_counts +
                resp_trackers$overall_pairs$current_counts

            # For overlap, we want the final state (not cumulative)
            combined_trackers$overall_overlap$current_counts <- resp_trackers$overall_overlap$current_counts
        }
    }

    return(combined_trackers)
}

# Post-design dominance removal for greedy methods
remove_dominant_questions_greedy <- function(
    design_matrix,
    opt_env,
    encoded_profiles,
    final_trackers
) {
    attempts <- 0
    max_attempts <- opt_env$n$max_attempts
    method <- opt_env$method

    message("Checking for dominant alternatives and replacing if found...")

    # Use the provided tracker state (now always available)
    trackers <- final_trackers

    # Pre-compute eligible profiles (reuse existing optimization)
    eligible_profiles_list <- get_eligible_profiles_list_greedy(opt_env)

    # Track initial problem count for summary message
    initial_problems <- NULL

    while (attempts < max_attempts) {
        attempts <- attempts + 1

        # Find all problematic questions using existing functions from random method
        problem_questions <- find_problematic_questions(design_matrix, opt_env)

        if (length(problem_questions) == 0) {
            break # Design is valid
        }

        # Store initial problem count for summary
        if (is.null(initial_problems)) {
            initial_problems <- length(problem_questions)
        }

        # Replace problematic questions using greedy selection (no per-iteration message)
        for (q in problem_questions) {
            # Remove old question from trackers before replacing
            old_profiles <- design_matrix[q, ]
            trackers <- remove_question_from_trackers(
                trackers,
                old_profiles,
                method
            )

            # Reset question-level trackers
            trackers <- reset_question_trackers_matrix(trackers, method)

            # Generate new question using greedy logic
            for (alt in 1:opt_env$n$alts) {
                eligible_profiles <- eligible_profiles_list[[alt]]

                # Select profile using greedy algorithm
                selected_profile <- select_profile_greedy(
                    eligible_profiles,
                    trackers,
                    design_matrix,
                    q,
                    alt,
                    opt_env,
                    encoded_profiles
                )

                # Update design matrix
                design_matrix[q, alt] <- selected_profile

                # Update trackers
                trackers <- update_greedy_trackers_matrix(
                    trackers,
                    selected_profile,
                    design_matrix,
                    q,
                    alt,
                    method
                )
            }
        }
    }

    # Find all remaining problematic questions (if any)
    remaining_problems <- find_problematic_questions(design_matrix, opt_env)
    if (length(remaining_problems) > 0) {
        warning(sprintf(
            "Could not remove all dominant alternatives after %d attempts. %d questions still have dominance issues.",
            max_attempts,
            length(remaining_problems)
        ))
    } else {
        if (!is.null(initial_problems) && initial_problems > 0) {
            message(sprintf(
                "All dominant alternatives successfully removed (%d questions fixed in %d iterations).",
                initial_problems,
                attempts
            ))
        } else {
            message("All dominant alternatives successfully removed.")
        }
    }

    return(design_matrix)
}

# Remove a question's profiles from trackers
remove_question_from_trackers <- function(trackers, old_profiles, method) {
    # Remove from overall trackers by subtracting
    for (profile_id in old_profiles) {
        if (profile_id != 0) {
            # Skip zeros
            profile_key <- as.character(profile_id)

            # Subtract from overall frequency tracker
            update_vector <- trackers$overall_freq$update_matrix[profile_key, ]
            trackers$overall_freq$current_counts <- trackers$overall_freq$current_counts -
                update_vector

            if (method %in% c("minoverlap", "balanced")) {
                # Subtract from overall pairwise tracker
                update_vector_pairs <- trackers$overall_pairs$update_matrix[
                    profile_key,
                ]
                trackers$overall_pairs$current_counts <- trackers$overall_pairs$current_counts -
                    update_vector_pairs
            }
        }
    }

    return(trackers)
}

# ===== ENCODING AND TRACKER INITIALIZATION =====

# Get encoded profiles for fast lookup
get_encoded_profiles <- function(profiles) {
    # Convert all non-profileID columns to character for dummy coding
    profiles_for_dummy <- profiles[,
        -which(names(profiles) == "profileID"),
        drop = FALSE
    ]
    profiles_for_dummy <- as.data.frame(
        lapply(profiles_for_dummy, as.character),
        stringsAsFactors = FALSE
    )

    # Create dummy variables
    profiles_dummy <- fastDummies::dummy_cols(
        profiles_for_dummy,
        remove_selected_columns = TRUE
    )

    # Add profileID back
    profiles_dummy$profileID <- profiles$profileID

    return(profiles_dummy)
}

# Initialize matrix-based tracking structures
initialize_greedy_trackers_matrix <- function(
    attr_names,
    profiles,
    encoded_profiles,
    method
) {
    # All methods need basic frequency tracking
    trackers <- list(
        question_freq = initialize_frequency_tracker_matrix(encoded_profiles),
        overall_freq = initialize_frequency_tracker_matrix(encoded_profiles)
    )

    # Advanced methods need pairwise and overlap tracking
    if (method %in% c("minoverlap", "balanced")) {
        trackers$question_pairs = initialize_pairwise_tracker_matrix(
            attr_names,
            profiles,
            encoded_profiles
        )
        trackers$overall_pairs = initialize_pairwise_tracker_matrix(
            attr_names,
            profiles,
            encoded_profiles
        )
        trackers$question_overlap = initialize_overlap_tracker_matrix(
            encoded_profiles
        )
        trackers$overall_overlap = initialize_overlap_tracker_matrix(
            encoded_profiles
        )
    }

    return(trackers)
}

# Reset question-level trackers to zero
reset_question_trackers_matrix <- function(trackers, method) {
    # Reset question-level frequency tracker
    trackers$question_freq$current_counts[] <- 0

    # Reset question-level advanced trackers if needed
    if (method %in% c("minoverlap", "balanced")) {
        trackers$question_pairs$current_counts[] <- 0
        trackers$question_overlap$current_counts[] <- 0
    }

    return(trackers)
}

# Update tracking structures using matrix operations
update_greedy_trackers_matrix <- function(
    trackers,
    profile_id,
    resp_design,
    q,
    alt,
    method
) {
    # Convert profile_id to character for consistent indexing
    profile_key <- as.character(profile_id)

    # Update basic frequency trackers using vectorized operations
    trackers$question_freq <- update_frequency_tracker_matrix(
        trackers$question_freq,
        profile_key
    )
    trackers$overall_freq <- update_frequency_tracker_matrix(
        trackers$overall_freq,
        profile_key
    )

    # Update advanced trackers if needed
    if (method %in% c("minoverlap", "balanced")) {
        trackers$question_pairs <- update_pairwise_tracker_matrix(
            trackers$question_pairs,
            profile_key
        )
        trackers$overall_pairs <- update_pairwise_tracker_matrix(
            trackers$overall_pairs,
            profile_key
        )

        # For overlap tracking, we need the current question state
        current_question_profiles <- resp_design[q, 1:alt]
        current_question_profiles <- current_question_profiles[
            current_question_profiles != 0
        ]

        trackers$question_overlap <- update_overlap_tracker_matrix(
            trackers$question_overlap,
            current_question_profiles
        )
        trackers$overall_overlap <- update_overlap_tracker_matrix(
            trackers$overall_overlap,
            current_question_profiles
        )
    }

    return(trackers)
}

# ===== MATRIX-BASED FREQUENCY TRACKING =====

# Initialize frequency tracker as matrix system
initialize_frequency_tracker_matrix <- function(encoded_profiles) {
    # Extract dummy-coded columns (exclude profileID)
    dummy_cols <- setdiff(names(encoded_profiles), "profileID")

    # Create update matrix: rows = profileIDs, cols = dummy variables
    update_matrix <- as.matrix(encoded_profiles[, dummy_cols])
    rownames(update_matrix) <- as.character(encoded_profiles$profileID)

    # Initialize tracker vector (starts at zero)
    current_counts <- stats::setNames(rep(0, length(dummy_cols)), dummy_cols)

    return(list(
        current_counts = current_counts,
        update_matrix = update_matrix,
        dummy_cols = dummy_cols
    ))
}

# Update frequency tracker using matrix lookup
update_frequency_tracker_matrix <- function(freq_tracker_matrix, profile_key) {
    # Get update vector for this profile (single row lookup)
    update_vector <- freq_tracker_matrix$update_matrix[profile_key, ]

    # Vectorized addition
    freq_tracker_matrix$current_counts <- freq_tracker_matrix$current_counts +
        update_vector

    return(freq_tracker_matrix)
}

# ===== MATRIX-BASED PAIRWISE TRACKING =====

# Initialize pairwise tracker as matrix system
initialize_pairwise_tracker_matrix <- function(
    attr_names,
    profiles,
    encoded_profiles
) {
    # Create pairwise interaction terms
    pairwise_matrix <- create_pairwise_interaction_matrix(
        attr_names,
        profiles,
        encoded_profiles
    )

    # Create update matrix
    update_matrix <- as.matrix(pairwise_matrix[,
        -which(names(pairwise_matrix) == "profileID")
    ])
    rownames(update_matrix) <- as.character(pairwise_matrix$profileID)

    # Initialize tracker vector
    pairwise_cols <- setdiff(names(pairwise_matrix), "profileID")
    current_counts <- stats::setNames(
        rep(0, length(pairwise_cols)),
        pairwise_cols
    )

    return(list(
        current_counts = current_counts,
        update_matrix = update_matrix,
        pairwise_cols = pairwise_cols
    ))
}

# Create pairwise interaction matrix
create_pairwise_interaction_matrix <- function(
    attr_names,
    profiles,
    encoded_profiles
) {
    # Start with encoded profiles
    result <- encoded_profiles
    dummy_cols <- setdiff(names(encoded_profiles), "profileID")

    # Create interaction terms for each pair of attributes
    for (i in 1:(length(attr_names) - 1)) {
        for (j in (i + 1):length(attr_names)) {
            attr1 <- attr_names[i]
            attr2 <- attr_names[j]

            # Find dummy columns for each attribute
            attr1_cols <- dummy_cols[grepl(paste0("^", attr1, "_"), dummy_cols)]
            attr2_cols <- dummy_cols[grepl(paste0("^", attr2, "_"), dummy_cols)]

            # Create interaction terms
            for (col1 in attr1_cols) {
                for (col2 in attr2_cols) {
                    interaction_name <- paste(col1, col2, sep = "_x_")
                    result[[interaction_name]] <- result[[col1]] *
                        result[[col2]]
                }
            }
        }
    }

    return(result)
}

# Update pairwise tracker using matrix lookup
update_pairwise_tracker_matrix <- function(
    pairwise_tracker_matrix,
    profile_key
) {
    # Get update vector for this profile
    update_vector <- pairwise_tracker_matrix$update_matrix[profile_key, ]

    # Vectorized addition
    pairwise_tracker_matrix$current_counts <- pairwise_tracker_matrix$current_counts +
        update_vector

    return(pairwise_tracker_matrix)
}

# ===== MATRIX-BASED OVERLAP TRACKING =====

# Initialize overlap tracker as matrix system
initialize_overlap_tracker_matrix <- function(encoded_profiles) {
    # Same structure as frequency tracker since overlap uses same dummy variables
    return(initialize_frequency_tracker_matrix(encoded_profiles))
}

# Update overlap tracker based on current question state
update_overlap_tracker_matrix <- function(
    overlap_tracker_matrix,
    question_profiles
) {
    # Reset tracker
    overlap_tracker_matrix$current_counts[] <- 0

    # Sum up all profiles in current question
    for (profile_id in question_profiles) {
        profile_key <- as.character(profile_id)
        update_vector <- overlap_tracker_matrix$update_matrix[profile_key, ]
        overlap_tracker_matrix$current_counts <- overlap_tracker_matrix$current_counts +
            update_vector
    }

    return(overlap_tracker_matrix)
}

# ===== PROFILE SELECTION AND SCORING =====

# Get eligible profiles based on basic constraints (unchanged - already pre-computed)
get_eligible_profiles_list_greedy <- function(opt_env) {
    n_alts <- opt_env$n$alts
    result <- list()

    # No labels - all profiles are elligible for each alt
    if (is.null(opt_env$label_constraints)) {
        options <- opt_env$available_profile_ids
        for (i in seq(n_alts)) {
            result[[i]] <- options
        }
        return(result)
    }

    # For labeled designs, only consider profiles from the correct label group
    for (i in seq(n_alts)) {
        options <- opt_env$label_constraints$groups[[i]]
        result[[i]] <- options
    }
    return(result)
}

# Select profile using greedy algorithm
select_profile_greedy <- function(
    eligible_profiles,
    trackers,
    resp_design,
    current_q,
    current_alt,
    opt_env,
    encoded_profiles
) {
    if (length(eligible_profiles) == 0) {
        stop("No eligible profiles available")
    }

    method <- opt_env$method

    # Calculate scores for each eligible profile
    profile_scores <- numeric(length(eligible_profiles))
    names(profile_scores) <- as.character(eligible_profiles)

    for (i in seq_along(eligible_profiles)) {
        profile_id <- eligible_profiles[i]

        # Check if this profile would create duplicates in current question
        if (profile_id %in% resp_design[current_q, 1:(current_alt - 1)]) {
            profile_scores[i] <- Inf # Penalize duplicates heavily
            next
        }

        # Calculate score based on method using matrix operations
        base_score <- calculate_greedy_score_matrix(
            profile_id,
            trackers,
            resp_design,
            current_q,
            current_alt,
            method,
            encoded_profiles
        )

        # Apply balance_by adjustment if balance_by constraints exist
        if (!is.null(opt_env$balance_by_constraints)) {
            # Get the weight for this profile (higher weight = more underrepresented)
            profile_weight <- opt_env$balance_by_constraints$profile_weights[profile_id]
            # Convert weight to score adjustment: higher weight = lower score (better)
            # Use a mild adjustment to allow the greedy algorithm to still work
            balance_adjustment <- 1 / sqrt(profile_weight)  # Softer than 1/weight
            profile_scores[i] <- base_score * balance_adjustment
        } else {
            profile_scores[i] <- base_score
        }
    }

    # Select profile with best (lowest) score
    best_indices <- which(profile_scores == min(profile_scores))

    if (length(best_indices) == 1) {
        return(eligible_profiles[best_indices])
    } else {
        # Tie-breaking: random selection among tied profiles
        selected_index <- sample(best_indices, 1)
        return(eligible_profiles[selected_index])
    }
}

# ===== MATRIX-BASED SCORING FUNCTIONS =====

# Calculate score using matrix operations
calculate_greedy_score_matrix <- function(
    profile_id,
    trackers,
    resp_design,
    current_q,
    current_alt,
    method,
    encoded_profiles
) {
    profile_key <- as.character(profile_id)

    if (method == "shortcut") {
        # Original shortcut: only frequency matters
        freq_score <- get_frequency_score_matrix(profile_key, trackers)
        return(freq_score)
    }

    if (method == "minoverlap") {
        # Only consider overlap, completely ignore frequency balance
        overlap_score <- get_overlap_score_matrix(
            profile_key,
            trackers,
            resp_design,
            current_q,
            current_alt,
            method,
            encoded_profiles
        )
        return(overlap_score)
    } else if (method == "balanced") {
        # Maximize overall balance, ALLOW overlap if it helps balance
        freq_score <- get_frequency_score_matrix(profile_key, trackers)
        pairwise_score <- get_pairwise_score_matrix(profile_key, trackers)

        # Weight pairwise interactions more heavily to create distinctly different behavior
        return(freq_score * 50 + pairwise_score * 200)
    } else {
        stop("Unknown method: ", method)
    }
}

# Calculate frequency score using matrix operations
get_frequency_score_matrix <- function(profile_key, trackers) {
    # Get the update vector for this profile
    update_vector <- trackers$question_freq$update_matrix[profile_key, ]

    # Calculate scores: question frequency * 1000 + overall frequency
    question_scores <- trackers$question_freq$current_counts *
        update_vector *
        1000
    overall_scores <- trackers$overall_freq$current_counts * update_vector

    # Sum all scores
    total_score <- sum(question_scores + overall_scores)

    return(total_score)
}

# Calculate pairwise score using matrix operations
get_pairwise_score_matrix <- function(profile_key, trackers) {
    # Get the update vector for this profile
    update_vector <- trackers$question_pairs$update_matrix[profile_key, ]

    # Calculate scores: question frequency * 1000 + overall frequency
    question_scores <- trackers$question_pairs$current_counts *
        update_vector *
        1000
    overall_scores <- trackers$overall_pairs$current_counts * update_vector

    # Sum all scores
    total_score <- sum(question_scores + overall_scores)

    return(total_score)
}

# Calculate overlap score using matrix operations
get_overlap_score_matrix <- function(
    profile_key,
    trackers,
    resp_design,
    current_q,
    current_alt,
    method,
    encoded_profiles
) {
    # Get the update vector for this profile
    update_vector <- trackers$question_overlap$update_matrix[profile_key, ]

    # Calculate future counts (current + 1 for each attribute level this profile contributes to)
    future_counts <- trackers$question_overlap$current_counts + update_vector

    if (method == "minoverlap") {
        # Heavily penalize overlap - exponential penalty for multiple occurrences
        overlap_penalties <- future_counts^3 * update_vector
    } else if (method == "balanced") {
        # Allow some overlap - linear penalty, less severe
        overlap_penalties <- future_counts * update_vector
    }

    # Sum all penalties
    total_score <- sum(overlap_penalties)

    return(total_score)
}

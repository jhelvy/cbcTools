# ===== MAIN GREEDY DESIGN GENERATION =====

# Generate greedy design (shortcut, minoverlap, or balanced)
generate_greedy_design <- function(opt_env) {
    n <- opt_env$n
    method <- opt_env$method # "shortcut", "minoverlap", or "balanced"

    message(
        "Generating ",
        method,
        " design for ",
        n$resp,
        " respondents using ",
        n$cores,
        " cores..."
    )

    # Generate designs in parallel
    if (n$cores == 1 || n$resp == 1) {
        # Sequential execution
        resp_designs <- lapply(1:n$resp, function(resp) {
            generate_greedy_for_respondent(resp, opt_env)
        })
    } else {
        # Parallel execution
        if (Sys.info()[['sysname']] == 'Windows') {
            cl <- parallel::makeCluster(min(n$cores, n$resp), "PSOCK")
            # Export necessary functions and objects to cluster
            parallel::clusterExport(
                cl,
                c(
                    "generate_greedy_for_respondent",
                    "initialize_greedy_trackers",
                    "update_greedy_trackers",
                    "select_profile_greedy",
                    "reset_greedy_trackers",
                    "calculate_greedy_score",
                    "get_pairwise_score",
                    "get_overlap_score",
                    "get_frequency_score",
                    "initialize_frequency_tracker",
                    "reset_frequency_tracker",
                    "update_frequency_tracker",
                    "initialize_pairwise_tracker",
                    "initialize_overlap_tracker",
                    "reset_pairwise_tracker",
                    "update_pairwise_tracker",
                    "update_overlap_tracker",
                    "get_eligible_profiles_greedy",
                    "filter_dominant_profiles",
                    "would_create_dominance",
                    "get_probs"
                ),
                envir = environment()
            )

            resp_designs <- suppressMessages(suppressWarnings(
                parallel::parLapply(cl, 1:n$resp, function(resp) {
                    generate_greedy_for_respondent(resp, opt_env)
                })
            ))
            parallel::stopCluster(cl)
        } else {
            resp_designs <- suppressMessages(suppressWarnings(
                parallel::mclapply(
                    1:n$resp,
                    function(resp) {
                        generate_greedy_for_respondent(resp, opt_env)
                    },
                    mc.cores = min(n$cores, n$resp)
                )
            ))
        }
    }

    # Combine all respondent designs into a single matrix
    design_matrix <- do.call(rbind, resp_designs)

    return(list(
        design_matrix = design_matrix,
        total_attempts = 1,
        method = method
    ))
}

# Generate greedy design for a single respondent
generate_greedy_for_respondent <- function(resp_id, opt_env) {
    n <- opt_env$n
    attr_names <- opt_env$attr_names
    profiles <- opt_env$profiles
    method <- opt_env$method

    # Initialize trackers based on method requirements
    trackers <- initialize_greedy_trackers(attr_names, profiles, method)

    # Create design matrix for this respondent
    resp_design <- matrix(0, nrow = n$q, ncol = n$alts)

    # Generate each question for this respondent
    for (q in 1:n$q) {
        # Reset question-level trackers
        trackers <- reset_greedy_trackers(trackers, method, attr_names)

        # Generate each alternative in this question
        for (alt in 1:n$alts) {
            # Get eligible profiles based on constraints (including dominance filtering)
            eligible_profiles <- get_eligible_profiles_greedy(
                alt,
                opt_env,
                resp_design,
                q
            )

            # Select best profile using greedy algorithm
            selected_profile <- select_profile_greedy(
                eligible_profiles,
                trackers,
                resp_design,
                q,
                alt,
                opt_env
            )

            # Store selected profile
            resp_design[q, alt] <- selected_profile

            # Update trackers
            trackers <- update_greedy_trackers(
                trackers,
                selected_profile,
                resp_design,
                q,
                alt,
                profiles,
                attr_names,
                method
            )
        }
    }

    return(resp_design)
}

# ===== TRACKER INITIALIZATION AND MANAGEMENT =====

# Initialize tracking structures based on method requirements
initialize_greedy_trackers <- function(attr_names, profiles, method) {
    # All methods need basic frequency tracking
    trackers <- list(
        question_freq = initialize_frequency_tracker(attr_names, profiles),
        overall_freq = initialize_frequency_tracker(attr_names, profiles)
    )

    # Advanced methods need pairwise and overlap tracking
    if (method %in% c("minoverlap", "balanced")) {
        trackers$question_pairs <- initialize_pairwise_tracker(
            attr_names,
            profiles
        )
        trackers$overall_pairs <- initialize_pairwise_tracker(
            attr_names,
            profiles
        )
        trackers$question_overlap <- initialize_overlap_tracker(
            attr_names,
            profiles
        )
        trackers$overall_overlap <- initialize_overlap_tracker(
            attr_names,
            profiles
        )
    }

    return(trackers)
}

# Reset question-level trackers based on method
reset_greedy_trackers <- function(trackers, method, attr_names) {
    # All methods reset frequency trackers
    trackers$question_freq <- reset_frequency_tracker(trackers$question_freq)

    # Advanced methods reset additional trackers
    if (method %in% c("minoverlap", "balanced")) {
        trackers$question_pairs <- reset_pairwise_tracker(
            trackers$question_pairs,
            attr_names
        )
    }

    return(trackers)
}

# Update tracking structures based on method requirements
update_greedy_trackers <- function(
    trackers,
    profile_id,
    resp_design,
    q,
    alt,
    profiles,
    attr_names,
    method
) {
    # All methods update basic frequencies
    trackers$question_freq <- update_frequency_tracker(
        trackers$question_freq,
        profile_id,
        profiles,
        attr_names
    )
    trackers$overall_freq <- update_frequency_tracker(
        trackers$overall_freq,
        profile_id,
        profiles,
        attr_names
    )

    # Advanced methods update additional trackers
    if (method %in% c("minoverlap", "balanced")) {
        # Update pairwise frequencies
        trackers$question_pairs <- update_pairwise_tracker(
            trackers$question_pairs,
            profile_id,
            profiles,
            attr_names
        )
        trackers$overall_pairs <- update_pairwise_tracker(
            trackers$overall_pairs,
            profile_id,
            profiles,
            attr_names
        )

        # Update overlap tracking
        current_question_profiles <- resp_design[q, 1:alt]
        current_question_profiles <- current_question_profiles[
            current_question_profiles != 0
        ]

        trackers$question_overlap <- update_overlap_tracker(
            trackers$question_overlap,
            current_question_profiles,
            profiles,
            attr_names
        )
        trackers$overall_overlap <- update_overlap_tracker(
            trackers$overall_overlap,
            current_question_profiles,
            profiles,
            attr_names
        )
    }

    return(trackers)
}

# ===== BASIC FREQUENCY TRACKING =====

# Initialize frequency tracker for attributes
initialize_frequency_tracker <- function(attr_names, profiles) {
    freq_tracker <- list()

    for (attr in attr_names) {
        levels <- unique(profiles[[attr]])
        freq_tracker[[attr]] <- stats::setNames(rep(0, length(levels)), levels)
    }

    return(freq_tracker)
}

# Reset question-level frequency tracker
reset_frequency_tracker <- function(freq_tracker) {
    for (attr in names(freq_tracker)) {
        freq_tracker[[attr]][] <- 0 # Reset all counts to 0
    }
    return(freq_tracker)
}

# Update frequency tracker with new profile
update_frequency_tracker <- function(
    freq_tracker,
    profile_id,
    profiles,
    attr_names
) {
    # Get the profile data
    profile_row <- profiles[profiles$profileID == profile_id, ]

    # Update frequency for each attribute
    for (attr in attr_names) {
        level <- as.character(profile_row[[attr]])
        freq_tracker[[attr]][level] <- freq_tracker[[attr]][level] + 1
    }

    return(freq_tracker)
}

# ===== PAIRWISE FREQUENCY TRACKING =====

# Initialize pairwise frequency tracker
initialize_pairwise_tracker <- function(attr_names, profiles) {
    pairs_tracker <- list()

    # For each pair of attributes
    for (i in 1:(length(attr_names) - 1)) {
        for (j in (i + 1):length(attr_names)) {
            attr1 <- attr_names[i]
            attr2 <- attr_names[j]
            pair_name <- paste(attr1, attr2, sep = "_x_")

            # Get all possible level combinations
            levels1 <- unique(profiles[[attr1]])
            levels2 <- unique(profiles[[attr2]])

            # Create combination names
            combinations <- expand.grid(
                levels1,
                levels2,
                stringsAsFactors = FALSE
            )
            comb_names <- paste(
                combinations$Var1,
                combinations$Var2,
                sep = "_&_"
            )

            pairs_tracker[[pair_name]] <- stats::setNames(
                rep(0, length(comb_names)),
                comb_names
            )
        }
    }

    return(pairs_tracker)
}

# Reset pairwise tracker for new question
reset_pairwise_tracker <- function(pairs_tracker, attr_names) {
    for (pair_name in names(pairs_tracker)) {
        pairs_tracker[[pair_name]][] <- 0
    }
    return(pairs_tracker)
}

# Update pairwise frequency tracker
update_pairwise_tracker <- function(
    pairs_tracker,
    profile_id,
    profiles,
    attr_names
) {
    profile_row <- profiles[profiles$profileID == profile_id, ]

    # For each pair of attributes
    for (i in 1:(length(attr_names) - 1)) {
        for (j in (i + 1):length(attr_names)) {
            attr1 <- attr_names[i]
            attr2 <- attr_names[j]
            pair_name <- paste(attr1, attr2, sep = "_x_")

            level1 <- as.character(profile_row[[attr1]])
            level2 <- as.character(profile_row[[attr2]])
            comb_name <- paste(level1, level2, sep = "_&_")

            if (comb_name %in% names(pairs_tracker[[pair_name]])) {
                pairs_tracker[[pair_name]][comb_name] <- pairs_tracker[[
                    pair_name
                ]][comb_name] +
                    1
            }
        }
    }

    return(pairs_tracker)
}

# ===== OVERLAP TRACKING =====

# Initialize overlap frequency tracker
initialize_overlap_tracker <- function(attr_names, profiles) {
    overlap_tracker <- list()

    for (attr in attr_names) {
        levels <- unique(profiles[[attr]])
        # Track how many times each level appears in the same question
        overlap_tracker[[attr]] <- stats::setNames(rep(0, length(levels)), levels)
    }

    return(overlap_tracker)
}

# Update overlap tracker based on current question state
update_overlap_tracker <- function(
    overlap_tracker,
    question_profiles,
    profiles,
    attr_names
) {
    # Reset tracker
    for (attr in attr_names) {
        overlap_tracker[[attr]][] <- 0
    }

    # Count occurrences of each level in current question
    for (profile_id in question_profiles) {
        profile_row <- profiles[profiles$profileID == profile_id, ]

        for (attr in attr_names) {
            level <- as.character(profile_row[[attr]])
            overlap_tracker[[attr]][level] <- overlap_tracker[[attr]][level] + 1
        }
    }

    return(overlap_tracker)
}

# ===== PROFILE SELECTION AND SCORING =====

# Get eligible profiles based on constraints (updated version with dominance support)
get_eligible_profiles_greedy <- function(
    alt,
    opt_env,
    resp_design = NULL,
    current_q = NULL
) {
    # Start with basic constraint filtering
    if (!is.null(opt_env$label_constraints)) {
        # For labeled designs, only consider profiles from the correct label group
        label_group_index <- alt
        if (label_group_index <= length(opt_env$label_constraints$groups)) {
            eligible_profiles <- opt_env$label_constraints$groups[[
                label_group_index
            ]]
        } else {
            return(c()) # No eligible profiles
        }
    } else {
        # Regular design: consider all profiles
        eligible_profiles <- opt_env$available_profile_ids
    }

    # Apply dominance filtering if requested and priors available
    if (
        opt_env$remove_dominant &&
            opt_env$has_priors &&
            !is.null(resp_design) &&
            !is.null(current_q)
    ) {
        # Filter out profiles that would create dominant alternatives
        eligible_profiles <- filter_dominant_profiles(
            eligible_profiles,
            resp_design,
            current_q,
            alt,
            opt_env
        )
    }

    return(eligible_profiles)
}

# Select profile using greedy algorithm
select_profile_greedy <- function(
    eligible_profiles,
    trackers,
    resp_design,
    current_q,
    current_alt,
    opt_env
) {
    if (length(eligible_profiles) == 0) {
        stop("No eligible profiles available")
    }

    method <- opt_env$method
    attr_names <- opt_env$attr_names
    profiles <- opt_env$profiles

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

        # Calculate score based on method
        profile_scores[i] <- calculate_greedy_score(
            profile_id,
            trackers,
            resp_design,
            current_q,
            current_alt,
            method,
            profiles,
            attr_names
        )
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

# ===== SCORING FUNCTIONS =====

# Calculate score based on method requirements with distinct objectives
calculate_greedy_score <- function(
    profile_id,
    trackers,
    resp_design,
    current_q,
    current_alt,
    method,
    profiles,
    attr_names
) {
    if (method == "shortcut") {
        # Original shortcut: only frequency matters
        freq_score <- get_frequency_score(
            profile_id,
            trackers,
            profiles,
            attr_names
        )
        return(freq_score)
    }

    if (method == "minoverlap") {
        # Only consider overlap, completely ignore frequency balance
        overlap_score <- get_overlap_score(
            profile_id,
            trackers,
            resp_design,
            current_q,
            current_alt,
            method,
            profiles,
            attr_names
        )
        return(overlap_score)
    } else if (method == "balanced") {
        # Maximize overall balance, ALLOW overlap if it helps balance
        freq_score <- get_frequency_score(
            profile_id,
            trackers,
            profiles,
            attr_names
        )
        pairwise_score <- get_pairwise_score(
            profile_id,
            trackers,
            profiles,
            attr_names
        )

        # Weight pairwise interactions more heavily to create distinctly different behavior
        return(freq_score * 50 + pairwise_score * 200)
    } else {
        stop("Unknown method: ", method)
    }
}

# Calculate frequency score (same as original shortcut algorithm)
get_frequency_score <- function(profile_id, trackers, profiles, attr_names) {
    profile_row <- profiles[profiles$profileID == profile_id, ]

    total_score <- 0
    for (attr in attr_names) {
        level <- as.character(profile_row[[attr]])

        # Primary: frequency within current question, Secondary: overall frequency
        question_count <- trackers$question_freq[[attr]][level]
        overall_count <- trackers$overall_freq[[attr]][level]

        attr_score <- question_count * 1000 + overall_count
        total_score <- total_score + attr_score
    }

    return(total_score)
}

# Calculate pairwise frequency score
get_pairwise_score <- function(profile_id, trackers, profiles, attr_names) {
    profile_row <- profiles[profiles$profileID == profile_id, ]

    total_score <- 0

    # For each pair of attributes
    for (i in 1:(length(attr_names) - 1)) {
        for (j in (i + 1):length(attr_names)) {
            attr1 <- attr_names[i]
            attr2 <- attr_names[j]
            pair_name <- paste(attr1, attr2, sep = "_x_")

            level1 <- as.character(profile_row[[attr1]])
            level2 <- as.character(profile_row[[attr2]])
            comb_name <- paste(level1, level2, sep = "_&_")

            if (comb_name %in% names(trackers$question_pairs[[pair_name]])) {
                # Primary: frequency within current question, Secondary: overall frequency
                question_count <- trackers$question_pairs[[pair_name]][
                    comb_name
                ]
                overall_count <- trackers$overall_pairs[[pair_name]][comb_name]

                pair_score <- question_count * 1000 + overall_count
                total_score <- total_score + pair_score
            }
        }
    }

    return(total_score)
}

# Calculate overlap score (method-dependent)
get_overlap_score <- function(
    profile_id,
    trackers,
    resp_design,
    current_q,
    current_alt,
    method,
    profiles,
    attr_names
) {
    profile_row <- profiles[profiles$profileID == profile_id, ]

    total_score <- 0

    for (attr in attr_names) {
        level <- as.character(profile_row[[attr]])

        # How many times would this level appear in current question if we add this profile?
        current_count <- trackers$question_overlap[[attr]][level]
        future_count <- current_count + 1

        if (method == "minoverlap") {
            # Heavily penalize overlap - exponential penalty for multiple occurrences
            overlap_penalty <- future_count^3
        } else if (method == "balanced") {
            # Allow some overlap - linear penalty, less severe
            overlap_penalty <- future_count
        }

        total_score <- total_score + overlap_penalty
    }

    return(total_score)
}

# ===== DOMINANCE FILTERING (if needed) =====

# Helper function to filter out profiles that would create dominance
filter_dominant_profiles <- function(
    eligible_profiles,
    resp_design,
    current_q,
    current_alt,
    opt_env
) {
    if (length(eligible_profiles) == 0) {
        return(eligible_profiles)
    }

    # Get profiles already selected for this question
    existing_profiles <- resp_design[current_q, 1:(current_alt - 1)]
    existing_profiles <- existing_profiles[existing_profiles != 0]

    if (length(existing_profiles) == 0) {
        # First alternative in question - no dominance check needed yet
        return(eligible_profiles)
    }

    valid_profiles <- c()

    for (profile_id in eligible_profiles) {
        # Create test question with current profile added
        test_question <- c(existing_profiles, profile_id)

        # Check if this would create dominance
        if (!would_create_dominance(test_question, opt_env)) {
            valid_profiles <- c(valid_profiles, profile_id)
        }
    }

    return(valid_profiles)
}

# Check if a specific question configuration would create dominance
would_create_dominance <- function(question_profiles, opt_env) {
    # Create mini design matrix for this question
    test_matrix <- matrix(question_profiles, nrow = 1)

    # Check total dominance
    if ("total" %in% opt_env$dominance_types) {
        probs <- get_probs(test_matrix, opt_env)
        if (opt_env$is_bayesian) {
            probs <- rowMeans(probs)
        }
        if (any(probs > opt_env$dominance_threshold)) {
            return(TRUE)
        }
    }

    # Check partial dominance
    if (
        "partial" %in%
            opt_env$dominance_types &&
            !is.null(opt_env$partial_utilities)
    ) {
        design_vector <- as.vector(test_matrix)
        partials <- opt_env$partial_utilities[design_vector, , drop = FALSE]

        # Check if any profile dominates others
        if (nrow(partials) > 1) {
            for (i in 1:nrow(partials)) {
                dominates_all <- TRUE
                for (j in 1:nrow(partials)) {
                    if (i != j) {
                        # Check if profile i dominates profile j
                        if (!all(partials[i, ] >= partials[j, ])) {
                            dominates_all <- FALSE
                            break
                        }
                    }
                }
                if (dominates_all) {
                    return(TRUE) # Found a dominant profile
                }
            }
        }
    }

    return(FALSE)
}

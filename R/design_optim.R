# D-optimal designs ----

generate_optimized_design <- function(opt_env) {
    # Set up parallel processing
    n <- opt_env$n
    message(
        "Running ",
        n$start,
        " design searches using ",
        n$cores,
        " cores..."
    )

    # Create list of different random starting designs (base designs only)
    start_designs <- lapply(1:n$start, function(i) {
        generate_initial_random_matrix(opt_env)
    })

    # Run optimization in parallel
    if (Sys.info()[['sysname']] == 'Windows') {
        cl <- parallel::makeCluster(n$cores, "PSOCK")
        # Export necessary functions to cluster
        parallel::clusterExport(
            cl,
            c(
                "optimize_design",
                "sample_question_profiles",
                "compute_design_d_error",
                "find_problematic_questions"
            ),
            envir = environment()
        )

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
            if (idx == best_index) "  (Best)" else ""
        ))
    }

    return(best_result)
}

optimize_design <- function(design_matrix, opt_env) {
    method <- opt_env$method

    if (method == "stochastic") {
        return(optimize_design_stochastic(design_matrix, opt_env))
    } else if (method == "modfed") {
        return(optimize_design_modfed(design_matrix, opt_env))
    } else if (method == "cea") {
        return(optimize_design_cea(design_matrix, opt_env))
    } else {
        stop("Unknown optimization method: ", method)
    }
}

# Stochastic optimization (keep improving until no improvement found)
optimize_design_stochastic <- function(design_matrix, opt_env) {
    # Compute initial D-error
    current_d_error <- compute_design_d_error(design_matrix, opt_env)

    n <- opt_env$n

    for (iter in 1:n$max_iter) {
        message("Iteration ", iter, ": D-error = ", round(current_d_error, 6))
        improved <- FALSE
        start_d_error <- current_d_error

        # Try improving each position in the design
        for (q in 1:n$questions) {
            for (alt in 1:n$alts) {
                current_profile <- design_matrix[q, alt]

                # Get eligible profiles for this position
                if (!is.null(opt_env$label_constraints)) {
                    # For labeled designs, only consider profiles from the correct label group
                    label_group_index <- alt
                    if (
                        label_group_index <=
                            length(opt_env$label_constraints$groups)
                    ) {
                        eligible_profiles <- opt_env$label_constraints$groups[[
                            label_group_index
                        ]]
                    } else {
                        next # Skip if label group doesn't exist
                    }
                } else {
                    # Regular design: consider all profiles
                    eligible_profiles <- opt_env$available_profile_ids
                }

                # Remove current profile from candidates
                candidate_profiles <- setdiff(
                    eligible_profiles,
                    current_profile
                )
                if (length(candidate_profiles) == 0) {
                    next
                }

                # Keep trying random profiles until we find one that doesn't improve
                attempted_profiles <- c()
                position_improved <- FALSE

                while (
                    length(attempted_profiles) < length(candidate_profiles)
                ) {
                    # Sample a new profile we haven't tried yet
                    remaining_candidates <- setdiff(
                        candidate_profiles,
                        attempted_profiles
                    )
                    if (length(remaining_candidates) == 0) {
                        break
                    }

                    new_profile <- sample(remaining_candidates, 1)
                    attempted_profiles <- c(attempted_profiles, new_profile)

                    # Create test design
                    test_design <- design_matrix
                    test_design[q, alt] <- new_profile

                    # Check all constraints
                    problem_questions <- find_problematic_questions(
                        test_design,
                        opt_env
                    )
                    if (length(problem_questions) > 0) {
                        if (q %in% problem_questions) {
                            next # Try next candidate - this doesn't count as "no improvement"
                        }
                    }

                    # Compute D-error for test design
                    test_d_error <- compute_design_d_error(test_design, opt_env)

                    # Check if this improves the current design
                    if (test_d_error < current_d_error) {
                        # Accept improvement and keep current profile updated
                        design_matrix[q, alt] <- new_profile
                        current_d_error <- test_d_error
                        improved <- TRUE
                        position_improved <- TRUE
                        # Continue searching for more improvements at this position
                    } else {
                        # No improvement found - stop searching at this position
                        break
                    }
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
        method = "stochastic",
        total_attempts = iter
    ))
}

# Modified Fedorov optimization (exhaustive profile swapping)
optimize_design_modfed <- function(design_matrix, opt_env) {
    # Compute initial D-error
    current_d_error <- compute_design_d_error(design_matrix, opt_env)

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

                # Get eligible profiles for this position
                if (!is.null(opt_env$label_constraints)) {
                    # For labeled designs, only consider profiles from the correct label group
                    label_group_index <- alt
                    if (
                        label_group_index <=
                            length(opt_env$label_constraints$groups)
                    ) {
                        eligible_profiles <- opt_env$label_constraints$groups[[
                            label_group_index
                        ]]
                    } else {
                        next # Skip if label group doesn't exist
                    }
                } else {
                    # Regular design: consider all profiles
                    eligible_profiles <- opt_env$available_profile_ids
                }

                # Try ALL eligible profiles for this position (exhaustive search)
                for (new_profile in eligible_profiles) {
                    if (new_profile == current_profile) {
                        next
                    }

                    # Create test design
                    test_design <- design_matrix
                    test_design[q, alt] <- new_profile

                    # Check all constraints
                    problem_questions <- find_problematic_questions(
                        test_design,
                        opt_env
                    )
                    if (length(problem_questions) > 0) {
                        if (q %in% problem_questions) {
                            next
                        }
                    }

                    # Compute D-error for test design
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
        method = "modfed",
        total_attempts = iter
    ))
}

# Coordinate Exchange Algorithm optimization (attribute-by-attribute)
optimize_design_cea <- function(design_matrix, opt_env) {
    # Compute initial D-error
    current_d_error <- compute_design_d_error(design_matrix, opt_env)

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

                # For each attribute, try all possible levels (coordinate exchange)
                for (attr in opt_env$attr_names) {
                    attr_levels <- get_attribute_levels(attr, opt_env)

                    for (level in attr_levels) {
                        # Find profile that matches current profile except for this attribute
                        candidate_profile <- find_profile_with_attribute_change(
                            current_profile,
                            attr,
                            level,
                            opt_env
                        )

                        if (
                            is.null(candidate_profile) ||
                                candidate_profile == current_profile
                        ) {
                            next
                        }

                        # Check label constraints
                        if (!is.null(opt_env$label_constraints)) {
                            label_group_index <- alt
                            if (
                                label_group_index <=
                                    length(opt_env$label_constraints$groups)
                            ) {
                                eligible_profiles <- opt_env$label_constraints$groups[[
                                    label_group_index
                                ]]
                                if (!candidate_profile %in% eligible_profiles) {
                                    next
                                }
                            }
                        }

                        # Create test design
                        test_design <- design_matrix
                        test_design[q, alt] <- candidate_profile

                        # Check all constraints
                        problem_questions <- find_problematic_questions(
                            test_design,
                            opt_env
                        )
                        if (length(problem_questions) > 0) {
                            if (q %in% problem_questions) {
                                next
                            }
                        }

                        # Compute D-error for test design
                        test_d_error <- compute_design_d_error(
                            test_design,
                            opt_env
                        )

                        # Accept if improvement
                        if (test_d_error < best_d_error) {
                            best_profile <- candidate_profile
                            best_d_error <- test_d_error
                        }
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
        method = "cea",
        total_attempts = iter
    ))
}

# Helper functions for CEA

# Get all possible levels for an attribute
get_attribute_levels <- function(attr, opt_env) {
    profiles <- opt_env$profiles
    if (attr %in% names(profiles)) {
        return(unique(profiles[[attr]]))
    }
    return(NULL)
}

# Find a profile that matches the current profile except for one attribute
find_profile_with_attribute_change <- function(
    current_profile_id,
    attr,
    new_level,
    opt_env
) {
    profiles <- opt_env$profiles

    # Get current profile
    current_profile_row <- profiles[profiles$profileID == current_profile_id, ]
    if (nrow(current_profile_row) == 0) {
        return(NULL)
    }

    # Create target profile (change only the specified attribute)
    target_profile <- current_profile_row
    target_profile[[attr]] <- new_level

    # Find matching profile in profiles
    for (i in 1:nrow(profiles)) {
        match_found <- TRUE
        for (col in opt_env$attr_names) {
            if (profiles[i, col] != target_profile[[col]]) {
                match_found <- FALSE
                break
            }
        }
        if (match_found) {
            return(profiles$profileID[i])
        }
    }

    return(NULL) # No matching profile found
}

# Repeat base design across respondents with block allocation (Fixed)
# Used for optimized designs to take the base design and repeat it
# across multiple respondents with proper block allocation and optional randomization
# Fix for repeat_design_across_respondents function in design.R
# Add attribute preservation at the beginning and end of the function

repeat_design_across_respondents <- function(base_design, opt_env) {
    n <- opt_env$n
    if (n$resp == 1) {
        return(base_design) # No need to repeat
    }

    # Preserve important attributes from base design
    categorical_structure_attr <- attr(base_design, "categorical_structure")
    encoding_attr <- attr(base_design, "encoding")

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
                    resp_design <- randomize_question_order(
                        resp_design,
                        n$q,
                        n$alts_total
                    )
                }

                # Randomize alternative order if requested
                if (opt_env$randomize_alts) {
                    # Pass the original n_alts (excluding no-choice)
                    resp_design <- randomize_alternative_order(
                        resp_design,
                        opt_env$no_choice,
                        n$q,
                        n$alts
                    )
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
                resp_design <- randomize_question_order(
                    resp_design,
                    n$q,
                    n$alts_total
                )
            }

            # Randomize alternative order if requested
            if (opt_env$randomize_alts) {
                # Pass the original n_alts (excluding no-choice)
                resp_design <- randomize_alternative_order(
                    resp_design,
                    opt_env$no_choice,
                    n$q,
                    n$alts
                )
            }

            full_design_list[[resp]] <- resp_design
        }
    }

    # Combine all respondents
    full_design <- do.call(rbind, full_design_list)

    # Now re-sort for potentially randomized row order
    full_design <- full_design[
        order(full_design$respID, full_design$qID, full_design$altID),
    ]
    row.names(full_design) <- NULL

    # Regenerate IDs based on current (potentially randomized) row order
    total_questions <- n$q * n$resp
    full_design$obsID <- rep(1:total_questions, each = n$alts_total)

    # Reorder columns
    full_design <- full_design[, c(get_id_names(), get_var_names(full_design))]

    # Restore important attributes to the final design
    attr(full_design, "categorical_structure") <- categorical_structure_attr
    attr(full_design, "encoding") <- encoding_attr

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

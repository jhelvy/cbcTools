generate_idefix_design_with_fallback <- function(opt_env) {
    # First check if idefix is available
    if (!requireNamespace("idefix", quietly = TRUE)) {
        warning(
            "idefix package not available. Falling back to cbcTools implementation.",
            call. = FALSE
        )
        return(generate_optimized_design(opt_env))
    }

    # Try idefix design generation with comprehensive error handling
    tryCatch(
        {
            # Attempt idefix design generation
            idefix_result <- generate_idefix_design(opt_env)

            # Additional validation of idefix result
            validate_idefix_result(idefix_result, opt_env)

            return(idefix_result)
        },
        error = function(e) {
            # Log the specific error for debugging
            warning(
                "idefix design generation failed: ",
                conditionMessage(e),
                "\nFalling back to cbcTools implementation.",
                call. = FALSE
            )

            # Fallback to cbcTools implementation
            return(generate_optimized_design(opt_env))
        },
        warning = function(w) {
            # Handle warnings from idefix - decide whether to continue or fallback
            if (should_fallback_on_warning(w)) {
                warning(
                    "idefix generated warnings that suggest fallback: ",
                    conditionMessage(w),
                    "\nFalling back to cbcTools implementation.",
                    call. = FALSE
                )
                return(generate_optimized_design(opt_env))
            } else {
                # Continue with idefix but propagate the warning
                warning(conditionMessage(w), call. = FALSE)
                idefix_result <- generate_idefix_design(opt_env)
                validate_idefix_result(idefix_result, opt_env)
                return(idefix_result)
            }
        }
    )
}

generate_idefix_design <- function(opt_env) {
    method <- opt_env$method

    # Convert cbcTools format to idefix format
    idefix_inputs <- convert_to_idefix_format(opt_env)

    # Validate inputs before calling idefix
    validate_idefix_inputs(idefix_inputs, opt_env)

    # Generate design using idefix with improved error handling
    if (method == "cea") {
        design_result <- idefix::CEA(
            lvls = idefix_inputs$lvls,
            coding = idefix_inputs$coding,
            c.lvls = idefix_inputs$c.lvls,
            par.draws = idefix_inputs$par_draws,
            n.alts = idefix_inputs$n_alts,
            n.sets = idefix_inputs$n_sets,
            n.blocks = idefix_inputs$n_blocks,
            alt.cte = idefix_inputs$alt_cte,
            no.choice = idefix_inputs$no_choice,
            parallel = should_use_parallel(opt_env),
            n.start = opt_env$n$start
        )
    } else if (method == "modfed") {
        design_result <- idefix::Modfed(
            cand.set = idefix_inputs$cand_set,
            par.draws = idefix_inputs$par_draws,
            n.alts = idefix_inputs$n_alts,
            n.sets = idefix_inputs$n_sets,
            n.blocks = idefix_inputs$n_blocks,
            alt.cte = idefix_inputs$alt_cte,
            no.choice = idefix_inputs$no_choice,
            parallel = should_use_parallel(opt_env),
            n.start = opt_env$n$start,
            max.iter = opt_env$n$max_iter
        )
    } else {
        stop("Method '", method, "' not supported with idefix integration")
    }

    # Convert idefix result back to cbcTools format
    design_matrix <- convert_from_idefix_format(
        design_result,
        idefix_inputs,
        opt_env
    )

    return(list(
        design_matrix = design_matrix,
        d_error = design_result$BestDesign$DB.error,
        method = paste0(method, "_idefix"),
        total_attempts = 1 # idefix handles multiple starts internally
    ))
}

should_use_parallel <- function(opt_env) {
    # More conservative parallel processing logic

    # Don't use parallel if only 1 core requested
    if (opt_env$n$cores <= 1) {
        return(FALSE)
    }

    # Don't use parallel for small problems (can be slower)
    n_params <- opt_env$n$params
    if (n_params <= 6) {
        return(FALSE)
    }

    # Don't use parallel if problem is too small
    total_choice_sets <- opt_env$n$blocks * opt_env$n$q
    if (total_choice_sets <= 8) {
        return(FALSE)
    }

    # Use parallel for larger problems
    return(TRUE)
}

validate_idefix_result <- function(idefix_result, opt_env) {
    # Basic validation of idefix result structure
    required_fields <- c("design_matrix", "d_error", "method", "total_attempts")
    missing_fields <- setdiff(required_fields, names(idefix_result))

    if (length(missing_fields) > 0) {
        stop(
            "idefix result missing required fields: ",
            paste(missing_fields, collapse = ", ")
        )
    }

    # Validate design matrix dimensions - idefix returns optimally blocked design
    expected_rows <- opt_env$n$blocks * opt_env$n$q # Total choice sets across all blocks
    expected_cols <- opt_env$n$alts

    if (!is.matrix(idefix_result$design_matrix)) {
        stop("idefix result design_matrix is not a matrix")
    }

    if (nrow(idefix_result$design_matrix) != expected_rows) {
        stop(sprintf(
            "idefix design_matrix has %d rows, expected %d",
            nrow(idefix_result$design_matrix),
            expected_rows
        ))
    }

    if (ncol(idefix_result$design_matrix) != expected_cols) {
        stop(sprintf(
            "idefix design_matrix has %d columns, expected %d",
            ncol(idefix_result$design_matrix),
            expected_cols
        ))
    }

    # Validate D-error is finite and positive
    if (!is.finite(idefix_result$d_error) || idefix_result$d_error <= 0) {
        stop(sprintf(
            "idefix returned invalid D-error: %s",
            idefix_result$d_error
        ))
    }

    # Validate profileIDs are within expected range
    profile_ids <- as.vector(idefix_result$design_matrix)
    valid_ids <- opt_env$available_profile_ids
    if (opt_env$no_choice) {
        valid_ids <- c(valid_ids, 0) # 0 is valid for no-choice
    }

    invalid_ids <- setdiff(profile_ids, valid_ids)
    if (length(invalid_ids) > 0) {
        stop(sprintf(
            "idefix returned invalid profile IDs: %s",
            paste(utils::head(invalid_ids, 5), collapse = ", ")
        ))
    }

    # Check for obvious problems in design matrix
    check_design_matrix_quality(idefix_result$design_matrix, opt_env)

    invisible(TRUE)
}

check_design_matrix_quality <- function(design_matrix, opt_env) {
    # Check for duplicate profiles within choice sets
    for (i in 1:nrow(design_matrix)) {
        question_profiles <- design_matrix[i, ]
        if (length(unique(question_profiles)) != length(question_profiles)) {
            stop(
                "idefix returned design with duplicate profiles in choice set ",
                i
            )
        }
    }

    # Check for completely empty design
    if (all(design_matrix == 0)) {
        stop("idefix returned empty design matrix")
    }

    # Additional quality checks could be added here
    invisible(TRUE)
}

should_fallback_on_warning <- function(warning_condition) {
    # Define warning messages that should trigger fallback
    warning_msg <- conditionMessage(warning_condition)

    fallback_triggers <- c(
        "singular",
        "convergence",
        "infinite",
        "NaN",
        "optimization failed",
        "design generation failed",
        "unable to find valid design"
    )

    # Check if any fallback trigger is present in the warning message
    return(any(sapply(fallback_triggers, function(trigger) {
        grepl(trigger, warning_msg, ignore.case = TRUE)
    })))
}

# =============================================================================
# Conversion Functions: cbcTools -> idefix
# =============================================================================

convert_to_idefix_format <- function(opt_env) {
    profiles <- opt_env$profiles
    priors <- opt_env$priors
    n <- opt_env$n

    # Get attribute information
    profile_attrs <- setdiff(names(profiles), "profileID")

    # Determine levels and coding for each attribute
    lvls <- c()
    coding <- c()
    c.lvls <- list()
    c_lvl_counter <- 1

    for (attr in profile_attrs) {
        values <- profiles[[attr]]
        if (is.numeric(values)) {
            # Continuous attribute
            unique_vals <- sort(unique(values))
            lvls <- c(lvls, length(unique_vals))
            coding <- c(coding, "C")
            c.lvls[[c_lvl_counter]] <- unique_vals
            c_lvl_counter <- c_lvl_counter + 1
        } else {
            # Categorical attribute (dummy coding)
            unique_vals <- if (is.factor(values)) {
                levels(values)
            } else {
                unique(values)
            }
            lvls <- c(lvls, length(unique_vals))
            coding <- c(coding, "D")
        }
    }

    # Handle candidate set creation
    if (opt_env$method == "cea") {
        # CEA doesn't use candidate set, needs full factorial
        cand_set <- NULL

        # Validate that profiles are full factorial for CEA
        expected_profiles <- prod(lvls)
        if (nrow(profiles) < expected_profiles) {
            stop("CEA method with idefix requires full factorial profiles")
        }
    } else {
        # Modfed uses candidate set
        cand_set <- create_idefix_candidate_set(
            profiles,
            lvls,
            coding,
            c.lvls,
            profile_attrs
        )
    }

    # Convert priors to idefix format
    idefix_priors <- convert_priors_to_idefix(priors, profile_attrs, opt_env)

    # Handle alternative specific constants and no-choice
    alt_cte_info <- setup_alt_cte_and_nochoice(opt_env)

    # Calculate idefix parameters to match cbcTools blocking approach
    if (n$blocks > 1) {
        # cbcTools approach: each respondent gets n_q questions
        # So we need n_blocks * n_q total choice sets, divided into n_blocks blocks
        idefix_n_sets <- n$blocks * n$q # Total choice sets needed
        idefix_n_blocks <- n$blocks # Number of blocks
    } else {
        # Single block case
        idefix_n_sets <- n$q
        idefix_n_blocks <- 1
    }

    return(list(
        lvls = lvls,
        coding = coding,
        c.lvls = if (length(c.lvls) > 0) c.lvls else NULL,
        cand_set = cand_set,
        par_draws = idefix_priors$par_draws,
        n_alts = alt_cte_info$n_alts,
        n_sets = idefix_n_sets, # Total choice sets
        n_blocks = idefix_n_blocks, # Number of blocks
        alt_cte = alt_cte_info$alt_cte,
        no_choice = alt_cte_info$no_choice,
        attr_mapping = idefix_priors$attr_mapping
    ))
}

create_idefix_candidate_set <- function(
    profiles,
    lvls,
    coding,
    c.lvls,
    profile_attrs
) {
    # Check if profiles are restricted
    profiles_restricted <- is_profiles_restricted(profiles, lvls)

    if (!profiles_restricted) {
        # Use idefix::Profiles for unrestricted case
        return(idefix::Profiles(
            lvls = lvls,
            coding = coding,
            c.lvls = c.lvls
        ))
    }

    # For restricted profiles, manually dummy-code
    return(create_restricted_candidate_set(
        profiles,
        lvls,
        coding,
        c.lvls,
        profile_attrs
    ))
}

create_restricted_candidate_set <- function(
    profiles,
    lvls,
    coding,
    c.lvls,
    profile_attrs
) {
    # Get the profile data without profileID
    profile_lvls <- profiles[, profile_attrs, drop = FALSE]

    # Get type information
    type_ids <- get_type_ids(profiles)

    # Create unrestricted candidate set as template for column names
    template_cand_set <- idefix::Profiles(
        lvls = lvls,
        coding = coding,
        c.lvls = c.lvls
    )

    # Manually dummy-code the restricted profiles
    if (any(type_ids$discrete)) {
        discrete_cols <- names(profile_lvls)[type_ids$discrete]
        cand_set_res <- fastDummies::dummy_cols(
            profile_lvls,
            select_columns = discrete_cols,
            remove_first_dummy = TRUE,
            remove_selected_columns = TRUE
        )
    } else {
        cand_set_res <- profile_lvls
    }

    # Reorder columns to match idefix expectations
    name_order <- profile_attrs
    names_coded <- names(cand_set_res)
    cols <- c()

    for (i in seq_len(length(coding))) {
        if (coding[i] == "C") {
            name_match <- name_order[i]
        } else {
            name_match <- names_coded[grepl(
                paste0(name_order[i], "_"),
                names_coded
            )]
        }
        cols <- c(cols, name_match)
    }

    cand_set_res <- cand_set_res[, cols, drop = FALSE]
    names(cand_set_res) <- colnames(template_cand_set)
    cand_set_res <- as.matrix(cand_set_res)
    row.names(cand_set_res) <- seq(nrow(cand_set_res))

    return(cand_set_res)
}

is_profiles_restricted <- function(profiles, lvls) {
    # Check if we have all possible combinations
    expected_combinations <- prod(lvls)
    actual_combinations <- nrow(profiles)
    return(actual_combinations < expected_combinations)
}

# =============================================================================
# Prior Conversion Functions
# =============================================================================

convert_priors_to_idefix <- function(priors, profile_attrs, opt_env) {
    if (is.null(priors)) {
        stop("idefix integration requires priors to be specified")
    }

    # Build parameter vector in idefix order
    par_vector <- c()
    attr_mapping <- list() # Track mapping for reconstruction

    # Handle no-choice first if present
    if (opt_env$no_choice && !is.null(priors$attrs$no_choice)) {
        no_choice_val <- priors$attrs$no_choice$mean
        par_vector <- c(par_vector, no_choice_val)
        attr_mapping$no_choice <- list(
            type = "no_choice",
            indices = 1,
            mean = no_choice_val
        )
    }

    # Handle main effects in profile order
    param_index <- length(par_vector) + 1

    for (attr in profile_attrs) {
        attr_info <- priors$attrs[[attr]]

        if (attr_info$continuous) {
            # Continuous parameter
            par_vector <- c(par_vector, attr_info$mean)
            attr_mapping[[attr]] <- list(
                type = "continuous",
                indices = param_index,
                mean = attr_info$mean
            )
            param_index <- param_index + 1
        } else {
            # Categorical parameter (effects coding)
            if (!is.null(names(attr_info$mean))) {
                # Named parameters
                level_names <- names(attr_info$mean)
                par_values <- attr_info$mean
            } else {
                # Unnamed parameters - use level order
                all_levels <- attr_info$levels
                level_names <- all_levels[-1] # Skip reference level
                par_values <- attr_info$mean
                names(par_values) <- level_names
            }

            par_vector <- c(par_vector, par_values)
            attr_mapping[[attr]] <- list(
                type = "categorical",
                indices = param_index:(param_index + length(par_values) - 1),
                levels = level_names,
                means = par_values,
                reference = setdiff(attr_info$levels, level_names)[1]
            )
            param_index <- param_index + length(par_values)
        }
    }

    # Handle parameter draws for Bayesian designs
    if (!is.null(priors$par_draws)) {
        # Use existing parameter draws, but need to reorder for idefix
        par_draws <- reorder_par_draws_for_idefix(
            priors$par_draws,
            attr_mapping,
            opt_env
        )
    } else {
        # For fixed priors, replicate the parameter vector without variance
        n_draws <- 50
        par_draws <- matrix(
            rep(par_vector, each = n_draws),
            nrow = n_draws,
            ncol = length(par_vector),
            byrow = FALSE
        )
    }

    # Handle alternative specific constants format
    if (opt_env$no_choice || !is.null(opt_env$label)) {
        par_draws <- format_par_draws_for_asc(par_draws, attr_mapping, opt_env)
    }

    return(list(
        par_draws = par_draws,
        attr_mapping = attr_mapping
    ))
}

reorder_par_draws_for_idefix <- function(par_draws, attr_mapping, opt_env) {
    # Reorder parameter draws to match idefix expectations
    # idefix expects: [ASCs, main effects]

    # Get current parameter names from cbcTools
    current_names <- colnames(par_draws)

    # Handle case where colnames might be NULL
    if (is.null(current_names)) {
        # If no column names, assume the order matches the parameter vector order
        warning("par_draws has no column names, assuming standard order")
        return(par_draws)
    }

    # Build new order based on attr_mapping
    new_order <- c()

    # Add no-choice first if present
    if (opt_env$no_choice && "no_choice" %in% names(attr_mapping)) {
        no_choice_idx <- which(current_names == "no_choice")
        if (length(no_choice_idx) > 0) {
            new_order <- c(new_order, no_choice_idx)
        }
    }

    # Add main effects in order
    for (attr in names(attr_mapping)) {
        if (attr == "no_choice") {
            next
        }

        mapping <- attr_mapping[[attr]]
        if (mapping$type == "continuous") {
            param_idx <- which(current_names == attr)
            if (length(param_idx) > 0) {
                new_order <- c(new_order, param_idx)
            }
        } else {
            # Categorical - find dummy coded parameters
            for (level in mapping$levels) {
                # Try multiple naming conventions
                possible_names <- c(
                    paste0(attr, level),
                    paste0(attr, "_", level),
                    paste0(attr, ".", level)
                )

                param_idx <- which(current_names %in% possible_names)
                if (length(param_idx) > 0) {
                    new_order <- c(new_order, param_idx[1]) # Take first match
                }
            }
        }
    }

    # Validate that we found all parameters
    if (length(new_order) != ncol(par_draws)) {
        warning(sprintf(
            "Could not match all parameters: found %d out of %d",
            length(new_order),
            ncol(par_draws)
        ))
        # Fallback to original order
        return(par_draws)
    }

    return(par_draws[, new_order, drop = FALSE])
}

format_par_draws_for_asc <- function(par_draws, attr_mapping, opt_env) {
    # Split parameter draws into ASC and main effects for idefix

    asc_indices <- c()
    main_indices <- c()

    # Identify ASC parameters (no_choice, labeled)
    if (opt_env$no_choice && "no_choice" %in% names(attr_mapping)) {
        asc_indices <- c(asc_indices, attr_mapping$no_choice$indices)
    }

    # All other parameters are main effects
    all_indices <- 1:ncol(par_draws)
    main_indices <- setdiff(all_indices, asc_indices)

    if (length(asc_indices) > 0) {
        asc_draws <- matrix(
            par_draws[, asc_indices],
            ncol = length(asc_indices)
        )
        main_draws <- par_draws[, main_indices, drop = FALSE]
        return(list(asc_draws, main_draws))
    } else {
        return(par_draws)
    }
}

# =============================================================================
# Alternative Specific Constants and No-Choice Setup
# =============================================================================

setup_alt_cte_and_nochoice <- function(opt_env) {
    n_alts <- opt_env$n$alts
    alt_cte <- rep(0, n_alts)
    no_choice <- FALSE

    if (opt_env$no_choice) {
        # Add no-choice alternative
        n_alts <- n_alts + 1
        alt_cte <- c(alt_cte, 1) # ASC for no-choice
        no_choice <- TRUE
    }

    if (!is.null(opt_env$label)) {
        # Labeled design - add ASCs for alternatives 2, 3, etc.
        # First alternative (reference) gets no ASC
        for (i in 2:n_alts) {
            alt_cte[i] <- 1
        }
    }

    return(list(
        n_alts = n_alts,
        alt_cte = if (any(alt_cte == 1)) alt_cte else NULL,
        no_choice = no_choice
    ))
}

# =============================================================================
# Conversion Functions: idefix -> cbcTools
# =============================================================================

convert_from_idefix_format <- function(design_result, idefix_inputs, opt_env) {
    # Check if we have blocks in the result
    blocks <- design_result$BestDesign$Blocks
    if (!is.null(blocks) && length(blocks) > 0) {
        # Handle blocked design
        return(convert_blocked_idefix_design(
            design_result,
            idefix_inputs,
            opt_env
        ))
    } else {
        # Handle non-blocked design (original approach)
        return(convert_single_idefix_design(
            design_result,
            idefix_inputs,
            opt_env
        ))
    }
}

convert_blocked_idefix_design <- function(
    design_result,
    idefix_inputs,
    opt_env
) {
    # Process each block separately and combine
    blocks <- design_result$BestDesign$Blocks
    all_designs <- list()

    for (block_idx in seq_along(blocks)) {
        block_design <- blocks[[block_idx]]

        # Decode this block's design
        decoded_block <- idefix::Decode(
            des = block_design,
            n.alts = idefix_inputs$n_alts,
            alt.cte = idefix_inputs$alt_cte,
            lvl.names = get_level_names_for_decode(opt_env$profiles),
            c.lvls = idefix_inputs$c.lvls,
            coding = idefix_inputs$coding,
            no.choice = if (idefix_inputs$no_choice) {
                idefix_inputs$n_alts
            } else {
                NULL
            }
        )

        # Convert to data frame and add block identifier
        block_df <- decoded_block$design
        profile_attrs <- setdiff(names(opt_env$profiles), "profileID")
        names(block_df) <- profile_attrs

        # FILTER OUT NO-CHOICE ROWS if no_choice option is enabled
        if (opt_env$no_choice) {
            # Identify no-choice rows (rows with all NA values)
            no_choice_rows <- apply(block_df[, profile_attrs], 1, function(x) {
                all(is.na(x))
            })

            if (any(no_choice_rows)) {
                # Remove no-choice rows - they'll be added back later
                block_df <- block_df[!no_choice_rows, , drop = FALSE]
            }
        }

        # Add block ID for tracking
        block_df$blockID <- block_idx

        all_designs[[block_idx]] <- block_df
    }

    # Combine all blocks
    combined_design <- do.call(rbind, all_designs)

    # Use the original join_profiles function (no modifications needed)
    design_with_ids <- join_profiles(combined_design, opt_env$profiles)

    # Convert to design matrix format expected by cbcTools
    design_matrix <- convert_blocked_to_design_matrix(design_with_ids, opt_env)

    return(design_matrix)
}

convert_single_idefix_design <- function(
    design_result,
    idefix_inputs,
    opt_env
) {
    # Get the optimized design from idefix
    idefix_design <- design_result$BestDesign$design

    # Decode the design back to readable format
    decoded_design <- idefix::Decode(
        des = idefix_design,
        n.alts = idefix_inputs$n_alts,
        alt.cte = idefix_inputs$alt_cte,
        lvl.names = get_level_names_for_decode(opt_env$profiles),
        c.lvls = idefix_inputs$c.lvls,
        coding = idefix_inputs$coding,
        no.choice = if (idefix_inputs$no_choice) idefix_inputs$n_alts else NULL
    )

    # Convert back to cbcTools format using the decoded design directly
    design_df <- decoded_design$design
    profile_attrs <- setdiff(names(opt_env$profiles), "profileID")
    names(design_df) <- profile_attrs

    # FILTER OUT NO-CHOICE ROWS if no_choice option is enabled
    if (opt_env$no_choice) {
        # Identify no-choice rows (rows with all NA values)
        no_choice_rows <- apply(design_df[, profile_attrs], 1, function(x) {
            all(is.na(x))
        })

        if (any(no_choice_rows)) {
            # Remove no-choice rows - they'll be added back later by design_matrix_no_choice()
            design_df <- design_df[!no_choice_rows, , drop = FALSE]
        }
    }

    # Now use the original join_profiles function (no modifications needed)
    design_with_ids <- join_profiles(design_df, opt_env$profiles)

    # Convert to design matrix format expected by cbcTools
    design_matrix <- convert_to_design_matrix(design_with_ids, opt_env)

    return(design_matrix)
}

convert_blocked_to_design_matrix <- function(design_with_ids, opt_env) {
    # Convert blocked design to design matrix format
    n_sets <- opt_env$n$blocks * opt_env$n$q # Total choice sets
    n_alts <- opt_env$n$alts

    design_matrix <- matrix(0, nrow = n_sets, ncol = n_alts)

    # Process each block - idefix returns blocks with equal numbers of choice sets
    sets_per_block <- opt_env$n$q # Each block should have n_q choice sets

    for (block_id in 1:opt_env$n$blocks) {
        block_rows <- design_with_ids[design_with_ids$blockID == block_id, ]

        # Calculate the starting row for this block in the design matrix
        block_start_row <- (block_id - 1) * sets_per_block + 1

        # Process each choice set within this block
        n_sets_in_block <- nrow(block_rows) / n_alts

        for (set_idx in 1:n_sets_in_block) {
            set_start_row <- (set_idx - 1) * n_alts + 1
            set_end_row <- set_start_row + n_alts - 1

            if (set_end_row <= nrow(block_rows)) {
                profile_ids <- block_rows$profileID[set_start_row:set_end_row]

                # Calculate position in overall design matrix
                matrix_row <- block_start_row + set_idx - 1

                if (matrix_row <= nrow(design_matrix)) {
                    design_matrix[matrix_row, ] <- profile_ids
                }
            }
        }
    }

    return(design_matrix)
}

get_level_names_for_decode <- function(profiles) {
    # Create level names list for idefix::Decode
    profile_attrs <- setdiff(names(profiles), "profileID")
    lvl_names <- list()

    for (attr in profile_attrs) {
        values <- profiles[[attr]]
        if (is.numeric(values)) {
            lvl_names[[length(lvl_names) + 1]] <- as.character(sort(unique(
                values
            )))
        } else {
            unique_vals <- if (is.factor(values)) {
                levels(values)
            } else {
                unique(values)
            }
            lvl_names[[length(lvl_names) + 1]] <- as.character(unique_vals)
        }
    }

    return(lvl_names)
}

join_profiles <- function(design, profiles) {
    # This function matches the decoded design to profile IDs
    # Similar to your old join_profiles function

    profile_attrs <- setdiff(names(profiles), "profileID")

    # Initialize profileID column
    design$profileID <- NA

    # Match each row in design to profiles
    for (i in 1:nrow(design)) {
        design_row <- design[i, profile_attrs, drop = FALSE]

        # Find matching profile
        for (j in 1:nrow(profiles)) {
            profile_row <- profiles[j, profile_attrs, drop = FALSE]

            # Check if all attributes match
            matches <- TRUE
            for (attr in profile_attrs) {
                if (is.numeric(design_row[[attr]])) {
                    # For continuous variables, exact match
                    if (design_row[[attr]] != profile_row[[attr]]) {
                        matches <- FALSE
                        break
                    }
                } else {
                    # For categorical variables, convert both to character for comparison
                    if (
                        as.character(design_row[[attr]]) !=
                            as.character(profile_row[[attr]])
                    ) {
                        matches <- FALSE
                        break
                    }
                }
            }

            if (matches) {
                design$profileID[i] <- profiles$profileID[j]
                break
            }
        }

        if (is.na(design$profileID[i])) {
            warning("Could not match profile in row ", i)
        }
    }

    return(design)
}

convert_to_design_matrix <- function(design_with_ids, opt_env) {
    # With idefix blocking, the design already has optimal block allocation
    # We just need to convert to the matrix format cbcTools expects

    n_sets <- opt_env$n$blocks * opt_env$n$q # Total choice sets
    n_alts <- opt_env$n$alts

    design_matrix <- matrix(0, nrow = n_sets, ncol = n_alts)

    for (i in 1:n_sets) {
        start_row <- (i - 1) * n_alts + 1
        end_row <- start_row + n_alts - 1

        if (end_row <= nrow(design_with_ids)) {
            profile_ids <- design_with_ids$profileID[start_row:end_row]
            design_matrix[i, ] <- profile_ids
        }
    }

    return(design_matrix)
}

# =============================================================================
# Helper Functions
# =============================================================================

get_type_ids <- function(profiles) {
    # Replicate the function from cbcTools for type detection
    profile_data <- profiles[,
        setdiff(names(profiles), "profileID"),
        drop = FALSE
    ]
    types <- lapply(profile_data, class)
    test <- function(x) {
        x[1]
    }
    type_names <- unlist(lapply(types, test))

    ids <- list()
    ids$discrete <- type_names %in% c("factor", "character")
    ids$continuous <- !ids$discrete

    return(ids)
}

get_type_ids <- function(profiles) {
    types <- get_col_types(profiles[, 2:ncol(profiles)])
    ids <- list()
    ids$discrete <- types %in% c("factor", "character")
    ids$continuous <- !ids$discrete
    return(ids)
}

get_col_types <- function(data) {
    types <- lapply(data, class)
    test <- function(x) {
        x[1]
    }
    return(unlist(lapply(types, test)))
}

check_inputs_profiles <- function(levels) {
    for (i in 1:length(levels)) {
        check_vector <- !is.vector(levels[[i]])
        check_name <- is.null(names(levels)[i])
        if (check_vector | check_name) {
            stop(
                'Each item in "..." must be a named vector where the names are ',
                'attributes and the values in the vector are levels of that attribute'
            )
        }
    }

    for (attr_name in names(levels)) {
        if (length(levels[[attr_name]]) < 2) {
            stop(sprintf(
                "Attribute '%s' must have at least 2 levels for meaningful choice experiments",
                attr_name
            ))
        }
    }
}

validate_profiles <- function(profiles) {
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }
}

validate_design <- function(design) {
    if (!inherits(design, "cbc_design")) {
        stop("profiles must be a cbc_design object created by cbc_design()")
    }
}

# Validate correlations list
validate_correlations <- function(correlations) {
    if (!is.list(correlations)) {
        stop(
            "correlations must be a list of correlation specifications created by cor_spec()"
        )
    }
    if (!all(sapply(correlations, inherits, "cbc_correlation"))) {
        stop("all correlations must be created using cor_spec()")
    }
}

# Helper function to create a simple hash of profiles structure
digest_profiles <- function(profiles) {
    # Create a simple hash based on attribute info and structure
    attr_info <- attr(profiles, "attribute_info")
    structure_string <- paste(
        names(attr_info),
        sapply(attr_info, function(x) {
            paste(x$type, x$n_levels, collapse = "_")
        }),
        collapse = "|"
    )
    # Use a simple hash - in production you might want digest::digest()
    abs(sum(utf8ToInt(structure_string)))
}

# Validate that priors are compatible with profiles
validate_priors <- function(priors, profiles, no_choice) {
    if (is.null(priors)) {
        return(TRUE)
    }
    if (no_choice) {
        if (!"no_choice" %in% names(priors$pars)) {
            stop(
                "Since 'no_choice = TRUE', you must provide a 'no_choice' ",
                "value with cbc_priors()"
            )
        }
    }
    if (!inherits(priors, "cbc_priors")) {
        stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }

    priors_meta <- priors$profiles_metadata
    current_hash <- digest_profiles(profiles)

    # Check if profiles structure has changed
    if (priors_meta$profile_hash != current_hash) {
        current_attr_info <- attr(profiles, "attribute_info")

        # More detailed comparison
        if (!identical(priors_meta$attribute_info, current_attr_info)) {
            warning(
                "Priors were created for different profile attributes or levels. ",
                "Consider recreating priors with cbc_priors().",
                call. = FALSE
            )
        } else if (priors_meta$n_profiles != nrow(profiles)) {
            message(
                "Priors were created for profiles with ",
                priors_meta$n_profiles,
                " rows, but current profiles have ",
                nrow(profiles),
                " rows. ",
                "This is typically fine if you've applied restrictions."
            )
        }
    }
}

# Validate that priors are compatible with profiles
validate_priors_profiles <- function(priors, profiles) {
    if (!inherits(priors, "cbc_priors")) {
        stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }

    priors_meta <- priors$profiles_metadata
    current_hash <- digest_profiles(profiles)

    # Check if profiles structure has changed
    if (priors_meta$profile_hash != current_hash) {
        current_attr_info <- attr(profiles, "attribute_info")

        if (!identical(priors_meta$attribute_info, current_attr_info)) {
            warning(
                "Priors were created for different profile attributes or levels. ",
                "Consider recreating priors with cbc_priors().",
                call. = FALSE
            )
        } else if (priors_meta$n_profiles != nrow(profiles)) {
            message(
                "Priors were created for profiles with ",
                priors_meta$n_profiles,
                " rows, but current profiles have ",
                nrow(profiles),
                " rows. ",
                "This is typically fine if you've applied restrictions."
            )
        }
    }

    invisible(TRUE)
}

# Validates all inputs to ensure they meet requirements for design generation
validate_design_inputs <- function(
    profiles,
    method,
    priors,
    n_alts,
    n_q,
    n_resp,
    n_blocks,
    no_choice,
    label,
    balance_by,
    randomize_questions,
    randomize_alts,
    remove_dominant,
    dominance_types,
    dominance_threshold,
    max_dominance_attempts,
    include_probs,
    use_idefix,
    coding
) {
    # Validate profiles
    validate_profiles(profiles)

    if (nrow(profiles) == 0) {
        stop("profiles must contain at least one profile")
    }

    # Validate method
    valid_methods <- get_methods_all()
    if (!method %in% valid_methods) {
        stop("method must be one of: ", paste(valid_methods, collapse = ", "))
    }

    # Validate coding
    valid_coding <- c("effects", "dummy", "standard")
    if (!coding %in% valid_coding) {
        stop("coding must be one of: ", paste(valid_coding, collapse = ", "))
    }

    # CEA-specific validation: requires full factorial profiles
    if (method == "cea") {
        validate_cea_profiles(profiles)
    }

    # Validate include_probs
    if (include_probs) {
        if (is.null(priors)) {
            stop(
                "'priors' must be specified if include_probs = TRUE"
            )
        }
    }

    # Validate priors
    if (!is.null(priors) && !inherits(priors, "cbc_priors")) {
        stop(
            "priors must be a cbc_priors object created by cbc_priors() or NULL"
        )
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
            warning(
                "For random designs with n_resp = 1, consider using an optimized method for better efficiency"
            )
        }
    } else if (method %in% c("stochastic", "modfed", "cea")) {
        if (n_blocks == 1) {
            message(sprintf(
                "%s design will be optimized into 1 design block, then allocated across %d respondents",
                tools::toTitleCase(method),
                n_resp
            ))
        }
        if (n_blocks > 1) {
            message(sprintf(
                "%s design will be optimized into %d design blocks, then allocated across %d respondents",
                tools::toTitleCase(method),
                n_blocks,
                n_resp
            ))
        }
    }

    # Check that priors are appropriate if specified

    if (!is.null(priors)) {
        # Check that prior names aren't missing
        prior_names <- names(priors$attrs)
        profile_attrs <- get_var_names(profiles)
        missing <- setdiff(profile_attrs, prior_names)
        if (length(missing) > 0) {
            stop(
                '"priors" is missing the following variables: \n\n',
                paste(missing, collapse = "\n")
            )
        }

        # Check that prior levels aren't missing
        type_ids <- get_type_ids(profiles)
        for (id in which(type_ids$discrete)) {
            col_index <- id + 1
            attr <- names(profiles)[col_index]
            n_lvls <- length(unique(profiles[, col_index])) - 1
            prior_means <- priors$attrs[[attr]]$mean
            if (length(prior_means) != n_lvls) {
                stop(
                    'Invalid number of values provided in "priors" for the "',
                    attr,
                    '" attribute. Please provide ',
                    n_lvls,
                    ' values'
                )
            }
        }
        for (id in which(type_ids$continuous)) {
            col_index <- id + 1
            attr <- names(profiles)[col_index]
            prior_means <- priors$attrs[[attr]]$mean
            if (length(prior_means) != 1) {
                stop(
                    'Invalid number of values provided in "priors" for the "',
                    prior_names[id],
                    '" attribute. Please provide 1 value'
                )
            }
        }
    }

    # Check feasibility constraints
    if (n_alts > nrow(profiles)) {
        stop(sprintf(
            "n_alts (%d) cannot be larger than the number of available profiles (%d)",
            n_alts,
            nrow(profiles)
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
                n_label_levels,
                n_alts
            ))
        }
    }

    # Validate balance_by constraints
    if (!is.null(balance_by)) {
        # Check that balance_by attributes exist in profiles
        missing_attrs <- setdiff(balance_by, names(profiles))
        if (length(missing_attrs) > 0) {
            stop(sprintf(
                "balance_by attributes not found in profiles: %s",
                paste(missing_attrs, collapse = ", ")
            ))
        }

        # Check that balance_by doesn't include profileID
        if ("profileID" %in% balance_by) {
            stop("balance_by cannot include 'profileID'")
        }

        # Validate that balance_by creates meaningful groups
        balance_key <- do.call(paste, c(profiles[balance_by], sep = "|"))
        n_balance_groups <- length(unique(balance_key))

        if (n_balance_groups == 1) {
            stop(
                "balance_by attributes create only one group. All profiles have identical values for the specified attributes."
            )
        }

        if (n_balance_groups > nrow(profiles) / 2) {
            warning(
                sprintf(
                    "balance_by creates %d groups from %d profiles. This may not provide meaningful balance.",
                    n_balance_groups,
                    nrow(profiles)
                ),
                call. = FALSE
            )
        }
    }

    # Check for conflicts between label and balance_by
    if (!is.null(label) && !is.null(balance_by)) {
        stop(
            "Cannot use both 'label' and 'balance_by' arguments simultaneously. Use one or the other."
        )
    }

    # Check method compatibility with balance_by
    if (!is.null(balance_by)) {
        incompatible_methods <- c("stochastic", "modfed", "cea")
        if (method %in% incompatible_methods) {
            stop(sprintf(
                "balance_by is not supported with method '%s'. Compatible methods are: %s",
                method,
                paste(
                    c("random", "shortcut", "minoverlap", "balanced"),
                    collapse = ", "
                )
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

        if (
            !is.numeric(dominance_threshold) ||
                dominance_threshold <= 0 ||
                dominance_threshold >= 1
        ) {
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
                    min_label_count,
                    n_q
                ))
            }
        } else {
            # For random designs, only check n_q (each respondent independent)
            max_possible_questions <- choose(nrow(profiles), n_alts)

            if (n_q > max_possible_questions) {
                stop(sprintf(
                    "Requested %d questions per respondent but only %d unique combinations possible with %d profiles and %d alternatives per question",
                    n_q,
                    max_possible_questions,
                    nrow(profiles),
                    n_alts
                ))
            }
        }
    } else {
        # For non-random designs, check n_q * n_blocks (base design size)
        max_possible_questions <- choose(nrow(profiles), n_alts)
        base_design_questions <- n_q * n_blocks

        if (base_design_questions > max_possible_questions) {
            stop(sprintf(
                "Requested %d questions in base design but only %d unique combinations possible with %d profiles and %d alternatives per question",
                base_design_questions,
                max_possible_questions,
                nrow(profiles),
                n_alts
            ))
        }
    }

    # Validate no-choice with priors
    if (no_choice && !is.null(priors) && !priors$has_no_choice) {
        stop(
            "no_choice = TRUE requires priors to include a no_choice parameter. Use cbc_priors(..., no_choice = value)"
        )
    }
    if (!is.null(priors) && priors$has_no_choice) {
        if (!no_choice) {
            stop(
                "Since priors has a no_choice value, must set no_choice = TRUE in cbc_design()"
            )
        }
    }

    # Check that number of questions per respondents is larger than the
    # unique number of choice sets
    if (n_q > floor(nrow(profiles) / n_alts)) {
        # The first if statement is because the next one only matters with a
        # small number of profiles, so most cases where n is large the next
        # if statement isn't necessary. If the number of profiles is too large,
        # the next if statement will error because R integers have a maximum
        # value of 2^31 - 1. See this issue:
        # https://github.com/jhelvy/cbcTools/issues/10#issuecomment-1535454495
        n <- nrow(profiles)
        k <- n_alts
        ncomb <- choose(n, k) # More robust
        # ncomb <- factorial(n) / (factorial(k)*(factorial(n-k)))
        if (n_q > ncomb) {
            stop(
                'The number of questions per respondent, specified by "n_q", ',
                "is larger than the number of unique sets of choice sets. ",
                'You can correct this by decreasing "n_q" to be less than ',
                ncomb,
                ', decreasing "n_alts", or add more attributes / levels ',
                "to increase the number of choice set combinations."
            )
        }
    }

    if (use_idefix) {
        # Check if idefix is available
        if (!requireNamespace("idefix", quietly = TRUE)) {
            stop(
                "Package 'idefix' is required for use_idefix = TRUE. Please install it with install.packages('idefix')"
            )
        }

        # Check method compatibility
        if (!method %in% c("cea", "modfed")) {
            stop(
                "Only 'cea' and 'modfed' methods can be used if use_idefix = TRUE"
            )
        }

        # If no priors provided, we'll create zero priors automatically
        # (This is handled in setup_optimization_environment)
    }

    invisible(TRUE)
}

# CEA validation function
validate_cea_profiles <- function(profiles) {
    # Check if profiles appear to be restricted (not full factorial)

    # Get attribute info
    attr_info <- attr(profiles, "attribute_info")
    if (is.null(attr_info)) {
        # If no metadata, try to infer from the data
        attr_names <- setdiff(names(profiles), "profileID")
        total_combinations <- 1

        for (attr in attr_names) {
            n_levels <- length(unique(profiles[[attr]]))
            total_combinations <- total_combinations * n_levels
        }

        if (nrow(profiles) < total_combinations) {
            stop(sprintf(
                "CEA method requires full factorial profiles (all possible attribute combinations). ",
                "Expected %d profiles but found %d. ",
                "CEA cannot optimize when some attribute combinations are missing. ",
                "Consider using 'stochastic' or 'modfed' methods instead, or use unrestricted profiles.",
                total_combinations,
                nrow(profiles)
            ))
        }
    } else {
        # Use metadata if available
        total_removed <- attr(profiles, "total_removed") %||% 0
        restrictions_applied <- attr(profiles, "restrictions_applied")

        if (
            total_removed > 0 ||
                (!is.null(restrictions_applied) &&
                    length(restrictions_applied) > 0)
        ) {
            stop(
                "CEA method requires full factorial profiles (all possible attribute combinations). ",
                "The provided profiles have restrictions applied, which means some attribute ",
                "combinations are missing. CEA cannot optimize when some combinations are unavailable. ",
                "Consider using 'stochastic' or 'modfed' methods instead, or use unrestricted profiles."
            )
        }

        # Double-check by calculating expected combinations
        total_combinations <- 1
        for (attr_name in names(attr_info)) {
            total_combinations <- total_combinations *
                attr_info[[attr_name]]$n_levels
        }

        if (nrow(profiles) < total_combinations) {
            stop(sprintf(
                "CEA method requires full factorial profiles. Expected %d profiles but found %d. ",
                "Some attribute combinations appear to be missing.",
                total_combinations,
                nrow(profiles)
            ))
        }
    }

    invisible(TRUE)
}

validate_idefix_inputs <- function(idefix_inputs, opt_env) {
    # Basic structure validation
    required_fields <- c("lvls", "coding", "par_draws", "n_alts", "n_sets")
    missing_fields <- setdiff(required_fields, names(idefix_inputs))

    if (length(missing_fields) > 0) {
        stop(
            "Missing required idefix inputs: ",
            paste(missing_fields, collapse = ", ")
        )
    }

    # Validate lvls and coding
    if (length(idefix_inputs$lvls) != length(idefix_inputs$coding)) {
        stop("lvls and coding must have same length")
    }

    if (any(idefix_inputs$lvls < 1)) {
        stop("All lvls must be positive integers")
    }

    if (!all(idefix_inputs$coding %in% c("C", "D", "E"))) {
        stop("coding must contain only 'C', 'D', or 'E' values")
    }

    # Validate parameter draws format and content
    if (is.list(idefix_inputs$par_draws)) {
        # ASC format validation
        if (length(idefix_inputs$par_draws) != 2) {
            stop("par_draws list must have exactly 2 elements for ASC format")
        }

        asc_draws <- idefix_inputs$par_draws[[1]]
        main_draws <- idefix_inputs$par_draws[[2]]

        if (!is.matrix(asc_draws) || !is.matrix(main_draws)) {
            stop("Both elements of par_draws list must be matrices")
        }

        if (nrow(asc_draws) != nrow(main_draws)) {
            stop("ASC and main effect draws must have same number of rows")
        }

        # Check for missing values
        if (any(is.na(asc_draws)) || any(is.na(main_draws))) {
            stop("par_draws contains missing values")
        }

        # Check for infinite values
        if (any(!is.finite(asc_draws)) || any(!is.finite(main_draws))) {
            stop("par_draws contains infinite values")
        }
    } else if (is.matrix(idefix_inputs$par_draws)) {
        # Regular format validation
        if (
            nrow(idefix_inputs$par_draws) < 1 ||
                ncol(idefix_inputs$par_draws) < 1
        ) {
            stop("par_draws matrix must have positive dimensions")
        }

        # Check for missing values
        if (any(is.na(idefix_inputs$par_draws))) {
            stop("par_draws contains missing values")
        }

        # Check for infinite values
        if (any(!is.finite(idefix_inputs$par_draws))) {
            stop("par_draws contains infinite values")
        }
    } else {
        stop("par_draws must be a matrix or list of matrices")
    }

    # Validate candidate set for Modfed
    if (opt_env$method == "modfed") {
        if (is.null(idefix_inputs$cand_set)) {
            stop("Modfed requires candidate set")
        }
        if (!is.matrix(idefix_inputs$cand_set)) {
            stop("candidate set must be a matrix")
        }
        if (nrow(idefix_inputs$cand_set) < idefix_inputs$n_alts) {
            stop("candidate set must have at least n_alts rows")
        }

        # Check for missing values in candidate set
        if (any(is.na(idefix_inputs$cand_set))) {
            stop("candidate set contains missing values")
        }
    }

    # Validate design dimensions
    if (idefix_inputs$n_alts < 2) {
        stop("n_alts must be at least 2")
    }

    if (idefix_inputs$n_sets < 1) {
        stop("n_sets must be at least 1")
    }

    # Validate alt_cte if present
    if (!is.null(idefix_inputs$alt_cte)) {
        if (length(idefix_inputs$alt_cte) != idefix_inputs$n_alts) {
            stop("alt_cte length must equal n_alts")
        }
        if (!all(idefix_inputs$alt_cte %in% c(0, 1))) {
            stop("alt_cte must contain only 0 and 1 values")
        }
    }

    invisible(TRUE)
}

get_dominant_obs <- function(
    design, priors, no_choice, dominance_types, dominance_threshold
) {
    if (is.null(priors)) {
        return(c())
    }

    # Get unique choice sets (obsIDs)
    unique_obs <- unique(design$obsID)
    dominant_obs <- c()

    for (obs_id in unique_obs) {
        choice_set_rows <- design[design$obsID == obs_id, ]

        # Use the same dominance checking logic as in optdesign.R
        is_dominant <- check_choice_set_dominance(
            choice_set_rows, priors, NULL, dominance_types, dominance_threshold
        )

        if (is_dominant) {
            dominant_obs <- c(dominant_obs, obs_id)
        }
    }

    # Return row indices for dominant choice sets
    return(which(design$obsID %in% dominant_obs))
}


# Helper function to check dominance for a single choice set
check_choice_set_dominance <- function(
    choice_set_rows,
    priors,
    profiles,
    dominance_types,
    dominance_threshold
) {
    # If priors are NULL, no dominance checking is possible
    if (is.null(priors)) {
        return(FALSE)
    }

    # Get attribute columns
    attr_cols <- get_var_names(choice_set_rows)

    # Get random parameters for encoding
    randPars <- get_rand_pars(priors)

    # Encode the design matrix for this choice set
    codedData <- logitr::recodeData(choice_set_rows, attr_cols, randPars)
    X_q <- codedData$X

    # Compute total utilities
    total_utilities <- X_q %*% priors$pars

    # Convert to probabilities using logit model
    exp_utils <- exp(total_utilities)
    choice_probs <- exp_utils / sum(exp_utils)

    # Check total utility dominance if requested
    if ("total" %in% dominance_types) {
        max_prob <- max(choice_probs)
        if (max_prob > dominance_threshold) {
            return(TRUE)  # Dominance detected
        }
    }

    # Check partial utility dominance if requested
    if ("partial" %in% dominance_types) {
        # Reuse existing function from inspect.R
        has_partial_dominance <- check_partial_dominance(
            choice_set_rows, X_q, priors, attr_cols
        )
        if (has_partial_dominance) {
            return(TRUE)  # Dominance detected
        }
    }

    return(FALSE)  # No dominance detected
}

# Helper function to check partial utility dominance
check_partial_dominance <- function(q_data, X_q, priors, attr_cols) {
    # Get alternatives in this choice question
    alternatives <- split(q_data, q_data$altID)
    alt_indices <- split(seq_len(nrow(q_data)), q_data$altID)

    n_alts <- length(alternatives)
    if (n_alts < 2) {
        return(FALSE)
    }

    # For each alternative, compute partial utilities for each original attribute
    # We need to map back from the encoded matrix to original attributes

    # Get the parameter names and their mapping to original attributes
    par_names <- names(priors$pars)

    # Create a matrix to store partial utilities by alternative and attribute
    partial_utils <- matrix(0, nrow = n_alts, ncol = length(attr_cols))
    rownames(partial_utils) <- names(alternatives)
    colnames(partial_utils) <- attr_cols

    # For each original attribute, sum up the relevant parameter contributions
    for (attr in attr_cols) {
        # Find parameters that belong to this attribute
        attr_pars <- par_names[grepl(paste0("^", attr), par_names)]

        if (length(attr_pars) > 0) {
            # Get the column indices in X_q for these parameters
            attr_cols_X <- which(colnames(X_q) %in% attr_pars)

            if (length(attr_cols_X) > 0) {
                # Compute partial utility for each alternative
                for (i in seq_along(alternatives)) {
                    alt_idx <- alt_indices[[i]][1] # Take first row for this alternative
                    X_alt <- X_q[alt_idx, attr_cols_X, drop = FALSE]
                    partial_utils[i, attr] <- sum(X_alt * priors$pars[attr_cols_X])
                }
            }
        }
    }

    # Check if one alternative has the highest partial utility for ALL attributes
    for (i in seq_len(n_alts)) {
        is_best_for_all <- TRUE
        for (j in seq_along(attr_cols)) {
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

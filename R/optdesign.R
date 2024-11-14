# Helper function to compute information matrix and D-error for a single parameter draw
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

initialize_design_optimization <- function(design, priors, varNames) {
    # Validate priors object and set up objects for random pars
    validate_priors(priors)
    randPars <- get_rand_pars(priors)
    par_draws <- priors$par_draws

    # Initial encoding of design matrix and create lookup table
    obsID <- design$obsID
    reps <- table(obsID)

    # Create X matrix lookup for all possible profiles
    all_profiles <- unique(design[, c("profileID", varNames)])
    all_profiles_coded <- logitr::recodeData(all_profiles, varNames, randPars)
    X_lookup <- list(
        profileIDs = all_profiles$profileID,
        X_rows = all_profiles_coded$X
    )

    # Get initial X matrix using lookup
    X <- matrix(0, nrow = nrow(design), ncol = ncol(X_lookup$X_rows))
    for (i in seq_len(nrow(design))) {
        pid <- design$profileID[i]
        lookup_idx <- which(X_lookup$profileIDs == pid)
        X[i,] <- X_lookup$X_rows[lookup_idx,]
    }
    X_list <- split(as.data.frame(X), obsID)
    X_list <- lapply(X_list, as.matrix)
    K <- ncol(X_list[[1]])

    # If no draws, then compute standard D error, otherwise use DB error
    if (is.null(par_draws)) {
        P_draws <- NULL
        n_draws <- NULL
        draws_list <- NULL

        # Get predicted probabilities, then compute error
        probs <- compute_probs(obsID, reps, atts, priors, X)

        # Convert probabilities into list of vectors
        P_list <- split(probs, obsID)

        # Compute information matrices
        info_matrices <- compute_info_matrix(X_list, P_list)
        M_list <- info_matrices$M_list
        M_total <- info_matrices$M_total

        # Store in list
        fixed_list <- list(
            P_list = P_list,
            M_list = M_list,
            M_total = M_total
        )

        # Compute D-error
        d_error <- det(M_total)^(-1/K)

    } else {
        fixed_list <- NULL
        P_draws <- logit_draws(par_draws, X, obsID, reps)
        n_draws <- ncol(P_draws)

        # Sample calc, but storing for each set of parameter draws
        draws_list <- list()
        for (i in seq(n_draws)) {
            # Convert probabilities into list of vectors
            P_list <- split(P_draws[,i], obsID)

            # Compute information matrices for this draw
            info_matrices <- compute_info_matrix(X_list, P_list)

            # Store in list of draws
            draws_list[[i]] <- list(
                P_list = P_list,
                M_list = info_matrices$M_list,
                M_total = info_matrices$M_total,
                d_error = det(info_matrices$M_total)^(-1/K)
            )
        }

        # DB-error is average of DP-errors
        d_error <- mean(unlist(lapply(draws_list, function(x) x$d_error)))
    }

    # Return all precomputed objects
    return(list(
        design = design,
        X_lookup = X_lookup,
        X_list = X_list,
        d_error = d_error,
        K = K,
        n_draws = n_draws,
        P_draws = P_draws,
        fixed_list = fixed_list,
        draws_list = draws_list
    ))
}

# Modified update_choice_set_draws to use both X lookup and compute_info_matrix
update_choice_set_draws <- function(opt_state, temp_rows, s, q_rowIDs) {
    # Update design
    opt_state$design[q_rowIDs,] <- temp_rows

    # Get new X matrix rows from lookup
    new_X <- matrix(0, nrow = nrow(temp_rows), ncol = ncol(opt_state$X_lookup$X_rows))
    for (i in seq_len(nrow(temp_rows))) {
        pid <- temp_rows$profileID[i]
        lookup_idx <- which(opt_state$X_lookup$profileIDs == pid)
        new_X[i,] <- opt_state$X_lookup$X_rows[lookup_idx,]
    }

    # Update X_list for this observation
    opt_state$X_list[[s]] <- new_X

    # For each draw, update matrices and compute D-error
    n_draws <- opt_state$n_draws
    new_d_errors <- numeric(n_draws)
    new_M_lists <- list()
    new_M_totals <- list()

    for (i in seq_len(n_draws)) {
        # Get probabilities for this draw
        V <- new_X %*% t(opt_state$par_draws)[i,]
        new_probs <- logit(V, temp_rows$obsID, rep(nrow(temp_rows), 1))

        # Update P_list for this draw
        opt_state$draws_list[[i]]$P_list[[s]] <- new_probs

        # Calculate new matrices for this choice set and draw
        info_matrices <- compute_info_matrix(
            list(new_X),
            list(new_probs)
        )

        # Update M_total by removing old M and adding new M
        new_M_total <- opt_state$draws_list[[i]]$M_total -
            opt_state$draws_list[[i]]$M_list[[s]] +
            info_matrices$M_list[[1]]

        # Store updated matrices
        new_M_lists[[i]] <- info_matrices$M_list
        new_M_totals[[i]] <- new_M_total

        # Compute D-error for this draw
        new_d_errors[i] <- det(new_M_total)^(-1/opt_state$K)
    }

    # Average D-error across draws
    new_d_error <- mean(new_d_errors)

    # If improvement found, update all matrices
    if (new_d_error < opt_state$d_error) {
        opt_state$d_error <- new_d_error
        for (i in seq_len(n_draws)) {
            opt_state$draws_list[[i]]$M_list[[s]] <- new_M_lists[[i]][[1]]
            opt_state$draws_list[[i]]$M_total <- new_M_totals[[i]]
            opt_state$draws_list[[i]]$d_error <- new_d_errors[i]
        }
    }

    return(opt_state)
}

# Similarly modify update_choice_set for non-Bayesian case
update_choice_set <- function(opt_state, temp_rows, s, q_rowIDs, null_prior) {
    # Update design
    opt_state$design[q_rowIDs,] <- temp_rows

    # Get new X matrix rows from lookup
    new_X <- matrix(0, nrow = nrow(temp_rows), ncol = ncol(opt_state$X_lookup$X_rows))
    for (i in seq_len(nrow(temp_rows))) {
        pid <- temp_rows$profileID[i]
        lookup_idx <- which(opt_state$X_lookup$profileIDs == pid)
        new_X[i,] <- opt_state$X_lookup$X_rows[lookup_idx,]
    }

    # Update X_list for this observation
    opt_state$X_list[[s]] <- new_X

    # Update P_list for this observation
    if (!null_prior) {
        new_probs <- sim_probs_prior(temp_rows, opt_state$model, null_prior)$predicted_prob
    } else {
        new_probs <- opt_state$P_list[[s]]
    }
    opt_state$P_list[[s]] <- new_probs

    # Calculate new matrices using compute_info_matrix
    info_matrices <- compute_info_matrix(list(new_X), list(new_probs))
    new_M <- info_matrices$M_list[[1]]

    # Update M_total by removing old M and adding new M
    opt_state$M_total <- opt_state$M_total - opt_state$M_list[[s]] + new_M
    opt_state$M_list[[s]] <- new_M

    # Update D-error
    opt_state$d_error <- det(opt_state$M_total)^(-1/opt_state$K)

    return(opt_state)
}

# Helper function to get eligible profiles for a labeled design
get_eligible_profiles <- function(profiles, current_profileIDs, label = NULL, current_label_value = NULL) {
    # If no label is specified, return all profiles except current ones
    if (is.null(label)) {
        return(setdiff(profiles$profileID, current_profileIDs))
    }

    # For labeled designs, only return profiles matching the current label value
    matching_profiles <- profiles[which(profiles[[label]] == current_label_value), ]
    return(setdiff(matching_profiles$profileID, current_profileIDs))
}

# Updated optimize_design function with label handling
optimize_design <- function(
    design,
    profiles,
    priors,
    varNames,
    n_q,
    n_alts,
    max_iter,
    label = NULL
) {
    # Initialize optimization state
    opt_state <- initialize_design_optimization(design, priors, varNames)

    iter <- 1
    improved <- TRUE

    while (improved && iter <= max_iter) {
        improved <- FALSE
        start_d_error <- opt_state$d_error

        # Loop through questions
        for (s in 1:n_q) {
            question_improved <- TRUE
            q_rowIDs <- which(opt_state$design$qID == s)
            question_rows <- opt_state$design[q_rowIDs,]

            while (question_improved) {
                question_improved <- FALSE

                # Get current profiles in this question
                current_profileIDs <- question_rows$profileID

                # Loop through alternatives
                for (j in 1:n_alts) {
                    best_d_error <- opt_state$d_error

                    # Get current label value for this alternative if using labels
                    current_label_value <- if (!is.null(label)) question_rows[j, label] else NULL

                    # Get eligible profiles for this alternative
                    temp_profileIDs <- get_eligible_profiles(
                        profiles,
                        current_profileIDs,
                        label,
                        current_label_value
                    )

                    # Try each possible profile
                    for (new_profileID in temp_profileIDs) {
                        # Create temporary design with new profile
                        temp_rows <- question_rows
                        new_profile <- profiles[which(profiles$profileID == new_profileID),]
                        temp_row_j <- temp_rows[j,]
                        temp_row_j[,c('profileID', varNames)] <- new_profile
                        temp_rows[j,] <- temp_row_j

                        # Update optimization state for this choice set
                        temp_state <- update_choice_set(opt_state, temp_rows, s, q_rowIDs, null_prior)
                        temp_d_error <- temp_state$d_error

                        # Update state if improvement found
                        if (temp_d_error < best_d_error) {
                            best_d_error <- temp_d_error
                            opt_state <- temp_state
                            question_rows <- temp_rows
                            question_improved <- TRUE
                        }
                    }
                }
            }

            # Check if question modifications improved overall D-error
            if (opt_state$d_error < start_d_error) {
                improved <- TRUE
            }
        }

        iter <- iter + 1
    }

    return(opt_state)
}

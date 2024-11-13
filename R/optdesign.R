# Helper function to initialize model and precompute matrices for d-error calculation
initialize_design_optimization <- function(design, priors, varNames) {

    # Validate priors object and set up objects for random pars
    validate_priors(priors)
    randPars <- get_rand_pars(priors)
    par_draws <- priors$par_draws

    # Initial encoding of design matrix
    obsID <- design$obsID
    reps <- table(obsID)
    codedData <- logitr::recodeData(design, varNames, randPars)
    X <- codedData$X
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

        # Pre-compute initial information matrices for each choice set
        M_list <- mapply(function(X, P) {
            P_matrix <- diag(as.vector(P)) - tcrossprod(P)
            t(X) %*% P_matrix %*% X
        }, X_list, P_list, SIMPLIFY = FALSE)

        # Compute initial total information matrix and D-error
        M_total <- Reduce("+", M_list)

        # Compute D-error
        d_error <- det(M_total)^(-1/K)
    } else {
        P_draws <- logit_draws(par_draws, X, obsID, reps)
        n_draws <- ncol(P_draws)

        # Sample calc, but storing for each set of parameter draws
        draws_list <- list()
        for (i in seq(n_draws)) {

            # Convert probabilities into list of vectors
            P_list <- split(P_draws[,i], obsID)

            # Pre-compute initial information matrices for each choice set
            M_list <- mapply(function(X, P) {
                P_matrix <- diag(as.vector(P)) - tcrossprod(P)
                t(X) %*% P_matrix %*% X
            }, X_list, P_list, SIMPLIFY = FALSE)

            # Compute initial total information matrix and D-error
            M_total <- Reduce("+", M_list)

            # Compute D-error
            d_error <- det(M_total)^(-1/K)

            # Store in lists of draws
            draws_list[[i]] <- list(
                P_list = P_list,
                M_list = M_list,
                M_total = M_total,
                d_error = d_error
            )
        }

        # DB-error is average of DP-errors
        d_error <- mean(unlist(lapply(draws_list, function(x) x$d_error)))
    }

    # Return all precomputed objects
    return(list(
        design = design,
        X_list = X_list,
        P_list = P_list,
        M_list = M_list,
        M_total = M_total,
        d_error = d_error,
        K = K,
        n_draws = n_draws,
        P_draws = P_draws,
        draws_list = draws_list
    ))
}

# Helper function to update a single choice set in the optimization
update_choice_set <- function(opt_state, temp_rows, s, q_rowIDs, null_prior) {
    # Update design
    opt_state$design[q_rowIDs,] <- temp_rows

    # Encode new design rows
    new_X <- logitr::recodeData(
        temp_rows,
        opt_state$model$inputs$pars,
        opt_state$model$inputs$randPars
    )$X

    # Update X_list for this observation
    opt_state$X_list[[s]] <- new_X

    # Update P_list for this observation
    if (!null_prior) {
        new_probs <- sim_probs_prior(temp_rows, opt_state$model, null_prior)$predicted_prob
    } else {
        new_probs <- opt_state$P_list[[s]]
    }
    opt_state$P_list[[s]] <- new_probs

    # Calculate new M matrix for this choice set
    P_matrix <- diag(as.vector(new_probs)) - tcrossprod(new_probs)
    new_M <- t(new_X) %*% P_matrix %*% new_X

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

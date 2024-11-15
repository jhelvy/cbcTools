get_X_matrix <- function(profileIDs, X_lookup) {
    # Merge profileIDs with lookup table
    X <- merge(data.frame(profileID = profileIDs), X_lookup,
               by = "profileID", sort = FALSE)
    if (nrow(X) != length(profileIDs)) {
        missing <- setdiff(profileIDs, X_lookup$profileID)
        stop("Missing profiles in lookup table: ", paste(missing, collapse = ", "))
    }
    # Convert to matrix, removing profileID column
    return(as.matrix(X[, -1]))
}

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

initialize_design_optimization <- function(design, profiles, priors, varNames, n_blocks) {
    # Validate priors object and set up objects for random pars
    validate_priors(priors)
    randPars <- get_rand_pars(priors)
    par_draws <- priors$par_draws

    # Initial encoding of design matrix and create lookup table
    obsID <- design$obsID
    reps <- table(obsID)

    # Create X matrix lookup using complete profiles data frame
    profiles_coded <- logitr::recodeData(profiles, varNames, randPars)

    # Store X lookup as a data frame with profileID
    X_lookup <- data.frame(
        profileID = profiles$profileID,
        profiles_coded$X
    )

    # Get initial X matrix using lookup
    X <- merge(design["profileID"], X_lookup, by = "profileID", sort = FALSE)
    X <- as.matrix(X[, -1]) # Remove profileID column for matrix operations
    X_list <- split(as.data.frame(X), obsID)
    X_list <- lapply(X_list, as.matrix)
    K <- ncol(X_list[[1]])

    # Store in list
    result <- list(
        design = design,
        X_lookup = X_lookup,
        X_list = X_list,
        K = K
    )

    # If there are no draws, compute standard D error
    if (is.null(par_draws)) {
        # Get predicted probabilities using priors
        if (is.null(priors)) {
            # Equal probabilities if no priors
            probs <- rep(1/reps, times = reps)
        } else {
            # Compute utilities and probabilities using prior parameters
            V <- X %*% priors$pars
            probs <- logit(V, obsID, reps)
        }

        # Convert probabilities into list of vectors
        P_list <- split(probs, obsID)

        # Compute information matrices
        info_matrices <- compute_info_matrix(X_list, P_list)
        M_list <- info_matrices$M_list
        M_total <- info_matrices$M_total

        # Compute D-error
        d_error <- det(M_total)^(-1/K)*n_blocks

        # Return result list
        result$P_list <- P_list
        result$M_list <- M_list
        result$M_total <- M_total
        result$d_error <- d_error
        result$pars <- priors$pars
        return(result)
    }

    # If there are draws, then compute the DB error
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
            d_error = det(info_matrices$M_total)^(-1/K)*n_blocks
        )
    }

    # DB-error is average of DP-errors
    d_error <- mean(unlist(lapply(draws_list, function(x) x$d_error)))

    # Return result list
    result$n_draws <- n_draws
    result$P_draws <- P_draws
    result$d_error <- d_error
    result$draws_list <- draws_list
    result$par_draws <- par_draws
    return(result)
}

update_choice_set <- function(opt_state, temp_rows, s, q_rowIDs, null_prior) {
    # Update design
    opt_state$design[q_rowIDs,] <- temp_rows

    # Get new X matrix rows using data frame lookup
    new_X <- get_X_matrix(temp_rows$profileID, opt_state$X_lookup)

    # Update X_list for this observation
    opt_state$X_list[[s]] <- new_X

    # Compute new probabilities
    if (null_prior) {
        # Equal probabilities if no priors
        new_probs <- rep(1/nrow(new_X), nrow(new_X))
    } else {
        # Compute utilities and probabilities using prior parameters
        V <- new_X %*% opt_state$pars
        new_probs <- logit(V, temp_rows$obsID, rep(nrow(new_X), 1))
    }

    opt_state$P_list[[s]] <- new_probs

    # Calculate new matrices using compute_info_matrix
    info_matrices <- compute_info_matrix(list(new_X), list(new_probs))
    new_M <- info_matrices$M_list[[1]]

    # Update M_total by removing old M and adding new M
    opt_state$M_total <- opt_state$M_total - opt_state$M_list[[s]] + new_M
    opt_state$M_list[[s]] <- new_M

    # Update D-error
    opt_state$d_error <- det(opt_state$M_total)^(-1/opt_state$K)*n_blocks

    return(opt_state)
}

# Updated update_choice_set_draws function
update_choice_set_draws <- function(opt_state, priors, temp_rows, s, q_rowIDs) {
    par_draws <- priors$par_draws

    # Update design
    opt_state$design[q_rowIDs,] <- temp_rows

    # Get new X matrix rows using data frame lookup
    new_X <- get_X_matrix(temp_rows$profileID, opt_state$X_lookup)

    # Update X_list for this observation
    opt_state$X_list[[s]] <- new_X

    # For each draw, update matrices and compute D-error
    n_draws <- opt_state$n_draws
    new_d_errors <- numeric(n_draws)
    new_M_lists <- list()
    new_M_totals <- list()
    obsID <- temp_rows$obsID
    reps <- table(obsID)
    P_draws <- logit_draws(par_draws, new_X, obsID, reps)

    for (i in seq_len(n_draws)) {
        # Convert probabilities into list of vectors
        P_list <- split(P_draws[,i], obsID)

        # Compute information matrices for this draw
        info_matrices <- compute_info_matrix(list(new_X), P_list)

        # Update P_list for this draw
        opt_state$draws_list[[i]]$P_list[[s]] <- unlist(P_list)

        # Update M_total by removing old M and adding new M
        new_M_total <- opt_state$draws_list[[i]]$M_total -
            opt_state$draws_list[[i]]$M_list[[s]] +
            info_matrices$M_list[[1]]

        # Store updated matrices
        new_M_lists[[i]] <- info_matrices$M_list
        new_M_totals[[i]] <- new_M_total

        # Compute D-error for this draw
        new_d_errors[i] <- det(new_M_total)^(-1/opt_state$K)*n_blocks
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

optimize_design <- function(
    design,
    profiles,
    priors,
    varNames,
    n_q,
    n_alts,
    max_iter,
    n_blocks,
    label
) {
    # Initialize optimization state
    opt_state <- initialize_design_optimization(design, profiles, priors, varNames, n_blocks)

    # Determine if we're using Bayesian optimization with draws
    using_draws <- !is.null(opt_state$draws_list)

    iter <- 1
    improved <- TRUE

    while (improved && iter <= max_iter) {

        # Print progress update every iteration
        message("Iteration ", iter, ": D-error = ", round(opt_state$d_error, 6))

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

                    if (length(temp_profileIDs) == 0) {
                        next  # Skip if no eligible profiles found
                    }

                    # Try each possible profile
                    for (new_profileID in temp_profileIDs) {
                        # Create temporary design with new profile
                        temp_rows <- question_rows
                        new_profile <- profiles[which(profiles$profileID == new_profileID),]
                        temp_row_j <- temp_rows[j,]
                        temp_row_j[,c('profileID', varNames)] <- new_profile
                        temp_rows[j,] <- temp_row_j

                        # Try to update optimization state for this choice set
                        tryCatch({
                            if (using_draws) {
                                temp_state <- update_choice_set_draws(
                                    opt_state, priors, temp_rows, s, q_rowIDs)
                            } else {
                                temp_state <- update_choice_set(
                                    opt_state, temp_rows, s, q_rowIDs, is.null(priors))
                            }
                            temp_d_error <- temp_state$d_error

                            # Update state if improvement found
                            if (temp_d_error < best_d_error) {
                                best_d_error <- temp_d_error
                                opt_state <- temp_state
                                question_rows <- temp_rows
                                question_improved <- TRUE
                            }
                        }, error = function(e) {
                            warning("Error updating choice set: ", e$message)
                        })
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

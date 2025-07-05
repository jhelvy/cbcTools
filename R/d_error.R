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

make_X_list <- function(design_matrix, opt_env) {
    # Use the main X_matrix (without interactions) for D-error calculation
    X_design <- opt_env$X_matrix
    if (opt_env$no_choice) {
        design_matrix <- design_matrix_no_choice(design_matrix, opt_env)
        # Add no-choice row to X_design (all zeros)
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

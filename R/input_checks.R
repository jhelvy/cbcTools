check_inputs_design <- function(
    profiles,
    n_resp,
    n_alts,
    n_q,
    n_blocks,
    n_draws,
    no_choice,
    n_start,
    label,
    priors,
    prior_no_choice,
    probs,
    method,
    max_iter,
    parallel
) {
    if (n_blocks > n_resp) {
        stop("Maximum allowable number of blocks is one block per respondent")
    }

    if (no_choice) {
        if (!is.null(priors) & is.null(prior_no_choice)) {
            stop(
                "If 'no_choice = TRUE', you must specify the prior utility ",
                'value for the "no choice" option using prior_no_choice'
            )

        }

    }

    if ((method != "CEA") & (method != "Modfed")) {
        stop('The method argument must be either "CEA" or "Modfed"')
    }

    if (!is.null(priors)) {
        prior_names <- names(priors)
        profile_names <- names(profiles[,2:ncol(profiles)])
        missing <- setdiff(profile_names, prior_names)
        if (length(missing) > 0) {
            stop(
                "'priors' is missing the following variables: \n\n",
                paste(missing, collapse = "\n")
            )
        }
    }

}

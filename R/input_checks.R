check_inputs_profiles <- function(levels) {
  for (i in 1:length(levels)) {
    check_vector <- !is.vector(levels[[i]])
    check_name <- is.null(names(levels)[i])
    if (check_vector | check_name) {
      stop("Each item in ... must be a named vector where the names are attributes and the values in the vector are levels of that attribute.")
    }
  }
}

check_inputs_restrict <- function(profiles) {
  # Check if profiles is a data frame
  if (!is.data.frame(profiles)) {
    stop("The 'profiles' argument must be a data frame.")
  }

  # Check if profiles has been created by the cbc_profiles function
  if (!"profileID" %in% colnames(profiles)) {
    stop(
        "The 'profiles' data frame must be created using the 'cbc_profiles' function ",
        "and contain the 'profileID' variable."
    )
  }
}

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

    # If using a Bayesian D-efficient design with a no choice option, user must
    # specify a value for prior_no_choice
    if (no_choice) {
        if (!is.null(priors) & is.null(prior_no_choice)) {
            stop(
                "If 'no_choice = TRUE', you must specify the prior utility ",
                'value for the "no choice" option using prior_no_choice'
            )

        }

    }

    # Check that user specified an appropriate method for Bayesian D-efficient
    # designs
    if ((method != "CEA") & (method != "Modfed")) {
        stop('The method argument must be either "Modfed" or "CEA"')
    }

    # Check if there are missing levels in priors (if priors are used)
    if (!is.null(priors)) {

        # Check that prior names aren't missing
        prior_names <- names(priors)
        profile_lvls <- profiles[,2:ncol(profiles)]
        missing <- setdiff(names(profile_lvls), prior_names)
        if (length(missing) > 0) {
            stop(
                "'priors' is missing the following variables: \n\n",
                paste(missing, collapse = "\n")
            )
        }

        # Check that prior levels aren't missing
        ids <- get_type_ids(profile_lvls)
        for (id in which(ids$discrete)) {
            n_lvls <- length(unique(profile_lvls[,id])) - 1
            if (length(priors[[id]]) != n_lvls) {
                stop(
                    "Invalid number of values provided in 'priors' for the '",
                    prior_names[id], "' attribute. Please provide ", n_lvls,
                    " values"
                )
            }
        }
        for (id in which(ids$continuous)) {
            if (length(priors[[id]]) != 1) {
                stop(
                    "Invalid number of values provided in 'priors' for the '",
                    prior_names[id], "' attribute. Please provide 1 value"
                )
            }
        }
    }

    # Check that the number of alternatives per observation is larger than
    # the number of unique profiles
    if (n_alts > nrow(profiles)) {
        stop(
            "The number of alternatives per observation, specified by n_alts, ",
            "is larger than the number of unique profiles. Either decrease ",
            "n_alts to be less than ", nrow(profiles), " or add more ",
            "attributes / levels to increase the number of profiles."
        )
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
                "The number of questions per respondent, specified by n_q, ",
                "is larger than the number of unique sets of choice sets. ",
                "You can correct this by decreasing n_q to be less than ",
                ncomb, ", decreasing n_alts, or add more attributes / levels ",
                "to increase the number of choice set combinations."
            )
        }
    }
}

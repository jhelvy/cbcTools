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
}

check_inputs_restrict <- function(profiles) {
  # Check if profiles is a data frame
  if (!is.data.frame(profiles)) {
    stop('The "profiles" argument must be a data frame.')
  }

  # Check if profiles has been created by the cbc_profiles function
  if (!"profileID" %in% colnames(profiles)) {
    stop(
      'The "profiles" data frame must be created using the "cbc_profiles()"',
      'function and contain the "profileID" variable.'
    )
  }
}

check_design_method <- function(method, priors) {

  # Check that an appropriate method is used

  if (! method %in% c(
    'random', 'full', 'orthogonal', 'dopt', 'CEA', 'Modfed'
  )) {
    stop(
      'The "method" argument must be set to "random", "full", ',
      '"orthogonal", "dopt", "CEA", or "Modfed"'
    )
  }

  # Check that a Bayesian method is used if priors are used

  if (!is.null(priors)) {
    if (!method_is_bayesian(method)) {
      # Set method to 'CEA' if priors are specified and
      # user didn't specify an appropriate method.
      warning(
        'Since "priors" are specified, the "method" must be either "CEA" ',
        'or "Modfed". The specified "method" is being ignored and set to ',
        '"CEA"\n'
      )
      method <- 'CEA'
    }
  }

  return(method)
}

check_inputs_design <- function(
    profiles,
    n_resp,
    n_alts,
    n_q,
    n_blocks,
    n_draws,
    n_start,
    no_choice,
    label,
    method,
    priors,
    prior_no_choice,
    probs,
    keep_d_eff,
    keep_db_error,
    max_iter,
    parallel,
    profiles_restricted
) {

    # Checks on blocking

    if (n_blocks < 1) {
      stop('n_blocks must be greater than or equal to 1')
    }

    if (n_blocks > n_resp) {
      stop("Maximum allowable number of blocks is one block per respondent")
    }

    if ((n_blocks > 1) & (method == 'random')) {
      stop(
        'The "random" method cannot use blocking. Either change the design ',
        'method or set "n_blocks = 1"'
      )
      if ((method == 'full') & profiles_restricted) {
        stop(
          'The "full" method cannot use restricted profiles when blocking ',
          'is used. Either set "n_blocks" to 1 or use an unrestricted ',
          'set of profiles'
        )
      }
    }

    # Checks on labeled designs

    if (!is.null(label)) {
      if (!method %in% c('random', 'full')) {
        stop(
          'Labeled designs are currently only supported with the "random" or ',
          '"full" method.'
        )
      }
    }

    # Check on restricted profile sets

    if (profiles_restricted) {
      if (!method %in% c('random', 'full')) {
        stop(
          'Restricted profile sets can only be used with the "random", "full" ',
          '"dopt", or "Modfed" methods'
        )
      }
    }

    # Check that priors are appropriate if specified

    if (!is.null(priors)) {

      # If using a Bayesian D-efficient design with a no choice option,
      # user must specify a value for prior_no_choice

      if (no_choice & is.null(prior_no_choice)) {
        stop(
          'If "no_choice = TRUE" with the "CEA" or "Modfed" method, you must ',
          'specify the prior utility for the "no choice" option using ',
          '"prior_no_choice"'
        )
      }

        # Check that prior names aren't missing
        prior_names <- names(priors)
        profile_lvls <- profiles[,2:ncol(profiles)]
        missing <- setdiff(names(profile_lvls), prior_names)
        if (length(missing) > 0) {
            stop(
                '"priors" is missing the following variables: \n\n',
                paste(missing, collapse = "\n")
            )
        }

        # Check that prior levels aren't missing
        type_ids <- get_type_ids(profiles)
        for (id in which(type_ids$discrete)) {
            n_lvls <- length(unique(profile_lvls[,id])) - 1
            if (length(priors[[id]]) != n_lvls) {
                stop(
                    'Invalid number of values provided in "priors" for the "',
                    prior_names[id], '" attribute. Please provide ', n_lvls,
                    ' values'
                )
            }
        }
        for (id in which(type_ids$continuous)) {
            if (length(priors[[id]]) != 1) {
                stop(
                    'Invalid number of values provided in "priors" for the "',
                    prior_names[id], '" attribute. Please provide 1 value'
                )
            }
        }
    }

    # Check that the number of alternatives per observation is larger than
    # the number of unique profiles
    if (n_alts > nrow(profiles)) {
      stop(
        'The number of alternatives per observation, specified by "n_alts", ',
        "is larger than the number of unique profiles. Either decrease ",
        '"n_alts" to be less than ', nrow(profiles), " or add more ",
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
            'The number of questions per respondent, specified by "n_q", ',
            "is larger than the number of unique sets of choice sets. ",
            'You can correct this by decreasing "n_q" to be less than ',
            ncomb, ', decreasing "n_alts", or add more attributes / levels ',
            "to increase the number of choice set combinations."
          )
        }
    }
}

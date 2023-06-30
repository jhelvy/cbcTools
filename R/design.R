#' Make a choice-based conjoint survey design
#'
#' This function creates a data frame containing a choice-based conjoint survey
#' design where each row is an alternative. Generate a variety of survey
#' designs, including full factorial designs, orthogonal designs, and
#' Bayesian D-efficient designs as well as designs with "no choice" options
#' and "labeled" (also known as "alternative specific") designs.
#'
#' @keywords experiment design mnl mxl mixed logit logitr idefix DoE.base
#' @param profiles A data frame in which each row is a possible profile.
#' This can be generated using the `cbc_profiles()` function.
#' @param n_resp Number of survey respondents.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent.
#' @param n_blocks Number of blocks used in Orthogonal or Bayesian D-efficient
#' designs. Max allowable is one block per respondent. Defaults to `1`, meaning
#' every respondent sees the same choice set.
#' @param n_draws Number of draws used in simulating the prior distribution
#' used in Bayesian D-efficient designs. Defaults to `50`.
#' @param n_start A numeric value indicating the number of random start designs
#' to use in obtaining a Bayesian D-efficient design. The default is `5`.
#' Increasing `n_start` can result in a more efficient design at the expense
#' of increased computational time.
#' @param no_choice Include a "no choice" option in the choice sets? Defaults
#' to `FALSE`. If `TRUE`, the total number of alternatives per question will be
#' one more than the provided `n_alts` argument.
#' @param label The name of the variable to use in a "labeled" design
#' (also called an "alternative-specific design") such that each set of
#' alternatives contains one of each of the levels in the `label` attribute.
#' Currently not compatible with Bayesian D-efficient designs. If used,
#' the `n_alts` argument will be ignored as its value is defined by the unique
#' number of levels in the `label` variable. Defaults to `NULL`.
#' @param method Choose the design method to use: `"full"`, `"orthogonal"`,
#' `"CEA"` or `"Modfed"`. Defaults to `"full"`. See details below for complete
#' description of each method.
#' @param priors A list of one or more assumed prior parameters used to
#' generate a Bayesian D-efficient design. Defaults to `NULL`
#' @param prior_no_choice Prior utility value for the "no choice" alternative.
#' Only required if `no_choice = TRUE`. Defaults to `NULL`.
#' @param probs If `TRUE`, for Bayesian D-efficient designs the resulting
#' design includes average predicted probabilities for each alternative in each
#' choice set given the sample from the prior preference distribution.
#' Defaults to `FALSE`.
#' @param keep_db_error If `TRUE`, for Bayesian D-efficient designs the returned
#' object will be a list containing the design and the DB-error score.
#' Defaults to `FALSE`.
#' @param max_iter A numeric value indicating the maximum number allowed
#' iterations when searching for a Bayesian D-efficient design. The default is
#' 50.
#' @param parallel Logical value indicating whether computations should be done
#' over multiple cores. The default is `FALSE`.
#' @details
#' The `method` argument determines the design method used. Options are:
#'
#' - `"full"`
#' - `"orthogonal"`
#' - `"CEA"`
#' - `"Modfed"`
#'
#' The `"full"` method uses a "full factorial" design where choice sets are
#' created by randomly selecting from the full set of `profiles`. Blocking can
#' used with these designs where blocks are created from subsets of the full
#' factorial design. For more information about blocking with full factorial
#' designs, see `?DoE.base::fac.design` as well as the JSS article on the
#' {DoE.base} package (Grömping, 2018).
#'
#' The `"orthogonal"` method first finds an orthogonal array from the full
#' set of `profiles` (if possible), then randomly selects from it to create
#' choice sets. For some designs an orthogonal array can't be found, in which
#' case a full factorial design is used. This approach is also sometimes called
#' a "main effects" design since orthogonal arrays focus the information on the
#' main effects at the expense of information about interaction effects. For
#' more information about orthogonal designs, see `?DoE.base::oa.design` as
#' well as the JSS article on the {DoE.base} package
#' (Grömping, 2018).
#'
#' For Bayesian D-efficient designs, use `"CEA"` or `"Modfed"` along with
#' specified `priors`. If `method` is set to `"CEA"` or `"Modfed"` but without
#' `priors` specified, a prior of all `0`s will be used and a warning message
#' stating this will be shown. If you are using a restricted set of `profiles`,
#' only the `"Modfed"` method can be used as `"CEA"` requires unrestricted
#' `profiles`. For more details on Bayesian D-efficient designs, see
#' `?idefix::CEA` and `?idefix::Modfed` as well as the JSS article on the
#' {idefix} package (Traets et al, 2020).
#' @references
#' Grömping, U. (2018). R Package DoE.base for Factorial Experiments. Journal of Statistical Software, 85(5), 1–41,
#' \doi{10.18637/jss.v085.i05}
#' Traets, F., Sanchez, D. G., & Vandebroek, M. (2020). Generating Optimal Designs for Discrete Choice Experiments in R: The idefix Package. Journal of Statistical Software, 96(3), 1–41,
#' \doi{10.18637/jss.v096.i03}
#' @return A data frame containing a choice-based conjoint survey design where
#' each row is an alternative.
#' @export
#' @examples
#' library(cbcTools)
#'
#' # A simple conjoint experiment about apples
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Make a survey design from all possible profiles
#' # (This is the default setting where method = 'full' for "full factorial")
#' design_full <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Make a survey design from an orthogonal array of profiles
#' design_ortho <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6,   # Number of questions per respondent
#'   method   = 'orthogonal'
#' )
#'
#' # Make a survey design from all possible profiles
#' # with a "no choice" option
#' design_full_nochoice <- cbc_design(
#'   profiles  = profiles,
#'   n_resp    = 300, # Number of respondents
#'   n_alts    = 3,   # Number of alternatives per question
#'   n_q       = 6,   # Number of questions per respondent
#'   no_choice = TRUE
#' )
#'
#' # Make a survey design from all possible profiles
#' # with each level of the "type" attribute appearing as an alternative
#' design_full_labeled <- cbc_design(
#'   profiles  = profiles,
#'   n_resp    = 300, # Number of respondents
#'   n_alts    = 3,   # Number of alternatives per question
#'   n_q       = 6,   # Number of questions per respondent
#'   label     = "type"
#' )
#'
#' # Make a Bayesian D-efficient design with a prior model specified
#' # Note that by speed can be improved by setting parallel = TRUE
#' design_bayesian <- cbc_design(
#'     profiles  = profiles,
#'     n_resp    = 300, # Number of respondents
#'     n_alts    = 3,   # Number of alternatives per question
#'     n_q       = 6,   # Number of questions per respondent
#'     n_start   = 1,   # Defauls to 5, set to 1 here for a quick example
#'     priors = list(
#'         price     = -0.1,
#'         type      = c(0.1, 0.2),
#'         freshness = c(0.1, 0.2)
#'     ),
#'     method = "CEA",
#'     parallel = FALSE
#' )
cbc_design <- function(
  profiles,
  n_resp,
  n_alts,
  n_q,
  n_blocks = 1,
  n_draws = 50,
  n_start = 5,
  no_choice = FALSE,
  label = NULL,
  method = "full",
  priors = NULL,
  prior_no_choice = NULL,
  probs = FALSE,
  keep_db_error = FALSE,
  max_iter = 50,
  parallel = FALSE
) {
  method <- check_design_method(method, priors)
  check_inputs_design(
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
    keep_db_error,
    max_iter,
    parallel
  )
  profiles <- as.data.frame(profiles) # tibbles break things
  if (method == 'full') {
    design <- make_design_full(
      profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
    )
  } else if (method == 'orthogonal') {
    design <- make_design_orthogonal(
      profiles, n_resp, n_alts, n_q, no_choice, label
    )
  } else {
    design <- make_design_bayesian(
      profiles, n_resp, n_alts, n_q, n_blocks, n_draws, n_start, no_choice,
      label, method, priors, prior_no_choice, probs, keep_db_error, max_iter,
      parallel
    )
  }
  # Reset row numbers
  row.names(design) <- NULL
  return(design)
}

# Randomize the design ----

# Sample from profiles to create randomized choice sets

get_randomized_design <- function(
  profiles, n_resp, n_alts, n_q, no_choice, label
) {
  if (is.null(label)) {
    design <- get_design_rand(profiles, n_resp, n_alts, n_q)
  } else {
    design <- get_design_rand_label(profiles, n_resp, n_alts, n_q, label)
  }
  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }
  design <- reorder_cols(design)
  return(design)
}

get_design_rand <- function(profiles, n_resp, n_alts, n_q) {
  design <- sample_profiles(profiles, size = n_resp * n_alts * n_q)
  design <- add_metadata(design, n_resp, n_alts, n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  dup_rows_obs <- get_dup_obs(design, n_alts)
  dup_rows_resp <- get_dup_resp(design, n_resp, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    # cat('Number dupe rows by obs:', length(dup_rows_obs), '\n')
    # cat('Number dupe rows by resp:', length(dup_rows_resp), '\n')
    dup_rows <- unique(c(dup_rows_obs, dup_rows_resp))
    new_rows <- sample_profiles(profiles, size = length(dup_rows))
    design[dup_rows, 1:ncol(new_rows)] <- new_rows
    # Recalculate duplicates
    dup_rows_obs <- get_dup_obs(design, n_alts)
    dup_rows_resp <- get_dup_resp(design, n_resp, n_q)
  }
  return(design)
}

sample_profiles <- function(profiles, size) {
  return(profiles[sample(
    x = seq_len(nrow(profiles)), size = size, replace = TRUE), ]
  )
}

add_metadata <- function(design, n_resp, n_alts, n_q) {
  n_rows_per_resp <- n_alts * n_q
  design$respID <- rep(seq(n_resp), each = n_rows_per_resp)
  design$qID <- rep(rep(seq(n_q), each = n_alts), n_resp)
  design$altID <- rep(seq(n_alts), n_resp * n_q)
  design$obsID <- rep(seq(n_resp * n_q), each = n_alts)
  row.names(design) <- NULL
  return(design)
}

get_dup_obs <- function(design, n_alts) {
  # Identify duplicate profiles for each observation (each choice set)
  counts <- tapply(
    design$profileID, design$obsID,
    FUN = function(x) length(unique(x))
  )
  dup_ids <- which(counts != n_alts)
  dup_rows <- which(design$obsID %in% dup_ids)
  return(dup_rows)
}

get_dup_resp <- function(design, n_resp, n_q) {
  # Identify duplicate choice sets for each respondent
  dup_ids <- unlist(lapply(
    1:n_resp,
    function(x) dup_obs_by_resp(design[which(design$respID == x),])
  ))
  dup_rows <- which(design$obsID %in% dup_ids)
  return(dup_rows)
}

dup_obs_by_resp <- function(df) {
  profiles_list <- tapply(
    df$profileID, df$obsID,
    FUN = function(x) sort(x)
  )
  # Convert the list of vectors to a data frame to check for duplicates
  dupe_df <- do.call(rbind, profiles_list)
  dup_ids <- which(duplicated(dupe_df))
  if (length(dup_ids) > 0) {
    return(as.numeric(names(dup_ids)))
  }
  return(NULL)
}

get_design_rand_label <- function(profiles, n_resp, n_alts, n_q, label) {
  n_levels <- length(unique(profiles[, label]))
  if (n_levels != n_alts) {
    warning(
      "The supplied 'n_alts' argument is being ignored and set to ", n_levels,
      " to match the number of unique levels in the ", label,
      " variable.\n"
    )
    # Over-ride user-provided n_alts as it is determined by the label
    n_alts <- n_levels
  }
  # Randomize rows by label
  labels <- split(profiles, profiles[label])
  design <- sample_profiles_by_group(labels, size = n_resp * n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  design <- add_metadata(design, n_resp, n_alts, n_q)
  dup_rows_obs <- get_dup_obs(design, n_alts)
  dup_rows_resp <- get_dup_resp(design, n_resp, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    # cat('Number dupe rows by obs:', length(dup_rows_obs), '\n')
    # cat('Number dupe rows by resp:', length(dup_rows_resp), '\n')
    dup_rows <- unique(c(dup_rows_obs, dup_rows_resp))
    new_rows <- sample_profiles_by_group(labels, size = length(dup_rows) / n_alts)
    design[dup_rows, 1:ncol(new_rows)] <- new_rows
    # Recalculate duplicates
    dup_rows_obs <- get_dup_obs(design, n_alts)
    dup_rows_resp <- get_dup_resp(design, n_resp, n_q)
  }
  return(design)
}

sample_profiles_by_group <- function(labels, size) {
  design <- lapply(labels, function(x) sample_profiles(x, size = size))
  design <- lapply(design, function(x) add_label_id(x))
  design <- do.call(rbind, design)
  design <- design[order(design$labelID), ]
  design$labelID <- NULL
  return(design)
}

add_label_id <- function(design) {
  design$labelID <- seq(nrow(design))
  return(design)
}

add_no_choice <- function(design, n_alts) {
  # Must dummy code categorical variables to include an outside good
  design <- dummy_code(design)
  # Create outside good rows
  design_og <- design[which(design$altID == 1), ]
  design_og[
    , !names(design_og) %in% c("respID", "qID", "altID", "obsID")
  ] <- 0
  design_og$altID <- n_alts + 1
  design_og$no_choice <- 1
  # Insert outside good rows into design
  design$no_choice <- 0
  design <- rbind(design, design_og)
  design <- design[order(design$obsID), ]
  return(design)
}

dummy_code <- function(design) {
  types <- get_col_types(design)
  nonnumeric <- names(types[!types %in% c("integer", "numeric")])
  if (length(nonnumeric) > 0) {
    design <- fastDummies::dummy_cols(design, nonnumeric)
    design[, nonnumeric] <- NULL
  }
  return(design)
}

get_col_types <- function(data) {
  types <- lapply(data, class)
  test <- function(x) {
    x[1]
  }
  return(unlist(lapply(types, test)))
}

reorder_cols <- function(design) {
    metaNames <- c("profileID", "respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(design), metaNames)
    design <- as.data.frame(design)[, c(metaNames, varNames)]
    return(design)
}

# Full Factorial Design ----

make_design_full <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
) {
  if (n_blocks > 1) {
    design <- make_design_full_blocked(
      profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
    )
  } else {
    design <- get_randomized_design(
      profiles, n_resp, n_alts, n_q, no_choice, label
    )
  }
  return(design)
}

make_design_full_blocked <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
) {
  # Make blocks
  design <- suppressMessages(as.data.frame(
    DoE.base::fac.design(
      factor.names = get_profile_list(profiles),
      blocks = n_blocks,
      block.name = "blockID"
    )
  ))
  design$blockID <- as.numeric(as.character(design$blockID))
  design <- design[,c(names(profiles)[2:ncol(profiles)], "blockID")]
  type_ids <- get_type_ids(profiles)
  profiles <- join_profiles(design, profiles, type_ids)

  # Randomize design within each block
  profiles <- split(profiles, profiles$blockID)
  # Make sure number of respondents divides well into blocks
  n_resp_list <- rep(n_resp / n_blocks, n_blocks)
  if (! all(n_resp_list %% 1 == 0)) {
    n_resp_list <- floor(n_resp_list)
    n_resp_list[n_blocks] <- n_resp_list[n_blocks] + 1
  }
  design <- list()
  for (i in 1:n_blocks) {
    design[[i]] <- get_randomized_design(
      profiles[[i]], n_resp_list[i], n_alts, n_q, no_choice, label
    )
  }
  design <- do.call(rbind, design)
  design <- add_metadata(design, n_resp, n_alts, n_q)
  return(design)
}

# Orthogonal Design ----

make_design_orthogonal <- function(
    profiles, n_resp, n_alts, n_q, no_choice, label
) {
    oa <- suppressMessages(as.data.frame(
      DoE.base::oa.design(
        factor.names = get_profile_list(profiles)
      )
    ))
    if (nrow(oa) == nrow(profiles)) {
        message("No orthogonal array found; using full factorial for design")
    } else {
        message(
          "Orthogonal array found; using ", nrow(oa), " out of ",
          nrow(profiles), " profiles for design"
        )
    }
    type_ids <- get_type_ids(profiles)
    oa <- join_profiles(oa, profiles, type_ids)
    design <- get_randomized_design(oa, n_resp, n_alts, n_q, no_choice, label)
    return(design)
}

# Bayesian D-efficient Design ----

make_design_bayesian <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, n_draws, n_start, no_choice,
    label, method, priors, prior_no_choice, probs, keep_db_error, max_iter,
    parallel
) {
    # Set up levels and coding
    profile_list <- get_profile_list(profiles)
    type_ids <- get_type_ids(profiles)
    lvl.names <- unname(profile_list)
    lvls <- unname(unlist(lapply(lvl.names, function(x) length(x))))
    coding <- rep("C", length(lvls))
    c.lvls <- NULL
    if (any(type_ids$continuous)) {
        c.lvls <- lvl.names[type_ids$continuous]
    }
    # lvl.names must be all characters for decoding process
    lvl.names <- lapply(lvl.names, function(x) as.character(x))
    if (any(type_ids$discrete)) {
        coding[type_ids$discrete] <- "D"
    }
    no_choice_alt <- NULL
    alt_cte <- rep(0, n_alts)
    if (no_choice) {
        n_alts <- n_alts + 1
        alt_cte <- c(alt_cte, 1)
        no_choice_alt <- n_alts
    }

    # Setup priors
    profile_lvls <- profiles[, 2:ncol(profiles)]
    varnames <- names(profile_lvls)
    if (is.null(priors)) {
        # No priors specified, so use all 0s
        warning(
            'Since the ', method, ' method is used but no priors were ',
            'specified, a zero prior will be used (all coefficients set to 0)'
        )
        priors <- lapply(profile_list, function(x) rep(0, length(x) - 1))
        priors[type_ids$continuous] <- 0
    }
    # Make sure order of priors matches order of attributes in profiles
    mu <- unlist(priors[varnames])
    if (no_choice) {
        mu <- c(prior_no_choice, mu)
    }
    sigma <- diag(length(mu))
    par_draws <- MASS::mvrnorm(n = n_draws, mu = mu, Sigma = sigma)
    n_alt_cte <- sum(alt_cte)
    if (n_alt_cte >= 1) {
        par_draws <- list(
            par_draws[, 1:n_alt_cte],
            par_draws[, (n_alt_cte + 1):ncol(par_draws)])
    }

    # Make the design
    profiles_restricted <- nrow(expand.grid(lvl.names)) > nrow(profiles)
    if (profiles_restricted & (method == "CEA")) {
      # "CEA" method only works with unrestricted profile set
      method <- "Modfed"
      warning(
        'The "CEA" algorithm requires the use of an unrestricted set of ',
        'profiles, so "Modfed" is being used instead.\n'
      )
    }
    if (method == "CEA") {
        D <- idefix::CEA(
            lvls = lvls,
            coding = coding,
            par.draws = par_draws,
            c.lvls = c.lvls,
            n.alts = n_alts,
            n.sets = n_q*n_blocks,
            no.choice = no_choice,
            n.start = n_start,
            alt.cte = alt_cte,
            parallel = parallel
        )
    } else {
        D <- idefix::Modfed(
            cand.set = defineCandidateSet(
              lvls, coding, c.lvls, profile_lvls, type_ids, profiles_restricted
            ),
            par.draws = par_draws,
            n.alts = n_alts,
            n.sets = n_q*n_blocks,
            no.choice = no_choice,
            n.start = n_start,
            alt.cte = alt_cte,
            parallel = parallel
        )
    }

    # Decode the design
    design_raw <- idefix::Decode(
        des = D$design,
        n.alts = n_alts,
        alt.cte = alt_cte,
        lvl.names = lvl.names,
        c.lvls = c.lvls,
        coding = coding,
        no.choice = no_choice_alt
    )

    # Join on profileIDs to design
    design <- design_raw$design
    names(design) <- varnames
    design <- join_profiles(design, profiles, type_ids)
    if (no_choice) {
      design <- add_no_choice_deff(design, n_alts, varnames[type_ids$discrete])
    }

    # Include probs?
    if (probs) {
      design$probs <- as.vector(t(D$probs))
    }

    # Add blockIDs
    design$blockID <- rep(seq(n_blocks), each = n_alts*n_q)

    # Repeat design to match number of respondents
    n_reps <- ceiling(n_resp / n_blocks)
    design <- design[rep(seq_len(nrow(design)), n_reps), ]
    row.names(design) <- NULL
    design <- design[1:(n_resp*n_q*n_alts), ]

    # Add metadata
    design <- add_metadata(design, n_resp, n_alts, n_q)
    design <- reorder_cols(design)

    # Print DB error
    message(
        "Bayesian D-efficient design found with DB-error of ",
        round(D$error, 5)
    )

    # Return list containing the design and DB error if keep_db_error = TRUE
    if (keep_db_error) {
        return(list(design = design, db_err = D$error))
    }

    return(design)
}

get_type_ids <- function(profiles) {
    types <- get_col_types(profiles[, 2:ncol(profiles)])
    ids <- list()
    ids$discrete <- types %in% c("factor", "character")
    ids$continuous <- !ids$discrete
    return(ids)
}

get_profile_list <- function(profiles) {
    profile_lvls <- profiles[, 2:ncol(profiles)]
    varnames <- names(profile_lvls)
    type_ids <- get_type_ids(profiles)
    profile_list <- list()
    for (i in seq_len(ncol(profile_lvls))) {
        if (type_ids$discrete[i]) {
            profile_list[[i]] <- levels(profile_lvls[,i])
        } else {
            profile_list[[i]] <- unique(profile_lvls[,i])
        }
    }
    names(profile_list) <- varnames
    return(profile_list)
}

defineCandidateSet <- function(
    lvls, coding, c.lvls, profile_lvls, type_ids, profiles_restricted
) {
  # Make candidate set with profiles, assuming non-restricted
  cand_set <- idefix::Profiles(
    lvls = lvls,
    coding = coding,
    c.lvls = c.lvls
  )
  if (!profiles_restricted) { return(cand_set) }

  # If restricted, need to manually dummy-code profiles to avoid
  # including restricted profiles
  cand_set_res <- fastDummies::dummy_cols(
    profile_lvls,
    select_columns = names(profile_lvls)[type_ids$discrete],
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )
  name_order <- names(profile_lvls)
  names_coded <- names(cand_set_res)
  cols <- c()
  for (i in seq_len(length(coding))) {
    if (coding[i] == "C") {
      name_match <- name_order[i]
    } else {
      name_match <- names_coded[grepl(paste0(name_order[i], "_"), names_coded)]
    }
    cols <- c(cols, name_match)
  }
  cand_set_res <- cand_set_res[,cols]
  names(cand_set_res) <- colnames(cand_set)
  cand_set_res <- as.matrix(cand_set_res)
  row.names(cand_set_res) <- seq(nrow(cand_set_res))
  return(cand_set_res)
}

join_profiles <- function(design, profiles, type_ids) {
  # Replaces the generated design with rows from profiles, which ensures
  # factor levels in profiles are maintained in design

  # Keep track of row order in design
  design$row_id <- seq(nrow(design))

  # Before joining profiles, ensure that all the data types are the same
  # as in profiles, otherwise join won't work properly

  # Convert numeric columns to actual numbers
  for (id in which(type_ids$continuous)) {
    design[,id] <- as.numeric(as.character(design[,id]))
  }

  # Convert character types to factors and set same levels as profiles
  for (id in which(type_ids$discrete)) {
    design[,id] <- factor(design[,id], levels = levels(profiles[,id+1]))
  }

  # Join on profileIDs, then reorder to retain design order
  varnames <- names(profiles[, 2:ncol(profiles)])
  design <- merge(design, profiles, by = varnames, all.x = TRUE)
  design <- design[order(design$row_id),]
  if ('blockID' %in% names(design)) { varnames <- c(varnames, 'blockID') }
  design <- design[c('profileID', varnames)]
  return(design)
}

add_no_choice_deff <- function(design, n_alts, varnames_discrete) {
  # First dummy code categorical variables
  design$obsID <- rep(seq(nrow(design) / n_alts), each = n_alts)
  design$altID <- rep(seq(n_alts), nrow(design) / n_alts)
  design <- design[which(design$altID != 4), ]
  design <- fastDummies::dummy_cols(
    design,
    select_columns = varnames_discrete,
    remove_first_dummy = TRUE
  )
  design <- design[,which(! names(design) %in% varnames_discrete)]
  design$no_choice <- 0
  # Insert dummy-coded outside good rows
  design_og <- design[which(design$altID == 1), ]
  design_og$altID <- n_alts
  design_og$profileID <- 0
  design_og[,
    which(! names(design_og) %in% c('profileID', 'altID', 'obsID'))] <- 0
  design_og$no_choice <- 1
  design <- rbind(design, design_og)
  design <- design[order(design$obsID, design$altID), ]
  design[,c('altID', 'obsID')] <- NULL
  return(design)
}

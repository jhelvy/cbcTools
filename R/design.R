#' Make a choice-based conjoint survey design
#'
#' This function creates a data frame containing a choice-based conjoint survey
#' design where each row is an alternative. Generate a variety of survey
#' designs, including full factorial designs, orthogonal designs, and Bayesian
#' D-efficient designs as well as designs with "no choice" options and "labeled"
#' (also known as "alternative specific") designs.
#'
#' @keywords experiment design mnl mxl mixed logit logitr idefix DoE.base
#' @param profiles A data frame in which each row is a possible profile. This
#'   can be generated using the `cbc_profiles()` function.
#' @param n_resp Number of survey respondents.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent.
#' @param n_blocks Number of blocks used in Orthogonal or Bayesian D-efficient
#'   designs. Max allowable is one block per respondent. Defaults to `1`,
#'   meaning every respondent sees the same choice set.
#' @param n_draws Number of draws used in simulating the prior distribution used
#'   in Bayesian D-efficient designs. Defaults to `50`.
#' @param n_start A numeric value indicating the number of random start designs
#'   to use in obtaining a Bayesian D-efficient design. The default is `5`.
#'   Increasing `n_start` can result in a more efficient design at the expense
#'   of increased computational time.
#' @param no_choice Include a "no choice" option in the choice sets? Defaults to
#'   `FALSE`. If `TRUE`, the total number of alternatives per question will be
#'   one more than the provided `n_alts` argument.
#' @param label The name of the variable to use in a "labeled" design (also
#'   called an "alternative-specific design") such that each set of alternatives
#'   contains one of each of the levels in the `label` attribute. Currently not
#'   compatible with Bayesian D-efficient designs. If used, the `n_alts`
#'   argument will be ignored as its value is defined by the unique number of
#'   levels in the `label` variable. Defaults to `NULL`.
#' @param method Choose the design method to use: `"random"`, `"full"`,
#'   `"orthogonal"`, `"dopt"`, `"CEA"`, or `"Modfed"`. Defaults to `"random"`.
#'   See details below for complete description of each method.
#' @param priors A list of one or more assumed prior parameters used to generate
#'   a Bayesian D-efficient design. Defaults to `NULL`
#' @param prior_no_choice Prior utility value for the "no choice" alternative.
#'   Only required if `no_choice = TRUE`. Defaults to `NULL`.
#' @param probs If `TRUE`, for Bayesian D-efficient designs the resulting design
#'   includes average predicted probabilities for each alternative in each
#'   choice set given the sample from the prior preference distribution.
#'   Defaults to `FALSE`.'
#' @param keep_d_eff If `TRUE`, for D-optimal designs (`method = "dopt"`) the
#'   returned object will be a list containing the design and the D-efficiency
#'   score. Defaults to `FALSE`.
#' @param keep_db_error If `TRUE`, for Bayesian D-efficient designs the
#'   returned object will be a list containing the design and the DB-error
#'   score. Defaults to `FALSE`.
#' @param max_iter A numeric value indicating the maximum number allowed
#'   iterations when searching for a Bayesian D-efficient design. The default is
#'   50.
#' @param parallel Logical value indicating whether computations should be done
#'   over multiple cores. The default is `FALSE`.
#' @details The `method` argument determines the design method used. Options
#'   are:
#'
#' - `"random"`
#' - `"full"`
#' - `"orthogonal"`
#' - `"dopt"`
#' - `"CEA"`
#' - `"Modfed"`
#'
#'   All methods ensure that the two following criteria are met:
#'
#'   1. No two profiles are the same within any one choice set.
#'   2. No two choice sets are the same within any one respondent.
#'
#'   The table below summarizes method compatibility with other design options,
#'   including the ability to include a "no choice" option, the creation of a
#'   "labeled" design (also called a "alternative-specific" design), the use
#'   of restricted profile, and the use of blocking.
#'
#'   Method | Include "no choice"? | Labeled designs? | Restricted profiles? | Blocking?
#'   ---|---|---|---|---
#'   `"random"`     | Yes | Yes | Yes | No
#'   `"full"`       | Yes | Yes | Yes | Yes
#'   `"orthogonal"` | Yes | No  | No  | Yes
#'   `"dopt"`       | Yes | No  | Yes | Yes
#'   `"CEA"`        | Yes | No  | No  | Yes
#'   `"Modfed"`     | Yes | No  | Yes | Yes
#'
#'   The `"random"` method (the default) creates a design where choice sets are
#'   created by randomly sampling from the full set of `profiles` *with
#'   *replacement. This means that few (if any) respondents will see the same
#'   sets of choice sets. This method is less efficient than other approaches
#'   and may lead to a deficient experiment in smaller sample sizes, though it
#'   guarantees equal ability to estimate main and interaction effects.
#'
#'   The `"full"` method for ("full factorial") creates a design where choice
#'   sets are created by randomly sampling from the full set of `profiles`
#'   *without replacement*. The choice sets are then repeated to meet the
#'   desired number of survey respondents (determined by `n_resp`). If blocking
#'   is used, choice set blocks are created using mutually exclusive subsets of
#'   `profiles` within each block. This method produces a design with similar
#'   performance with that of the `"random"` method, except the choice sets are
#'   repeated and thus there will be many more opportunities for different
#'   respondents to see the same choice sets. This method is less efficient than
#'   other approaches and may lead to a deficient experiment in smaller sample
#'   sizes, though it guarantees equal ability to estimate main and interaction
#'   effects. For more information about blocking with full factorial designs,
#'   see `?DoE.base::fac.design` as well as the JSS article on the {DoE.base}
#'   package (Grömping, 2018).
#'
#'   The `"orthogonal"` method creates a design where an orthogonal array from
#'   the full set of `profiles` is found and then choice sets are created by
#'   randomly sampling from this orthogonal array *without replacement*. The
#'   choice sets are then repeated to meet the desired number of survey
#'   respondents (determined by `n_resp`). If blocking is used, choice set
#'   blocks are created using mutually exclusive subsets of the orthogonal array
#'   within each block. For cases where an orthogonal array cannot be found, a
#'   full factorial design is used. This approach is also sometimes called a
#'   "main effects" design since orthogonal arrays focus the information on the
#'   main effects at the expense of information about interaction effects. For
#'   more information about orthogonal designs, see `?DoE.base::oa.design` as
#'   well as the JSS article on the {DoE.base} package (Grömping, 2018).
#'
#'   The `"dopt"` method creates a "D-optimal" design where an array from
#'   `profiles` is found that maximizes the D-efficiency of a linear model
#'   using the Federov algorithm, with the total number of unique choice sets
#'   determined by `n_q*n_blocks`. Choice sets are then created by randomly
#'   sampling from this array *without replacement*. The choice sets are then
#'   repeated to meet the desired number of survey respondents (determined by
#'   `n_resp`). If blocking is used, choice set blocks are created from the
#'   D-optimal array. For more information about the underlying algorithm
#'   for this method, see `?AlgDesign::optFederov`.
#'
#'   The `"CEA"` and `"Modfed"` methods use the specified `priors` to create a
#'   Bayesian D-efficient design for the choice sets, with the total number of
#'   unique choice sets determined by `n_q*n_blocks`. The choice sets are then
#'   repeated to meet the desired number of survey respondents (determined by
#'   `n_resp`). If `"CEA"` or `"Modfed"` is used without specifying `priors`, a
#'   prior of all `0`s will be used and a warning message stating this will be
#'   shown. In the opposite case, if `priors` are specified but neither Bayesian
#'   method is used, the `"CEA"` method will be used and a warning stating this
#'   will be shown. Restricted sets of `profiles` can only be used with
#'   `"Modfed"`. For more details on Bayesian D-efficient designs, see
#'   `?idefix::CEA` and `?idefix::Modfed` as well as the JSS article on the
#'   {idefix} package (Traets et al, 2020).
#' @references Grömping, U. (2018). R Package DoE.base for Factorial Experiments. Journal of Statistical Software, 85(5), 1–41
#' \doi{10.18637/jss.v085.i05}
#'
#'   Traets, F., Sanchez, D. G., & Vandebroek, M. (2020). Generating Optimal Designs for Discrete Choice Experiments in R: The idefix Package. Journal of Statistical Software, 96(3), 1–41,
#' \doi{10.18637/jss.v096.i03}
#'
#' Wheeler B (2022)._AlgDesign: Algorithmic Experimental Design. R package version 1.2.1,
#' \href{https://CRAN.R-project.org/package=AlgDesign}{https://CRAN.R-project.org/package=AlgDesign}.
#' @return The returned `design` data frame contains a choice-based conjoint
#' survey design where each row is an alternative. It includes the following
#' columns:
#'
#' - `profileID`: Identifies the profile in `profiles`.
#' - `respID`: Identifies each survey respondent.
#' - `qID`: Identifies the choice question answered by the respondent.
#' - `altID`:Identifies the alternative in any one choice observation.
#' - `obsID`: Identifies each unique choice observation across all respondents.
#' - `blockID`: If blocking is used, identifies each unique block.
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
#' # Make a survey by randomly sampling from all possible profiles
#' # (This is the default setting where method = 'random')
#' design_random <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 100, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Make a survey using a full factorial design and include a "no choice" option
#' design_full <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 100, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6,   # Number of questions per respondent
#'   method   = 'full', # Change this to use a different method, e.g. 'orthogonal', or 'dopt'
#'   no_choice = TRUE
#' )
#'
#' # Make a survey by randomly sampling from all possible profiles
#' # with each level of the "type" attribute appearing as an alternative
#' design_random_labeled <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 100, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6,   # Number of questions per respondent
#'   label    = "type"
#' )
#'
#' # Make a Bayesian D-efficient design with a prior model specified
#' # Note that by speed can be improved by setting parallel = TRUE
#' design_bayesian <- cbc_design(
#'     profiles  = profiles,
#'     n_resp    = 100, # Number of respondents
#'     n_alts    = 3,   # Number of alternatives per question
#'     n_q       = 6,   # Number of questions per respondent
#'     n_start   = 1,   # Defaults to 5, set to 1 here for a quick example
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
  method = "random",
  priors = NULL,
  prior_no_choice = NULL,
  probs = FALSE,
  keep_d_eff = FALSE,
  keep_db_error = FALSE,
  max_iter = 50,
  parallel = FALSE
) {
  method <- check_design_method(method, priors)
  profiles_restricted <- nrow(expand.grid(get_profile_list(profiles))) > nrow(profiles)
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
    keep_d_eff,
    keep_db_error,
    max_iter,
    parallel,
    profiles_restricted
  )
  profiles <- as.data.frame(profiles) # tibbles break things
  if (method == 'random') {
    design <- make_design_random(
      profiles, n_resp, n_alts, n_q, no_choice, label
    )
  } else if (method == 'full') {
    design <- make_design_full(
      profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
    )
  } else if (method == 'orthogonal') {
    design <- make_design_orthogonal(
      profiles, n_resp, n_alts, n_q, n_blocks, no_choice
    )
  } else if (method == 'dopt') {
    design <- make_design_dopt(
      profiles, n_resp, n_alts, n_q, n_blocks, no_choice, keep_d_eff
    )
  } else {
    design <- make_design_bayesian(
      profiles, n_resp, n_alts, n_q, n_blocks, n_draws, n_start, no_choice,
      label, method, priors, prior_no_choice, probs, keep_db_error, max_iter,
      parallel, profiles_restricted
    )
  }
  design <- reorder_cols(design)
  row.names(design) <- NULL
  return(design)
}

# General helpers ----

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

get_type_ids <- function(profiles) {
  types <- get_col_types(profiles[, 2:ncol(profiles)])
  ids <- list()
  ids$discrete <- types %in% c("factor", "character")
  ids$continuous <- !ids$discrete
  return(ids)
}

join_profiles <- function(design, profiles) {

  # Before joining profiles, ensure that all the data types are the same
  # as in profiles, otherwise join won't work properly

  type_ids <- get_type_ids(profiles)

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
  design <- merge(design, profiles, by = varnames, all.x = TRUE, sort = FALSE)
  if ('blockID' %in% names(design)) { varnames <- c(varnames, 'blockID') }
  design <- design[c('profileID', varnames)]
  return(design)
}

add_metadata <- function(design, n_resp, n_alts, n_q) {
  design$respID <- rep(seq(n_resp), each = n_alts * n_q)
  design$qID    <- rep(rep(seq(n_q), each = n_alts), n_resp)
  design$altID  <- rep(seq(n_alts), n_resp * n_q)
  design$obsID  <- rep(seq(n_resp * n_q), each = n_alts)
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
  # Convert vector list to a data frame to check for duplicates
  dupe_df <- do.call(rbind, profiles_list)
  dup_ids <- which(duplicated(dupe_df))
  if (length(dup_ids) > 0) {
    return(as.numeric(names(dup_ids)))
  }
  return(NULL)
}

add_no_choice <- function(design, n_alts) {
  # Must dummy code categorical variables to include an outside good
  design <- dummy_code(design)
  # Create outside good rows
  design_og <- design[which(design$altID == 1), ]
  design_og[,!names(design_og) %in% c("respID", "qID", "altID", "obsID")] <- 0
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
  test <- function(x) { x[1] }
  return(unlist(lapply(types, test)))
}

reorder_cols <- function(design) {
  metaNames <- c("profileID", "respID", "qID", "altID", "obsID")
  if ('blockID' %in% names(design)) { metaNames <- c(metaNames, 'blockID') }
  varNames <- setdiff(names(design), metaNames)
  design <- as.data.frame(design)[, c(metaNames, varNames)]
  return(design)
}

# Choice sets ----

make_random_sets <- function(profiles, n_alts) {
  n_q <- nrow(profiles)
  design <- sample_random_sets(profiles, n_alts, n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  dup_rows_obs <- get_dup_obs(design, n_alts)
  dup_rows_resp <- get_dup_resp(design, n_resp = 1, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    design <- sample_random_sets(profiles, n_alts, n_q)
    dup_rows_obs <- get_dup_obs(design, n_alts)
    dup_rows_resp <- get_dup_resp(design, n_resp = 1, n_q)
  }
  return(design)
}

make_random_sets_by_block <- function(profiles, n_alts, n_blocks) {
  # Make choice sets for each set of profileIDs
  profiles <- split(profiles, profiles$blockID)
  choice_sets <- list()
  for (i in 1:n_blocks) {
    choice_sets[[i]] <- make_random_sets(profiles[[i]], n_alts)
  }
  choice_sets <- do.call(rbind, choice_sets)
  return(choice_sets)
}

sample_random_sets <- function(profiles, n_alts, n_q) {
  # Make a randomized copy of the profiles for each alternative
  sets <- lapply(seq(n_alts), function(x) profiles[order(stats::runif(n_q)),])
  sets <- lapply(sets, function(x) {
    x$order <- seq(nrow(x))
    return(x)
  })
  sets <- do.call(rbind, sets)
  sets <- sets[order(sets$order),]
  sets <- add_metadata(sets, n_resp = 1, n_alts, n_q)
  sets$order <- NULL
  return(sets)
}

repeat_sets <- function(choice_sets, n_resp, n_alts, n_q, n_blocks) {
  # Repeat choice sets to match number of respondents
  if (n_blocks > 1) {
    choice_sets <- split(choice_sets, choice_sets$blockID)
    n_resp_block <- ceiling(n_resp / n_blocks)
    n_reps <- ceiling(n_resp_block / (nrow(choice_sets[[1]]) / n_alts / n_q))
    design <- list()
    for (i in seq_len(n_blocks)) {
      set <- choice_sets[[i]]
      temp <- set[rep(seq_len(nrow(set)), n_reps), ]
      design[[i]] <- temp[1:(n_resp_block*n_q*n_alts), ]
    }
    design <- do.call(rbind, design)
  } else {
    design <- choice_sets[rep(seq_len(nrow(choice_sets)), n_resp), ]
  }
  design <- design[1:(n_resp*n_q*n_alts), ]
  design <- add_metadata(design, n_resp, n_alts, n_q)
  return(design)
}

# Random Design ----

# Sample from profiles with replacement to create randomized choice sets

make_design_random <- function(
  profiles, n_resp, n_alts, n_q, no_choice, label
) {
  if (is.null(label)) {
    design <- design_rand_sample(profiles, n_resp, n_alts, n_q)
  } else {
    design <- design_rand_sample_label(profiles, n_resp, n_alts, n_q, label)
  }
  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }
  return(design)
}

design_rand_sample <- function(profiles, n_resp, n_alts, n_q) {
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

design_rand_sample_label <- function(profiles, n_resp, n_alts, n_q, label) {
  n_alts <- override_label_alts(profiles, label, n_alts)
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

sample_profiles <- function(profiles, size) {
  return(profiles[sample(
    x = seq_len(nrow(profiles)), size = size, replace = TRUE), ]
  )
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

override_label_alts <- function(profiles, label, n_alts) {
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
  return(n_alts)
}

# Full Factorial Design ----

# Arrange copies of the full set of profiles into choice sets by sampling
# without replacement

make_design_full <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
) {
  if (!is.null(label)) {
    return(make_design_full_labeled(
      profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
    ))
  }
  if (n_blocks > 1) {
    # Make blocks
    design <- suppressMessages(as.data.frame(DoE.base::fac.design(
        factor.names = get_profile_list(profiles),
        blocks = n_blocks,
        block.name = "blockID"
    )))
    # Make blockID a number, then join on profileIDs
    design$blockID <- as.numeric(as.character(design$blockID))
    design <- design[,c(names(profiles)[2:ncol(profiles)], "blockID")]
    profiles <- join_profiles(design, profiles)
    # Create random choice sets within each block
    choice_sets <- make_random_sets_by_block(profiles, n_alts, n_blocks)
  } else {
    choice_sets <- make_random_sets(profiles, n_alts)
  }
  design <- repeat_sets(choice_sets, n_resp, n_alts, n_q, n_blocks)
  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }
  return(design)
}

make_design_full_labeled <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, no_choice, label
) {
  n_alts <- override_label_alts(profiles, label, n_alts)
  labels <- unique(profiles[,label])
  profiles_orig <- profiles
  # Remove the label column from profiles
  profiles[label] <- NULL
  profiles$profileID <- NULL
  profiles <- profiles[!duplicated(profiles),]
  profiles$profileID <- seq(nrow(profiles))
  profiles <- profiles[c(
    'profileID', names(profiles)[which(names(profiles) != 'profileID')])]
  if (n_blocks > 1) {
    # Make blocks
    design <- suppressMessages(as.data.frame(DoE.base::fac.design(
      factor.names = get_profile_list(profiles),
      blocks = n_blocks,
      block.name = "blockID"
    )))
    # Make blockID a number, then join on profileIDs
    design$blockID <- as.numeric(as.character(design$blockID))
    design <- design[,c(names(profiles)[2:ncol(profiles)], "blockID")]
    profiles <- join_profiles(design, profiles)
    # Create random choice sets within each block
    choice_sets <- make_random_sets_by_block(profiles, n_alts, n_blocks)
  } else {
    choice_sets <- make_random_sets(profiles, n_alts)
  }
  # Add label attribute and original profileIDs
  choice_sets[label] <- rep(labels, nrow(choice_sets) / length(labels))
  choice_sets$profileID <- NULL
  choice_sets <- merge(choice_sets, profiles_orig, all.x = TRUE, sort = FALSE)
  design <- repeat_sets(choice_sets, n_resp, n_alts, n_q, n_blocks)
  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }
  return(design)
}

# Orthogonal Design ----

make_design_orthogonal <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, no_choice
) {
    # First obtain the orthogonal array
    oa <- suppressMessages(as.data.frame(DoE.base::oa.design(
      factor.names = get_profile_list(profiles)
    )))
    if (nrow(oa) == nrow(profiles)) {
      message("No orthogonal array found; using full factorial for design")
    } else {
      message(
        "Orthogonal array found; using ", nrow(oa), " out of ",
        nrow(profiles), " profiles for design"
      )
    }
    oa <- join_profiles(oa, profiles)
    if (n_blocks > 1) {
      q_per_resp <- nrow(oa) / n_blocks
      if (q_per_resp %% 1 != 0) {
        stop(
          'The number of blocks used cannot be evenly divided into the ',
          'orthogonal array. Use a different number for "n_blocks" that ',
          'is as factor of ', nrow(oa)
        )
      }
      if (q_per_resp < n_q) {
        stop(
          'The orthogonal array cannot be divided into ', n_blocks,
          ' blocks such that each respondent sees ', n_q,
          ' questions. Either decrease "n_blocks" or increase "n_q".'
        )
      }
      oa$blockID <- rep(seq(n_blocks), each = q_per_resp)
      # Create random choice sets within each block
      choice_sets <- make_random_sets_by_block(oa, n_alts, n_blocks)
    } else {
      choice_sets <- make_random_sets(oa, n_alts)
    }
    design <- repeat_sets(choice_sets, n_resp, n_alts, n_q, n_blocks)
    if (no_choice) {
      warning(
        'Using a "no choice" option with orthogonal designs may damage the ',
        'orthogonality properties.'
      )
      design <- add_no_choice(design, n_alts)
    }
    return(design)
}

# D-optimal Design ----

make_design_dopt <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, no_choice, keep_d_eff
) {
  # First obtain the d-optimal array
  des <- AlgDesign::optFederov(~., profiles[,2:length(profiles)], n_q*n_blocks)
  d_eff <- des$Ge
  profiles <- merge(des$design, profiles, all.x = TRUE)
  profiles <- profiles[c(
    'profileID', names(profiles)[which(names(profiles) != 'profileID')])]
  if (n_blocks > 1) {
    profiles$blockID <- rep(seq(n_blocks), each = nrow(profiles) / n_blocks)
    choice_sets <- make_random_sets_by_block(profiles, n_alts, n_blocks)
  } else {
    choice_sets <- make_random_sets(profiles, n_alts)
  }
  design <- repeat_sets(choice_sets, n_resp, n_alts, n_q, n_blocks)
  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }
  # Print D-efficiency
  message("D-optimal design found with D-efficiency of ", round(d_eff, 5))

  # Return list containing the design and DB error if keep_d_eff = TRUE
  if (keep_d_eff) {
    return(list(design = design, d_eff = d_eff))
  }
  return(design)
}

# Bayesian D-efficient Design ----

make_design_bayesian <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, n_draws, n_start, no_choice,
    label, method, priors, prior_no_choice, probs, keep_db_error, max_iter,
    parallel, profiles_restricted
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
    design <- join_profiles(design, profiles)

    if (no_choice) {
      design <- add_no_choice_bayesian(design, n_alts, varnames[type_ids$discrete])
    }

    if (probs) {
      design$probs <- as.vector(t(D$probs))
    }

    design$blockID <- rep(seq(n_blocks), each = n_alts*n_q)

    # Repeat design to match number of respondents
    design <- repeat_sets(design, n_resp, n_alts, n_q, n_blocks)

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

add_no_choice_bayesian <- function(design, n_alts, varnames_discrete) {
  # First dummy code categorical variables
  design$obsID <- rep(seq(nrow(design) / n_alts), each = n_alts)
  design$altID <- rep(seq(n_alts), nrow(design) / n_alts)
  design <- design[which(design$altID != n_alts), ]
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

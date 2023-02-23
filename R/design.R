#' Make a random or Bayesian D-efficient choice-based conjoint survey design
#'
#' This function creates a data frame containing a choice-based conjoint survey
#' design where each row is an alternative. Designs can be either a
#' randomized or Bayesian D-efficient, in which case an implementation of the
#' CEA or Modfed Federov algorithm is used via the {idefix} package
#'
#' @keywords logitr mnl mxl mixed logit design
#' @param profiles A data frame in which each row is a possible profile.
#' This can be generated using the `cbc_profiles()` function.
#' @param n_resp Number of survey respondents.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent.
#' @param n_blocks Number of blocks used in Bayesian D-efficient design.
#' Max allowable is one block per respondent, defaults to `1`, meaning every
#' respondent sees the same set of choice questions.
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
#' Currently only compatible with randomized designs. If used, the `n_alts`
#' argument will be ignored as its value is defined by the unique number of
#' levels in the `label` variable. Defaults to `NULL`.
#' @param priors A list of one or more assumed prior parameters used to
#' generate a Bayesian D-efficient design. If `NULL` (the default), a
#' randomized design will be generated.
#' @param prior_no_choice Prior utility value for the "no choice" alternative.
#' Only required if `no_choice = TRUE`. Defaults to `NULL`.
#' @param probs If `TRUE`, for Bayesian D-efficient designs the resulting
#' design includes average predicted probabilities for each alternative in each
#' choice set given the sample from the prior preference distribution.
#' Defaults to `FALSE`.
#' @param method Which method to use for obtaining a Bayesian D-efficient
#' design, `"CEA"` or `"Modfed"`? Defaults to `"CEA"`. See `?idefix::CEA` and
#' `?idefix::Modfed` for more details.
#' @param max_iter A numeric value indicating the maximum number allowed
#' iterations when searching for a Bayesian D-efficient design. The default is
#' 50.
#' @param parallel Logical value indicating whether computations should be done
#' over multiple cores. The default is `TRUE`.
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
#' # Make a randomized survey design
#' design_rand <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3, # Number of alternatives per question
#'   n_q      = 6 # Number of questions per respondent
#' )
#'
#' # Make a randomized survey design with a "no choice" option
#' design_rand_nochoice <- cbc_design(
#'   profiles  = profiles,
#'   n_resp    = 300, # Number of respondents
#'   n_alts    = 3, # Number of alternatives per question
#'   n_q       = 6, # Number of questions per respondent
#'   no_choice = TRUE
#' )
#'
#' # Make a randomized labeled survey design with each "type" appearing in
#' # each choice question
#' design_rand_labeled <- cbc_design(
#'   profiles  = profiles,
#'   n_resp    = 300, # Number of respondents
#'   n_alts    = 3, # Number of alternatives per question
#'   n_q       = 6, # Number of questions per respondent
#'   label     = "type"
#' )
#'
#' # Make a Bayesian D-efficient design with a prior model specified
#' design_deff <- cbc_design(
#'     profiles  = profiles,
#'     n_resp    = 300, # Number of respondents
#'     n_alts    = 3, # Number of alternatives per question
#'     n_q       = 6, # Number of questions per respondent
#'     n_start   = 1,
#'     priors = list(
#'         price     = -0.1,
#'         type      = c(0.1, 0.2),
#'         freshness = c(0.1, 0.2)
#'     ),
#'     parallel = FALSE
#' )
cbc_design <- function(
  profiles,
  n_resp,
  n_alts,
  n_q,
  n_blocks = 1,
  n_draws = 50,
  no_choice = FALSE,
  n_start = 5,
  label = NULL,
  priors = NULL,
  prior_no_choice = NULL,
  probs = FALSE,
  method = "CEA",
  max_iter = 50,
  parallel = TRUE
) {
  check_inputs_design(
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
  )
  profiles <- as.data.frame(profiles) # tibbles break things
  if (is.null(priors)) {
    design <- make_design_rand(profiles, n_resp, n_alts, n_q, no_choice, label)
  } else if (!is.null(label)) {
    warning(
      "The use of the 'label' argument is currently only compatible with ",
      "randomized designs, so the provided 'priors' are being ignored.\n"
    )
    design <- make_design_rand(profiles, n_resp, n_alts, n_q, no_choice, label)
  } else {
    design <- make_design_deff(
      profiles, n_resp, n_alts, n_q, n_blocks, n_draws, no_choice, n_start,
      label, priors, prior_no_choice, probs, method, max_iter, parallel
    )
  }
  # Reset row numbers
  row.names(design) <- NULL
  return(design)
}

# Randomized Design ----

make_design_rand <- function(profiles, n_resp, n_alts, n_q, no_choice, label) {
  design <- get_design_rand(profiles, n_resp, n_alts, n_q)
  if (!is.null(label)) {
    design <- get_design_rand_label(profiles, n_resp, n_alts, n_q, label)
  }
  if (no_choice) {
    design <- add_no_choice(design, n_alts)
  }
  design <- reorder_cols(design)
  return(design)
}

get_design_rand <- function(profiles, n_resp, n_alts, n_q) {
  design <- sample_rows(profiles, size = n_resp * n_alts * n_q)
  design <- remove_dups(design, n_resp, n_alts, n_q)
  return(design)
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
  for (i in seq_len(n_levels)) {
    design_label <- sample_rows(labels[[i]], size = n_resp * n_q)
    design_label$labelID <- seq(nrow(design_label))
    labels[[i]] <- design_label
  }
  design <- do.call(rbind, labels)
  design <- design[order(design$labelID), ]
  design$labelID <- NULL
  # Add meta data and remove cases with double alternatives
  design <- remove_dups(design, n_resp, n_alts, n_q)
  return(design)
}

sample_rows <- function(profiles, size) {
  sample_ids <- sample(
    x = seq_len(nrow(profiles)), size = size, replace = TRUE
  )
  return(profiles[sample_ids, ])
}

remove_dups <- function(design, n_resp, n_alts, n_q) {
  design <- add_metadata(design, n_resp, n_alts, n_q)
  dup_rows <- get_dups(design, n_alts)
  while (length(dup_rows) > 0) {
    # cat('Number repeated: ', length(dup_rows), '\n')
    new_rows <- sample(
      x = seq(nrow(design)), size = length(dup_rows), replace = F
    )
    design[dup_rows, ] <- design[new_rows, ]
    design <- add_metadata(design, n_resp, n_alts, n_q)
    dup_rows <- get_dups(design, n_alts)
  }
  return(design)
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

get_dups <- function(design, n_alts) {
  counts <- tapply(
    design$profileID, design$obsID,
    FUN = function(x) length(unique(x))
  )
  dup_ids <- which(counts != n_alts)
  dup_rows <- which(design$obsID %in% dup_ids)
  return(dup_rows)
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

# Bayesian D-efficient Design ----

make_design_deff <- function(
    profiles, n_resp, n_alts, n_q, n_blocks, n_draws, no_choice, n_start,
    label, priors, prior_no_choice, probs, method, max_iter, parallel
) {

    # Set up initial parameters for creating design

    mu <- unlist(priors)
    if (no_choice) {
        mu <- c(prior_no_choice, mu)
    }
    profile_lvls <- profiles[,2:ncol(profiles)]
    lvl.names <- unname(lapply(profile_lvls, function(x) unique(x)))
    lvls <- unname(unlist(lapply(lvl.names, function(x) length(x))))
    coding <- rep("C", length(lvls))
    types <- get_col_types(profile_lvls)
    id_discrete <- types %in% c("factor", "character")
    id_continuous <- !id_discrete
    c.lvls <- NULL
    if (any(id_continuous)) {
        c.lvls <- lvl.names[id_continuous]
    }
    # lvl.names must be all characters for decoding process
    lvl.names <- lapply(lvl.names, function(x) as.character(x))
    if (any(id_discrete)) {
        coding[id_discrete] <- "D"
    }
    no_choice_alt <- NULL
    alt_cte <- rep(0, n_alts)
    if (no_choice) {
        n_alts <- n_alts + 1
        alt_cte <- c(alt_cte, 1)
        no_choice_alt <- n_alts
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
            cand.set = idefix::Profiles(
                lvls = lvls,
                coding = coding,
                c.lvls = c.lvls
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
    des <- idefix::Decode(
        des = D$design,
        n.alts = n_alts,
        alt.cte = alt_cte,
        lvl.names = lvl.names,
        c.lvls = c.lvls,
        coding = coding,
        no.choice = no_choice_alt
    )
    des <- des$design
    varnames <- names(priors)
    if (no_choice) {
        # First join on the profileIDs to the raw de-coded design
        des_raw <- des
        names(des_raw) <- varnames
        des_raw$row_id <- seq(nrow(des_raw))
        des_raw <- merge(des_raw, profiles, by = varnames, all.x = TRUE)
        des_raw <- des_raw[c('row_id', 'profileID')]
        # Now use dummy-coded design and add on profileIDs
        des <- as.data.frame(D$design)
        codednames <- encode_names(des, varnames, lvl.names, id_discrete)
        names(des) <- c("no_choice", codednames)
        des <- des[c(codednames, "no_choice")]
        row.names(des) <- NULL
        des$row_id <- seq(nrow(des))
        des <- merge(des, des_raw, by = 'row_id')
        des$row_id <- NULL
        des <- des[c('profileID', codednames)]
        des$no_choice <- as.vector(D$design[,1])
    } else {
        # Join on profileIDs
        names(des) <- varnames
        des$row_id <- seq(nrow(des)) # Keep track of row
        des <- merge(des, profiles, by = varnames)
        des <- des[order(des$row_id),]
        # Convert numeric columns to actual numbers
        des[,which(id_continuous)] <- lapply(
            des[,which(id_continuous)], function(x) as.numeric(x)
        )
        des <- des[c('profileID', varnames)]
    }

    # Include probs?
    if (probs) {
        des$probs <- as.vector(t(D$probs))
    }

    # Add blockIDs
    des$blockID <- rep(seq(n_blocks), each = n_alts*n_q)

    # Repeat design to match number of respondents
    n_reps <- ceiling(n_resp / n_blocks)
    design <- des[rep(seq_len(nrow(des)), n_reps), ]
    row.names(design) <- NULL
    design <- design[1:(n_resp*n_q*n_alts), ]

    # Add metadata
    design <- add_metadata(design, n_resp, n_alts, n_q)
    design <- reorder_cols(design)

    # Print error
    message(
        "Bayesian D-efficient design found with DB-error of ",
        round(D$error, 5)
    )

    return(design)
}

encode_names <- function(des, varnames, lvl.names, id_discrete) {
    codednames <- list()
    for (i in 1:length(varnames)) {
        if (id_discrete[i]) {
            varlevels <- lvl.names[[i]]
            codednames[[i]] <- paste(
                varnames[i],
                varlevels[2:length(varlevels)], sep = "_"
            )
        } else {
            codednames[[i]] <- varnames[i]
        }
    }
    return(unlist(codednames))
}

#' Make a coded matrix of all possible combinations of attribute levels
#'
#' This function is a wrapper for the `Profiles()` function from the `idefix`
#' package, which creates a coded matrix of all possible combinations of
#' attribute levels defined in the `levels` argument.
#' @param levels A named list of vectors defining each attribute (the names)
#' and each level for each attribute (the vectors). For example, a design
#' with two attributes, `"price"` (numeric) and `"type"` (categorical),
#' that each had three levels could be defined as
#' `levels = list(price = c(1, 2, 3), type = c("type1", "type2", "type3"))`.
#' @return A data frame of all possible combinations of attribute levels.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Generate profiles of all combinations for each attribute and level
#' profiles <- cbc_profiles(levels)
cbc_profiles <- function(levels) {
    profiles <- expand.grid(levels)
    profiles$profileID <- seq(nrow(profiles))
    varNames <- varNames <- setdiff(names(profiles), "profileID")
    profiles <- profiles[,c("profileID", varNames)]
    return(profiles)
}

#' Make a random or D-efficient choice-based conjoint survey design
#'
#' This function creates a data frame containing a choice-based conjoint survey
#' design where each row is an alternative. Designs can be either fully
#' randomized or D-efficient, in which case an implementation of the
#' Modified Federov algorithm is used via the `idefix` package.
#'
#' @param profiles A data frame in which each row is a possible profile.
#' This can be generated using the `cbc_profiles()` function.
#' @param n_resp Number of survey respondents.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent.
#' @param no_choice Include a "none" option in the choice sets? Defaults to
#' `FALSE`. If `TRUE`, the total number of alternatives per question will be
#' one more than the provided `n_alts` argument.
#' @param d_eff If `TRUE`, returns a D-efficient design where each respondent
#' is shown the same design, otherwise a fully randomized design is returned.
#' Defaults to `FALSE`.
#' @param label The name of the variable to use in a "labeled" design
#' such that each set of alternatives contains one of each of the levels in
#' the `label` attribute. If used, the `n_alts` argument will be ignored as its
#' value is determined by the unique number of levels in the `label` variable.
#' Defaults to `NULL`.
#' @return A data frame containing a choice-based conjoint survey design where
#' each row is an alternative.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Generate profiles of all combinations for each attribute and level
#' profiles <- cbc_profiles(levels)
#'
#' # Make a fully random conjoint survey
#' survey <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Make a D-efficient conjoint survey
#' survey <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6,    # Number of questions per respondent
#'   d_eff    = TRUE
#' )
cbc_design <- function(
    profiles,
    n_resp,
    n_alts,
    n_q,
    no_choice = FALSE,
    d_eff = FALSE,
    label = NULL
) {
    profiles <- as.data.frame(profiles) # tibbles break things
    if (d_eff) {
        design <- make_design_eff(
            profiles, n_resp, n_alts, n_q, no_choice, label)
    } else {
        design <- make_design_rand(
            profiles, n_resp, n_alts, n_q, no_choice, label)
    }
    return(design)
}

# make_design_eff <- function(profiles, n_resp, n_alts, n_q, no_choice, label) {
#     survey <- get_eff_design(profiles, n_resp, n_alts, n_q, no_choice, label)
#     return(survey)
# }
#
# get_eff_design <- function(profiles, n_resp, n_alts, n_q, no_choice, label) {
#     mu <- rep(0, ncol(profiles))
#     alt_cte <- rep(0, n_alts)
#     if (no_choice) {
#         n_alts <- n_alts + 1
#         mu <- c(0, mu)
#         alt_cte <- c(alt_cte, 1)
#     }
#     sigma <- diag(length(mu))
#     par_draws <- MASS::mvrnorm(n = 500, mu = mu, Sigma = sigma)
#     n_alt_cte <- sum(alt_cte)
#     if (n_alt_cte >= 1) {
#         par_draws <- list(
#             par_draws[, 1:n_alt_cte],
#             par_draws[, (n_alt_cte + 1):ncol(par_draws)])
#     }
#     design <- idefix::Modfed(
#         cand.set = profiles,
#         n.sets = n_resp,
#         n.alts = n_alts,
#         n.start = 5,
#         alt.cte = alt_cte,
#         no.choice = no_choice,
#         par.draws = par_draws
#     )
#     return(design)
# }

# Randomized design ----

make_design_rand <- function(profiles, n_resp, n_alts, n_q, no_choice, label) {
    design <- design_rand(profiles, n_resp, n_alts, n_q)
    if (!is.null(label)) {
      design <- design_rand_label(profiles, n_resp, n_alts, n_q, label)
    }
    if (no_choice) {
       design <- add_no_choice(design, n_alts)
    }
    design <- reorder_cols(design)
    return(design)
}

design_rand <- function(profiles, n_resp, n_alts, n_q) {
  design <- sample_rows(profiles, size = n_resp*n_alts*n_q)
  design <- remove_dups(design, n_resp, n_alts, n_q)
  return(design)
}

design_rand_label <- function(profiles, n_resp, n_alts, n_q, label) {
    n_levels <- length(unique(profiles[,label]))
    if (n_levels != n_alts) {
        warning(paste0(
            "The n_alts argument is being set to ", n_levels,
            " to match the number of unique levels in the ", label,
            " variable"
        ))
        # Over-ride user-provided n_alts as it is determined by the label
        n_alts <- n_levels
    }
    # Randomize rows by label
    labels <- split(profiles, profiles[label])
    for (i in seq_len(n_levels)) {
        design_label <- sample_rows(labels[[i]], size = n_resp*n_q)
        design_label$labelID <- seq(nrow(design_label))
        labels[[i]] <- design_label
    }
    design <- do.call(rbind, labels)
    design <- design[order(design$labelID),]
    design$labelID <- NULL
    # Add meta data and remove cases with double alternatives
    design <- remove_dups(design, n_resp, n_alts, n_q)
    return(design)
}

sample_rows <- function(profiles, size) {
    sample_ids <- sample(
      x = seq_len(nrow(profiles)), size = size, replace = TRUE)
    return(profiles[sample_ids,])
}

remove_dups <- function(design, n_resp, n_alts, n_q) {
    design <- add_metadata(design, n_resp, n_alts, n_q)
    dup_rows <- get_dups(design, n_alts)
    while (length(dup_rows) > 0) {
        # cat('Number repeated: ', length(dup_rows), '\n')
        new_rows <- sample(
            x = seq(nrow(design)), size = length(dup_rows), replace = F)
        design[dup_rows,] <- design[new_rows,]
        design <- add_metadata(design, n_resp, n_alts, n_q)
        dup_rows <- get_dups(design, n_alts)
    }
    return(design)
}

add_metadata <- function(design, n_resp, n_alts, n_q) {
    n_rows_per_resp   <- n_alts*n_q
    design$respID     <- rep(seq(n_resp), each = n_rows_per_resp)
    design$qID        <- rep(rep(seq(n_q), each = n_alts), n_resp)
    design$altID      <- rep(seq(n_alts), n_resp*n_q)
    design$obsID      <- rep(seq(n_resp * n_q), each = n_alts)
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

reorder_cols <- function(design) {
    metaNames <- c("respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(design), metaNames)
    design <- as.data.frame(design)[,c(metaNames, varNames)]
    return(design)
}

add_no_choice <- function(design, n_alts) {
    # Must dummy code categorical variables to include an outside good
    design <- dummy_code(design)
    # Create outside good rows
    design_og <- design[which(design$altID == 1),]
    design_og[
        , !names(design_og) %in% c("respID", "qID", "altID", "obsID")] <- 0
    design_og$altID <- n_alts + 1
    design_og$no_choice <- 1
    # Insert outside good rows into design
    design$no_choice <- 0
    design <- rbind(design, design_og)
    design <- design[order(design$obsID),]
    return(design)
}

dummy_code <- function(design) {
    types <- get_col_types(design)
    nonnumeric <- names(types[!types %in% c("integer", "numeric")])
    if (length(nonnumeric) > 0) {
        design <- fastDummies::dummy_cols(design, nonnumeric)
        design[,nonnumeric] <- NULL
    }
    return(design)
}

get_col_types <- function(data) {
  types <- lapply(data, class)
  test <- function(x) {x[1]}
  return(unlist(lapply(types, test)))
}

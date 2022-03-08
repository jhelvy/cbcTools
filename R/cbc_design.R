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
    profiles$profile_id <- seq(nrow(profiles))
    varNames <- varNames <- setdiff(names(profiles), "profile_id")
    profiles <- profiles[,c("profile_id", varNames)]
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
    counts <- tapply(design$profile_id, design$obsID,
                     FUN = function(x) length(unique(x)))
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








repDf <- function(df, n) {
    return(df[rep(seq_len(nrow(df)), n), ])
}

randomizeSurvey <- function(doe, nResp, nAltsPerQ, nQPerResp, outsideGood) {
    survey <- initializeSurvey(doe, nResp, nAltsPerQ, nQPerResp)
    # Randomize rows
    n <- nResp*nAltsPerQ*nQPerResp
    survey <- randomizeRows(survey, n)
    # Add meta data and remove cases with double alternatives
    survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
    survey <- removeDuplicateAlts(survey, nResp, nAltsPerQ, nQPerResp)
    # Re-order column names
    survey <- reorderSurveyCols(survey)
    if (outsideGood) {
        survey <- addOutsideGood(survey, nAltsPerQ)
    }
    return(survey)
}



initializeSurvey <- function(doe, nResp, nAltsPerQ, nQPerResp, label = NULL) {
    # Replicate doe if the needed number of observations (n) is larger than
    # the number of observations in the doe (n_doe)
    n <- nResp*nAltsPerQ*nQPerResp
    n_doe <- nrow(doe)
    nReps <- ceiling(n / n_doe)
    if (!is.null(label)) {
        # If there is a label, then need to replicate based on the
        # lowest number of any one label
        n_labels <- min(table(doe[label]))
        n <- nResp*nQPerResp
        nReps <- ceiling(n / n_labels)
    }
    survey <- doe
    if (nReps > 1) {
        survey <- repDf(doe, nReps)
    }
    row.names(survey) <- NULL
    return(survey)
}

randomizeRows <- function(survey, n) {
    sample_ids <- sample(x = seq_len(nrow(survey)), size = n, replace = FALSE)
    return(survey[sample_ids,])
}

addMetaData <- function(survey, nResp, nAltsPerQ, nQPerResp) {
    nRowsPerResp      <- nAltsPerQ*nQPerResp
    survey$respID     <- rep(seq(nResp), each = nRowsPerResp)
    survey$qID        <- rep(rep(seq(nQPerResp), each = nAltsPerQ), nResp)
    survey$altID      <- rep(seq(nAltsPerQ), nResp*nQPerResp)
    survey$obsID      <- rep(seq(nResp * nQPerResp), each = nAltsPerQ)
    row.names(survey) <- NULL
    return(survey)
}

removeDuplicateAlts <- function(survey, nResp, nAltsPerQ, nQPerResp) {
    duplicateRows <- getDuplicateRows(survey, nAltsPerQ)
    while (length(duplicateRows) > 0) {
        # cat('Number repeated: ', length(duplicateRows), '\n')
        newRows <- sample(
            x = seq(nrow(survey)), size = length(duplicateRows), replace = F)
        survey[duplicateRows,] <- survey[newRows,]
        survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
        duplicateRows <- getDuplicateRows(survey, nAltsPerQ)
    }
    return(survey)
}

getDuplicateRows <- function(survey, nAltsPerQ) {
    counts <- tapply(survey$rowID, survey$obsID,
                     FUN = function(x) length(unique(x)))
    duplicateIDs <- which(counts != nAltsPerQ)
    duplicateRows <- which(survey$obsID %in% duplicateIDs)
    return(duplicateRows)
}

reorderSurveyCols <- function(survey) {
    metaNames <- c("respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(survey), metaNames)
    survey <- as.data.frame(survey)[,c(metaNames, varNames)]
    survey$rowID <- NULL
    return(survey)
}

addOutsideGood <- function(survey, nAltsPerQ) {
    survey <- dummyCodeCheck(survey)
    # Create outside good rows
    survey_og <- survey[which(survey$altID == 1),]
    survey_og[,!names(survey_og)%in%c("respID", "qID", "altID", "obsID")] <- 0
    survey_og$altID <- nAltsPerQ + 1
    survey_og$outsideGood <- 1
    # Add outside good to survey
    survey$outsideGood <- 0
    survey <- rbind(survey, survey_og)
    survey <- survey[order(survey$obsID),]
    return(survey)
}

dummyCodeCheck <- function(survey) {
    types <- getColumnTypes(survey)
    nonnumeric <- names(types[!types %in% c("integer", "numeric")])
    if (length(nonnumeric) > 0) {
        warning(paste0(
            "The following variables are non-numeric:\n",
            paste0(nonnumeric, collapse = ", "), "\n\n",
            "Converting them to dummy-coded variables in order to add ",
            "outside good"
        ))
        survey <- fastDummies::dummy_cols(survey, nonnumeric)
        survey[,nonnumeric] <- NULL
    }
    return(survey)
}

getColumnTypes <- function(data) {
  types <- lapply(data, class)
  test <- function(x) {x[1]}
  return(unlist(lapply(types, test)))
}

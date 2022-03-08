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
#' # Generate profiles of all possible combinations for each attribute and level
#' profiles <- cbc_profiles(levels)
cbc_profiles <- function(levels) {
    return(expand.grid(levels))
}

#' Make a random or D-efficient choice-based conjoint survey design
#'
#' This function creates a data frame containing a choice-based conjoint survey
#' design where each row is an alternative. Designs can be either fully
#' randomized or D-efficient, in which case an implementation of the
#' Modified Federov algorithm is used via the `idefix` package.
#'
#' @param profiles A numeric matrix in which each row is a possible profile.
#' This can be generated using the `cbc_profiles()` function.
#' @param n_resp Number of survey respondents.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent.
#' @param no_choice Include a "none" option in the choice sets? Defaults to
#' `FALSE`. If `TRUE`, the total number of alternatives per question will be
#' one more than the provided `n_alts` argument.
#' @param d_eff Set to `TRUE` to return a D-efficient design, otherwise a fully
#' randomized design is returned. Defaults to `FALSE`.
#' @param label The name of the variable to use in a "labeled" design
#' such that each set of alternatives contains one of each of the levels in
#' the `label` attribute. If included, the `n_alts` argument will be
#' ignored as its value is determined by the unique number of levels in the
#' `label` variable. Defaults to `NULL`.
#' @return A data frame containing a choice-based conjoint survey design where
#' each row is an alternative.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Generate all profiles with price as a continuous variable
#' profiles <- cbc_profiles(levels, coding = c("C", "D", "D"))
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
    profiles$id <- seq(nrow(profiles)) # Set these to remove dupes later
    if (d_eff) {
        design <- make_design_eff(
            profiles, n_resp, n_alts, n_q, no_choice, label)
    } else {
        design <- make_design_rand(
            profiles, n_resp, n_alts, n_q, no_choice, label)
    }
    return(design)
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

randomizeSurveyLabeled <- function(
    doe, nResp, nAltsPerQ, nQPerResp, outsideGood, label
) {
    nLabelLevels <- length(unique(doe[,label]))
    if (nLabelLevels != nAltsPerQ) {
        warning(paste0(
            "The nAltsPerQ argument is being set to ", nLabelLevels,
            " to match the number of unique levels in the ", label, " variable"
        ))
        # Over-ride user-provided nAltsPerQ as it is determined by the label
        nAltsPerQ <- nLabelLevels
    }
    survey <- initializeSurvey(doe, nResp, nAltsPerQ, nQPerResp, label)
    # Randomize rows by label
    labels <- split(survey, survey[label])
    for (i in seq_len(length(labels))) {
        n <- nResp*nQPerResp
        survey_label <- randomizeRows(labels[[i]], n)
        survey_label$labelID <- seq(nrow(survey_label))
        labels[[i]] <- survey_label
    }
    survey <- do.call(rbind, labels)
    survey <- survey[order(survey$labelID),]
    survey$labelID <- NULL
    # Add meta data and remove cases with double alternatives
    survey <- addMetaData(survey, nResp, nAltsPerQ, nQPerResp)
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

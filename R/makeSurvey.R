#' Make a randomized conjoint survey from a design of experiment
#'
#' @param doe A design of experiment data frame. Each row is an alternative,
#' each column is an attribute.
#' @param nResp Maximum number of survey respondents.
#' @param nAltsPerQ Number of alternatives per question.
#' @param nQPerResp Number of questions per respondent.
#' @param outsideGood Include an outside good in the choice sets? Defaults to
#' `FALSE`. If included, the total number of alternatives per question will be
#' one more than the provided `nAltsPerQ` argument.
#' @param label The name of the variable to use in a "labeled" design
#' such that each set of alternatives contains one of each of the levels in
#' the `label` attribute. If included, the `nAltsPerQ` argument will be
#' ignored as its value is determined by the unique number of levels in the
#' `label` variable. Defaults to `NULL`.
#' @param group No longer used as of v0.0.9 - if provided, this is passed
#' to the `label` argument and a warning is displayed.
#' @return Returns a conjoint survey by randomly sampling from a design of
#' experiment data frame. The sampling is done iteratively to remove the
#' possibility of duplicate alternatives appearing in the same choice question.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a full-factorial design of experiment
#' doe <- makeDoe(levels)
#'
#' # Re-code levels
#' doe <- recodeDoe(doe, levels)
#'
#' # Make the conjoint survey by randomly sampling from the doe
#' survey <- makeSurvey(
#'   doe       = doe,  # Design of experiment
#'   nResp     = 2000, # Total number of respondents (upper bound)
#'   nAltsPerQ = 3,    # Number of alternatives per question
#'   nQPerResp = 6     # Number of questions per respondent
#' )
makeSurvey <- function(
    doe,
    nResp,
    nAltsPerQ,
    nQPerResp,
    outsideGood = FALSE,
    label = NULL,
    group
) {
    # Argument names were changed in v0.0.9
    calls <- names(sapply(match.call(), deparse))[-1]
    if (any("group" %in% calls)) {
        label <- group
        warning(
            "The 'group' argument is outdate as of v0.0.9. Use 'label' instead"
        )
    }
    doe <- as.data.frame(doe) # tibbles break things
    doe$rowID <- seq(nrow(doe)) # Have to set these now to remove dupes later
    if (is.null(label)) {
        return(randomizeSurvey(doe, nResp, nAltsPerQ, nQPerResp, outsideGood))
    }
    return(randomizeSurveyLabeled(
        doe, nResp, nAltsPerQ, nQPerResp, outsideGood, label))
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

#' Make a choice-based conjoint survey design
#'
#' @param levels A named list of vectors defining each attribute (the names)
#' and each level for each attribute (the vectors). For example, a design
#' with two attributes, `"price"` and `"type"`, that each had three levels
#' should be defined as `levels = list(price = c(1, 2, 3), type = c("1", "2", "3"))`.
#' Numeric values will be treated as continuous levels and character values
#' will be treated as discrete levels.
#' @param nResp Maximum number of survey respondents.
#' @param nAltsPerQ Number of alternatives per question.
#' @param nQPerResp Number of questions per respondent.
#' @param outsideGood Include an outside good in the choice sets? Defaults to
#' `FALSE`. If `TRUE`, the total number of alternatives per question will be
#' one more than the provided `nAltsPerQ` argument.
#' @param random Set to `TRUE` to return a fully randomized design, otherwise
#' returns a D-efficient design. Defaults to `TRUE`.
#' @param label The name of the variable to use in a "labeled" design
#' such that each set of alternatives contains one of each of the levels in
#' the `label` attribute. If included, the `nAltsPerQ` argument will be
#' ignored as its value is determined by the unique number of levels in the
#' `label` variable. Defaults to `NULL`.
#' @param maxTrials The maximum number of trials to be used in a fractional
#' factorial design. Defaults to `NA`, in which case it is set to the number of
#' alternatives in the full factorial design. If `search = TRUE`, then all
#' feasible designs will be calculated up to `maxTrials` or until a perfect
#' D-efficiency (1) is found.
#' @param search If `TRUE`, all feasible designs are calculated up to
#' `maxTrials` or until a perfect D-efficiency (1) is found, after which a
#' summary of the search results is printed and the top-ranked D-efficient
#' design is returned. Defaults to `FALSE`.
#' @return Returns a data frame containing a choice-based conjoint survey
#' design.
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
#' survey <- cbc_design(
#'   doe       = doe,  # Design of experiment
#'   nResp     = 2000, # Total number of respondents (upper bound)
#'   nAltsPerQ = 3,    # Number of alternatives per question
#'   nQPerResp = 6     # Number of questions per respondent
#' )
cbc_design <- function(
    levels,
    nResp,
    nAltsPerQ,
    nQPerResp,
    outsideGood = FALSE,
    random = TRUE,
    label = NULL,
    maxTrials = NA,
    search = FALSE
) {
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

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
#' @param random Set to `TRUE` to return a fully randomized design, otherwise
#' returns a D-efficient design. Defaults to `TRUE`.
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
#'   n_q      = 6,   # Number of questions per respondent
#' )
#'
#' # Make a D-efficient conjoint survey
#' survey <- cbc_design(
#'   profiles = profiles,
#'   n_resp   = 300, # Number of respondents
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#'   random   = FALSE
#' )
cbc_design <- function(
    profiles,
    n_resp,
    n_alts,
    n_q,
    no_choice = FALSE,
    random = TRUE,
    label = NULL
) {
    profiles <- as.data.frame(profiles) # tibbles break things
    profiles$id <- seq(nrow(profiles)) # Set these to remove dupes later
    if (random) {
        design <- make_design_rand(
            profiles, n_resp, n_alts, n_q, no_choice, label)
    } else {
        design <- make_design_eff(
            profiles, n_resp, n_alts, n_q, no_choice, label)
    }
    return(design)
}

make_design_rand <- function(profiles, n_resp, n_alts, n_q, no_choice, label) {
    design <- sample_rows(profiles, n = n_resp*n_alts*n_q)
    design <- remove_dups(design, n_resp, n_alts, n_q)
    design <- reorder_cols(design)
    if (no_choice) {
        design <- add_no_choice(design, n_alts)
    }
    return(design)
}

make_design_eff <- function(profiles, n_resp, n_alts, n_q, no_choice, label) {
    nLabelLevels <- length(unique(doe[,label]))
    if (nLabelLevels != n_alts) {
        warning(paste0(
            "The n_alts argument is being set to ", nLabelLevels,
            " to match the number of unique levels in the ", label, " variable"
        ))
        # Over-ride user-provided n_alts as it is determined by the label
        n_alts <- nLabelLevels
    }
    survey <- initializeSurvey(doe, n_resp, n_alts, n_q, label)
    # Randomize rows by label
    labels <- split(survey, survey[label])
    for (i in seq_len(length(labels))) {
        n <- n_resp*n_q
        survey_label <- randomize_rows(labels[[i]], n)
        survey_label$labelID <- seq(nrow(survey_label))
        labels[[i]] <- survey_label
    }
    survey <- do.call(rbind, labels)
    survey <- survey[order(survey$labelID),]
    survey$labelID <- NULL
    # Add meta data and remove cases with double alternatives
    survey <- add_metadata(survey, n_resp, n_alts, n_q)
    # Re-order column names
    survey <- reorder_cols(survey)
    if (no_choice) {
        survey <- addno_choice(survey, n_alts)
    }
    return(survey)
}

sample_rows <- function(profiles, n) {
    sample_ids <- sample(x = seq_len(nrow(profiles)), size = n, replace = TRUE)
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
    counts <- tapply(design$id, design$obsID,
                     FUN = function(x) length(unique(x)))
    dup_ids <- which(counts != n_alts)
    dup_rows <- which(design$obsID %in% dup_ids)
    return(dup_rows)
}

reorder_cols <- function(design) {
    metaNames <- c("respID", "qID", "altID", "obsID")
    varNames <- setdiff(names(design), metaNames)
    design <- as.data.frame(design)[,c(metaNames, varNames)]
    design$id <- NULL
    return(design)
}

add_no_choice <- function(design, n_alts) {
    # Create outside good rows
    design_og <- design[which(design$altID == 1),]
    design_og[
        , !names(design_og) %in% c("respID", "qID", "altID", "obsID")] <- 0
    design_og$altID <- n_alts + 1
    design_og$no_choice <- 1
    # Insert outside good rows in design
    design$no_choice <- 0
    design <- rbind(design, design_og)
    design <- design[order(design$obsID),]
    return(design)
}

#' Make a coded matrix of all possible combinations of attribute levels
#'
#' This function is a wrapper for the `Profiles()` function from the `idefix`
#' package, which creates a coded matrix of all possible combinations of
#' attribute levels defined in the `levels` argument.
#' @param levels A named list of vectors defining each attribute (the names)
#' and each level for each attribute (the vectors). For example, a design
#' with two attributes, `"price"` and `"type"`, that each had three levels
#' should be defined as
#' `levels = list(price = c(1, 2, 3), type = c("1", "2", "3"))`.
#' @param coding The type of coding to be used for each attribute. Valid
#' arguments for are `"C"` for continuous attributes, `"D"` for dummy-coded
#' attributes, and `"E"` for effects-coded attributes When using `"C"` the
#' values provided for the associated attribute will be directly used in the
#' design matrix. Defaults to `NULL`, in which case all attributes are
#' dummy-coded.
#' @return A coded matrix of all possible combinations of attribute levels.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Generate all profiles with all attributes dummy-coded
#' profiles <- cbc_profiles(levels)
#'
#' # Generate all profiles with price as a continuous variable
#' profiles <- cbc_profiles(levels, coding = c("C", "D", "D"))
cbc_profiles <- function(levels, coding = NULL) {
    lvls <- unlist(lapply(levels, length))
    if (is.null(coding)) {
        coding <- rep("D", length(lvls))
    }
    if (any(coding == "C")) {
        clvls <- levels[which(coding == "C")]
        return(Profiles(lvls = lvls, coding = coding, c.lvls = clvls))
    }
    return(idefix::Profiles(lvls = lvls, coding = coding))
}





rep_df <- function(df, n) {
  result <- df[rep(seq_len(nrow(df)), n), ]
  row.names(result) <- NULL
  return(result)
}

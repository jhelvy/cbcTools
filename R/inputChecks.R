checkDoeInputs <- function(ff, type, nTrials, minLevels, minTrials) {
    if (is.na(nTrials)) {
        stop(
            'Fractional factorial designs require a numeric input for the ',
            '"nTrials" argument.')
    }
    maxTrials <- nrow(ff)
    if (nTrials > maxTrials) {
        stop(
            'There are only ', maxTrials, ' trials in the full factorial ',
            'design. Set nTrials <= ', maxTrials
        )
    }
    if (nTrials <= minLevels) {
        stop(
            'Based on the levels in "levels", the fractional design must have ',
            'at least ', minLevels, ' levels. Set nTrials > ', minLevels
        )
    }
    if (!is.null(minTrials)) {
        if (minTrials > nTrials) {
            stop('minTrials cannot be greater than nTrials')
        }
    }
    if (! type %in% c("D", "A", "I")) {
        stop('The type argument must be "D", "A", or "I"')
    }
}

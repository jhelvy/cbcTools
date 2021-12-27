#' Make a design of experiment
#'
#' @param levels A named list of vectors defining each attribute (the names)
#' and each level for each attribute (the vectors). For example, a design
#' with two attributes, "price" and "type", that each had three levels should
#' be defined as `levels = list(price = c(1, 2, 3), type = c(1, 2, 3))`.
#' @param type The type of design. Defaults to `NULL`, which returns a full
#' factorial design. Set to `"D"`, `"A"`, and `"I"` to obtain a fractional
#' factorial design with either D, A, or I optimality criteria.
#' @param nTrials The number of trials to be used in a fractional factorial
#' design. Defaults to `NA`, but must be a number less than the number of
#' alternatives in the full factorial design if the `type` argument is anything
#' other than `NULL`. If `search = TRUE`, then all feasible designs will be
#' calculated up to `nTrials` or until a perfect D-efficiency is found.
#' @param minTrials If `search == TRUE` and `minTrials` is a number less than
#' or equal to `nTrials`, then the feasible designs are calculated over the
#' range of `minTrials` to `nTrials`. Defaults to `NULL`, in which case the
#' search starts at the lowest feasible number of levels.
#' @param search If `TRUE`, all feasible designs are calculated up to `nTrials`
#' or until a perfect D-efficiency is found, after which a summary of the
#' search results is printed and the top-ranked d-efficient design is returned.
#' @param nRepeats Number of times the design search using
#' `AlgDesign::optFederov()` is repeated for any one instance of `nTrials`.
#' Defaults to `5`.
#' @return Returns a full factorial or fraction factorial design of experiment.
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
#' # Make a fraction-factorial design of experiment based on D-efficiency
#' doe <- makeDoe(levels, type = "D", nTrials = 100)
makeDoe <- function(
    levels,
    type      = NULL,
    nTrials   = NA,
    minTrials = NULL,
    search    = FALSE,
    nRepeats  = 5
) {
    vars <- unlist(lapply(levels, length))
    doe <- getFullFactorial(vars)
    if (!is.null(type)) {
        minLevels <- sum(vars) - length(vars)
        checkDoeInputs(doe, type, nTrials, minLevels, minTrials)
        if (search) {
            if (!is.null(minTrials)) {
                minLevels <- minTrials
            } else {
                # Add a cushion so you get less singularity problems
                minLevels <- minLevels + 3
            }
            if (minLevels >= nTrials) {
                minLevels <- nTrials
            }
        } else {
            minLevels <- nTrials
        }
        doe <- searchDesigns(doe, nTrials, type, minLevels, nRepeats)
    }
    return(doe)
}

getFullFactorial <- function(vars) {
    ff <- AlgDesign::gen.factorial(
        levels = vars, varNames = names(vars), factors = "all"
    )
    return(ff)
}

searchDesigns <- function(ff, nTrials, type, minLevels, nRepeats) {
    results <- list()
    for (i in seq(minLevels, nTrials)) {
        result <- computeDesign(ff, nTrials = i, type, nRepeats)
        results[[as.character(i)]] <- result
        if (result$d == 1) { break }
    }
    result <- aggregateDoeSearch(results)
    result <- result[order(-result$d, result$balanced),]
    print(result)
    return(results[[as.character(result$nTrials[1])]]$doe)
}

computeDesign <- function(ff, nTrials, type, nRepeats) {
    result <- AlgDesign::optFederov(
        data = ff, nTrials = nTrials, criterion = type,
        approximate = FALSE, nRepeats = nRepeats
    )
    row.names(result$design) <- NULL
    doe <- result$design
    return(list(
        doe = doe,
        d = result$Dea,
        balanced = isBalanced(doe),
        nTrials = nTrials
    ))
}

isBalanced <- function(doe) {
  counts <- apply(doe, 2, table)
  numCounts <- unlist(lapply(counts, function(x) length(unique(x))))
  if (any(numCounts != 1)) {
    return(FALSE)
  }
  return(TRUE)
}

aggregateDoeSearch <- function(results) {
    result <- data.frame(
        nTrials = unlist(lapply(results, function(x) x$nTrials)),
        d = unlist(lapply(results, function(x) x$d)),
        balanced = unlist(lapply(results, function(x) x$balanced))
    )
    row.names(result) <- NULL
    return(result)
}

#' Evaluate a design of experiment
#'
#' Obtain a list of information about a given design, including the
#' D-efficiency and whether or not the design is balanced.
#' @param doe A design of experiment data frame. Each row is an alternative,
#' each column is an attribute.
#' @return Returns a list of information about a given design, including the
#' D-efficiency and whether or not the design is balanced.
#' `levels` argument.
#' @export
#' @examples
#' # Define the attributes and levels
#' levels <- list(
#'   price     = seq(1, 4, 0.5), # $ per pound
#'   type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
#'   freshness = c('Excellent', 'Average', 'Poor')
#' )
#'
#' # Make a fraction-factorial design of experiment based on D-efficiency
#' doe <- makeDoe(levels, type = "D", nTrials = 70)
#'
#' # Print the design efficiency and whether it is balanced:
#' evaluateDoe(doe)
evaluateDoe <- function(doe) {
    vars <- apply(doe, 2, function(x) length(unique(x)))
    ff <- getFullFactorial(vars)
    design <- decodeDoe(doe, vars)
    frml <- stats::formula("~-1 + .")
    eff <- AlgDesign::eval.design(frml = frml, design = design, X = ff)
    return(list(
        d_eff = eff$Deffbound,
        balanced = isBalanced(doe)
    ))
}

decodeDoe <- function(doe, vars) {
  levels <- lapply(vars, function(x) seq_len(x))
  for (i in seq_len(length(levels))) {
    col <- which(names(doe) == names(levels)[i])
    doe[,col] <- as.factor(doe[,col])
    levels(doe[,col]) <- levels[[i]]
  }
  return(doe)
}

#' Re-code the levels in a design of experiment
#'
#' @param doe A design of experiment data frame. Each row is an alternative,
#' each column is an attribute.
#' @param levels A named list of vectors defining each attribute (the names)
#' and each level for each attribute (the vectors). For example, a design
#' with two attributes, "price" and "type", that each had three levels should
#' be defined as `levels = list(price = c(1, 2, 3), type = c(1, 2, 3))`.
#' @return Returns a re-coded design of experiment using the labels in the
#' `levels` argument.
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
recodeDoe <- function(doe, levels) {
  type_numeric <- unlist(lapply(levels, is.numeric))
  for (i in seq_len(length(levels))) {
    col <- which(names(doe) == names(levels)[i])
    levels(doe[,col]) <- levels[[i]]
    if (type_numeric[i]) {
      doe[,col] <- as.numeric(as.character(doe[,col]))
    } else {
      doe[,col] <- as.character(doe[,col])
    }
  }
  return(doe)
}

#' Display version number and date when the package is loaded.
#' @noRd
.onAttach <- function(libname, pkgname) {
  desc  <- utils::packageDescription(pkgname, libname)
  packageStartupMessage(
    "Version:  ", desc$Version, "\n",
    "Author:   ", "John Paul Helveston (George Washington University)", "\n\n",
    "Consider submitting praise at\n",
    "https://github.com/jhelvy/cbcTools/issues/3.\n\n",
    "Please cite the package in your publications, see:\ncitation(\"cbcTools\")"
  )
}

get_id_names <- function() {
    return(c("profileID", "blockID", "respID", "qID", "altID", "obsID"))
}

get_var_names <- function(design) {
    return(setdiff(names(design), get_id_names()))
}

validate_priors <- function(priors) {
    if (!is.null(priors)) {
        if (!inherits(priors, "cbc_priors")) {
            stop("priors must be created using cbc_priors()")
        }
    }
}

get_rand_pars <- function(priors) {
    if (is.null(priors)) { return(NULL) }
    randPars <- names(which(sapply(priors$attrs, function(x) x$random)))
    if (length(randPars) == 0) { return(NULL) }
    return(randPars)
}

# Null-coalescing operator helper
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

#' Check if object is a cbc_profiles object
#' @param x Object to check
#' @return Logical indicating if x is a cbc_profiles object
#' @export
is.cbc_profiles <- function(x) {
  inherits(x, "cbc_profiles")
}

#' Check if object is a cbc_design object
#' @param x Object to check
#' @return Logical indicating if x is a cbc_design object
#' @export
is.cbc_design <- function(x) {
  inherits(x, "cbc_design")
}

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

# Null-coalescing operator helper
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

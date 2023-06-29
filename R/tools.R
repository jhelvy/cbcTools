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

method_is_bayesian <- function(method) {
    return(method %in% c('CEA', 'Modfed'))
}

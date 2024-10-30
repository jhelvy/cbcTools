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

get_id_names <- function() {
    return(c("profileID", "blockID", "respID", "qID", "altID", "obsID"))
}

get_var_names <- function(design) {
    return(setdiff(names(design), get_id_names()))
}

set_block_ids <- function(design, n_blocks) {
    if (n_blocks > 1) {
        design$blockID <- rep(seq(n_blocks), each = nrow(design) / n_blocks)
    } else {
        design$blockID <- 1
    }
    return(design)
}

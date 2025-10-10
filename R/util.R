#' Display version number and date when the package is loaded.
#' @noRd
.onAttach <- function(libname, pkgname) {
    desc <- utils::packageDescription(pkgname, libname)
    packageStartupMessage(
        "Version:  ",
        desc$Version,
        "\n",
        "Author:   ",
        "John Paul Helveston (George Washington University)",
        "\n\n",
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
get_methods_optimal <- function() {
    return(c(
        "stochastic",
        "modfed",
        "cea"
    ))
}

get_methods_greedy <- function() {
    return(c(
        "shortcut",
        "minoverlap",
        "balanced"
    ))
}

get_methods_all <- function() {
    return(c(
        "random",
        get_methods_optimal(),
        get_methods_greedy()
    ))
}

set_num_cores <- function(n_cores) {
    cores_available <- parallel::detectCores()
    max_cores <- cores_available - 1
    # CRAN checks limits you to 2 cores
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && (chk != "false")) {
        return(2L)
    }
    if (is.null(n_cores)) {
        return(max_cores)
    } else if (!is.numeric(n_cores)) {
        warning(
            "Non-numeric value provided for n_cores...setting n_cores to ",
            max_cores
        )
        return(max_cores)
    } else if (n_cores > cores_available) {
        warning(
            "Cannot use ",
            n_cores,
            " cores because your machine only has ",
            cores_available,
            " available...setting n_cores to ",
            max_cores
        )
        return(max_cores)
    }
    return(n_cores)
}


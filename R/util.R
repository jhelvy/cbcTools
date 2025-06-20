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

validate_profiles <- function(profiles) {
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }
}

#' Validate that priors are compatible with profiles
validate_priors <- function(priors, profiles) {
    if (is.null(priors)) { return(TRUE) }
    if (!inherits(priors, "cbc_priors")) {
        stop("priors must be a cbc_priors object created by cbc_priors()")
    }
    if (!inherits(profiles, "cbc_profiles")) {
        stop("profiles must be a cbc_profiles object created by cbc_profiles()")
    }

    priors_meta <- priors$profiles_metadata
    current_hash <- digest_profiles(profiles)

    # Check if profiles structure has changed
    if (priors_meta$profile_hash != current_hash) {
        current_attr_info <- attr(profiles, "attribute_info")

        # More detailed comparison
        if (!identical(priors_meta$attribute_info, current_attr_info)) {
            warning(
                "Priors were created for different profile attributes or levels. ",
                "Consider recreating priors with cbc_priors().",
                call. = FALSE
            )
        } else if (priors_meta$n_profiles != nrow(profiles)) {
            message(
                "Priors were created for profiles with ", priors_meta$n_profiles,
                " rows, but current profiles have ", nrow(profiles), " rows. ",
                "This is typically fine if you've applied restrictions."
            )
        }
    }
}

# Helper function to create a simple hash of profiles structure
digest_profiles <- function(profiles) {
    # Create a simple hash based on attribute info and structure
    attr_info <- attr(profiles, "attribute_info")
    structure_string <- paste(
        names(attr_info),
        sapply(attr_info, function(x) paste(x$type, x$n_levels, collapse = "_")),
        collapse = "|"
    )
    # Use a simple hash - in production you might want digest::digest()
    abs(sum(utf8ToInt(structure_string)))
}

validate_method <- function(method) {
    valid_methods <- c("sequential", "random")
    if (!method %in% valid_methods) {
        stop("method must be one of: ", paste(valid_methods, collapse = ", "))
    }
}

validate_dominance_inputs <- function(
    dominance_types, dominance_threshold, max_dominance_attempts
) {
    # Validate dominance_types
    valid_types <- c("total", "partial")
    if (!all(dominance_types %in% valid_types)) {
        stop("dominance_types must be one or more of: ", paste(valid_types, collapse = ", "))
    }
    # Validate threshold
    if (dominance_threshold <= 0 || dominance_threshold >= 1) {
        stop("dominance_threshold must be between 0 and 1")
    }

    # Validate max replacements
    if (max_dominance_attempts < 1) {
        stop("max_dominance_attempts must be at least 1")
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

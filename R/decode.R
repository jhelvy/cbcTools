#' Convert dummy-coded CBC data back to categorical format
#'
#' This function converts dummy-coded CBC designs or choice data back to their original
#' categorical format. This is useful for displaying choice questions to
#' respondents or for analysis that requires categorical variables. Only
#' works for data without no-choice options, as no-choice data cannot
#' be meaningfully converted back to categorical format.
#'
#' @param data A `cbc_design` or `cbc_choices` object with dummy-coded categorical variables
#' @return The input object with categorical variables restored to their original format
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles with categorical variables
#' profiles <- cbc_profiles(
#'   price = c(10, 20, 30),
#'   quality = c("Low", "Medium", "High"),
#'   brand = c("A", "B")
#' )
#'
#' # Create design (will be dummy-coded by default)
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # Convert design back to categorical format
#' design_categorical <- cbc_decode(design)
#' head(design_categorical)
#'
#' # Also works with choice data
#' choices <- cbc_choices(design)
#' choices_categorical <- cbc_decode(choices)
#' head(choices_categorical)
cbc_decode <- function(data) {
    # Check input class
    if (!inherits(data, c("cbc_design", "cbc_choices"))) {
        stop(
            "Input must be a cbc_design or cbc_choices object created by cbc_design() or cbc_choices()"
        )
    }

    # Check if data has no-choice option
    if (inherits(data, "cbc_design")) {
        design_params <- attr(data, "design_params")
        has_no_choice <- !is.null(design_params) && design_params$no_choice
    } else if (inherits(data, "cbc_choices")) {
        # For choices, check if there's a no_choice column
        has_no_choice <- "no_choice" %in% names(data)
    }

    if (has_no_choice) {
        stop(
            "Cannot convert data with no-choice option back to categorical format.\n",
            "No-choice data must remain dummy-coded to maintain consistency."
        )
    }

    # Check if data is dummy-coded
    if (!is_dummy_coded(data)) {
        message("Data is already in categorical format.")
        return(data)
    }

    # Get categorical structure information
    categorical_structure <- attr(data, "categorical_structure")
    if (is.null(categorical_structure)) {
        warning(
            "No categorical structure information found. Data may already be decoded or created with an older version."
        )
        return(data)
    }

    # Apply decoding
    decoded_data <- decode_categorical_variables(
        data,
        categorical_structure
    )

    # Update attributes
    attr(decoded_data, "is_dummy_coded") <- FALSE

    # Copy over all other attributes
    data_attrs <- names(attributes(data))
    data_attrs <- setdiff(
        data_attrs,
        c("names", "class", "row.names", "is_dummy_coded")
    )

    for (attr_name in data_attrs) {
        attr(decoded_data, attr_name) <- attr(data, attr_name)
    }

    # Preserve original class
    class(decoded_data) <- class(data)
    return(decoded_data)
}

# Helper functions (same as before but work with both object types)

is_dummy_coded <- function(data) {
    is_coded <- attr(data, "is_dummy_coded")
    if (is.null(is_coded)) {
        # If no attribute, try to infer from column names
        # Look for column names that suggest dummy coding (e.g., "qualityHigh", "brandB")
        categorical_structure <- attr(data, "categorical_structure")
        if (!is.null(categorical_structure)) {
            categorical_attrs <- names(categorical_structure)[
                sapply(categorical_structure, function(x) x$is_categorical)
            ]

            # Check if any original categorical column names are missing
            missing_categoricals <- setdiff(categorical_attrs, names(data))
            if (length(missing_categoricals) > 0) {
                return(TRUE) # Likely dummy-coded if original categorical columns are missing
            }
        }
        return(FALSE)
    }
    return(is_coded)
}

# Decode categorical variables from dummy coding
decode_categorical_variables <- function(data, categorical_structure) {
    decoded_data <- data

    # Get categorical attributes
    categorical_attrs <- names(categorical_structure)[
        sapply(categorical_structure, function(x) x$is_categorical)
    ]

    for (attr in categorical_attrs) {
        levels_info <- categorical_structure[[attr]]
        levels_order <- levels_info$levels
        reference_level <- levels_info$reference_level
        non_ref_levels <- setdiff(levels_order, reference_level)

        # Find dummy columns for this attribute
        dummy_cols <- paste0(attr, non_ref_levels)
        existing_dummy_cols <- intersect(dummy_cols, names(data))

        if (length(existing_dummy_cols) > 0) {
            # Reconstruct categorical variable
            n_rows <- nrow(data)
            categorical_values <- rep(reference_level, n_rows)

            # Set non-reference levels based on dummy variables
            for (dummy_col in existing_dummy_cols) {
                level_name <- gsub(paste0("^", attr), "", dummy_col)
                is_this_level <- data[[dummy_col]] == 1
                categorical_values[is_this_level] <- level_name
            }

            # Add reconstructed categorical variable
            decoded_data[[attr]] <- factor(
                categorical_values,
                levels = levels_order
            )

            # Remove dummy columns
            decoded_data[existing_dummy_cols] <- NULL
        }
    }

    return(decoded_data)
}

#' Encode categorical variables in a CBC design
#'
#' This function converts categorical variables between different coding schemes.
#' Standard coding keeps categorical variables as-is (factor or character).
#' Dummy coding uses a reference category (all zeros) with indicator variables
#' for other levels. Effects coding uses -1 for the reference category to ensure
#' coefficients sum to zero.
#'
#' @param data A `cbc_design` or `cbc_choices` object
#' @param coding Character. Type of encoding: "dummy" (default), "standard", or "effects"
#' @return The input object with specified encoding applied
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
#' # Create design (defaults to standard coding)
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # Convert to dummy coding
#' design_dummy <- cbc_encode(design, "dummy")
#' head(design_dummy)
#'
#' # Convert to effects coding
#' design_effects <- cbc_encode(design, "effects")
#' head(design_effects)
#'
#' # Convert back to standard
#' design_standard <- cbc_encode(design_effects, "standard")
#' head(design_standard)
cbc_encode <- function(data, coding = "dummy") {
    # Check input class
    if (!inherits(data, c("cbc_design", "cbc_choices"))) {
        stop(
            "Input must be a cbc_design or cbc_choices object created by cbc_design() or cbc_choices()"
        )
    }

    # Get current encoding
    current_coding <- attr(data, "encoding") %||% "standard"

    if (current_coding == coding) {
        message("Data is already in '", coding, "' encoding")
        return(data)
    }

    # Get categorical structure
    categorical_structure <- attr(data, "categorical_structure")
    if (is.null(categorical_structure)) {
        warning(
            "No categorical structure information found. Data may not have any categorical variables or was created with an older version."
        )
        return(data)
    }

    # First convert to standard if not already
    if (current_coding != "standard") {
        data <- decode_to_standard(data, categorical_structure)
    }

    # Then encode to target
    if (coding == "dummy") {
        data <- encode_dummy(data, categorical_structure)
    } else if (coding == "effects") {
        data <- encode_effects(data, categorical_structure)
    }

    # Update encoding attribute
    attr(data, "encoding") <- coding

    # Copy over all other attributes
    return(data)
}

# Helper function: decode any encoding back to standard
decode_to_standard <- function(data, categorical_structure) {
    decoded_data <- data

    # Check if this is a no-choice design
    has_no_choice <- "no_choice" %in% names(data)
    if (has_no_choice) {
        no_choice_rows <- data$no_choice == 1
    }

    # Get categorical attributes
    categorical_attrs <- names(categorical_structure)[
        sapply(categorical_structure, function(x) x$is_categorical)
    ]

    for (attr in categorical_attrs) {
        levels_info <- categorical_structure[[attr]]
        levels_order <- levels_info$levels
        reference_level <- levels_info$reference_level
        non_ref_levels <- setdiff(levels_order, reference_level)

        # Find coded columns for this attribute
        if (attr(data, "encoding") == "dummy") {
            coded_cols <- paste0(attr, non_ref_levels)
        } else if (attr(data, "encoding") == "effects") {
            coded_cols <- paste0(attr, non_ref_levels)
        }

        existing_coded_cols <- intersect(coded_cols, names(data))

        if (length(existing_coded_cols) > 0) {
            # Reconstruct categorical variable
            n_rows <- nrow(data)
            categorical_values <- rep(reference_level, n_rows)

            # Set non-reference levels based on coded variables
            for (coded_col in existing_coded_cols) {
                level_name <- gsub(paste0("^", attr), "", coded_col)
                is_this_level <- data[[coded_col]] == 1
                categorical_values[is_this_level] <- level_name
            }

            # Add reconstructed categorical variable
            decoded_data[[attr]] <- factor(
                categorical_values,
                levels = levels_order
            )

            # Set NA for no-choice rows
            if (has_no_choice) {
                decoded_data[[attr]][no_choice_rows] <- NA
            }

            # Remove coded columns
            decoded_data[existing_coded_cols] <- NULL
        }
    }

    # Set continuous variables back to NA for no-choice rows
    if (has_no_choice) {
        continuous_attrs <- names(categorical_structure)[
            sapply(categorical_structure, function(x) !x$is_categorical)
        ]

        for (attr in continuous_attrs) {
            if (attr %in% names(decoded_data)) {
                decoded_data[[attr]][no_choice_rows] <- NA
            }
        }
    }

    return(decoded_data)
}

# Helper function: encode to dummy coding
encode_dummy <- function(data, categorical_structure) {
    encoded_data <- data

    # Check if this is a no-choice design
    has_no_choice <- "no_choice" %in% names(data)
    if (has_no_choice) {
        no_choice_rows <- data$no_choice == 1
    }

    # Get categorical attributes
    categorical_attrs <- names(categorical_structure)[
        sapply(categorical_structure, function(x) x$is_categorical)
    ]

    for (attr in categorical_attrs) {
        if (!attr %in% names(data)) {
            next
        }

        levels_info <- categorical_structure[[attr]]
        levels_order <- levels_info$levels
        reference_level <- levels_info$reference_level
        non_ref_levels <- setdiff(levels_order, reference_level)

        # Create dummy variables
        for (level in non_ref_levels) {
            dummy_col_name <- paste0(attr, level)
            encoded_data[[dummy_col_name]] <- as.integer(data[[attr]] == level)

            # Convert NA to 0 for no-choice rows
            if (has_no_choice) {
                encoded_data[[dummy_col_name]][is.na(encoded_data[[
                    dummy_col_name
                ]])] <- 0
            }
        }

        # Remove original categorical variable
        encoded_data[[attr]] <- NULL
    }

    # Convert NA to 0 for continuous variables in no-choice rows
    if (has_no_choice) {
        continuous_attrs <- names(categorical_structure)[
            sapply(categorical_structure, function(x) !x$is_categorical)
        ]

        for (attr in continuous_attrs) {
            if (attr %in% names(encoded_data)) {
                encoded_data[[attr]][no_choice_rows] <- 0
            }
        }
    }

    return(encoded_data)
}

# Helper function: encode to effects coding
encode_effects <- function(data, categorical_structure) {
    encoded_data <- data

    # Check if this is a no-choice design
    has_no_choice <- "no_choice" %in% names(data)
    if (has_no_choice) {
        no_choice_rows <- data$no_choice == 1
    }

    # Get categorical attributes
    categorical_attrs <- names(categorical_structure)[
        sapply(categorical_structure, function(x) x$is_categorical)
    ]

    for (attr in categorical_attrs) {
        if (!attr %in% names(data)) {
            next
        }

        levels_info <- categorical_structure[[attr]]
        levels_order <- levels_info$levels
        reference_level <- levels_info$reference_level
        non_ref_levels <- setdiff(levels_order, reference_level)

        # Create effects-coded variables
        for (level in non_ref_levels) {
            effects_col_name <- paste0(attr, level)
            # 1 for this level, -1 for reference, 0 for others
            encoded_data[[effects_col_name]] <- ifelse(
                data[[attr]] == level,
                1,
                ifelse(data[[attr]] == reference_level, -1, 0)
            )

            # Convert NA to 0 for no-choice rows
            if (has_no_choice) {
                encoded_data[[effects_col_name]][is.na(encoded_data[[
                    effects_col_name
                ]])] <- 0
            }
        }

        # Remove original categorical variable
        encoded_data[[attr]] <- NULL
    }

    # Convert NA to 0 for continuous variables in no-choice rows
    if (has_no_choice) {
        continuous_attrs <- names(categorical_structure)[
            sapply(categorical_structure, function(x) !x$is_categorical)
        ]

        for (attr in continuous_attrs) {
            if (attr %in% names(encoded_data)) {
                encoded_data[[attr]][no_choice_rows] <- 0
            }
        }
    }

    return(encoded_data)
}

# Internal function to get encoding for metrics calculation
get_standard_encoding <- function(data) {
    current_encoding <- attr(data, "encoding") %||% "standard"

    if (current_encoding == "standard") {
        return(data)
    }

    # Convert to standard for metrics
    categorical_structure <- attr(data, "categorical_structure")
    if (is.null(categorical_structure)) {
        return(data)
    }

    return(decode_to_standard(data, categorical_structure))
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

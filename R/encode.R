#' Encode categorical variables in a CBC design
#'
#' This function converts categorical variables between different coding schemes.
#' Standard coding keeps categorical variables as-is (factor or character).
#' Dummy coding uses a reference category (all zeros) with indicator variables
#' for other levels. Effects coding uses -1 for the reference category to ensure
#' coefficients sum to zero.
#'
#' @param data A `cbc_design` or `cbc_choices` object
#' @param coding Character. Type of encoding: "standard", "dummy", or "effects".
#'   If NULL and ref_levels is NULL, data is returned unchanged. If NULL and
#'   ref_levels is specified, the current encoding is maintained.
#' @param ref_levels Named list specifying reference levels for categorical variables.
#'   For example: `list(powertrain = "Gasoline", brand = "A")`. If NULL (default),
#'   uses the first level of each categorical variable as reference.
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
#' design_dummy <- cbc_encode(design, coding = "dummy")
#' head(design_dummy)
#'
#' # Convert to effects coding
#' design_effects <- cbc_encode(design, coding = "effects")
#' head(design_effects)
#'
#' # Convert back to standard
#' design_standard <- cbc_encode(design_dummy, coding = "standard")
#' head(design_standard)
#'
#' # Custom reference levels with dummy coding
#' design_dummy2 <- cbc_encode(
#'   design,
#'   coding = "dummy",
#'   ref_levels = list(quality = "Medium", brand = "B")
#' )
#' head(design_dummy2)
#'
#' # Update reference levels without changing encoding
#' design_updated <- cbc_encode(
#'   design_dummy,
#'   ref_levels = list(quality = "High")
#' )
#' head(design_updated)
cbc_encode <- function(data, coding = NULL, ref_levels = NULL) {
    # Validate and potentially reconstruct design object
    data <- validate_or_reconstruct(
        data,
        require_class = FALSE,
        allow_choices = TRUE,
        context = "cbc_encode()"
    )

    # Get current encoding
    current_coding <- attr(data, "encoding") %||% "standard"

    # If no coding or ref_levels specified, return with message
    if (is.null(coding) && is.null(ref_levels)) {
        message(
            "No encoding or reference levels specified. Data returned unchanged."
        )
        return(data)
    }

    # If only ref_levels specified, keep current encoding
    if (is.null(coding)) {
        coding <- current_coding
    }

    # Validate coding argument
    valid_codings <- c("standard", "dummy", "effects")
    if (!coding %in% valid_codings) {
        stop(
            "coding must be one of: ",
            paste0("'", valid_codings, "'", collapse = ", "),
            call. = FALSE
        )
    }

    # Check if already in target coding and no ref_levels to update
    if (current_coding == coding && is.null(ref_levels)) {
        message("Data is already in '", coding, "' encoding")
        return(data)
    }

    # Get categorical structure (should be present after validate_or_reconstruct)
    categorical_structure <- attr(data, "categorical_structure")
    if (is.null(categorical_structure)) {
        # Try to infer it one more time based on current encoding
        categorical_structure <- infer_categorical_structure(
            data,
            current_coding
        )

        if (
            is.null(categorical_structure) || length(categorical_structure) == 0
        ) {
            warning(
                "No categorical structure information found. Data may not have any categorical variables.",
                call. = FALSE
            )
            return(data)
        }

        # Store the inferred structure
        attr(data, "categorical_structure") <- categorical_structure
    }

    # First convert to standard if not already
    if (current_coding != "standard") {
        data <- decode_to_standard(data, categorical_structure)
    }

    # Update reference levels if specified
    if (!is.null(ref_levels)) {
        result <- update_reference_levels(
            data,
            categorical_structure,
            ref_levels
        )
        data <- result$data
        categorical_structure <- result$categorical_structure
    }

    # Then encode to target
    if (coding == "dummy") {
        data <- encode_dummy(data, categorical_structure)
    } else if (coding == "effects") {
        data <- encode_effects(data, categorical_structure)
    }

    # Update encoding attribute and categorical structure
    attr(data, "encoding") <- coding
    attr(data, "categorical_structure") <- categorical_structure

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

# Helper function to update reference levels
update_reference_levels <- function(data, categorical_structure, ref_levels) {
    # Validate ref_levels input
    if (!is.list(ref_levels)) {
        stop("ref_levels must be a named list", call. = FALSE)
    }

    if (is.null(names(ref_levels)) || any(names(ref_levels) == "")) {
        stop(
            "ref_levels must be a named list (e.g., list(powertrain = 'Gasoline'))",
            call. = FALSE
        )
    }

    # Get categorical attributes
    categorical_attrs <- names(categorical_structure)[
        sapply(categorical_structure, function(x) x$is_categorical)
    ]

    # Check that specified attributes exist in categorical structure
    invalid_attrs <- setdiff(names(ref_levels), categorical_attrs)
    if (length(invalid_attrs) > 0) {
        stop(
            sprintf(
                "Attribute '%s' not found in categorical structure",
                invalid_attrs[1]
            ),
            call. = FALSE
        )
    }

    # Update each specified attribute
    for (attr in names(ref_levels)) {
        new_ref <- ref_levels[[attr]]

        # Validate that new reference level exists
        all_levels <- categorical_structure[[attr]]$levels
        if (!new_ref %in% all_levels) {
            stop(
                sprintf(
                    "Level '%s' not found in attribute '%s'",
                    new_ref,
                    attr
                ),
                call. = FALSE
            )
        }

        # Reorder factor levels to put new reference first
        current_levels <- all_levels
        new_levels <- c(new_ref, setdiff(current_levels, new_ref))

        # Update the factor in the data
        data[[attr]] <- factor(data[[attr]], levels = new_levels)

        # Update the categorical structure
        categorical_structure[[attr]]$levels <- new_levels
        categorical_structure[[attr]]$reference_level <- new_ref
    }

    return(list(
        data = data,
        categorical_structure = categorical_structure
    ))
}

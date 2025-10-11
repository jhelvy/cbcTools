# Helper functions to validate and reconstruct design objects

#' Validate and potentially reconstruct a CBC design object
#'
#' This internal function checks if data is a valid CBC design/choices object
#' or can be treated as one. If the data has lost its class but has the right
#' structure (ID columns), it attempts to reconstruct minimal attributes.
#'
#' @param data Input data (cbc_design, cbc_choices, or data.frame)
#' @param require_class Logical. If TRUE, strictly requires proper class. If FALSE,
#'   allows data.frames with correct structure.
#' @param allow_choices Logical. If TRUE, allows cbc_choices objects. Default TRUE.
#' @return A list with: valid (logical), data (potentially reconstructed),
#'   message (character or NULL)
#' @keywords internal
validate_design_object <- function(data, require_class = FALSE, allow_choices = TRUE) {

    # Check if it's already the right class
    valid_classes <- if (allow_choices) {
        c("cbc_design", "cbc_choices")
    } else {
        "cbc_design"
    }

    if (inherits(data, valid_classes)) {
        return(list(
            valid = TRUE,
            data = data,
            message = NULL
        ))
    }

    # If require_class is TRUE, fail here
    if (require_class) {
        return(list(
            valid = FALSE,
            data = data,
            message = sprintf(
                "Input must be a %s object",
                paste(valid_classes, collapse = " or ")
            )
        ))
    }

    # Try to treat as a data frame with proper structure
    if (!is.data.frame(data)) {
        return(list(
            valid = FALSE,
            data = data,
            message = "Input must be a data.frame or cbc_design/cbc_choices object"
        ))
    }

    # Check for required ID columns
    required_ids <- c("profileID", "qID", "altID", "obsID")
    has_resp_id <- "respID" %in% names(data)
    has_block_id <- "blockID" %in% names(data)

    missing_ids <- setdiff(required_ids, names(data))

    if (length(missing_ids) > 0) {
        return(list(
            valid = FALSE,
            data = data,
            message = sprintf(
                "Missing required ID columns: %s",
                paste(missing_ids, collapse = ", ")
            )
        ))
    }

    # Check for at least one respondent/block ID
    if (!has_resp_id && !has_block_id) {
        return(list(
            valid = FALSE,
            data = data,
            message = "Must have either 'respID' or 'blockID' column"
        ))
    }

    # Data has valid structure - try to reconstruct minimal attributes
    reconstructed <- reconstruct_design_attributes(data)

    return(list(
        valid = TRUE,
        data = reconstructed,
        message = "Data structure is valid but missing class attributes. Reconstructed minimal attributes."
    ))
}

#' Reconstruct minimal design attributes from a data frame
#'
#' @param data A data.frame with valid CBC design structure
#' @return The data with reconstructed attributes
#' @keywords internal
reconstruct_design_attributes <- function(data) {

    # Determine if this is a design or choices object
    has_choice <- "choice" %in% names(data)
    has_no_choice <- "no_choice" %in% names(data)

    # Infer encoding from column structure
    encoding <- infer_encoding(data)

    # Reconstruct categorical structure if possible
    categorical_structure <- infer_categorical_structure(data, encoding)

    # Set attributes
    attr(data, "encoding") <- encoding
    attr(data, "categorical_structure") <- categorical_structure

    # Set appropriate class
    if (has_choice) {
        class(data) <- c("cbc_choices", "data.frame")
    } else {
        class(data) <- c("cbc_design", "data.frame")
    }

    return(data)
}

#' Infer encoding type from column names and values
#'
#' @param data A data.frame
#' @return Character: "standard", "dummy", or "effects"
#' @keywords internal
infer_encoding <- function(data) {
    # Get non-ID columns
    id_cols <- c("profileID", "respID", "blockID", "qID", "altID", "obsID",
                 "choice", "no_choice", "prob")
    attr_cols <- setdiff(names(data), id_cols)

    if (length(attr_cols) == 0) {
        return("standard")
    }

    # Check for dummy/effects coding patterns
    # Look for columns like "attributeLevel" that are ALSO binary (0/1)
    # This distinguishes dummy variables from regular camelCase names
    has_compound_names <- any(grepl("[a-z][A-Z]", attr_cols)) ||
                          any(grepl("_[A-Z]", attr_cols))

    if (has_compound_names) {
        # Check if any compound-named columns are binary (0/1)
        # This is the key indicator of dummy/effects coding
        compound_cols <- attr_cols[grepl("[a-z][A-Z]", attr_cols) |
                                   grepl("_[A-Z]", attr_cols)]

        has_binary_compound <- any(sapply(compound_cols, function(col) {
            if (!is.numeric(data[[col]])) return(FALSE)
            unique_vals <- unique(data[[col]][!is.na(data[[col]])])
            all(unique_vals %in% c(0, 1, -1))
        }))

        if (!has_binary_compound) {
            # Compound names but not binary - likely just camelCase variables
            return("standard")
        }

        # Check if any columns have negative values (effects coding)
        has_negatives <- any(sapply(compound_cols, function(col) {
            if (is.numeric(data[[col]])) {
                any(data[[col]] < 0, na.rm = TRUE)
            } else {
                FALSE
            }
        }))

        if (has_negatives) {
            return("effects")
        } else {
            return("dummy")
        }
    }

    return("standard")
}

#' Infer categorical structure from data
#'
#' @param data A data.frame
#' @param encoding Current encoding type
#' @return A list of categorical structure information
#' @keywords internal
infer_categorical_structure <- function(data, encoding) {
    # Get non-ID columns
    id_cols <- c("profileID", "respID", "blockID", "qID", "altID", "obsID",
                 "choice", "no_choice", "prob")
    attr_cols <- setdiff(names(data), id_cols)

    if (length(attr_cols) == 0) {
        return(NULL)
    }

    categorical_structure <- list()

    if (encoding == "standard") {
        # Infer from actual data types
        for (col in attr_cols) {
            if (is.factor(data[[col]]) || is.character(data[[col]])) {
                levels_order <- if (is.factor(data[[col]])) {
                    levels(data[[col]])
                } else {
                    unique(data[[col]][!is.na(data[[col]])])
                }

                categorical_structure[[col]] <- list(
                    is_categorical = TRUE,
                    levels = levels_order,
                    reference_level = levels_order[1]
                )
            } else if (is.numeric(data[[col]])) {
                categorical_structure[[col]] <- list(
                    is_categorical = FALSE
                )
            }
        }
    } else {
        # For dummy/effects coding, try to infer original attributes
        # This is more complex - group columns by common prefixes
        categorical_structure <- infer_from_coded_columns(data, attr_cols, encoding)
    }

    return(categorical_structure)
}

#' Infer categorical structure from dummy/effects coded columns
#'
#' @param data A data.frame
#' @param attr_cols Column names to analyze
#' @param encoding Encoding type
#' @return A list of categorical structure
#' @keywords internal
infer_from_coded_columns <- function(data, attr_cols, encoding) {
    categorical_structure <- list()

    # Find columns that look like dummy/effects variables
    # Pattern: starts with lowercase, then has uppercase (attributeLevel)
    # But only if they are binary (0/1 values only, excluding NA)
    potential_dummies <- attr_cols[sapply(attr_cols, function(col) {
        has_camel_case <- grepl("^[a-z]+[A-Z]", col)
        if (!has_camel_case) return(FALSE)

        # Check if values are binary (0/1 only)
        if (!is.numeric(data[[col]])) return(FALSE)

        unique_vals <- unique(data[[col]][!is.na(data[[col]])])
        all(unique_vals %in% c(0, 1))
    })]

    if (length(potential_dummies) == 0) {
        # All columns are continuous
        for (col in attr_cols) {
            categorical_structure[[col]] <- list(is_categorical = FALSE)
        }
        return(categorical_structure)
    }

    # Group by attribute (everything before first capital letter)
    get_attr_name <- function(col) {
        gsub("([a-z]+)([A-Z].*)", "\\1", col)
    }

    get_level_name <- function(col) {
        gsub("([a-z]+)([A-Z].*)", "\\2", col)
    }

    attr_groups <- split(potential_dummies, sapply(potential_dummies, get_attr_name))

    # For each attribute group, reconstruct structure
    for (attr in names(attr_groups)) {
        cols <- attr_groups[[attr]]
        levels <- sapply(cols, get_level_name)

        # Infer reference level (the missing one)
        # For now, we can't reliably infer this without more context
        categorical_structure[[attr]] <- list(
            is_categorical = TRUE,
            levels = c("Reference", levels),
            reference_level = "Reference"
        )
    }

    # Add any remaining columns as continuous
    continuous_cols <- setdiff(attr_cols, potential_dummies)
    for (col in continuous_cols) {
        categorical_structure[[col]] <- list(is_categorical = FALSE)
    }

    return(categorical_structure)
}

#' User-friendly wrapper to validate design with error messages
#'
#' @param data Input data
#' @param require_class Whether to require proper class
#' @param allow_choices Whether to allow cbc_choices objects
#' @param context Character describing where this is being called from (for error messages)
#' @return Validated/reconstructed data, or stops with error
#' @keywords internal
validate_or_reconstruct <- function(data,
                                    require_class = FALSE,
                                    allow_choices = TRUE,
                                    context = "this function") {

    result <- validate_design_object(data, require_class, allow_choices)

    if (!result$valid) {
        stop(result$message, call. = FALSE)
    }

    if (!is.null(result$message)) {
        message(result$message)
    }

    return(result$data)
}
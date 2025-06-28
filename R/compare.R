#' Compare multiple choice experiment designs
#'
#' This function compares multiple CBC designs across key quality metrics
#' including D-error, balance, overlap, and structural characteristics.
#' Useful for evaluating different design methods or parameter settings.
#'
#' @param ... Any number of `cbc_design` objects to compare, separated by commas.
#'   Can be named for clearer output (e.g., `random = design1, sequential = design2`).
#' @param metrics Character vector specifying which metrics to compare.
#'   Options: "structure", "efficiency", "balance", "overlap", or "all" (default).
#'   Can specify multiple: `c("efficiency", "balance")`
#' @param sort_by Character. Metric to sort designs by. Options: "d_error" (default),
#'   "balance", "overlap", "profiles_used", "generation_time", or "none"
#' @param ascending Logical. If TRUE, sort in ascending order (lower is better).
#'   If FALSE, sort in descending order (higher is better). Default depends on metric.
#' @return A `cbc_comparison` object containing comparison results, printed in
#'   a formatted table.
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C"),
#'   quality = c("Low", "High")
#' )
#'
#' # Create different designs
#' design_random <- cbc_design(
#'   profiles = profiles,
#'   method = "random",
#'   n_alts = 2, n_q = 4
#' )
#'
#' design_sequential <- cbc_design(
#'   profiles = profiles,
#'   method = "sequential",
#'   n_alts = 2, n_q = 4
#' )
#'
#' # Compare designs
#' cbc_compare(design_random, design_sequential)
#'
#' # Named comparison with specific metrics
#' cbc_compare(
#'   Random = design_random,
#'   Sequential = design_sequential,
#'   metrics = c("efficiency", "balance"),
#'   sort_by = "d_error"
#' )
cbc_compare <- function(..., metrics = "all", sort_by = "d_error", ascending = NULL) {

    # Get designs and their names
    designs <- list(...)
    design_names <- get_design_names(designs, match.call())

    # Validate inputs
    validate_compare_inputs(designs, metrics, sort_by)

    # Handle "all" metrics
    if ("all" %in% metrics) {
        metrics <- c("structure", "efficiency", "balance", "overlap")
    }

    # Set default sort order based on metric
    if (is.null(ascending)) {
        ascending <- get_default_sort_order(sort_by)
    }

    # Extract comparison data
    comparison_data <- extract_comparison_data(designs, design_names, metrics)

    # Sort if requested
    if (sort_by != "none") {
        comparison_data <- sort_comparison_data(comparison_data, sort_by, ascending)
    }

    # Create comparison object
    result <- list(
        data = comparison_data,
        metrics_compared = metrics,
        sort_by = sort_by,
        ascending = ascending,
        n_designs = length(designs),
        comparison_time = Sys.time()
    )

    class(result) <- c("cbc_comparison", "list")
    return(result)
}

# Helper function to get design names
get_design_names <- function(designs, call) {
    # Get names from the call if provided
    call_names <- names(call)[-1]  # Remove function name

    if (!is.null(call_names) && any(call_names != "")) {
        # Use provided names, fill in missing ones
        design_names <- call_names
        unnamed_indices <- which(design_names == "" | is.na(design_names))
        design_names[unnamed_indices] <- paste("Design", unnamed_indices)
    } else {
        # Create default names
        design_names <- paste("Design", seq_along(designs))
    }

    return(design_names)
}

# Validate comparison inputs
validate_compare_inputs <- function(designs, metrics, sort_by) {
    # Check that all inputs are cbc_design objects
    for (i in seq_along(designs)) {
        if (!inherits(designs[[i]], "cbc_design")) {
            stop(sprintf("Argument %d is not a cbc_design object", i))
        }
    }

    # Check minimum number of designs
    if (length(designs) < 2) {
        stop("At least 2 designs are required for comparison")
    }

    # Validate metrics
    valid_metrics <- c("structure", "efficiency", "balance", "overlap", "all")
    invalid_metrics <- setdiff(metrics, valid_metrics)
    if (length(invalid_metrics) > 0) {
        stop("Invalid metrics: ", paste(invalid_metrics, collapse = ", "),
             ". Valid options are: ", paste(valid_metrics, collapse = ", "))
    }

    # Validate sort_by
    valid_sort <- c("d_error", "balance", "overlap", "profiles_used",
                    "generation_time", "none")
    if (!sort_by %in% valid_sort) {
        stop("Invalid sort_by: ", sort_by,
             ". Valid options are: ", paste(valid_sort, collapse = ", "))
    }
}

# Get default sort order for different metrics
get_default_sort_order <- function(sort_by) {
    # Lower is better for these metrics
    lower_better <- c("d_error", "overlap", "generation_time")
    # Higher is better for these metrics
    higher_better <- c("balance", "profiles_used")

    if (sort_by %in% lower_better) {
        return(TRUE)  # ascending
    } else if (sort_by %in% higher_better) {
        return(FALSE)  # descending
    } else {
        return(TRUE)  # default to ascending
    }
}

# Extract comparison data from all designs
extract_comparison_data <- function(designs, design_names, metrics) {
    comparison_rows <- list()

    for (i in seq_along(designs)) {
        design <- designs[[i]]
        name <- design_names[i]

        # Extract data for this design
        row_data <- list(design_name = name)

        if ("structure" %in% metrics) {
            row_data <- c(row_data, extract_structure_metrics(design))
        }

        if ("efficiency" %in% metrics) {
            row_data <- c(row_data, extract_efficiency_metrics(design))
        }

        if ("balance" %in% metrics) {
            row_data <- c(row_data, extract_balance_metrics(design))
        }

        if ("overlap" %in% metrics) {
            row_data <- c(row_data, extract_overlap_metrics(design))
        }

        comparison_rows[[i]] <- row_data
    }

    # Convert to data frame
    comparison_df <- do.call(rbind, lapply(comparison_rows, function(x) {
        data.frame(x, stringsAsFactors = FALSE)
    }))

    return(comparison_df)
}

# Extract structural metrics
extract_structure_metrics <- function(design) {
    params <- attr(design, "design_params")
    summary_info <- attr(design, "design_summary")

    list(
        method = params$method,
        respondents = params$n_resp,
        questions = params$n_q,
        alternatives = params$n_alts,
        blocks = params$n_blocks,
        profiles_used = summary_info$n_profiles_used,
        profiles_available = summary_info$n_profiles_available,
        profile_usage_pct = round(summary_info$profile_usage_rate * 100, 1),
        generation_time = round(params$time_elapsed_sec, 3),
        no_choice = params$no_choice,
        labeled = !is.null(params$label)
    )
}

# Extract efficiency metrics
extract_efficiency_metrics <- function(design) {
    params <- attr(design, "design_params")

    # Initialize with NA values
    result <- list(
        d_error_null = NA,
        d_error_prior = NA
    )

    # Fill in available D-errors
    if (!is.null(params$d_error_null)) {
        result$d_error_null <- round(params$d_error_null, 6)
    }

    if (!is.null(params$d_error_prior)) {
        result$d_error_prior <- round(params$d_error_prior, 6)
    }

    return(result)
}

# Extract balance metrics
extract_balance_metrics <- function(design) {
    summary_info <- attr(design, "design_summary")

    if (!is.null(summary_info$efficiency$balance_score)) {
        balance_score <- round(summary_info$efficiency$balance_score, 3)
    } else {
        # Compute on the fly if not available
        balance_result <- compute_balance_metrics_internal(design)
        balance_score <- round(balance_result$overall_balance, 3)
    }

    list(balance_score = balance_score)
}

# Extract overlap metrics
extract_overlap_metrics <- function(design) {
    summary_info <- attr(design, "design_summary")

    if (!is.null(summary_info$efficiency$overlap_score)) {
        overlap_score <- round(summary_info$efficiency$overlap_score, 3)
    } else {
        # Compute on the fly if not available
        overlap_result <- compute_overlap_metrics_internal(design)
        overlap_score <- round(overlap_result$overall_overlap, 3)
    }

    list(overlap_score = overlap_score)
}

# Sort comparison data
sort_comparison_data <- function(comparison_data, sort_by, ascending) {
    # Handle different column names for d_error
    if (sort_by == "d_error") {
        # Choose the best available D-error metric
        if ("d_error_prior" %in% names(comparison_data) &&
            any(!is.na(comparison_data$d_error_prior))) {
            sort_col <- "d_error_prior"
        } else if ("d_error_null" %in% names(comparison_data)) {
            sort_col <- "d_error_null"
        } else {
            warning("No D-error metrics available for sorting")
            return(comparison_data)
        }
    } else {
        # Map sort_by to actual column names
        sort_col <- switch(sort_by,
                           "balance" = "balance_score",
                           "overlap" = "overlap_score",
                           "profiles_used" = "profiles_used",
                           "generation_time" = "generation_time",
                           sort_by
        )
    }

    if (!sort_col %in% names(comparison_data)) {
        warning("Sort column '", sort_col, "' not found in comparison data")
        return(comparison_data)
    }

    # Sort the data
    order_indices <- order(comparison_data[[sort_col]], decreasing = !ascending, na.last = TRUE)
    comparison_data[order_indices, ]
}

#' Print method for cbc_comparison objects
#' @param x A cbc_comparison object
#' @param ... Additional arguments passed to print
#' @export
print.cbc_comparison <- function(x, ...) {
    # Header
    cat("CBC Design Comparison\n")
    cat("=====================\n")
    cat(sprintf("Designs compared: %d\n", x$n_designs))
    cat(sprintf("Metrics: %s\n", paste(x$metrics_compared, collapse = ", ")))

    if (x$sort_by != "none") {
        order_desc <- if (x$ascending) "ascending" else "descending"
        cat(sprintf("Sorted by: %s (%s)\n", x$sort_by, order_desc))
    }

    # Structure section
    if ("structure" %in% x$metrics_compared) {
        cat("\nStructure\n")
        cat("=====================\n")
        print_structure_tables(x$data)
    }

    # Design Metrics section
    if (any(c("efficiency", "balance", "overlap") %in% x$metrics_compared)) {
        cat("\nDesign Metrics\n")
        cat("=====================\n")
        print_metrics_table(x$data, x$metrics_compared)
    }

    # Interpretation
    cat("\nInterpretation:\n")
    print_interpretation(x$metrics_compared)

    # Best performers
    cat("\nBest performers:\n")
    print_best_performers(x$data)

    cat("\nUse summary() for detailed information on any one design.\n")

    invisible(x)
}

# Print structure tables
print_structure_tables <- function(data) {
    # First table: Design, Method, respondents, questions
    table1 <- data[, c("design_name", "method", "respondents", "questions")]
    names(table1) <- c("Design", "Method", "respondents", "questions")
    print(table1, row.names = FALSE)

    # Second table: Alternatives, Blocks, Profile Usage
    profile_usage_formatted <- sprintf("(%d/%d) %s%%",
                                       data$profiles_used,
                                       data$profiles_available,
                                       data$profile_usage_pct)

    table2 <- data.frame(
        Alternatives = data$alternatives,
        Blocks = data$blocks,
        `Profile Usage` = profile_usage_formatted,
        check.names = FALSE
    )
    print(table2, row.names = FALSE)

    # Third table: No Choice, Labeled?
    table3 <- data.frame(
        `No Choice` = ifelse(data$no_choice, "Yes", "No"),
        `Labeled?` = ifelse(data$labeled, "Yes", "No"),
        check.names = FALSE
    )
    print(table3, row.names = FALSE)
}

# Print metrics table
print_metrics_table <- function(data, metrics) {
    # Start with design and method
    metrics_data <- data[, c("design_name", "method")]
    names(metrics_data) <- c("Design", "Method")

    # Add D-Error columns if efficiency is included
    if ("efficiency" %in% metrics) {
        d_error_null_formatted <- ifelse(is.na(data$d_error_null), "NA", sprintf("%.6f", data$d_error_null))
        d_error_prior_formatted <- ifelse(is.na(data$d_error_prior), "NA", sprintf("%.6f", data$d_error_prior))

        metrics_data$`D-Error (Null)` <- d_error_null_formatted
        metrics_data$`D-Error (Prior)` <- d_error_prior_formatted
    }

    # Add Balance if included
    if ("balance" %in% metrics) {
        balance_formatted <- ifelse(is.na(data$balance_score), "NA", sprintf("%.3f", data$balance_score))
        metrics_data$Balance <- balance_formatted
    }

    # Add Overlap if included
    if ("overlap" %in% metrics) {
        overlap_formatted <- ifelse(is.na(data$overlap_score), "NA", sprintf("%.3f", data$overlap_score))
        metrics_data$Overlap <- overlap_formatted
    }

    print(metrics_data, row.names = FALSE)
}

# Print interpretation of metrics
print_interpretation <- function(metrics) {
    interpretations <- c()

    if ("efficiency" %in% metrics) {
        interpretations <- c(interpretations, "- D-Error: Lower is better (design efficiency)")
    }

    if ("balance" %in% metrics) {
        interpretations <- c(interpretations, "- Balance: Higher is better (level distribution)")
    }

    if ("overlap" %in% metrics) {
        interpretations <- c(interpretations, "- Overlap: Lower is better (attribute variation)")
    }

    if ("structure" %in% metrics) {
        interpretations <- c(interpretations, "- Profile Usage: Higher means more profiles used")
    }

    if (length(interpretations) > 0) {
        cat(paste(interpretations, collapse = "\n"))
        cat("\n")
    }
}

# Print best performers for each metric
print_best_performers <- function(data) {
    performers <- c()

    # D-error best performer
    if ("d_error_prior" %in% names(data)) {
        d_errors <- data$d_error_prior[!is.na(data$d_error_prior)]
        if (length(d_errors) > 0) {
            best_idx <- which.min(data$d_error_prior)
            performers <- c(performers, sprintf("- D-Error: %s (%.6f)",
                                                data$design_name[best_idx],
                                                data$d_error_prior[best_idx]))
        }
    } else if ("d_error_null" %in% names(data)) {
        d_errors <- data$d_error_null[!is.na(data$d_error_null)]
        if (length(d_errors) > 0) {
            best_idx <- which.min(data$d_error_null)
            performers <- c(performers, sprintf("- D-Error: %s (%.6f)",
                                                data$design_name[best_idx],
                                                data$d_error_null[best_idx]))
        }
    }

    # Balance best performer
    if ("balance_score" %in% names(data)) {
        balance_scores <- data$balance_score[!is.na(data$balance_score)]
        if (length(balance_scores) > 0) {
            best_idx <- which.max(data$balance_score)
            performers <- c(performers, sprintf("- Balance: %s (%.3f)",
                                                data$design_name[best_idx],
                                                data$balance_score[best_idx]))
        }
    }

    # Overlap best performer
    if ("overlap_score" %in% names(data)) {
        overlap_scores <- data$overlap_score[!is.na(data$overlap_score)]
        if (length(overlap_scores) > 0) {
            best_idx <- which.min(data$overlap_score)
            performers <- c(performers, sprintf("- Overlap: %s (%.3f)",
                                                data$design_name[best_idx],
                                                data$overlap_score[best_idx]))
        }
    }

    # Profile usage best performer
    if ("profile_usage_pct" %in% names(data)) {
        best_idx <- which.max(data$profile_usage_pct)
        performers <- c(performers, sprintf("- Profile Usage: %s (%.1f%%)",
                                            data$design_name[best_idx],
                                            data$profile_usage_pct[best_idx]))
    }

    if (length(performers) > 0) {
        cat(paste(performers, collapse = "\n"))
        cat("\n")
    }
}

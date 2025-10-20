#' Print method for cbc_profiles objects
#' @param x A cbc_profiles object
#' @param ... Additional arguments passed to print
#' @return Returns the input `cbc_profiles` object invisibly (class: c("cbc_profiles", "data.frame")). This function is called for its side effect of printing a formatted summary of the CBC profiles object to the console, including attribute information, profile counts, any applied restrictions, and a preview of the data.
#' @export
print.cbc_profiles <- function(x, ...) {
    cat("CBC Profiles\n")
    cat("============\n")

    # Display attribute information
    attr_info <- attr(x, "attribute_info")
    for (attr in names(attr_info)) {
        cat(sprintf("%-12s: %s\n", attr, attr_info[[attr]]$summary))
    }

    # Display profile counts
    original_count <- attr(x, "original_count")
    current_count <- nrow(x)
    total_removed <- attr(x, "total_removed") %||% 0

    cat(sprintf("\nProfiles: %d", current_count))
    if (total_removed > 0) {
        cat(sprintf(
            " (originally %d, %d removed by restrictions)",
            original_count,
            total_removed
        ))
    }
    cat("\n")

    # Display restrictions if any
    restrictions <- attr(x, "restrictions_applied")
    if (!is.null(restrictions) && length(restrictions) > 0) {
        cat("\nRestrictions applied:\n")
        for (i in seq_along(restrictions)) {
            cat(sprintf("  %d. %s\n", i, restrictions[i]))
        }
        cat("\n")
    }

    cat("First few rows:\n")
    # Remove the class temporarily to avoid infinite recursion
    class(x) <- "data.frame"
    print(utils::head(x))
    if (nrow(x) > 6) {
        cat(sprintf("... and %d more rows\n", nrow(x) - 6))
    }

    invisible(x)
}

#' Print method for cbc_priors objects
#' @param x A cbc_priors object
#' @param ... Additional arguments passed to print
#' @return Returns the input `cbc_priors` object invisibly (class: c("cbc_priors", "list")). This function is called for its side effect of printing a formatted summary of the CBC priors specifications to the console, including parameter types, distributions, means, standard deviations, and any correlation structures.
#' @export
print.cbc_priors <- function(x, ...) {
    cat("CBC Prior Specifications:\n\n")

    for (attr in names(x$attrs)) {
        info <- x$attrs[[attr]]

        # Print attribute name
        cat(attr, ":\n", sep = "")

        # Print variable type and levels
        if (info$continuous) {
            cat("  Continuous variable\n")
            if (!is.null(info$levels)) {
                cat("  Levels:", paste(info$levels, collapse = ", "), "\n")
            }
            ref_level <- NULL
        } else {
            cat("  Categorical variable\n")
            cat("  Levels:", paste(info$levels, collapse = ", "), "\n")
            # Determine reference level for categorical variables
            if (!is.null(names(info$mean))) {
                ref_level <- setdiff(info$levels, names(info$mean))[1]
            } else {
                ref_level <- info$levels[1]
            }
            cat("  Reference level: ", ref_level, "\n", sep = "")
        }

        # Print parameter specifications
        if (info$random) {
            dist_names <- c(
                n = "Normal",
                ln = "Log-normal",
                cn = "Censored normal"
            )
            cat(
                "  Random - ",
                dist_names[info$dist],
                " distribution\n",
                sep = ""
            )

            if (!info$continuous && !is.null(names(info$mean))) {
                # Named categorical parameters
                for (level in names(info$mean)) {
                    cat("    ", level, ":\n", sep = "")
                    cat(
                        "      Mean: ",
                        round(info$mean[level], 3),
                        "\n",
                        sep = ""
                    )
                    cat(
                        "      SD:   ",
                        round(info$sd[level], 3),
                        "\n",
                        sep = ""
                    )
                }
            } else {
                # Continuous or unnamed categorical parameters
                cat(
                    "    Mean: ",
                    round(info$mean, 3),
                    if (!is.null(ref_level) && is.vector(info$mean)) {
                        paste0(" (vs ", ref_level, ")")
                    } else {
                        ""
                    },
                    "\n",
                    sep = ""
                )
                cat("    SD:   ", round(info$sd, 3), "\n", sep = "")
            }
        } else {
            cat("  Fixed parameter\n")
            if (!info$continuous && !is.null(names(info$mean))) {
                # Named categorical parameters
                for (level in names(info$mean)) {
                    cat(
                        "    ",
                        level,
                        ": ",
                        round(info$mean[level], 3),
                        "\n",
                        sep = ""
                    )
                }
            } else {
                # Continuous or unnamed categorical parameters
                cat(
                    "    Coefficient: ",
                    round(info$mean, 3),
                    if (!is.null(ref_level) && is.vector(info$mean)) {
                        paste0(" (vs ", ref_level, ")")
                    } else {
                        ""
                    },
                    "\n",
                    sep = ""
                )
            }
        }
        cat("\n")
    }

    # Print interactions if any
    if (!is.null(x$interactions) && length(x$interactions) > 0) {
        cat("Interactions:\n")
        for (i in seq_along(x$interactions)) {
            int <- x$interactions[[i]]
            cat("  ", int$attr1, sep = "")
            if (!is.null(int$level)) {
                cat("[", int$level, "]", sep = "")
            }
            cat(" \u00D7 ", int$attr2, sep = "")
            if (!is.null(int$with_level)) {
                cat("[", int$with_level, "]", sep = "")
            }
            cat(": ", round(int$value, 3), "\n", sep = "")
        }
        cat("\n")
    }

    if (!is.null(x$correlation)) {
        cat("Correlation Matrix:\n")
        print(round(x$correlation, 3))
    }

    invisible(x)
}

#' Concise print method for cbc_design objects
#' @param x A cbc_design object
#' @param ... Additional arguments passed to print
#' @return Returns the input `cbc_design` object invisibly (class: c("cbc_design", "data.frame")). This function is called for its side effect of printing a concise summary of the CBC design to the console, including design method, structure, D-error metrics, profile usage, and a preview of the design data.
#' @export
print.cbc_design <- function(x, ...) {
    # Check if this looks like actual design data or a transformed data frame
    # If key columns are missing, just print as data frame
    has_required_cols <- all(c("obsID", "altID") %in% names(x))

    if (!has_required_cols) {
        # This is a transformed data frame (e.g., from dplyr operations)
        # Print as a regular data frame
        class(x) <- setdiff(class(x), "cbc_design")
        return(print(x, ...))
    }

    # Extract basic information (may be NULL if reconstructed)
    params <- attr(x, "design_params")
    summary_info <- attr(x, "design_summary")
    encoding <- attr(x, "encoding") %||% "standard"

    # Infer basic structure if params missing
    if (is.null(params)) {
        has_resp_id <- "respID" %in% names(x)
        has_block_id <- "blockID" %in% names(x)

        n_resp <- if (has_resp_id) max(x$respID, na.rm = TRUE) else 1
        n_blocks <- if (has_block_id) max(x$blockID, na.rm = TRUE) else 1
        n_q <- max(x$qID, na.rm = TRUE)
        n_alts <- max(x$altID, na.rm = TRUE)

        cat("Design method: unknown\n")
        cat(sprintf("Encoding: %s\n", encoding))
        cat(sprintf(
            "Structure: %d respondents \u00D7 %d questions \u00D7 %d alternatives",
            n_resp,
            n_q,
            n_alts
        ))

        if (n_blocks > 1) {
            cat(sprintf(" [%d blocks]", n_blocks))
        }
        cat("\n")

        # Profile usage
        n_profiles_used <- length(unique(x$profileID[x$profileID != 0]))
        n_profiles_available <- max(x$profileID, na.rm = TRUE)
        cat(sprintf(
            "Profile usage: %d/%d (%.1f%%)\n",
            n_profiles_used,
            n_profiles_available,
            (n_profiles_used / n_profiles_available) * 100
        ))
    } else {
        # Full information available
        cat(sprintf("Design method: %s\n", params$method))
        cat(sprintf("Encoding: %s\n", encoding))
        cat(sprintf(
            "Structure: %d respondents \u00D7 %d questions \u00D7 %d alternatives",
            params$n_resp,
            params$n_q,
            params$n_alts
        ))

        if (params$n_blocks > 1) {
            cat(sprintf(" [%d blocks]", params$n_blocks))
        }
        cat("\n")

        # Show interaction information
        if (!is.null(params$has_interactions) && params$has_interactions) {
            cat(sprintf(
                "Interactions: %d interaction terms used in design search\n",
                params$n_interactions
            ))
        }

        # Profile usage
        cat(sprintf(
            "Profile usage: %d/%d (%.1f%%)\n",
            summary_info$n_profiles_used,
            summary_info$n_profiles_available,
            summary_info$profile_usage_rate * 100
        ))

        # D-error (show best available)
        if (!is.null(params$d_error_prior)) {
            cat(sprintf("D-error: %.6f", params$d_error_prior))
            if (!is.null(params$has_interactions) && params$has_interactions) {
                cat(" (with interactions)")
            }
            cat("\n")
        } else if (!is.null(params$d_error_null)) {
            cat(sprintf("D-error: %.6f\n", params$d_error_null))
        }
    }

    cat("\n")
    cat("\U0001F4A1 Use cbc_inspect() for a more detailed summary\n")
    if (encoding != "standard") {
        cat(
            "\U0001F4A1 Use cbc_encode(design, 'standard') to view categorical format\n"
        )
    }
    cat("\n")

    # Sample data
    cat("First few rows of design:\n")

    # Remove class temporarily to avoid infinite recursion
    design_df <- x
    class(design_df) <- "data.frame"
    print(utils::head(design_df))
    if (nrow(design_df) > 6) {
        cat(sprintf("... and %d more rows\n", nrow(design_df) - 6))
    }

    invisible(x)
}

#' Print method for cbc_choices objects
#' @param x A cbc_choices object
#' @param ... Additional arguments passed to print
#' @return Returns the input `cbc_choices` object invisibly (class: c("cbc_choices", "data.frame")). This function is called for its side effect of printing a formatted summary of the CBC choice data to the console, including choice task structure, simulation details, choice rates by alternative, and a preview of the choice data.
#' @export
print.cbc_choices <- function(x, ...) {
    # Check if this looks like actual choice data or a transformed data frame
    # If key columns are missing, just print as data frame
    has_required_cols <- all(c("obsID", "choice") %in% names(x))

    if (!has_required_cols) {
        # This is a transformed data frame (e.g., from dplyr operations)
        # Print as a regular data frame
        class(x) <- setdiff(class(x), "cbc_choices")
        return(print(x, ...))
    }

    cat("CBC Choice Data\n")
    cat("===============\n")

    # Get choice info
    choice_info <- attr(x, "choice_info")
    encoding <- attr(x, "encoding") %||% "standard"

    # Basic structure
    n_obs <- max(x$obsID, na.rm = TRUE)
    n_alts <- sum(x$obsID == 1, na.rm = TRUE)
    n_resp <- if ("respID" %in% names(x)) max(x$respID, na.rm = TRUE) else 1
    n_choices <- sum(x$choice, na.rm = TRUE)

    cat(sprintf("Encoding: %s\n", encoding))
    cat(sprintf("Observations: %d choice tasks\n", n_obs))
    cat(sprintf("Alternatives per task: %d\n", n_alts))
    if (n_resp > 1) {
        cat(sprintf("Respondents: %d\n", n_resp))
        cat(sprintf("Questions per respondent: %d\n", n_obs / n_resp))
    }
    cat(sprintf("Total choices made: %d\n", n_choices))

    # Choice simulation info
    if (!is.null(choice_info)) {
        cat(sprintf("\nSimulation method: %s\n", choice_info$simulation_method))
        if (!is.na(choice_info$d_error)) {
            cat(sprintf("Original design D-error: %.6f\n", choice_info$d_error))
        }
        if (choice_info$priors_used) {
            cat("Priors: Used for utility-based simulation\n")
        } else {
            cat("Priors: None (random choices)\n")
        }
        cat(sprintf(
            "Simulated at: %s\n",
            format(choice_info$simulated_at, "%Y-%m-%d %H:%M:%S")
        ))
    }

    # Choice distribution by alternative
    if ("altID" %in% names(x)) {
        choice_by_alt <- tapply(x$choice, x$altID, sum, na.rm = TRUE)
        choice_rates <- choice_by_alt / n_obs
        cat("\nChoice rates by alternative:\n")
        for (i in seq_along(choice_rates)) {
            cat(sprintf(
                "  Alt %d: %.1f%% (%d choices)\n",
                i,
                choice_rates[i] * 100,
                choice_by_alt[i]
            ))
        }
    }

    # No-choice option if present
    if ("no_choice" %in% names(x)) {
        no_choice_rate <- mean(x$choice[x$no_choice == 1], na.rm = TRUE)
        cat(sprintf("\nNo-choice rate: %.1f%%\n", no_choice_rate * 100))
    }

    if (encoding != "standard") {
        cat(
            "\n\U0001F4A1 Use cbc_encode(choices, 'standard') to view categorical format\n"
        )
    }

    cat("\nFirst few rows:\n")

    # Remove class temporarily to avoid infinite recursion
    choices_df <- x
    class(choices_df) <- "data.frame"
    print(utils::head(choices_df))
    if (nrow(choices_df) > 6) {
        cat(sprintf("... and %d more rows\n", nrow(choices_df) - 6))
    }

    invisible(x)
}

#' Print method for cbc_inspection objects
#' @param x A cbc_inspection object
#' @param ... Additional arguments passed to print
#' @return Returns the input `cbc_inspection` object invisibly (class: c("cbc_inspection", "list")). This function is called for its side effect of printing a comprehensive inspection report of the CBC design to the console, including sections on design structure, efficiency metrics, attribute balance, overlap analysis, and variable encoding.
#' @export
print.cbc_inspection <- function(x, ...) {
    # Print header
    cat("DESIGN SUMMARY\n")
    cat("=========================\n\n")

    # Structure section
    if (!is.null(x$structure)) {
        cat("STRUCTURE\n")
        cat("================\n")
        print_structure_section(x$structure, x$verbose)
        cat("\n")
    }

    # Efficiency section
    if (!is.null(x$efficiency)) {
        cat("SUMMARY METRICS\n")
        cat("=================\n")
        print_efficiency_section(x$efficiency, x$verbose)
        cat("\n")
    }

    # Encoding section
    if (!is.null(x$encoding)) {
        cat("VARIABLE ENCODING\n")
        cat("=================\n")
        print_encoding_section(x$encoding, x$verbose)
        cat("\n")
    }

    # Balance section
    if (!is.null(x$balance)) {
        cat("ATTRIBUTE BALANCE\n")
        cat("=================\n")
        print_balance_section(x$balance, x$verbose)
        cat("\n")
    }

    # Overlap section
    if (!is.null(x$overlap)) {
        cat("ATTRIBUTE OVERLAP\n")
        cat("=================\n")
        print_overlap_section(x$overlap, x$verbose)
        cat("\n")
    }

    invisible(x)
}

# Helper functions for printing each section in inspection

print_structure_section <- function(structure_data, verbose) {
    cat(sprintf("Method: %s\n", structure_data$method))

    if (!is.na(structure_data$created_at)) {
        cat(sprintf(
            "Created: %s\n",
            format(structure_data$created_at, "%Y-%m-%d %H:%M:%S")
        ))
    }

    if (verbose && !is.na(structure_data$generation_time)) {
        cat(sprintf(
            "Generation time: %.3f seconds\n",
            structure_data$generation_time
        ))
    }

    cat(sprintf("Respondents: %d\n", structure_data$n_resp))
    cat(sprintf("Questions per respondent: %d\n", structure_data$n_q))
    cat(sprintf("Alternatives per question: %d\n", structure_data$n_alts))

    if (structure_data$n_blocks > 1) {
        cat(sprintf("Blocks: %d\n", structure_data$n_blocks))
    }

    cat(sprintf("Total choice sets: %d\n", structure_data$n_choice_sets))
    cat(sprintf(
        "Profile usage: %d/%d (%.1f%%)\n",
        structure_data$n_profiles_used,
        structure_data$n_profiles_available,
        structure_data$profile_usage_rate * 100
    ))

    # Special features
    if (length(structure_data$features) > 0) {
        cat("Special features:\n")
        for (feature in structure_data$features) {
            cat("  \u2022 ", feature, "\n", sep = "")
        }
    }

    if (verbose && !is.na(structure_data$optimization_attempts)) {
        cat(sprintf(
            "Optimization attempts: %d\n",
            structure_data$optimization_attempts
        ))
    }
}

print_efficiency_section <- function(efficiency_data, verbose) {
    # D-error information
    if (
        efficiency_data$method != 'random' &&
            efficiency_data$method != 'unknown'
    ) {
        has_d_error <- FALSE

        if (
            !is.null(efficiency_data$d_error_prior) &&
                !is.na(efficiency_data$d_error_prior)
        ) {
            cat(sprintf(
                "D-error (with priors): %.6f\n",
                efficiency_data$d_error_prior
            ))
            has_d_error <- TRUE
        }
        if (
            !is.null(efficiency_data$d_error_null) &&
                !is.na(efficiency_data$d_error_null)
        ) {
            cat(sprintf(
                "D-error (null model): %.6f\n",
                efficiency_data$d_error_null
            ))
            has_d_error <- TRUE
        }
        if (has_d_error) {
            cat("(Lower values indicate more efficient designs)\n\n")
        }
    } else {
        cat("D-error calculation not available for this design\n")
    }

    # Quality metrics if available
    if (
        !is.null(efficiency_data$balance_score) &&
            !is.na(efficiency_data$balance_score)
    ) {
        cat(sprintf(
            "Overall balance score: %.3f (higher is better)\n",
            efficiency_data$balance_score
        ))
        cat(sprintf(
            "Overall overlap score: %.3f (lower is better)\n",
            efficiency_data$overlap_score
        ))

        if (
            verbose &&
                !is.null(efficiency_data$profiles_used) &&
                !is.na(efficiency_data$profiles_used)
        ) {
            cat(sprintf(
                "  Profiles used: %d/%d\n",
                efficiency_data$profiles_used,
                efficiency_data$profiles_available
            ))
        }
    }
}

print_encoding_section <- function(encoding_data, verbose) {
    encoding_names <- c(
        standard = "Standard (categorical)",
        dummy = "Dummy-coded",
        effects = "Effects-coded"
    )

    cat("Format: ", encoding_names[encoding_data$encoding], sep = "")

    # Show which variables are categorical
    if (
        !is.null(encoding_data$categorical_variables) &&
            length(encoding_data$categorical_variables) > 0
    ) {
        cat(sprintf(
            " (%s)",
            paste(encoding_data$categorical_variables, collapse = ", ")
        ))
    }
    cat("\n")

    if (verbose && !is.null(encoding_data$categorical_details)) {
        cat("\nCategorical variable details:\n")
        for (var in names(encoding_data$categorical_details)) {
            details <- encoding_data$categorical_details[[var]]
            cat(sprintf(
                "  %s: %s (reference: %s)\n",
                var,
                paste(details$levels, collapse = ", "),
                details$reference_level
            ))
        }
    }

    # Show encoding options
    if (encoding_data$encoding != "standard") {
        cat(
            "\U0001F4A1 Use cbc_encode(design, 'standard') to convert to categorical format\n"
        )
    } else {
        cat(
            "\U0001F4A1 Use cbc_encode() to convert to dummy or effects coding\n"
        )
    }
}

print_balance_section <- function(balance_data, verbose) {
    if (!is.null(balance_data$overall_balance)) {
        cat(sprintf(
            "Overall balance score: %.3f (higher is better)\n\n",
            balance_data$overall_balance
        ))
    }

    # Print detailed balance info
    if (!is.null(balance_data$individual_counts)) {
        cat("Individual attribute level counts:\n")
        for (i in seq_along(balance_data$individual_counts)) {
            attr_name <- names(balance_data$individual_counts)[i]
            cat(sprintf("\n%s:\n", attr_name))
            print(balance_data$individual_counts[[i]])

            # Show balance metric
            if (!is.null(balance_data$balance_metrics[[attr_name]])) {
                metric <- balance_data$balance_metrics[[attr_name]]
                if (verbose) {
                    cat(sprintf(
                        "  Balance score: %.3f (higher is better), CV: %.3f (lower is better)\n",
                        metric$balance_score,
                        metric$cv
                    ))
                } else {
                    cat(sprintf(
                        "  Balance score: %.3f (higher is better)\n",
                        metric$balance_score
                    ))
                }
            }
        }
    }
}

print_overlap_section <- function(overlap_data, verbose) {
    if (!is.null(overlap_data$overall_overlap)) {
        cat(sprintf(
            "Overall overlap score: %.3f (lower is better)\n\n",
            overlap_data$overall_overlap
        ))
    }

    # Print detailed overlap info
    if (!is.null(overlap_data$overlap_counts)) {
        cat("Counts of attribute overlap:\n")
        cat("(# of questions with N unique levels)\n\n")

        total_questions <- max(sapply(overlap_data$overlap_counts, function(x) {
            sum(as.numeric(x$unique_per_question))
        }))

        for (i in seq_along(overlap_data$overlap_counts)) {
            attr_name <- names(overlap_data$overlap_counts)[i]
            attr_data <- overlap_data$overlap_counts[[i]]

            cat(sprintf("%s: ", attr_name))

            if (attr_data$type == "continuous") {
                cat("Continuous variable\n")
                if (verbose) {
                    cat(
                        "  Unique levels: ",
                        paste(names(attr_data$value_counts), collapse = ", "),
                        "\n"
                    )
                }
            } else {
                cat("Categorical variable\n")
            }

            cat("  Questions by # unique levels:\n")

            # Get the overlap distribution
            unique_counts <- attr_data$unique_per_question
            max_levels <- attr_data$max_possible_unique

            # Process each level count
            for (level in 1:max_levels) {
                level_str <- as.character(level)
                count <- if (level_str %in% names(unique_counts)) {
                    unique_counts[[level_str]]
                } else {
                    0
                }
                percentage <- (count / total_questions) * 100

                # Create descriptive labels
                if (level == 1) {
                    label <- " (complete overlap): "
                } else if (level == max_levels) {
                    label <- " (no overlap):       "
                } else {
                    label <- " (partial overlap):  "
                }

                cat(sprintf(
                    "  %d%s%5.1f%%  (%d / %d questions)\n",
                    level,
                    label,
                    percentage,
                    count,
                    total_questions
                ))
            }

            # Show average unique levels
            if (!is.null(overlap_data$overlap_metrics[[attr_name]])) {
                metric <- overlap_data$overlap_metrics[[attr_name]]
                cat(sprintf(
                    "  Average unique levels per question: %.2f\n",
                    metric$avg_unique_levels
                ))
            }
            cat("\n")
        }
    }
}

#' Print method for cbc_comparison objects
#' @param x A cbc_comparison object
#' @param ... Additional arguments passed to print
#' @return Returns the input `cbc_comparison` object invisibly (class: c("cbc_comparison", "list")). This function is called for its side effect of printing a formatted comparison table of multiple CBC designs to the console, including design metrics, performance rankings, and interpretation guidelines.
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
    profile_usage_formatted <- sprintf(
        "(%d/%d) %s%%",
        data$profiles_used,
        data$profiles_available,
        data$profile_usage_pct
    )

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
        d_error_null_formatted <- ifelse(
            is.na(data$d_error_null),
            "NA",
            sprintf("%.6f", data$d_error_null)
        )
        d_error_prior_formatted <- ifelse(
            is.na(data$d_error_prior),
            "NA",
            sprintf("%.6f", data$d_error_prior)
        )

        metrics_data$`D-Error (Null)` <- d_error_null_formatted
        metrics_data$`D-Error (Prior)` <- d_error_prior_formatted
    }

    # Add Balance if included
    if ("balance" %in% metrics) {
        balance_formatted <- ifelse(
            is.na(data$balance_score),
            "NA",
            sprintf("%.3f", data$balance_score)
        )
        metrics_data$Balance <- balance_formatted
    }

    # Add Overlap if included
    if ("overlap" %in% metrics) {
        overlap_formatted <- ifelse(
            is.na(data$overlap_score),
            "NA",
            sprintf("%.3f", data$overlap_score)
        )
        metrics_data$Overlap <- overlap_formatted
    }

    print(metrics_data, row.names = FALSE)
}

# Print interpretation of metrics
print_interpretation <- function(metrics) {
    interpretations <- c()

    if ("efficiency" %in% metrics) {
        interpretations <- c(
            interpretations,
            "- D-Error: Lower is better (design efficiency)"
        )
    }

    if ("balance" %in% metrics) {
        interpretations <- c(
            interpretations,
            "- Balance: Higher is better (level distribution)"
        )
    }

    if ("overlap" %in% metrics) {
        interpretations <- c(
            interpretations,
            "- Overlap: Lower is better (attribute variation)"
        )
    }

    if ("structure" %in% metrics) {
        interpretations <- c(
            interpretations,
            "- Profile Usage: Higher means more profiles used"
        )
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
            performers <- c(
                performers,
                sprintf(
                    "- D-Error: %s (%.6f)",
                    data$design_name[best_idx],
                    data$d_error_prior[best_idx]
                )
            )
        }
    } else if ("d_error_null" %in% names(data)) {
        d_errors <- data$d_error_null[!is.na(data$d_error_null)]
        if (length(d_errors) > 0) {
            best_idx <- which.min(data$d_error_null)
            performers <- c(
                performers,
                sprintf(
                    "- D-Error: %s (%.6f)",
                    data$design_name[best_idx],
                    data$d_error_null[best_idx]
                )
            )
        }
    }

    # Balance best performer
    if ("balance_score" %in% names(data)) {
        balance_scores <- data$balance_score[!is.na(data$balance_score)]
        if (length(balance_scores) > 0) {
            best_idx <- which.max(data$balance_score)
            performers <- c(
                performers,
                sprintf(
                    "- Balance: %s (%.3f)",
                    data$design_name[best_idx],
                    data$balance_score[best_idx]
                )
            )
        }
    }

    # Overlap best performer
    if ("overlap_score" %in% names(data)) {
        overlap_scores <- data$overlap_score[!is.na(data$overlap_score)]
        if (length(overlap_scores) > 0) {
            best_idx <- which.min(data$overlap_score)
            performers <- c(
                performers,
                sprintf(
                    "- Overlap: %s (%.3f)",
                    data$design_name[best_idx],
                    data$overlap_score[best_idx]
                )
            )
        }
    }

    # Profile usage best performer
    if ("profile_usage_pct" %in% names(data)) {
        best_idx <- which.max(data$profile_usage_pct)
        performers <- c(
            performers,
            sprintf(
                "- Profile Usage: %s (%.1f%%)",
                data$design_name[best_idx],
                data$profile_usage_pct[best_idx]
            )
        )
    }

    if (length(performers) > 0) {
        cat(paste(performers, collapse = "\n"))
        cat("\n")
    }
}

#' Print method for cbc_power objects
#' @param x A cbc_power object
#' @param ... Additional arguments passed to print
#' @return Returns the input `cbc_power` object invisibly (class: c("cbc_power", "list")). This function is called for its side effect of printing a formatted summary of the CBC power analysis results to the console, including sample size ranges, significance levels, parameter summaries, and power estimates across different sample sizes.
#' @export
print.cbc_power <- function(x, ...) {
    cat("CBC Power Analysis Results\n")
    cat("==========================\n\n")

    # Basic info
    cat(sprintf(
        "Sample sizes tested: %d to %d (%d breaks)\n",
        min(x$sample_sizes),
        max(x$sample_sizes),
        x$n_breaks
    ))
    cat(sprintf("Significance level: %.3f\n", x$alpha))

    # Parameter summary
    params <- unique(x$power_summary$parameter)
    cat(sprintf("Parameters: %s\n\n", paste(params, collapse = ", ")))

    # Power summary at different sample sizes
    cat("Power summary (probability of detecting true effect):\n")

    # Show every few sample sizes for readability
    sizes_to_show <- unique(x$power_summary$sample_size)
    if (length(sizes_to_show) > 5) {
        indices <- round(seq(1, length(sizes_to_show), length.out = 5))
        sizes_to_show <- sizes_to_show[indices]
    }

    for (size in sizes_to_show) {
        subset_data <- x$power_summary[x$power_summary$sample_size == size, ]
        cat(sprintf("\nn = %d:\n", size))
        for (i in 1:nrow(subset_data)) {
            cat(sprintf(
                "  %-12s: Power = %.3f, SE = %.4f\n",
                subset_data$parameter[i],
                subset_data$power[i],
                subset_data$std_error[i]
            ))
        }
    }

    cat("\nUse plot() to visualize power curves.\n")
    cat("Use summary() for detailed power analysis.\n")

    invisible(x)
}

#' Plot method for cbc_power objects
#' @param x A cbc_power object
#' @param type Type of plot: "power" for power curves or "se" for standard error curves
#' @param power_threshold Power threshold for horizontal reference line (only for power plots). Defaults to 0.8
#' @param ... Additional arguments passed to ggplot
#' @return Returns a ggplot2 object (class: "gg", "ggplot") that can be further customized, saved, or displayed. The plot visualizes either statistical power curves or standard error curves across different sample sizes for each parameter in the power analysis, with appropriate axis labels, legends, and reference lines.
#' @export
plot.cbc_power <- function(x, type = "power", power_threshold = 0.8, ...) {
    sample_size <- power <- parameter <- std_error <- NULL

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "Package 'ggplot2' is required for plotting. Please install it with install.packages('ggplot2')"
        )
    }

    if (!type %in% c("power", "se")) {
        stop("type must be 'power' or 'se'")
    }

    if (type == "power") {
        p <- ggplot2::ggplot(
            x$power_summary,
            ggplot2::aes(x = sample_size, y = power)
        ) +
            ggplot2::geom_hline(
                yintercept = power_threshold,
                color = "red",
                linetype = "dashed",
                alpha = 0.7
            ) +
            ggplot2::geom_line(ggplot2::aes(color = parameter), linewidth = 1) +
            ggplot2::geom_point(ggplot2::aes(color = parameter), size = 2) +
            ggplot2::theme_bw() +
            ggplot2::labs(
                x = "Sample Size (number of respondents)",
                y = "Statistical Power",
                title = "Power Analysis Results",
                subtitle = sprintf(
                    "Dashed line shows %.0f%% power threshold",
                    power_threshold * 100
                ),
                color = "Parameter"
            ) +
            ggplot2::ylim(0, 1)
    } else {
        p <- ggplot2::ggplot(
            x$power_summary,
            ggplot2::aes(x = sample_size, y = std_error)
        ) +
            ggplot2::geom_line(ggplot2::aes(color = parameter), linewidth = 1) +
            ggplot2::geom_point(ggplot2::aes(color = parameter), size = 2) +
            ggplot2::theme_bw() +
            ggplot2::labs(
                x = "Sample Size (number of respondents)",
                y = "Standard Error",
                title = "Standard Error vs Sample Size",
                color = "Parameter"
            )
    }

    return(p)
}

#' Summary method for cbc_power objects
#' @param object A cbc_power object
#' @param power_threshold Minimum power threshold to report sample size requirements
#' @param ... Additional arguments
#' @return Returns the input `cbc_power` object invisibly (class: c("cbc_power", "list")). This function is called for its side effect of printing a detailed summary to the console showing sample size requirements for achieving specified power thresholds for each parameter, including exact power levels and standard errors at the required sample sizes.
#' @export
summary.cbc_power <- function(object, power_threshold = 0.8, ...) {
    cat("CBC Power Analysis Summary\n")
    cat("===========================\n\n")

    # For each parameter, find sample size needed for threshold power
    params <- unique(object$power_summary$parameter)

    cat(sprintf(
        "Sample size requirements for %.0f%% power:\n\n",
        power_threshold * 100
    ))

    for (param in params) {
        param_data <- object$power_summary[
            object$power_summary$parameter == param,
        ]
        param_data <- param_data[order(param_data$sample_size), ]

        # Find first sample size that achieves threshold power
        sufficient_power <- param_data$power >= power_threshold

        if (any(sufficient_power)) {
            required_size <- min(param_data$sample_size[sufficient_power])
            final_power <- param_data$power[
                param_data$sample_size == required_size
            ][1]
            final_se <- param_data$std_error[
                param_data$sample_size == required_size
            ][1]

            cat(sprintf(
                "%-15s: n >= %d (achieves %.1f%% power, SE = %.4f)\n",
                param,
                required_size,
                final_power * 100,
                final_se
            ))
        } else {
            max_power <- max(param_data$power)
            max_size <- max(param_data$sample_size)
            cat(sprintf(
                "%-15s: Threshold not reached (max %.1f%% power at n = %d)\n",
                param,
                max_power * 100,
                max_size
            ))
        }
    }

    cat("\n")
    invisible(object)
}

# ============================================================================
# Attribute preservation methods for cbc_profiles
# ============================================================================

#' @export
`[.cbc_profiles` <- function(x, i, j, drop = FALSE) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("[")

    if (is.data.frame(result)) {
        class(result) <- c("cbc_profiles", "data.frame")

        custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
        for (attr_name in custom_attr_names) {
            attr(result, attr_name) <- custom_attrs[[attr_name]]
        }
    }

    return(result)
}

#' @export
`[<-.cbc_profiles` <- function(x, i, j, value) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("[<-")

    class(result) <- c("cbc_profiles", "data.frame")

    custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
    for (attr_name in custom_attr_names) {
        attr(result, attr_name) <- custom_attrs[[attr_name]]
    }

    return(result)
}

#' @export
`names<-.cbc_profiles` <- function(x, value) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("names<-")

    class(result) <- c("cbc_profiles", "data.frame")

    custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
    for (attr_name in custom_attr_names) {
        attr(result, attr_name) <- custom_attrs[[attr_name]]
    }

    return(result)
}

# ============================================================================
# Attribute preservation methods for cbc_design
# ============================================================================

#' @export
`[.cbc_design` <- function(x, i, j, drop = FALSE) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("[")

    if (is.data.frame(result)) {
        class(result) <- c("cbc_design", "data.frame")

        custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
        for (attr_name in custom_attr_names) {
            attr(result, attr_name) <- custom_attrs[[attr_name]]
        }
    }

    return(result)
}

#' @export
`[<-.cbc_design` <- function(x, i, j, value) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("[<-")

    class(result) <- c("cbc_design", "data.frame")

    custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
    for (attr_name in custom_attr_names) {
        attr(result, attr_name) <- custom_attrs[[attr_name]]
    }

    return(result)
}

#' @export
`names<-.cbc_design` <- function(x, value) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("names<-")

    class(result) <- c("cbc_design", "data.frame")

    custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
    for (attr_name in custom_attr_names) {
        attr(result, attr_name) <- custom_attrs[[attr_name]]
    }

    return(result)
}

# ============================================================================
# Attribute preservation methods for cbc_choices
# ============================================================================

#' @export
`[.cbc_choices` <- function(x, i, j, drop = FALSE) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("[")

    if (is.data.frame(result)) {
        class(result) <- c("cbc_choices", "data.frame")

        custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
        for (attr_name in custom_attr_names) {
            attr(result, attr_name) <- custom_attrs[[attr_name]]
        }
    }

    return(result)
}

#' @export
`[<-.cbc_choices` <- function(x, i, j, value) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("[<-")

    class(result) <- c("cbc_choices", "data.frame")

    custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
    for (attr_name in custom_attr_names) {
        attr(result, attr_name) <- custom_attrs[[attr_name]]
    }

    return(result)
}

#' @export
`names<-.cbc_choices` <- function(x, value) {
    custom_attrs <- attributes(x)
    standard_attrs <- c("names", "row.names", "class")

    result <- NextMethod("names<-")

    class(result) <- c("cbc_choices", "data.frame")

    custom_attr_names <- setdiff(names(custom_attrs), standard_attrs)
    for (attr_name in custom_attr_names) {
        attr(result, attr_name) <- custom_attrs[[attr_name]]
    }

    return(result)
}

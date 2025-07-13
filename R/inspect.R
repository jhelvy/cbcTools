#' Comprehensive design quality inspection
#'
#' This function provides detailed inspection of choice experiment designs
#' across multiple dimensions including design structure, efficiency metrics,
#' attribute balance, overlap patterns, and variable encoding.
#'
#' @param design A `cbc_design` object created by `cbc_design()`
#' @param sections Character vector specifying which sections to show.
#'   Options: "structure", "efficiency", "balance", "overlap", "encoding", or "all" (default).
#'   Can specify multiple: `c("balance", "overlap")`
#' @param verbose Logical. If TRUE, shows additional technical details.
#'   If FALSE (default), shows simplified output.
#' @return A `cbc_inspection` object containing the inspection results
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Create profiles and design
#' profiles <- cbc_profiles(
#'   price = c(1, 2, 3),
#'   type = c("A", "B", "C"),
#'   quality = c("Low", "High")
#' )
#'
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 2,
#'   n_q = 4
#' )
#'
#' # Inspect all sections (default) - prints automatically
#' cbc_inspect(design)
#'
#' # Store results for later use
#' inspection <- cbc_inspect(design, sections = "balance")
#' inspection  # prints the same output
#'
#' # Verbose output with technical details
#' cbc_inspect(design, verbose = TRUE)
cbc_inspect <- function(design, sections = "all", verbose = FALSE) {
  # Validate inputs
  if (!inherits(design, "cbc_design") && !inherits(design, "cbc_choices")) {
    stop(
      "design must be a cbc_design object created by cbc_design() or a cbc_choices object created by cbc_choices()"
    )
  }

  # Handle "all" sections
  if ("all" %in% sections) {
    sections <- c("structure", "efficiency", "balance", "overlap", "encoding")
  }

  # Validate sections
  valid_sections <- c(
    "structure",
    "efficiency",
    "balance",
    "overlap",
    "encoding"
  )
  invalid_sections <- setdiff(sections, valid_sections)
  if (length(invalid_sections) > 0) {
    stop(
      "Invalid sections: ",
      paste(invalid_sections, collapse = ", "),
      ". Valid options are: ",
      paste(valid_sections, collapse = ", ")
    )
  }

  # Extract design information
  params <- attr(design, "design_params")
  summary_info <- attr(design, "design_summary")

  # Initialize results list
  results <- list()

  # Structure section
  if ("structure" %in% sections) {
    results$structure <- inspect_structure_section(
      design,
      params,
      summary_info,
      verbose
    )
  }

  # Efficiency section
  if ("efficiency" %in% sections) {
    results$efficiency <- inspect_efficiency_section(
      design,
      params,
      summary_info,
      verbose
    )
  }

  # Balance section
  if ("balance" %in% sections) {
    results$balance <- inspect_balance_section(design, summary_info, verbose)
  }

  # Overlap section
  if ("overlap" %in% sections) {
    results$overlap <- inspect_overlap_section(design, summary_info, verbose)
  }

  # Encoding section
  if ("encoding" %in% sections) {
    results$encoding <- inspect_encoding_section(design, params, verbose)
  }

  # Add metadata
  results$sections_requested <- sections
  results$verbose <- verbose
  results$design_info <- list(
    method = params$method,
    d_error = params$d_error_prior %||% params$d_error_null,
    n_choice_sets = summary_info$n_choice_sets,
    profiles_used = summary_info$n_profiles_used,
    profiles_available = summary_info$n_profiles_available
  )

  class(results) <- c("cbc_inspection", "list")
  return(results)
}

# Helper functions for each section

inspect_structure_section <- function(design, params, summary_info, verbose) {
  # Special features
  features <- c()
  if (params$no_choice) {
    features <- c(features, "No-choice option")
  }
  if (!is.null(params$label)) {
    features <- c(features, paste("Labeled design:", params$label))
  }
  if (!is.null(params$remove_dominant) && params$remove_dominant) {
    features <- c(
      features,
      paste(
        "Dominance removal:",
        paste(params$dominance_types, collapse = ", ")
      )
    )
  }

  return(list(
    method = params$method,
    created_at = params$created_at,
    generation_time = params$time_elapsed_sec,
    n_resp = params$n_resp,
    n_q = params$n_q,
    n_alts = params$n_alts,
    n_blocks = params$n_blocks,
    n_choice_sets = summary_info$n_choice_sets,
    n_profiles_used = summary_info$n_profiles_used,
    n_profiles_available = summary_info$n_profiles_available,
    profile_usage_rate = summary_info$profile_usage_rate,
    features = features,
    optimization_attempts = summary_info$optimization_attempts
  ))
}

inspect_efficiency_section <- function(design, params, summary_info, verbose) {
  return(list(
    method = params$method,
    d_error_prior = params$d_error_prior,
    d_error_null = params$d_error_null,
    balance_score = summary_info$efficiency$balance_score %||% NA,
    overlap_score = summary_info$efficiency$overlap_score %||% NA,
    profiles_used = summary_info$efficiency$profiles_used %||% NA,
    profiles_available = summary_info$efficiency$profiles_available %||% NA
  ))
}

inspect_balance_section <- function(design, summary_info, verbose) {
  # Use pre-computed metrics if available
  if (!is.null(summary_info$efficiency$balance_score)) {
    balance_details <- summary_info$efficiency$balance_details
    result <- inspect_balance_detailed(design, balance_details, verbose)
    result$overall_balance <- summary_info$efficiency$balance_score
  } else {
    # Compute on the fly if not pre-computed
    result <- inspect_balance_detailed(design, verbose = verbose)
  }

  return(result)
}

inspect_overlap_section <- function(design, summary_info, verbose) {
  # Use pre-computed metrics if available
  if (!is.null(summary_info$efficiency$overlap_score)) {
    overlap_details <- summary_info$efficiency$overlap_details
    result <- inspect_overlap_detailed(design, overlap_details, verbose)
    result$overall_overlap <- summary_info$efficiency$overlap_score
  } else {
    # Compute on the fly if not pre-computed
    result <- inspect_overlap_detailed(design, verbose = verbose)
  }

  return(result)
}

inspect_encoding_section <- function(design, params, verbose) {
  is_dummy_coded <- attr(design, "is_dummy_coded") %||%
    params$dummy_coded %||%
    TRUE
  categorical_structure <- attr(design, "categorical_structure")

  categorical_variables <- NULL
  categorical_details <- NULL

  if (!is.null(categorical_structure)) {
    categorical_variables <- names(categorical_structure)[
      sapply(categorical_structure, function(info) info$is_categorical)
    ]

    if (verbose) {
      categorical_details <- list()
      for (var in names(categorical_structure)) {
        info <- categorical_structure[[var]]
        if (info$is_categorical) {
          categorical_details[[var]] <- list(
            levels = info$levels,
            reference_level = info$reference_level
          )
        }
      }
    }
  }

  return(list(
    is_dummy_coded = is_dummy_coded,
    categorical_variables = categorical_variables,
    categorical_details = categorical_details,
    no_choice = params$no_choice
  ))
}

# Detailed balance inspection
inspect_balance_detailed <- function(
  design,
  balance_details = NULL,
  verbose = FALSE
) {
  if (is.null(balance_details)) {
    # Compute balance metrics
    balance_result <- compute_balance_metrics_internal(design)
    counts <- balance_result$individual_counts
    balance_metrics <- balance_result$balance_metrics
  } else {
    # Use pre-computed data - need to recompute counts for display
    atts <- setdiff(
      names(design),
      c("respID", "qID", "altID", "obsID", "profileID", "blockID")
    )
    counts <- lapply(atts, function(attr) table(design[[attr]]))
    names(counts) <- atts
    balance_metrics <- balance_details
  }

  return(list(
    individual_counts = counts,
    balance_metrics = balance_metrics,
    overall_balance = mean(sapply(balance_metrics, function(x) x$balance_score))
  ))
}

# Detailed overlap inspection
inspect_overlap_detailed <- function(
  design,
  overlap_details = NULL,
  verbose = FALSE
) {
  if (is.null(overlap_details)) {
    # Compute overlap metrics
    overlap_result <- compute_overlap_metrics_internal(design)
    overlap_counts <- overlap_result$overlap_counts
    overlap_metrics <- overlap_result$overlap_metrics
  } else {
    # Use pre-computed data - need to recompute counts for display
    atts <- setdiff(
      names(design),
      c("respID", "qID", "altID", "obsID", "profileID", "blockID")
    )
    overlap_counts <- lapply(atts, function(attr) {
      get_att_overlap_counts(attr, design)
    })
    names(overlap_counts) <- atts
    overlap_metrics <- overlap_details
  }

  total_questions <- max(design$obsID, na.rm = TRUE)

  return(list(
    overlap_counts = overlap_counts,
    overlap_metrics = overlap_metrics,
    overall_overlap = mean(sapply(overlap_metrics, function(x) {
      x$complete_overlap_rate
    }))
  ))
}

# Helper functions

calculate_balance_metrics <- function(counts) {
  metrics <- list()

  for (attr in names(counts)) {
    attr_counts <- counts[[attr]]
    expected_count <- sum(attr_counts) / length(attr_counts)

    # Calculate coefficient of variation
    cv <- stats::sd(attr_counts) / mean(attr_counts)

    # Balance score: higher is better, max = 1 (perfect balance)
    balance_score <- 1 / (1 + cv)

    metrics[[attr]] <- list(
      balance_score = balance_score,
      cv = cv,
      expected_count = expected_count,
      actual_counts = attr_counts
    )
  }

  return(metrics)
}

calculate_overlap_metrics <- function(overlap_counts, design) {
  metrics <- list()
  total_questions <- max(design$obsID, na.rm = TRUE)

  for (attr in names(overlap_counts)) {
    attr_data <- overlap_counts[[attr]]

    # Extract the unique_per_question counts regardless of continuous/categorical
    if (attr_data$type == "continuous") {
      counts <- attr_data$unique_per_question
    } else {
      counts <- attr_data$unique_per_question
    }

    # Complete overlap is when only 1 unique level appears
    complete_overlap_count <- if ("1" %in% names(counts)) counts[["1"]] else 0
    complete_overlap_rate <- complete_overlap_count / total_questions

    # Average unique levels per question
    unique_levels <- as.numeric(names(counts))
    question_counts <- as.numeric(counts)
    avg_unique_levels <- sum(unique_levels * question_counts) /
      sum(question_counts)

    metrics[[attr]] <- list(
      complete_overlap_count = complete_overlap_count,
      complete_overlap_rate = complete_overlap_rate,
      avg_unique_levels = avg_unique_levels,
      total_questions = total_questions,
      distribution = counts,
      max_possible = attr_data$max_possible_unique
    )
  }

  return(metrics)
}

# Helper function - updated to handle the new display format
get_att_overlap_counts <- function(attr_name, design) {
  # Check if this is a continuous variable
  values <- design[[attr_name]]
  is_continuous <- is.numeric(values)

  if (is_continuous) {
    # For continuous variables, show the actual unique values that appear
    # and how many questions each appears in
    unique_vals <- sort(unique(values))

    # Count how many times each unique value appears
    val_counts <- table(values)

    # Also calculate how many unique values appear per question
    unique_per_q <- tapply(
      design[[attr_name]],
      design$obsID,
      FUN = function(x) length(unique(x))
    )
    unique_counts <- table(unique_per_q)

    # Return both pieces of information
    return(list(
      type = "continuous",
      value_counts = val_counts,
      unique_per_question = unique_counts,
      total_unique_values = length(unique_vals),
      max_possible_unique = length(unique_vals)
    ))
  } else {
    # For categorical variables, use the original logic
    counts <- tapply(
      design[[attr_name]],
      design$obsID,
      FUN = function(x) length(unique(x))
    )
    unique_counts <- table(counts)

    # Also get the total possible levels
    total_levels <- length(unique(values))

    return(list(
      type = "categorical",
      unique_per_question = unique_counts,
      max_possible_unique = total_levels
    ))
  }
}

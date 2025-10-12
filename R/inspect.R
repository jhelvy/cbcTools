#' Comprehensive design quality inspection
#'
#' This function provides detailed inspection of choice experiment designs
#' across multiple dimensions including design structure, efficiency metrics,
#' attribute balance, overlap patterns, and variable encoding.
#'
#' @param design A `cbc_design` or `cbc_choices` object created by `cbc_design()`
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
  # Validate and potentially reconstruct design object
  design <- validate_or_reconstruct(
    design,
    require_class = FALSE,
    allow_choices = TRUE,
    context = "cbc_inspect()"
  )

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

  # Extract design information (may be NULL if reconstructed)
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
    method = if (!is.null(params)) params$method else "unknown",
    d_error = if (!is.null(params)) {
      params$d_error_prior %||% params$d_error_null
    } else {
      NA
    },
    n_choice_sets = if (!is.null(summary_info)) {
      summary_info$n_choice_sets
    } else {
      max(design$obsID, na.rm = TRUE)
    },
    profiles_used = if (!is.null(summary_info)) {
      summary_info$n_profiles_used
    } else {
      length(unique(design$profileID[design$profileID != 0]))
    },
    profiles_available = if (!is.null(summary_info)) {
      summary_info$n_profiles_available
    } else {
      max(design$profileID, na.rm = TRUE)
    }
  )

  class(results) <- c("cbc_inspection", "list")
  return(results)
}

# Helper functions for each section

inspect_structure_section <- function(design, params, summary_info, verbose) {
  # Handle missing params gracefully
  if (is.null(params)) {
    # Infer basic structure from data
    has_resp_id <- "respID" %in% names(design)
    has_block_id <- "blockID" %in% names(design)

    n_resp <- if (has_resp_id) max(design$respID, na.rm = TRUE) else 1
    n_blocks <- if (has_block_id) max(design$blockID, na.rm = TRUE) else 1
    n_q <- max(design$qID, na.rm = TRUE)
    n_alts <- max(design$altID, na.rm = TRUE)
    n_profiles_used <- length(unique(design$profileID[design$profileID != 0]))
    n_profiles_available <- max(design$profileID, na.rm = TRUE)

    return(list(
      method = "unknown",
      created_at = NA,
      generation_time = NA,
      n_resp = n_resp,
      n_q = n_q,
      n_alts = n_alts,
      n_blocks = n_blocks,
      n_choice_sets = max(design$obsID, na.rm = TRUE),
      n_profiles_used = n_profiles_used,
      n_profiles_available = n_profiles_available,
      profile_usage_rate = n_profiles_used / n_profiles_available,
      features = character(0),
      optimization_attempts = NA
    ))
  }

  # Special features
  features <- c()
  if (!is.null(params$no_choice) && params$no_choice) {
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
  # Handle missing params/summary_info
  if (is.null(params)) {
    return(list(
      method = "unknown",
      d_error_prior = NA,
      d_error_null = NA,
      balance_score = NA,
      overlap_score = NA,
      profiles_used = length(unique(design$profileID[design$profileID != 0])),
      profiles_available = max(design$profileID, na.rm = TRUE)
    ))
  }

  return(list(
    method = params$method,
    d_error_prior = params$d_error_prior,
    d_error_null = params$d_error_null,
    balance_score = if (!is.null(summary_info)) {
      summary_info$efficiency$balance_score
    } else {
      NA
    },
    overlap_score = if (!is.null(summary_info)) {
      summary_info$efficiency$overlap_score
    } else {
      NA
    },
    profiles_used = if (!is.null(summary_info)) {
      summary_info$efficiency$profiles_used
    } else {
      NA
    },
    profiles_available = if (!is.null(summary_info)) {
      summary_info$efficiency$profiles_available
    } else {
      NA
    }
  ))
}

inspect_balance_section <- function(design, summary_info, verbose) {
  # Use pre-computed metrics if available
  if (
    !is.null(summary_info) && !is.null(summary_info$efficiency$balance_score)
  ) {
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
  if (
    !is.null(summary_info) && !is.null(summary_info$efficiency$overlap_score)
  ) {
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
  encoding <- attr(design, "encoding") %||% "standard"
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

  # Handle missing params
  no_choice <- if (!is.null(params)) {
    params$no_choice
  } else {
    ("no_choice" %in% names(design))
  }

  return(list(
    encoding = encoding,
    categorical_variables = categorical_variables,
    categorical_details = categorical_details,
    no_choice = no_choice
  ))
}

# UPDATE: compute_design_efficiency_metrics to convert to standard first
compute_design_efficiency_metrics <- function(design) {
  # Convert to standard encoding for accurate metrics
  design_standard <- get_standard_encoding(design)

  # Balance metrics
  balance_result <- compute_balance_metrics_internal(design_standard)

  # Overlap metrics
  overlap_result <- compute_overlap_metrics_internal(design_standard)

  return(list(
    balance_score = balance_result$overall_balance,
    balance_details = balance_result$balance_metrics,
    overlap_score = overlap_result$overall_overlap,
    overlap_details = overlap_result$overlap_metrics,
    profiles_used = length(unique(design_standard$profileID[
      design_standard$profileID != 0
    ])),
    profiles_available = max(design_standard$profileID, na.rm = TRUE)
  ))
}

# UPDATE: Internal function for balance computation
compute_balance_metrics_internal <- function(design) {
  # Ensure we're working with standard encoding
  design_standard <- get_standard_encoding(design)

  # Get attribute columns (exclude no_choice if present)
  atts <- setdiff(
    names(design_standard),
    c(
      "respID",
      "qID",
      "altID",
      "obsID",
      "profileID",
      "blockID",
      "no_choice",
      "prob"
    )
  )

  # Get counts of each individual attribute (handles NA from no-choice)
  counts <- lapply(atts, function(attr) {
    table(design_standard[[attr]], useNA = "no") # Exclude NA values
  })
  names(counts) <- atts

  # Calculate balance metrics for each attribute
  balance_metrics <- calculate_balance_metrics(counts)

  # Calculate overall balance score
  overall_balance <- mean(sapply(balance_metrics, function(x) {
    x$balance_score
  }))

  return(list(
    individual_counts = counts,
    balance_metrics = balance_metrics,
    overall_balance = overall_balance
  ))
}

# UPDATE: Internal function for overlap computation
compute_overlap_metrics_internal <- function(design) {
  # Ensure we're working with standard encoding
  design_standard <- get_standard_encoding(design)

  # Get attribute columns (exclude no_choice if present)
  atts <- setdiff(
    names(design_standard),
    c(
      "respID",
      "qID",
      "altID",
      "obsID",
      "profileID",
      "blockID",
      "no_choice",
      "prob"
    )
  )

  # Calculate overlap for each attribute
  overlap_counts <- lapply(atts, function(attr) {
    get_att_overlap_counts(attr, design_standard)
  })
  names(overlap_counts) <- atts

  # Calculate overlap metrics
  overlap_metrics <- calculate_overlap_metrics(overlap_counts, design_standard)

  # Calculate overall overlap score (average of complete overlap rates)
  overall_overlap <- mean(sapply(overlap_metrics, function(x) {
    x$complete_overlap_rate
  }))

  return(list(
    overlap_counts = overlap_counts,
    overlap_metrics = overlap_metrics,
    overall_overlap = overall_overlap
  ))
}

# UPDATE: inspect_balance_detailed
inspect_balance_detailed <- function(
  design,
  balance_details = NULL,
  verbose = FALSE
) {
  # Convert to standard encoding first
  design_standard <- get_standard_encoding(design)

  if (is.null(balance_details)) {
    # Compute balance metrics
    balance_result <- compute_balance_metrics_internal(design_standard)
    counts <- balance_result$individual_counts
    balance_metrics <- balance_result$balance_metrics
  } else {
    # Use pre-computed data - need to recompute counts for display
    atts <- setdiff(
      names(design_standard),
      c(
        "respID",
        "qID",
        "altID",
        "obsID",
        "profileID",
        "blockID",
        "no_choice",
        "prob"
      )
    )
    counts <- lapply(atts, function(attr) {
      table(design_standard[[attr]], useNA = "no")
    })
    names(counts) <- atts
    balance_metrics <- balance_details
  }

  return(list(
    individual_counts = counts,
    balance_metrics = balance_metrics,
    overall_balance = mean(sapply(balance_metrics, function(x) x$balance_score))
  ))
}

# UPDATE: inspect_overlap_detailed
inspect_overlap_detailed <- function(
  design,
  overlap_details = NULL,
  verbose = FALSE
) {
  # Convert to standard encoding first
  design_standard <- get_standard_encoding(design)

  if (is.null(overlap_details)) {
    # Compute overlap metrics
    overlap_result <- compute_overlap_metrics_internal(design_standard)
    overlap_counts <- overlap_result$overlap_counts
    overlap_metrics <- overlap_result$overlap_metrics
  } else {
    # Use pre-computed data - need to recompute counts for display
    atts <- setdiff(
      names(design_standard),
      c(
        "respID",
        "qID",
        "altID",
        "obsID",
        "profileID",
        "blockID",
        "no_choice",
        "prob"
      )
    )
    overlap_counts <- lapply(atts, function(attr) {
      get_att_overlap_counts(attr, design_standard)
    })
    names(overlap_counts) <- atts
    overlap_metrics <- overlap_details
  }

  total_questions <- max(design_standard$obsID, na.rm = TRUE)

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

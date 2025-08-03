context("Testing cbc_compare()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

# Create shared test fixtures
setup_comparison_test_data <- function() {
  profiles <- cbc_profiles(
    price = c(1, 2, 3, 4),
    type = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c("Poor", "Average", "Excellent")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  # Create different designs for comparison
  design_random <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 3,
    n_q = 6,
    n_resp = 50
  )

  design_shortcut <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "shortcut",
    n_alts = 3,
    n_q = 6,
    n_resp = 50
  )

  design_minoverlap <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "minoverlap",
    n_alts = 3,
    n_q = 6,
    n_resp = 50
  )

  list(
    profiles = profiles,
    priors = priors,
    design_random = design_random,
    design_shortcut = design_shortcut,
    design_minoverlap = design_minoverlap
  )
}

# Create optimal designs for testing (with D-errors)
setup_optimal_comparison_data <- function() {
  skip_if_not_installed("idefix")

  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.3)
  )

  design_stochastic <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "stochastic",
    n_alts = 2,
    n_q = 4,
    n_resp = 30,
    max_iter = 3,
    n_start = 2,
    use_idefix = FALSE
  )

  design_modfed <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "modfed",
    n_alts = 2,
    n_q = 4,
    n_resp = 30,
    max_iter = 3,
    n_start = 2,
    use_idefix = TRUE
  )

  list(
    profiles = profiles,
    priors = priors,
    design_stochastic = design_stochastic,
    design_modfed = design_modfed
  )
}

# =============================================================================
# VALIDATION HELPER FUNCTIONS
# =============================================================================

# Validate comparison object structure
validate_comparison_structure <- function(comparison, expected_designs) {
  # Basic class and structure
  expect_s3_class(comparison, "cbc_comparison")
  expect_s3_class(comparison, "list")

  # Required components
  expect_true("data" %in% names(comparison))
  expect_true("metrics_compared" %in% names(comparison))
  expect_true("sort_by" %in% names(comparison))
  expect_true("ascending" %in% names(comparison))
  expect_true("n_designs" %in% names(comparison))

  # Data should be a data frame
  expect_s3_class(comparison$data, "data.frame")

  # Should have correct number of designs
  expect_equal(comparison$n_designs, expected_designs)
  expect_equal(nrow(comparison$data), expected_designs)

  # Required columns
  expect_true("design_name" %in% names(comparison$data))
  expect_true("method" %in% names(comparison$data))

  # Metadata should be reasonable
  expect_type(comparison$metrics_compared, "character")
  expect_type(comparison$sort_by, "character")
  expect_type(comparison$ascending, "logical")
}

# Validate specific metric columns are present
validate_metric_columns <- function(comparison, metrics) {
  data <- comparison$data

  for (metric in metrics) {
    if (metric == "structure") {
      expect_true("respondents" %in% names(data))
      expect_true("questions" %in% names(data))
      expect_true("alternatives" %in% names(data))
      expect_true("profiles_used" %in% names(data))
    } else if (metric == "efficiency") {
      # At least one D-error column should be present for optimal methods
      has_d_error <- any(c("d_error_null", "d_error_prior") %in% names(data))
      if (any(data$method %in% c("stochastic", "modfed", "cea"))) {
        expect_true(has_d_error)
      }
    } else if (metric == "balance") {
      expect_true("balance_score" %in% names(data))
    } else if (metric == "overlap") {
      expect_true("overlap_score" %in% names(data))
    }
  }
}

# Validate sorting is correct
validate_sorting <- function(comparison) {
  if (comparison$sort_by == "none") {
    return() # No sorting to validate
  }

  data <- comparison$data
  sort_col <- comparison$sort_by

  # Map sort_by to actual column names
  if (sort_col == "d_error") {
    if ("d_error_prior" %in% names(data) && any(!is.na(data$d_error_prior))) {
      sort_col <- "d_error_prior"
    } else {
      sort_col <- "d_error_null"
    }
  } else if (sort_col == "balance") {
    sort_col <- "balance_score"
  } else if (sort_col == "overlap") {
    sort_col <- "overlap_score"
  }

  if (sort_col %in% names(data)) {
    values <- data[[sort_col]]
    values <- values[!is.na(values)] # Remove NAs for comparison

    if (length(values) > 1) {
      if (comparison$ascending) {
        expect_true(all(diff(values) >= 0))
      } else {
        expect_true(all(diff(values) <= 0))
      }
    }
  }
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Basic comparison with two designs works", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  comparison <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    sort_by = "none" # Avoid D-error sorting for non-optimal designs
  )

  validate_comparison_structure(comparison, 2)
  validate_metric_columns(
    comparison,
    c("structure", "efficiency", "balance", "overlap")
  )

  # Should have default metrics
  expect_setequal(
    comparison$metrics_compared,
    c("structure", "efficiency", "balance", "overlap")
  )

  # Should have specified sort
  expect_equal(comparison$sort_by, "none")
})

test_that("Named design comparison works", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  comparison <- cbc_compare(
    Random = test_data$design_random,
    Shortcut = test_data$design_shortcut,
    Minoverlap = test_data$design_minoverlap
  )

  validate_comparison_structure(comparison, 3)

  # Should use provided names
  expect_setequal(
    comparison$data$design_name,
    c("Random", "Shortcut", "Minoverlap")
  )

  # Should have corresponding methods
  expect_true("random" %in% comparison$data$method)
  expect_true("shortcut" %in% comparison$data$method)
  expect_true("minoverlap" %in% comparison$data$method)
})

test_that("Multiple designs comparison works", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  comparison <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    test_data$design_minoverlap
  )

  validate_comparison_structure(comparison, 3)

  # All designs should be represented
  expect_equal(nrow(comparison$data), 3)
  expect_setequal(comparison$data$method, c("random", "shortcut", "minoverlap"))
})

# =============================================================================
# METRIC SELECTION TESTS
# =============================================================================

test_that("Specific metric selection works", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  # Test structure only
  comparison_structure <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    metrics = "structure",
    sort_by = "none" # Avoid D-error sorting for non-optimal designs
  )

  validate_comparison_structure(comparison_structure, 2)
  validate_metric_columns(comparison_structure, "structure")
  expect_equal(comparison_structure$metrics_compared, "structure")

  # Test efficiency only
  comparison_efficiency <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    metrics = "efficiency",
    sort_by = "none" # Avoid D-error sorting for non-optimal designs
  )

  validate_comparison_structure(comparison_efficiency, 2)
  validate_metric_columns(comparison_efficiency, "efficiency")
  expect_equal(comparison_efficiency$metrics_compared, "efficiency")

  # Test multiple specific metrics
  comparison_multiple <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    metrics = c("balance", "overlap"),
    sort_by = "balance" # Sort by balance instead of D-error
  )

  validate_comparison_structure(comparison_multiple, 2)
  validate_metric_columns(comparison_multiple, c("balance", "overlap"))
  expect_setequal(comparison_multiple$metrics_compared, c("balance", "overlap"))
})

test_that("All metrics option works", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  comparison <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    metrics = "all",
    sort_by = "none" # Avoid D-error sorting for non-optimal designs
  )

  validate_comparison_structure(comparison, 2)
  expect_setequal(
    comparison$metrics_compared,
    c("structure", "efficiency", "balance", "overlap")
  )
})

# =============================================================================
# SORTING TESTS
# =============================================================================

test_that("Different sorting options work", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  # Test balance sorting
  comparison_balance <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    test_data$design_minoverlap,
    sort_by = "balance"
  )

  validate_comparison_structure(comparison_balance, 3)
  expect_equal(comparison_balance$sort_by, "balance")
  validate_sorting(comparison_balance)

  # Test overlap sorting
  comparison_overlap <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    sort_by = "overlap"
  )

  validate_comparison_structure(comparison_overlap, 2)
  expect_equal(comparison_overlap$sort_by, "overlap")
  validate_sorting(comparison_overlap)

  # Test no sorting
  comparison_none <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    sort_by = "none"
  )

  validate_comparison_structure(comparison_none, 2)
  expect_equal(comparison_none$sort_by, "none")
})

test_that("Ascending and descending sort work", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  # Test ascending (default for balance - higher is better)
  comparison_asc <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    sort_by = "balance",
    ascending = FALSE # Descending for balance (higher first)
  )

  validate_comparison_structure(comparison_asc, 2)
  expect_false(comparison_asc$ascending)
  validate_sorting(comparison_asc)

  # Test descending (for overlap - lower is better)
  comparison_desc <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    sort_by = "overlap",
    ascending = TRUE # Ascending for overlap (lower first)
  )

  validate_comparison_structure(comparison_desc, 2)
  expect_true(comparison_desc$ascending)
  validate_sorting(comparison_desc)
})

test_that("Default sort order is appropriate for metrics", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  # Test default for balance (should be descending - higher is better)
  comparison_balance <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    sort_by = "balance"
  )

  expect_false(comparison_balance$ascending) # Higher balance is better

  # Test default for overlap (should be ascending - lower is better)
  comparison_overlap <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    sort_by = "overlap"
  )

  expect_true(comparison_overlap$ascending) # Lower overlap is better
})

# =============================================================================
# D-ERROR COMPARISON TESTS
# =============================================================================

test_that("D-error comparison works with optimal designs", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_optimal_comparison_data()

  comparison <- cbc_compare(
    test_data$design_stochastic,
    test_data$design_modfed,
    metrics = "efficiency"
  )

  validate_comparison_structure(comparison, 2)
  validate_metric_columns(comparison, "efficiency")

  # Should have D-error columns
  expect_true(any(
    c("d_error_null", "d_error_prior") %in% names(comparison$data)
  ))

  # D-errors should be positive numbers
  if ("d_error_null" %in% names(comparison$data)) {
    d_errors <- comparison$data$d_error_null[
      !is.na(comparison$data$d_error_null)
    ]
    expect_true(all(d_errors > 0))
  }

  if ("d_error_prior" %in% names(comparison$data)) {
    d_errors <- comparison$data$d_error_prior[
      !is.na(comparison$data$d_error_prior)
    ]
    expect_true(all(d_errors > 0))
  }
})

test_that("D-error sorting prioritizes prior over null", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_optimal_comparison_data()

  comparison <- cbc_compare(
    test_data$design_stochastic,
    test_data$design_modfed,
    sort_by = "d_error"
  )

  validate_comparison_structure(comparison, 2)

  # If both d_error_prior and d_error_null exist, should sort by prior
  if (all(c("d_error_prior", "d_error_null") %in% names(comparison$data))) {
    # Check that data is sorted by d_error_prior when available
    prior_values <- comparison$data$d_error_prior[
      !is.na(comparison$data$d_error_prior)
    ]
    if (length(prior_values) > 1) {
      expect_true(all(diff(prior_values) >= 0)) # Should be ascending (lower is better)
    }
  }
})

# =============================================================================
# SPECIAL DESIGN FEATURES TESTS
# =============================================================================

test_that("No-choice designs are compared correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  priors_nochoice <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    quality = c("High" = 0.5),
    no_choice = -1.0
  )

  priors_regular <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    quality = c("High" = 0.5)
  )

  design_nochoice <- cbc_design(
    profiles = profiles,
    priors = priors_nochoice,
    method = "random",
    n_alts = 2,
    n_q = 4,
    n_resp = 20,
    no_choice = TRUE
  )

  design_regular <- cbc_design(
    profiles = profiles,
    priors = priors_regular,
    method = "random",
    n_alts = 2,
    n_q = 4,
    n_resp = 20,
    no_choice = FALSE
  )

  comparison <- cbc_compare(
    NoChoice = design_nochoice,
    Regular = design_regular
  )

  validate_comparison_structure(comparison, 2)

  # Should show no-choice information
  expect_true("no_choice" %in% names(comparison$data))
  expect_true(comparison$data$no_choice[
    comparison$data$design_name == "NoChoice"
  ])
  expect_false(comparison$data$no_choice[
    comparison$data$design_name == "Regular"
  ])
})

test_that("Blocked designs are compared correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not_installed("idefix")

  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.3)
  )

  design_single <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "stochastic",
    n_alts = 2,
    n_q = 4,
    n_resp = 20,
    n_blocks = 1,
    max_iter = 3,
    n_start = 2,
    use_idefix = FALSE
  )

  design_blocked <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "stochastic",
    n_alts = 2,
    n_q = 4,
    n_resp = 20,
    n_blocks = 2,
    max_iter = 3,
    n_start = 2,
    use_idefix = FALSE
  )

  comparison <- cbc_compare(
    Single = design_single,
    Blocked = design_blocked
  )

  validate_comparison_structure(comparison, 2)

  # Should show block information
  expect_true("blocks" %in% names(comparison$data))
  expect_equal(
    comparison$data$blocks[comparison$data$design_name == "Single"],
    1
  )
  expect_equal(
    comparison$data$blocks[comparison$data$design_name == "Blocked"],
    2
  )
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("Input validation works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  # Single design should error
  expect_error(
    cbc_compare(test_data$design_random),
    "At least 2 designs are required"
  )

  # Invalid design object
  expect_error(
    cbc_compare(test_data$design_random, "not_a_design"),
    "is not a cbc_design object"
  )

  # Invalid metrics
  expect_error(
    cbc_compare(
      test_data$design_random,
      test_data$design_shortcut,
      metrics = "invalid_metric"
    ),
    "Invalid metrics"
  )

  # Invalid sort_by
  expect_error(
    cbc_compare(
      test_data$design_random,
      test_data$design_shortcut,
      sort_by = "invalid_sort"
    ),
    "Invalid sort_by"
  )
})

test_that("Missing data is handled gracefully", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create designs that might have missing metrics
  profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  design1 <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  design2 <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  # Should handle comparison even with missing D-errors
  comparison <- cbc_compare(design1, design2)

  validate_comparison_structure(comparison, 2)

  # D-errors may be NA for random designs, but shouldn't error
  expect_s3_class(comparison, "cbc_comparison")
})

# =============================================================================
# PRINT METHOD TESTS
# =============================================================================

test_that("Print method works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  comparison <- cbc_compare(
    Random = test_data$design_random,
    Shortcut = test_data$design_shortcut
  )

  # Should print without error
  expect_output(print(comparison), "CBC Design Comparison")
  expect_output(print(comparison), "Designs compared: 2")
  expect_output(print(comparison), "Random")
  expect_output(print(comparison), "Shortcut")
})

test_that("Print method shows appropriate sections", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  # Structure metrics
  comparison_structure <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    metrics = "structure",
    sort_by = "none"
  )

  expect_output(print(comparison_structure), "Structure")

  # Design metrics
  comparison_metrics <- cbc_compare(
    test_data$design_random,
    test_data$design_shortcut,
    metrics = c("efficiency", "balance"),
    sort_by = "balance" # Sort by balance instead of D-error
  )

  expect_output(print(comparison_metrics), "Design Metrics")
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================

test_that("Comparison completes in reasonable time", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_comparison_test_data()

  # Should be fast for basic comparison
  expect_lt(
    system.time({
      cbc_compare(
        test_data$design_random,
        test_data$design_shortcut,
        test_data$design_minoverlap
      )
    })[["elapsed"]],
    3 # Should complete in under 3 seconds
  )
})

test_that("Large number of designs can be compared", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create multiple similar designs
  profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  designs <- list()
  for (i in 1:5) {
    designs[[i]] <- cbc_design(
      profiles = profiles,
      method = "random",
      n_alts = 2,
      n_q = 3,
      n_resp = 10
    )
  }

  # Should handle multiple designs
  comparison <- do.call(cbc_compare, designs)

  validate_comparison_structure(comparison, 5)
  expect_equal(nrow(comparison$data), 5)
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Comparison works with different design complexities", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Simple design
  simple_profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  simple_design <- cbc_design(
    profiles = simple_profiles,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  # Complex design
  complex_profiles <- cbc_profiles(
    price = c(1, 2, 3, 4, 5),
    type = c("A", "B", "C", "D"),
    quality = c("Low", "Medium", "High"),
    available = c("Yes", "No")
  )

  complex_priors <- cbc_priors(
    profiles = complex_profiles,
    price = -0.1,
    type = c("B" = 0.1, "C" = 0.2, "D" = 0.3),
    quality = c("Medium" = 0.1, "High" = 0.3),
    available = c("No" = -0.5)
  )

  complex_design <- cbc_design(
    profiles = complex_profiles,
    priors = complex_priors,
    method = "shortcut",
    n_alts = 3,
    n_q = 6,
    n_resp = 30
  )

  comparison <- cbc_compare(
    Simple = simple_design,
    Complex = complex_design
  )

  validate_comparison_structure(comparison, 2)

  # Should show different profile usage
  expect_true(
    comparison$data$profiles_used[comparison$data$design_name == "Simple"] <
      comparison$data$profiles_used[comparison$data$design_name == "Complex"]
  )
})

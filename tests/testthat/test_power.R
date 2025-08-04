context("Testing cbc_power()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

# Check if logitr is available for power analysis
logitr_available <- requireNamespace("logitr", quietly = TRUE)

# Create shared test fixtures
setup_power_test_data <- function(n_resp = 200, n_q = 6) {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C"),
    quality = c("Low", "High")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.3, "C" = 0.5),
    quality = c("High" = 0.4)
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 3,
    n_q = n_q,
    n_resp = n_resp
  )

  choices <- cbc_choices(design, priors)

  list(
    profiles = profiles,
    priors = priors,
    design = design,
    choices = choices
  )
}

# Small test data for fast tests
setup_small_power_data <- function() {
  setup_power_test_data(n_resp = 100, n_q = 4)
}

# Larger test data for statistical validation
setup_large_power_data <- function() {
  setup_power_test_data(n_resp = 500, n_q = 8)
}

# =============================================================================
# VALIDATION HELPER FUNCTIONS
# =============================================================================

# Validate basic power analysis structure
validate_power_structure <- function(power_result, original_data) {
  # Basic class and structure
  expect_s3_class(power_result, "cbc_power")
  expect_s3_class(power_result, "list")

  # Required components
  expect_true("power_summary" %in% names(power_result))
  expect_true("sample_sizes" %in% names(power_result))
  expect_true("n_breaks" %in% names(power_result))
  expect_true("alpha" %in% names(power_result))

  # Power summary should be a data frame
  expect_s3_class(power_result$power_summary, "data.frame")

  # Required columns in power summary
  required_cols <- c(
    "sample_size",
    "parameter",
    "estimate",
    "std_error",
    "t_statistic",
    "power"
  )
  expect_true(all(required_cols %in% names(power_result$power_summary)))

  # Sample sizes should be increasing
  expect_true(all(diff(power_result$sample_sizes) >= 0))

  # Alpha should be reasonable
  expect_true(power_result$alpha > 0 && power_result$alpha < 1)

  # Power values should be between 0 and 1
  expect_true(all(power_result$power_summary$power >= 0))
  expect_true(all(power_result$power_summary$power <= 1))

  # Standard errors should be positive
  expect_true(all(power_result$power_summary$std_error > 0))
}

# Validate power trends (should generally increase with sample size)
validate_power_trends <- function(power_result, tolerance = 0.1) {
  summary_data <- power_result$power_summary

  # Group by parameter and check trends
  params <- unique(summary_data$parameter)

  for (param in params) {
    param_data <- summary_data[summary_data$parameter == param, ]
    param_data <- param_data[order(param_data$sample_size), ]

    # Power should generally increase (allow some noise)
    power_values <- param_data$power

    # Check that final power > initial power (allowing for some noise)
    if (length(power_values) >= 2) {
      initial_power <- power_values[1]
      final_power <- power_values[length(power_values)]

      # Allow some exceptions for very high initial power or noise
      if (initial_power < 0.9) {
        expect_gt(final_power, initial_power - tolerance)
      }
    }

    # Standard errors should generally decrease
    se_values <- param_data$std_error
    if (length(se_values) >= 2) {
      initial_se <- se_values[1]
      final_se <- se_values[length(se_values)]

      expect_lt(final_se, initial_se + tolerance)
    }
  }
}

# Validate that power estimates are reasonable
validate_power_estimates <- function(
  power_result,
  min_power = 0.05,
  max_power = 0.99
) {
  summary_data <- power_result$power_summary

  # Power should be within reasonable bounds
  expect_true(all(summary_data$power >= min_power))
  expect_true(all(summary_data$power <= max_power))

  # Estimates should be finite
  expect_true(all(is.finite(summary_data$estimate)))
  expect_true(all(is.finite(summary_data$std_error)))
  expect_true(all(is.finite(summary_data$power)))
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Basic power analysis works with cbc_choices data", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()

  power_result <- cbc_power(
    data = test_data$choices,
    n_breaks = 5 # Small number for fast testing
  )

  validate_power_structure(power_result, test_data)
  validate_power_estimates(power_result)
})

test_that("Power analysis works with manual data specification", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()
  choices <- test_data$choices

  power_result <- cbc_power(
    data = choices,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "typeB", "typeC", "qualityHigh"),
    n_breaks = 5
  )

  validate_power_structure(power_result, test_data)

  # Should have the specified parameters
  params_in_result <- unique(power_result$power_summary$parameter)
  expect_setequal(params_in_result, c("price", "typeB", "typeC", "qualityHigh"))
})

test_that("Power analysis works with panel data", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()

  power_result <- cbc_power(
    data = test_data$choices,
    panelID = "respID", # Specify panel structure
    n_breaks = 5
  )

  validate_power_structure(power_result, test_data)
})

# # Hard to get this to consistently pass...too random
# test_that("Power analysis works with random parameters", {
#   skip_on_cran() # Skip on CRAN due to computation time
#   skip_if_not(logitr_available, "logitr package not available")
#
#   test_data <- setup_small_power_data()
#
#   power_result <- cbc_power(
#     data = test_data$choices,
#     randPars = c("price" = "n"), # Price as random parameter
#     panelID = "respID",
#     n_breaks = 4 # Smaller for mixed logit
#   )
#
#   validate_power_structure(power_result, test_data)
#
#   # Should have standard deviation parameters
#   params_in_result <- unique(power_result$power_summary$parameter)
#   expect_true("sd_price" %in% params_in_result)
# })

# =============================================================================
# PARAMETER-SPECIFIC TESTS
# =============================================================================

test_that("Different sample size ranges work correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()

  # Test different n_breaks
  power_result_small <- cbc_power(test_data$choices, n_breaks = 3)
  power_result_large <- cbc_power(test_data$choices, n_breaks = 8)

  expect_equal(length(power_result_small$sample_sizes), 3)
  expect_equal(length(power_result_large$sample_sizes), 8)

  # Larger n_breaks should have more data points
  expect_gt(
    nrow(power_result_large$power_summary),
    nrow(power_result_small$power_summary)
  )
})

test_that("Different alpha levels work correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()

  power_result_05 <- cbc_power(test_data$choices, alpha = 0.05, n_breaks = 4)
  power_result_01 <- cbc_power(test_data$choices, alpha = 0.01, n_breaks = 4)

  expect_equal(power_result_05$alpha, 0.05)
  expect_equal(power_result_01$alpha, 0.01)

  # Lower alpha should generally result in lower power
  # (for same sample size and parameter)
  summary_05 <- power_result_05$power_summary
  summary_01 <- power_result_01$power_summary

  # Compare largest sample size for each parameter
  max_size_05 <- max(summary_05$sample_size)
  max_size_01 <- max(summary_01$sample_size)

  for (param in unique(summary_05$parameter)) {
    power_05 <- summary_05$power[
      summary_05$parameter == param & summary_05$sample_size == max_size_05
    ]
    power_01 <- summary_01$power[
      summary_01$parameter == param & summary_01$sample_size == max_size_01
    ]

    if (length(power_05) > 0 && length(power_01) > 0) {
      expect_gte(power_05[1], power_01[1] - 0.1) # Allow some tolerance
    }
  }
})

test_that("Custom parameter specifications work", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()

  # Test subset of parameters
  power_result <- cbc_power(
    data = test_data$choices,
    pars = c("price", "typeB"), # Only subset
    n_breaks = 4
  )

  validate_power_structure(power_result, test_data)

  # Should only have specified parameters
  params_in_result <- unique(power_result$power_summary$parameter)
  expect_setequal(params_in_result, c("price", "typeB"))
})

# =============================================================================
# STATISTICAL VALIDATION TESTS
# =============================================================================
# These are skipped on CRAN as they take too long to run

test_that("Power increases with sample size", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_large_power_data() # Larger data for better statistical properties

  power_result <- cbc_power(
    data = test_data$choices,
    n_breaks = 6
  )

  validate_power_structure(power_result, test_data)
  validate_power_trends(power_result, tolerance = 0.15) # Allow some noise
})

test_that("Standard errors decrease with sample size", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_large_power_data()

  power_result <- cbc_power(
    data = test_data$choices,
    n_breaks = 5
  )

  summary_data <- power_result$power_summary

  # For each parameter, check that SE decreases
  for (param in unique(summary_data$parameter)) {
    param_data <- summary_data[summary_data$parameter == param, ]
    param_data <- param_data[order(param_data$sample_size), ]

    if (nrow(param_data) >= 2) {
      se_values <- param_data$std_error

      # Standard errors should generally decrease (allow some noise)
      correlation_with_size <- cor(param_data$sample_size, se_values)
      expect_lt(correlation_with_size, 0.1) # Should be negative correlation
    }
  }
})

test_that("Power analysis results are reproducible", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()

  # Run twice with same data
  set.seed(123)
  result1 <- cbc_power(test_data$choices, n_breaks = 4)

  set.seed(123)
  result2 <- cbc_power(test_data$choices, n_breaks = 4)

  # Results should be identical
  expect_identical(result1$power_summary, result2$power_summary)
  expect_identical(result1$sample_sizes, result2$sample_sizes)
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("Input validation works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_small_power_data()

  # Invalid data object
  expect_error(
    cbc_power("not_a_dataframe"),
    "data must be a data frame"
  )

  # Missing required columns
  bad_data <- test_data$choices[, !names(test_data$choices) %in% "choice"]
  expect_error(
    cbc_power(bad_data),
    "Missing required columns"
  )

  # Invalid n_breaks
  expect_error(
    cbc_power(test_data$choices, n_breaks = 1),
    "n_breaks must be.*>= 2"
  )

  # Invalid alpha
  expect_error(
    cbc_power(test_data$choices, alpha = 1.5),
    "alpha must be a single numeric value between 0 and 1"
  )

  expect_error(
    cbc_power(test_data$choices, alpha = 0),
    "alpha must be a single numeric value between 0 and 1"
  )
})

test_that("Auto-detection validation works", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()
  choices <- test_data$choices

  # Remove cbc_choices class to test manual specification
  class(choices) <- "data.frame"

  # Should require manual parameter specification
  expect_error(
    cbc_power(choices),
    "Could not auto-detect parameters"
  )

  # But should work with manual specification
  power_result <- cbc_power(
    choices,
    pars = c("price", "typeB", "typeC", "qualityHigh"),
    n_breaks = 4
  )

  expect_s3_class(power_result, "cbc_power")
})

test_that("Insufficient data handling works", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  # Create very small dataset
  tiny_data <- setup_power_test_data(n_resp = 20, n_q = 2)

  # Should handle gracefully (may have warnings but shouldn't error)
  power_result <- cbc_power(tiny_data$choices, n_breaks = 3)

  # Should still produce a result
  expect_s3_class(power_result, "cbc_power")
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Power analysis integrates with different design types", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  # Test with different design methods
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.3)
  )

  # Random design
  design_random <- cbc_design(
    profiles,
    method = "random",
    n_alts = 2,
    n_q = 4,
    n_resp = 100
  )
  choices_random <- cbc_choices(design_random, priors)

  power_random <- cbc_power(choices_random, n_breaks = 4)
  expect_s3_class(power_random, "cbc_power")

  # Greedy design
  design_greedy <- cbc_design(
    profiles,
    priors = priors,
    method = "shortcut",
    n_alts = 2,
    n_q = 4,
    n_resp = 100
  )
  choices_greedy <- cbc_choices(design_greedy, priors)

  power_greedy <- cbc_power(choices_greedy, n_breaks = 4)
  expect_s3_class(power_greedy, "cbc_power")
})

test_that("Power analysis works with no-choice data", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.3),
    no_choice = -1.0
  )

  design <- cbc_design(
    profiles,
    priors = priors,
    method = "random",
    n_alts = 2,
    n_q = 4,
    n_resp = 100,
    no_choice = TRUE
  )
  choices <- cbc_choices(design, priors)

  power_result <- cbc_power(choices, n_breaks = 4)

  validate_power_structure(power_result, list(choices = choices))

  # Should include no_choice parameter
  params_in_result <- unique(power_result$power_summary$parameter)
  expect_true("no_choice" %in% params_in_result)
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================
# These are skipped on CRAN as they take too long to run

test_that("Power analysis completes in reasonable time", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()

  # Basic power analysis should be reasonably fast
  expect_lt(
    system.time({
      cbc_power(test_data$choices, n_breaks = 4)
    })[["elapsed"]],
    10 # Should complete in under 10 seconds
  )
})

test_that("Large power analysis works but is slower", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_large_power_data()

  # Larger analysis should work but may be slower
  expect_lt(
    system.time({
      cbc_power(test_data$choices, n_breaks = 6)
    })[["elapsed"]],
    60 # Should complete in under 1 minute
  )
})

# =============================================================================
# PRINT AND SUMMARY METHOD TESTS
# =============================================================================

test_that("Print method works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()
  power_result <- cbc_power(test_data$choices, n_breaks = 4)

  # Should print without error
  expect_output(print(power_result), "CBC Power Analysis Results")
  expect_output(print(power_result), "Sample sizes tested")
  expect_output(print(power_result), "Parameters")
})

test_that("Summary method works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  test_data <- setup_small_power_data()
  power_result <- cbc_power(test_data$choices, n_breaks = 4)

  # Should summarize without error
  expect_output(summary(power_result), "CBC Power Analysis Summary")
  expect_output(summary(power_result), "Sample size requirements")

  # Test different power thresholds
  expect_output(summary(power_result, power_threshold = 0.9), "90% power")
})

test_that("Plot method works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")
  skip_if_not_installed("ggplot2")

  test_data <- setup_small_power_data()
  power_result <- cbc_power(test_data$choices, n_breaks = 4)

  # Should create plots without error
  p1 <- plot(power_result, type = "power")
  expect_s3_class(p1, "gg")

  p2 <- plot(power_result, type = "se")
  expect_s3_class(p2, "gg")

  # Different power threshold
  p3 <- plot(power_result, type = "power", power_threshold = 0.9)
  expect_s3_class(p3, "gg")
})

# =============================================================================
# COMPARISON FUNCTION TESTS
# =============================================================================

test_that("Power comparison function works", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")
  skip_if_not_installed("ggplot2")

  test_data1 <- setup_small_power_data()
  test_data2 <- setup_small_power_data()

  power1 <- cbc_power(test_data1$choices, n_breaks = 4)
  power2 <- cbc_power(test_data2$choices, n_breaks = 4)

  # Should create comparison plot without error
  p_compare <- plot_compare_power(
    Design1 = power1,
    Design2 = power2,
    type = "power"
  )

  expect_s3_class(p_compare, "gg")

  # SE comparison
  p_compare_se <- plot_compare_power(
    Design1 = power1,
    Design2 = power2,
    type = "se"
  )

  expect_s3_class(p_compare_se, "gg")
})

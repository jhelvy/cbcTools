context("Testing cbc_design()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

# Create shared test fixtures to avoid repeated profile creation
setup_test_profiles <- function() {
  cbc_profiles(
    price = c(1, 2, 3),
    type = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c("Poor", "Average", "Excellent"),
    weight = c(0.5, 1.0, 1.5) # Continuous variable
  )
}

# Small test parameters for speed
fast_params <- list(
  n_alts = 3,
  n_q = 6,
  n_resp = 10,
  max_iter = 3,
  n_start = 2
)

# Create different prior types for testing
setup_test_priors <- function(profiles, type = "simple") {
  switch(
    type,
    "simple" = cbc_priors(
      profiles = profiles,
      price = -0.1,
      type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      freshness = c("Average" = 0.1, "Excellent" = 0.4),
      weight = 0.05
    ),
    "nochoice" = cbc_priors(
      profiles = profiles,
      price = -0.1,
      type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      freshness = c("Average" = 0.1, "Excellent" = 0.4),
      weight = 0.05,
      no_choice = -1.0
    ),
    "interactions" = cbc_priors(
      profiles = profiles,
      price = -0.1,
      type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      freshness = c("Average" = 0.1, "Excellent" = 0.4),
      weight = 0.05,
      interactions = list(
        int_spec(
          between = c("price", "type"),
          with_level = "Gala",
          value = 0.1
        ),
        int_spec(
          between = c("type", "freshness"),
          level = "Gala",
          with_level = "Average",
          value = 0.2
        )
      )
    ),
    "random" = cbc_priors(
      profiles = profiles,
      price = rand_spec("n", mean = -0.1, sd = 0.05),
      type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      freshness = c("Average" = 0.1, "Excellent" = 0.4),
      weight = 0.05,
      n_draws = 20 # Small for testing
    )
  )
}

# =============================================================================
# DESIGN VALIDATION HELPERS
# =============================================================================

# Comprehensive design validation function
validate_design_structure <- function(design, expected_params) {
  params <- attr(design, "design_params")
  summary_info <- attr(design, "design_summary")

  # Basic class and structure
  expect_s3_class(design, "cbc_design")
  expect_s3_class(design, "data.frame")

  # Required columns
  required_cols <- c("profileID", "respID", "qID", "altID", "obsID")
  if (params$n_blocks > 1) {
    required_cols <- c(required_cols, "blockID")
  }
  expect_true(all(required_cols %in% names(design)))

  # Dimensions
  expected_rows <- expected_params$n_resp *
    expected_params$n_q *
    (expected_params$n_alts + ifelse(expected_params$no_choice, 1, 0))
  expect_equal(nrow(design), expected_rows)

  # Parameter consistency
  expect_equal(params$n_q, expected_params$n_q)
  expect_equal(params$n_alts, expected_params$n_alts)
  expect_equal(params$n_resp, expected_params$n_resp)
  expect_equal(params$no_choice, expected_params$no_choice)

  # Metadata presence
  expect_type(summary_info, "list")
  expect_true("n_profiles_used" %in% names(summary_info))
  expect_true("profile_usage_rate" %in% names(summary_info))

  # ID column integrity
  expect_true(all(design$respID %in% 1:expected_params$n_resp))
  expect_true(all(design$qID %in% 1:expected_params$n_q))
  expect_equal(max(design$obsID), expected_params$n_resp * expected_params$n_q)

  # No duplicate profiles within questions
  for (obs in unique(design$obsID)) {
    obs_profiles <- design$profileID[design$obsID == obs]
    obs_profiles <- obs_profiles[obs_profiles != 0] # Exclude no-choice
    expect_equal(
      length(obs_profiles),
      length(unique(obs_profiles)),
      info = paste("Duplicate profiles in observation", obs)
    )
  }
}

# Validate no-choice specific features
validate_nochoice_features <- function(design) {
  if (attr(design, "design_params")$no_choice) {
    # Should have no-choice rows
    expect_true(any(design$profileID == 0))
    expect_true("no_choice" %in% names(design))
    expect_true(all(design$no_choice[design$profileID == 0] == 1))
    expect_true(all(design$no_choice[design$profileID != 0] == 0))
  } else {
    # Should not have no-choice features
    expect_false(any(design$profileID == 0))
    expect_false("no_choice" %in% names(design))
  }
}

# Validate D-error calculations for optimal methods
validate_d_errors <- function(design, method) {
  params <- attr(design, "design_params")

  if (method %in% c("stochastic", "modfed", "cea")) {
    # Should have null D-error
    expect_true(!is.null(params$d_error_null))
    expect_true(is.numeric(params$d_error_null))
    expect_true(is.finite(params$d_error_null))
    expect_true(params$d_error_null > 0)

    # Should have prior D-error if priors were used
    if (!is.null(attr(design, "priors"))) {
      expect_true(!is.null(params$d_error_prior))
      expect_true(is.numeric(params$d_error_prior))
      expect_true(is.finite(params$d_error_prior))
      expect_true(params$d_error_prior > 0)
    }
  }
}

# Validate interaction handling
validate_interactions <- function(design, has_interactions) {
  params <- attr(design, "design_params")

  if (has_interactions) {
    expect_true(params$has_interactions)
    expect_true(params$n_interactions > 0)
  } else {
    expect_false(params$has_interactions %||% FALSE)
    expect_equal(params$n_interactions %||% 0, 0)
  }
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Random design with no priors works", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp
  )

  expected <- list(
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    no_choice = FALSE
  )

  validate_design_structure(design, expected)
  validate_nochoice_features(design)
  expect_equal(attr(design, "design_params")$method, "random")
})

test_that("Random design with no-choice works", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    no_choice = TRUE
  )

  expected <- list(
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    no_choice = TRUE
  )

  validate_design_structure(design, expected)
  validate_nochoice_features(design)
})

# =============================================================================
# METHOD-SPECIFIC TESTS
# =============================================================================
# These are skipped on CRAN as they take too long to run

test_that("Greedy methods work with simple priors", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "simple")

  methods <- c("shortcut", "minoverlap", "balanced")

  for (method in methods) {
    design <- cbc_design(
      profiles = profiles,
      priors = priors,
      method = method,
      n_alts = fast_params$n_alts,
      n_q = fast_params$n_q,
      n_resp = fast_params$n_resp
    )

    expected <- list(
      n_alts = fast_params$n_alts,
      n_q = fast_params$n_q,
      n_resp = fast_params$n_resp,
      no_choice = FALSE
    )

    validate_design_structure(design, expected)
    expect_equal(attr(design, "design_params")$method, method)
  }
})

test_that("Optimal methods work with simple priors", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not_installed("idefix")

  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "simple")

  methods <- c("stochastic", "modfed", "cea")

  for (method in methods) {
    design <- cbc_design(
      profiles = profiles,
      priors = priors,
      method = method,
      n_alts = fast_params$n_alts,
      n_q = fast_params$n_q,
      n_resp = fast_params$n_resp,
      max_iter = fast_params$max_iter,
      n_start = fast_params$n_start,
      use_idefix = TRUE
    )

    expected <- list(
      n_alts = fast_params$n_alts,
      n_q = fast_params$n_q,
      n_resp = fast_params$n_resp,
      no_choice = FALSE
    )

    validate_design_structure(design, expected)
    validate_d_errors(design, method)
    expect_equal(attr(design, "design_params")$method, method)
  }
})

# =============================================================================
# FEATURE-SPECIFIC TESTS
# =============================================================================

test_that("No-choice option works across methods", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "nochoice")

  # Test different methods
  methods <- c("random", "shortcut", "stochastic")

  for (method in methods) {
    # Skip stochastic if idefix not available
    if (method == "stochastic" && !requireNamespace("idefix", quietly = TRUE)) {
      skip("idefix not available")
    }

    design <- cbc_design(
      profiles = profiles,
      priors = if (method == "random") NULL else priors,
      method = method,
      n_alts = fast_params$n_alts,
      n_q = fast_params$n_q,
      n_resp = fast_params$n_resp,
      no_choice = TRUE,
      max_iter = if (method == "stochastic") fast_params$max_iter else NULL,
      n_start = if (method == "stochastic") fast_params$n_start else NULL
    )

    expected <- list(
      n_alts = fast_params$n_alts,
      n_q = fast_params$n_q,
      n_resp = fast_params$n_resp,
      no_choice = TRUE
    )

    validate_design_structure(design, expected)
    validate_nochoice_features(design)
  }
})

test_that("Interaction terms work correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not_installed("idefix")

  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "interactions")

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "stochastic",
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    max_iter = fast_params$max_iter,
    n_start = fast_params$n_start,
    use_idefix = FALSE # Use cbcTools implementation for more control
  )

  expected <- list(
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    no_choice = FALSE
  )

  validate_design_structure(design, expected)
  validate_interactions(design, TRUE)
})

test_that("Random parameters work correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not_installed("idefix")

  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "random")

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "stochastic",
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    max_iter = fast_params$max_iter,
    n_start = fast_params$n_start,
    use_idefix = FALSE
  )

  expected <- list(
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    no_choice = FALSE
  )

  validate_design_structure(design, expected)
  validate_d_errors(design, "stochastic")
})

test_that("Blocking works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not_installed("idefix")

  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "simple")

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "stochastic",
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    n_blocks = 2,
    max_iter = fast_params$max_iter,
    n_start = fast_params$n_start,
    use_idefix = FALSE
  )

  # Should have blockID column
  expect_true("blockID" %in% names(design))
  expect_true(all(design$blockID %in% 1:2))
  expect_equal(attr(design, "design_params")$n_blocks, 2)
})

# =============================================================================
# LABELED DESIGN TESTS
# =============================================================================

test_that("Labeled designs work correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create profiles with label attribute
  labeled_profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B"),
    brand = c("X", "Y") # This will be our label
  )

  priors <- cbc_priors(
    profiles = labeled_profiles,
    price = -0.1,
    type = c("B" = 0.2),
    brand = c("Y" = 0.3)
  )

  design <- cbc_design(
    profiles = labeled_profiles,
    priors = priors,
    method = "random",
    n_alts = 2, # Must match number of label levels
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp,
    label = "brand"
  )

  # Validate that each question has one profile from each brand
  for (obs in unique(design$obsID)) {
    obs_data <- design[design$obsID == obs, ]
    obs_profiles <- obs_data$profileID[obs_data$profileID != 0]
    profile_brands <- labeled_profiles$brand[
      labeled_profiles$profileID %in% obs_profiles
    ]
    expect_equal(length(unique(profile_brands)), 2)
  }

  expect_equal(attr(design, "design_params")$label, "brand")
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("Input validation works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()

  # Invalid method
  expect_error(
    cbc_design(profiles, method = "invalid"),
    "method must be one of"
  )

  # n_alts too large
  expect_error(
    cbc_design(profiles, method = "random", n_alts = 100, n_q = 4, n_resp = 5),
    "n_alts.*cannot be larger"
  )

  # Invalid no_choice without priors
  priors_no_nochoice <- setup_test_priors(profiles, "simple")
  expect_error(
    cbc_design(
      profiles,
      priors = priors_no_nochoice,
      method = "stochastic",
      n_alts = 2,
      n_q = 4,
      n_resp = 5,
      no_choice = TRUE
    ),
    "no_choice.*requires priors"
  )
})

# =============================================================================
# PERFORMANCE REGRESSION TESTS
# =============================================================================

test_that("Design generation completes in reasonable time", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "simple")

  # Random should be very fast
  expect_lt(
    system.time({
      cbc_design(profiles, method = "random", n_alts = 2, n_q = 6, n_resp = 10)
    })[["elapsed"]],
    2 # Should complete in under 2 seconds
  )

  # Greedy methods should be reasonably fast
  expect_lt(
    system.time({
      cbc_design(
        profiles,
        priors = priors,
        method = "shortcut",
        n_alts = 2,
        n_q = 6,
        n_resp = 10
      )
    })[["elapsed"]],
    5 # Should complete in under 5 seconds
  )
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Design integrates properly with cbc_choices", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "simple")

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp
  )

  # Should work with choice simulation
  choices <- cbc_choices(design, priors)

  expect_s3_class(choices, "cbc_choices")
  expect_equal(nrow(choices), nrow(design))
  expect_true("choice" %in% names(choices))
  expect_true(all(choices$choice %in% c(0, 1)))
})

test_that("Design works with cbc_inspect", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()
  priors <- setup_test_priors(profiles, "simple")

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "shortcut",
    n_alts = fast_params$n_alts,
    n_q = fast_params$n_q,
    n_resp = fast_params$n_resp
  )

  # Should work with inspection
  inspection <- cbc_inspect(design)

  expect_s3_class(inspection, "cbc_inspection")
  expect_true("structure" %in% names(inspection))
})

# =============================================================================
# BALANCE_BY FUNCTIONALITY TESTS
# =============================================================================

test_that("balance_by basic functionality works", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create profiles with attribute-specific feature (like EV range)
  profiles <- cbc_profiles(
    price = c(15, 20, 25),
    fuelEconomy = c(20, 25, 30),
    powertrain = c('gas', 'hybrid', 'electric'),
    range_electric = c(0, 100, 150, 200, 250)
  ) |>
    cbc_restrict(
      (powertrain == 'electric') & (range_electric == 0),
      (powertrain != 'electric') & (range_electric != 0)
    )

  # Design without balance_by
  design_unbalanced <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 3,
    n_q = 6,
    n_resp = 10
  ) |>
    cbc_decode()

  # Design with balance_by
  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 3,
    n_q = 6,
    n_resp = 10,
    balance_by = "powertrain"
  ) |>
    cbc_decode()

  expect_s3_class(design, "cbc_design")
  expect_true("powertrain" %in% names(design))

  # Check that all powertrain types are represented
  powertrain_counts_unbalanced <- table(design_unbalanced$powertrain)
  powertrain_counts <- table(design$powertrain)
  expect_equal(length(powertrain_counts), 3)
  expect_true(all(names(powertrain_counts) %in% c("gas", "hybrid", "electric")))

  # Check that balance is better than without balance_by
  # (This is a qualitative check - exact balance depends on sampling)
  expect_true(min(powertrain_counts) > 0)
  gap_unbalanced <- max(powertrain_counts_unbalanced) -
    min(powertrain_counts_unbalanced)
  gap_balanced <- max(powertrain_counts) - min(powertrain_counts)
  expect_true(gap_unbalanced - gap_balanced > 0)
})

test_that("balance_by with multiple attributes works", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create profiles with unbalanced attribute combinations
  # Similar to vehicle example but with multiple attributes
  profiles <- cbc_profiles(
    price = c(15, 20, 25),
    type = c('Fuji', 'Gala', 'Honeycrisp'),
    freshness = c('Poor', 'Average', 'Excellent'),
    organic = c('No', 'Yes')
  ) |>
    cbc_restrict(
      # Create imbalance: Honeycrisp apples are rarely poor quality
      (type == 'Honeycrisp') & (freshness == 'Poor'),
      # Fuji apples are rarely organic
      (type == 'Fuji') & (organic == 'Yes')
    )

  design_unbalanced <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 3,
    n_q = 6,
    n_resp = 10
  ) |>
    cbc_decode()

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 3,
    n_q = 6,
    n_resp = 10,
    balance_by = c("type", "freshness")
  ) |>
    cbc_decode()

  expect_s3_class(design, "cbc_design")

  # Check that multiple attributes are balanced
  combo_counts_unbalanced <- table(
    design_unbalanced$type,
    design_unbalanced$freshness
  )
  combo_counts <- table(design$type, design$freshness)
  expect_true(nrow(combo_counts) > 1)
  expect_true(ncol(combo_counts) > 1)
  gap_unbalanced <- max(combo_counts_unbalanced) -
    min(combo_counts_unbalanced)
  gap_balanced <- max(combo_counts) - min(combo_counts)
  expect_true(gap_unbalanced - gap_balanced > 0)
})

test_that("balance_by validation works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_test_profiles()

  # Test invalid attribute name
  expect_error(
    cbc_design(
      profiles = profiles,
      method = "random",
      n_alts = 3,
      n_q = 6,
      n_resp = 10,
      balance_by = "nonexistent_attr"
    ),
    "balance_by attributes not found in profiles"
  )

  # Test profileID in balance_by
  expect_error(
    cbc_design(
      profiles = profiles,
      method = "random",
      n_alts = 3,
      n_q = 6,
      n_resp = 10,
      balance_by = c("type", "profileID")
    ),
    "balance_by cannot include 'profileID'"
  )
})

test_that("balance_by conflicts with label", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- cbc_profiles(
    price = c(1, 2),
    brand = c("A", "B", "C")
  )

  expect_error(
    cbc_design(
      profiles = profiles,
      method = "random",
      n_alts = 3,
      n_q = 6,
      n_resp = 10,
      label = "brand",
      balance_by = "price"
    ),
    "Cannot use both 'label' and 'balance_by' arguments simultaneously"
  )
})

test_that("balance_by handles edge cases gracefully", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Test with single-valued attribute (should warn or error)
  profiles_single <- cbc_profiles(
    price = c(1, 2, 3),
    constant_attr = c("same", "same", "same"),
    type = c("A", "B", "C")
  )

  expect_error(
    cbc_design(
      profiles = profiles_single,
      method = "random",
      n_alts = 3,
      n_q = 6,
      n_resp = 10,
      balance_by = "constant_attr"
    ),
    "balance_by attributes create only one group"
  )
})

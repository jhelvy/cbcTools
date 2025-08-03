context("Testing cbc_choices()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

# Check if logitr is available for parameter recovery tests
logitr_available <- requireNamespace("logitr", quietly = TRUE)

# Create shared test fixtures
setup_choice_test_profiles <- function() {
  cbc_profiles(
    price = c(1, 1.5, 2, 2.5, 3),
    type = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c("Poor", "Average", "Excellent")
  )
}

# Parameters for parameter recovery tests (larger sample sizes)
recovery_params <- list(
  n_alts = 3,
  n_q = 8,
  n_resp = 1000 # Larger for better parameter recovery
)

# Parameters for basic functionality tests (smaller, faster)
basic_params <- list(
  n_alts = 3,
  n_q = 4,
  n_resp = 500
)

# =============================================================================
# VALIDATION HELPER FUNCTIONS
# =============================================================================

# Validate basic choice data structure
validate_choice_structure <- function(choices, design, has_nochoice = FALSE) {
  # Basic class and structure
  expect_s3_class(choices, "cbc_choices")
  expect_s3_class(choices, "data.frame")

  # Should have same number of rows as design
  expect_equal(nrow(choices), nrow(design))

  # Required columns
  expect_true("choice" %in% names(choices))
  expect_true(all(choices$choice %in% c(0, 1)))

  # Should have exactly one choice per observation
  choice_counts <- tapply(choices$choice, choices$obsID, sum)
  expect_true(all(choice_counts == 1))

  # If no-choice, should have no_choice column
  if (has_nochoice) {
    expect_true("no_choice" %in% names(choices))
    expect_true(all(choices$no_choice %in% c(0, 1)))
  }

  # Should preserve all design columns
  design_cols <- setdiff(names(design), "choice")
  expect_true(all(design_cols %in% names(choices)))
}

# Statistical test for parameter recovery
expect_parameter_recovery <- function(
  true_params,
  estimated_params,
  tolerance = 0.15,
  method = "relative"
) {
  # Ensure same parameter names and order
  common_names <- intersect(names(true_params), names(estimated_params))
  expect_true(
    length(common_names) > 0
  )

  true_vals <- true_params[common_names]
  est_vals <- estimated_params[common_names]

  for (param_name in common_names) {
    true_val <- true_vals[param_name]
    est_val <- est_vals[param_name]

    if (method == "relative") {
      # Relative error test
      if (abs(true_val) > 0.01) {
        # Avoid division by very small numbers
        rel_error <- abs((est_val - true_val) / true_val)
        expect_lt(
          rel_error,
          tolerance
        )
      } else {
        # Absolute error for near-zero parameters
        abs_error <- abs(est_val - true_val)
        expect_lt(
          abs_error,
          tolerance
        )
      }
    } else if (method == "absolute") {
      abs_error <- abs(est_val - true_val)
      expect_lt(
        abs_error,
        tolerance
      )
    }
  }
}

# Test random choice generation (should have roughly equal choice probabilities)
expect_random_choices <- function(choices, tolerance = 0.1) {
  choice_rates <- tapply(choices$choice, choices$altID, mean)
  expected_rate <- 1 / length(unique(choices$altID))

  for (alt in names(choice_rates)) {
    rate_diff <- abs(choice_rates[alt] - expected_rate)
    expect_lt(
      rate_diff,
      tolerance
    )
  }
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Random choices work without priors", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = basic_params$n_alts,
    n_q = basic_params$n_q,
    n_resp = basic_params$n_resp
  )

  choices <- cbc_choices(design)

  validate_choice_structure(choices, design)
  expect_random_choices(choices, tolerance = 0.2) # More lenient for small sample

  # Check choice_info metadata
  choice_info <- attr(choices, "choice_info")
  expect_equal(choice_info$simulation_method, "random")
  expect_false(choice_info$priors_used)
})

test_that("Utility-based choices work with priors", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = basic_params$n_alts,
    n_q = basic_params$n_q,
    n_resp = basic_params$n_resp
  )

  choices <- cbc_choices(design, priors)

  validate_choice_structure(choices, design)

  # Check choice_info metadata
  choice_info <- attr(choices, "choice_info")
  expect_equal(choice_info$simulation_method, "utility_based")
  expect_true(choice_info$priors_used)
})

test_that("No-choice option works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    no_choice = -1.5
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    priors = priors,
    n_alts = basic_params$n_alts,
    n_q = basic_params$n_q,
    n_resp = basic_params$n_resp,
    no_choice = TRUE
  )

  choices <- cbc_choices(design, priors)

  validate_choice_structure(choices, design, has_nochoice = TRUE)

  # Check that some no-choice options are selected (given negative utility)
  nochoice_rate <- mean(choices$choice[choices$no_choice == 1])
  expect_gt(nochoice_rate, 0) # Should have some no-choice selections
  expect_lt(nochoice_rate, 1) # But not all
})

# =============================================================================
# PARAMETER RECOVERY TESTS
# =============================================================================

test_that("Fixed parameters can be recovered accurately", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  profiles <- setup_choice_test_profiles()

  true_priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = recovery_params$n_alts,
    n_q = recovery_params$n_q,
    n_resp = recovery_params$n_resp
  )

  choices <- cbc_choices(design, true_priors)

  # Estimate model
  model <- logitr::logitr(
    data = choices,
    outcome = 'choice',
    obsID = 'obsID',
    pars = c(
      "price",
      "typeGala",
      "typeHoneycrisp",
      "freshnessAverage",
      "freshnessExcellent"
    )
  )

  # Test parameter recovery
  expect_parameter_recovery(
    true_priors$pars,
    stats::coef(model),
    tolerance = 0.15, # No further off then 0.15 in coefficient
    method = "absolute"
  )
})

test_that("Random parameters can be recovered accurately", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  profiles <- setup_choice_test_profiles()

  true_priors <- cbc_priors(
    profiles = profiles,
    price = rand_spec("n", mean = -0.1, sd = 0.03),
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3), # Keep some fixed
    freshness = rand_spec(
      "n",
      mean = c("Average" = 0.1, "Excellent" = 0.4),
      sd = c("Average" = 0.05, "Excellent" = 0.08)
    ),
    n_draws = 100
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = recovery_params$n_alts,
    n_q = recovery_params$n_q,
    n_resp = recovery_params$n_resp * 2 # More data for mixed logit
  )

  choices <- cbc_choices(design, true_priors)

  # Estimate mixed logit model
  model <- logitr::logitr(
    data = choices,
    outcome = 'choice',
    obsID = 'obsID',
    pars = c(
      "price",
      "typeGala",
      "typeHoneycrisp",
      "freshnessAverage",
      "freshnessExcellent"
    ),
    randPars = c(price = "n", freshnessAverage = "n", freshnessExcellent = "n"),
    panelID = "respID"
  )

  # Test mean parameter recovery
  estimated_coefs <- stats::coef(model)

  # Build true coefficient vector for comparison
  true_means <- c(
    price = true_priors$attrs$price$mean,
    typeGala = true_priors$attrs$type$mean["Gala"],
    typeHoneycrisp = true_priors$attrs$type$mean["Honeycrisp"],
    freshnessAverage = true_priors$attrs$freshness$mean["Average"],
    freshnessExcellent = true_priors$attrs$freshness$mean["Excellent"]
  )

  expect_parameter_recovery(
    true_means,
    estimated_coefs[names(true_means)],
    tolerance = 0.2, # More lenient for mixed logit
    method = "absolute"
  )

  # Test standard deviation recovery (more lenient)
  true_sds <- c(
    priceSd = true_priors$attrs$price$sd,
    freshnessAverageSd = true_priors$attrs$freshness$sd["Average"],
    freshnessExcellentSd = true_priors$attrs$freshness$sd["Excellent"]
  )

  sd_names <- paste0(
    "sd_",
    c("price", "freshnessAverage", "freshnessExcellent")
  )
  estimated_sds <- abs(estimated_coefs[sd_names])
  names(estimated_sds) <- names(true_sds)

  expect_parameter_recovery(
    true_sds,
    estimated_sds,
    tolerance = 0.3, # Even more lenient for SDs
    method = "absolute"
  )
})

test_that("No-choice parameters can be recovered accurately", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  profiles <- setup_choice_test_profiles()

  true_priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    no_choice = -1.2
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    priors = true_priors,
    n_alts = recovery_params$n_alts,
    n_q = recovery_params$n_q,
    n_resp = recovery_params$n_resp,
    no_choice = TRUE
  )

  choices <- cbc_choices(design, true_priors)

  # Estimate model with no-choice
  model <- logitr::logitr(
    data = choices,
    outcome = 'choice',
    obsID = 'obsID',
    pars = c(
      "price",
      "typeGala",
      "typeHoneycrisp",
      "freshnessAverage",
      "freshnessExcellent",
      "no_choice"
    )
  )

  expect_parameter_recovery(
    true_priors$pars,
    stats::coef(model),
    tolerance = 0.15,
    method = "absolute"
  )
})

test_that("Interaction parameters can be recovered accurately", {
  skip_on_cran() # Skip on CRAN due to computation time
  skip_if_not(logitr_available, "logitr package not available")

  # Simpler profiles for interactions
  profiles_int <- cbc_profiles(
    price = c(1, 2, 3),
    meat = c("Fish", "Steak"),
    wine = c("White", "Red")
  )

  true_priors <- cbc_priors(
    profiles = profiles_int,
    price = -0.1,
    meat = c("Steak" = 0.5),
    wine = c("Red" = 0.4),
    interactions = list(
      int_spec(
        between = c("meat", "wine"),
        level = "Steak",
        with_level = "Red",
        value = 1.2
      )
    )
  )

  design <- cbc_design(
    profiles = profiles_int,
    method = "random",
    priors = true_priors,
    n_alts = 3,
    n_q = 8,
    n_resp = 600 # More data for interaction recovery
  )

  choices <- cbc_choices(design, true_priors)

  # Estimate model with interaction
  model <- logitr::logitr(
    data = choices,
    outcome = 'choice',
    obsID = 'obsID',
    pars = c("price", "meatSteak", "wineRed", "meatSteak*wineRed")
  )

  # Build true parameter vector including interaction
  true_params_with_int <- true_priors$pars
  names(true_params_with_int) <- c(
    "price",
    "meatSteak",
    "wineRed",
    "meatSteak:wineRed"
  )

  estimated_coefs <- stats::coef(model)
  names(estimated_coefs) <- gsub("\\*", ":", names(estimated_coefs)) # logitr uses * but returns :

  expect_parameter_recovery(
    true_params_with_int,
    estimated_coefs,
    tolerance = 0.2, # More lenient for interactions
    method = "absolute"
  )
})

# =============================================================================
# EDGE CASES AND ERROR HANDLING
# =============================================================================

test_that("Different priors warning works", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()

  # Create design with one set of priors
  design_priors <- cbc_priors(
    profiles = profiles,
    price = -0.2,
    type = c("Gala" = 0.1, "Honeycrisp" = 0.2),
    freshness = c("Average" = 0.0, "Excellent" = 0.3)
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    priors = design_priors,
    n_alts = basic_params$n_alts,
    n_q = basic_params$n_q,
    n_resp = basic_params$n_resp
  )

  # Use different priors for choice simulation
  choice_priors <- cbc_priors(
    profiles = profiles,
    price = -0.1, # Different!
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  expect_warning(
    cbc_choices(design, choice_priors),
    "Different priors used"
  )
})

test_that("Invalid inputs are caught", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = basic_params$n_alts,
    n_q = basic_params$n_q,
    n_resp = basic_params$n_resp
  )

  # Invalid design object
  expect_error(
    cbc_choices("not_a_design"),
    "design must be a cbc_design object"
  )

  # Invalid priors object
  expect_error(
    cbc_choices(design, priors = "not_priors"),
    "priors must be a cbc_priors object"
  )
})

test_that("Choice consistency across multiple runs", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Test that choice simulation is deterministic given same seed
  profiles <- setup_choice_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = basic_params$n_alts,
    n_q = basic_params$n_q,
    n_resp = basic_params$n_resp
  )

  # Should get different results with different random seeds
  set.seed(123)
  choices1 <- cbc_choices(design, priors)

  set.seed(456)
  choices2 <- cbc_choices(design, priors)

  # Shouldn't be identical (very low probability)
  expect_false(identical(choices1$choice, choices2$choice))

  # But should get same results with same seed
  set.seed(123)
  choices3 <- cbc_choices(design, priors)

  expect_identical(choices1$choice, choices3$choice)
})

# =============================================================================
# PERFORMANCE AND INTEGRATION TESTS
# =============================================================================

test_that("Choice simulation completes in reasonable time", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 3,
    n_q = 6,
    n_resp = 100
  )

  # Should complete quickly
  expect_lt(
    system.time({
      cbc_choices(design, priors)
    })[["elapsed"]],
    2 # Should complete in under 2 seconds
  )
})

test_that("Choices work with different design methods", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()
  skip_on_cran() # Skip on CRAN due to computation time

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  methods <- c("random", "shortcut")

  for (method in methods) {
    design <- cbc_design(
      profiles = profiles,
      method = method,
      priors = if (method == "random") NULL else priors,
      n_alts = basic_params$n_alts,
      n_q = basic_params$n_q,
      n_resp = basic_params$n_resp
    )

    choices <- cbc_choices(design, priors)
    validate_choice_structure(choices, design)
  }
})

test_that("Random choices have expected statistical properties", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- setup_choice_test_profiles()

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 3,
    n_q = 10,
    n_resp = 200 # Larger sample for statistical test
  )

  choices <- cbc_choices(design) # Random choices

  # Test that choice rates are approximately equal across alternatives
  choice_rates <- tapply(choices$choice, choices$altID, mean)
  expected_rate <- 1 / 3

  # Chi-square test for equal probabilities
  choice_counts <- tapply(choices$choice, choices$altID, sum)
  total_choices <- sum(choice_counts)
  expected_counts <- rep(total_choices / 3, 3)

  chi_sq_stat <- sum((choice_counts - expected_counts)^2 / expected_counts)
  p_value <- 1 - stats::pchisq(chi_sq_stat, df = 2)

  # Should not reject null hypothesis of equal probabilities
  expect_gt(p_value, 0.05)
})

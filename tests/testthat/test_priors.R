context("Testing cbc_priors()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

# Create shared test profiles
setup_priors_test_profiles <- function() {
  cbc_profiles(
    price = c(1, 2, 3, 4),
    type = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c("Poor", "Average", "Excellent"),
    weight = c(0.5, 1.0, 1.5) # Continuous variable
  )
}

# Simple profiles for specific tests
setup_simple_profiles <- function() {
  cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )
}

# =============================================================================
# VALIDATION HELPER FUNCTIONS
# =============================================================================

# Validate basic priors structure
validate_priors_structure <- function(priors, profiles, expected_attrs = NULL) {
  # Basic class and structure
  expect_s3_class(priors, "cbc_priors")
  expect_s3_class(priors, "list")

  # Required components
  expect_true("attrs" %in% names(priors))
  expect_true("pars" %in% names(priors))
  expect_true("profiles_metadata" %in% names(priors))

  # Metadata consistency
  expect_type(priors$profiles_metadata, "list")
  expect_true("n_profiles" %in% names(priors$profiles_metadata))
  expect_equal(priors$profiles_metadata$n_profiles, nrow(profiles))

  # Parameter vector should be numeric
  expect_type(priors$pars, "double")
  expect_true(length(priors$pars) > 0)
  expect_true(all(is.finite(priors$pars)))

  # Check expected attributes if provided
  if (!is.null(expected_attrs)) {
    attr_names <- setdiff(names(profiles), "profileID")
    if (!("no_choice" %in% expected_attrs)) {
      expect_setequal(names(priors$attrs), attr_names)
    } else {
      expect_setequal(names(priors$attrs), c(attr_names, "no_choice"))
    }
  }
}

# Validate attribute-specific information
validate_attribute_info <- function(
  attr_info,
  attr_name,
  expected_type,
  expected_levels = NULL
) {
  expect_type(attr_info, "list")

  # Required fields
  expect_true("continuous" %in% names(attr_info))
  expect_true("random" %in% names(attr_info))
  expect_true("mean" %in% names(attr_info))

  # Type consistency
  if (expected_type == "continuous") {
    expect_true(attr_info$continuous)
    expect_type(attr_info$mean, "double")
    expect_length(attr_info$mean, 1)
  } else {
    expect_false(attr_info$continuous)
    expect_true("levels" %in% names(attr_info))
    if (!is.null(expected_levels)) {
      expect_equal(attr_info$levels, expected_levels)
    }
  }
}

# Validate random parameter specifications
validate_random_specs <- function(priors, attr_name) {
  attr_info <- priors$attrs[[attr_name]]

  expect_true(attr_info$random)
  expect_true("dist" %in% names(attr_info))
  expect_true("sd" %in% names(attr_info))
  expect_true(attr_info$dist %in% c("n", "ln", "cn"))

  # Should have parameter draws
  expect_true(!is.null(priors$par_draws))
  expect_true(is.matrix(priors$par_draws))
  expect_true(ncol(priors$par_draws) >= 1)
}

# Validate interaction specifications
validate_interactions <- function(priors, expected_interactions) {
  if (is.null(expected_interactions)) {
    expect_null(priors$interactions)
  } else {
    expect_equal(length(priors$interactions), length(expected_interactions))

    for (i in seq_along(expected_interactions)) {
      int <- priors$interactions[[i]]
      expected <- expected_interactions[[i]]

      expect_s3_class(int, "cbc_interaction")
      expect_equal(int$attr1, expected$attr1)
      expect_equal(int$attr2, expected$attr2)
      expect_equal(int$value, expected$value)
    }
  }
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Simple fixed priors work correctly", {
  profiles <- setup_simple_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    quality = c("High" = 0.5) # Named vector
  )

  validate_priors_structure(priors, profiles)

  # Check specific attribute info
  validate_attribute_info(priors$attrs$price, "price", "continuous")
  validate_attribute_info(
    priors$attrs$quality,
    "quality",
    "categorical",
    c("Low", "High")
  )

  # Check parameter values
  expect_equal(priors$attrs$price$mean, -0.1)
  expect_equal(priors$attrs$quality$mean[["High"]], 0.5)
  expect_false(priors$attrs$price$random)
  expect_false(priors$attrs$quality$random)

  # Should not have parameter draws for fixed parameters
  expect_null(priors$par_draws)
})

test_that("Unnamed categorical priors work correctly", {
  profiles <- setup_priors_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c(0.2, 0.3), # Unnamed - should use natural order
    freshness = c(0.1, 0.4),
    weight = 0.05
  )

  validate_priors_structure(priors, profiles)

  # Check that levels are assigned correctly (first level is reference)
  type_levels <- profiles$type
  if (is.factor(type_levels)) {
    expected_levels <- levels(type_levels)
  } else {
    expected_levels <- unique(type_levels)
  }

  expect_equal(names(priors$attrs$type$mean), expected_levels[-1]) # All but first
  expect_equal(unname(priors$attrs$type$mean), c(0.2, 0.3))
})

test_that("No-choice priors work correctly", {
  profiles <- setup_simple_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    quality = c("High" = 0.5),
    no_choice = -1.5
  )

  validate_priors_structure(
    priors,
    profiles,
    expected_attrs = c("price", "quality", "no_choice")
  )

  # Check no-choice attribute
  expect_true("no_choice" %in% names(priors$attrs))
  expect_equal(priors$attrs$no_choice$mean, -1.5)
  expect_true(priors$attrs$no_choice$continuous)
  expect_false(priors$attrs$no_choice$random)
  expect_true(priors$has_no_choice)

  # Should be included in parameter vector
  expect_true("no_choice" %in% names(priors$pars))
  expect_equal(priors$pars[["no_choice"]], -1.5)
})

# =============================================================================
# RANDOM PARAMETER TESTS
# =============================================================================

test_that("Random parameters work correctly", {
  profiles <- setup_priors_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = rand_spec("n", mean = -0.1, sd = 0.05),
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3), # Fixed
    freshness = rand_spec(
      "n",
      mean = c("Average" = 0.1, "Excellent" = 0.4),
      sd = c("Average" = 0.05, "Excellent" = 0.08)
    ),
    weight = 0.05, # Fixed
    n_draws = 100
  )

  validate_priors_structure(priors, profiles)
  validate_random_specs(priors, "price")
  validate_random_specs(priors, "freshness")

  # Check that fixed parameters are not random
  expect_false(priors$attrs$type$random)
  expect_false(priors$attrs$weight$random)

  # Check parameter draws dimensions
  expect_equal(nrow(priors$par_draws), 100)
  expect_true(ncol(priors$par_draws) >= 5) # At least 5 parameters

  # Check that random parameter names are in par_draws columns
  expect_true("price" %in% colnames(priors$par_draws))
  expect_true("freshnessAverage" %in% colnames(priors$par_draws))
  expect_true("freshnessExcellent" %in% colnames(priors$par_draws))
})

test_that("Different random distributions work", {
  profiles <- setup_simple_profiles()

  # Test different distributions
  priors_normal <- cbc_priors(
    profiles = profiles,
    price = rand_spec("n", mean = -0.1, sd = 0.05),
    quality = c("High" = 0.5)
  )

  priors_lognormal <- cbc_priors(
    profiles = profiles,
    price = rand_spec("ln", mean = -0.1, sd = 0.05),
    quality = c("High" = 0.5)
  )

  priors_censored <- cbc_priors(
    profiles = profiles,
    price = rand_spec("cn", mean = -0.1, sd = 0.05),
    quality = c("High" = 0.5)
  )

  expect_equal(priors_normal$attrs$price$dist, "n")
  expect_equal(priors_lognormal$attrs$price$dist, "ln")
  expect_equal(priors_censored$attrs$price$dist, "cn")

  # All should have parameter draws
  expect_false(is.null(priors_normal$par_draws))
  expect_false(is.null(priors_lognormal$par_draws))
  expect_false(is.null(priors_censored$par_draws))
})

test_that("Random no-choice parameter works", {
  profiles <- setup_simple_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    quality = c("High" = 0.5),
    no_choice = rand_spec("n", mean = -1.5, sd = 0.3),
    n_draws = 50
  )

  validate_priors_structure(
    priors,
    profiles,
    expected_attrs = c("price", "quality", "no_choice")
  )
  expect_true(priors$attrs$no_choice$random)
  expect_equal(priors$attrs$no_choice$dist, "n")
  expect_equal(priors$attrs$no_choice$mean, -1.5)
  expect_equal(priors$attrs$no_choice$sd, 0.3)

  # Should have parameter draws including no_choice
  expect_true("no_choice" %in% colnames(priors$par_draws))
})

# =============================================================================
# CORRELATION TESTS
# =============================================================================

test_that("Simple correlations work correctly", {
  profiles <- setup_priors_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = rand_spec(
      "n",
      mean = -0.1,
      sd = 0.05,
      correlations = list(cor_spec(with = "weight", value = 0.3))
    ),
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    weight = rand_spec("n", mean = 0.05, sd = 0.02),
    n_draws = 100
  )

  validate_priors_structure(priors, profiles)

  # Should have correlation matrix
  expect_false(is.null(priors$correlation))
  expect_true(is.matrix(priors$correlation))
  expect_equal(nrow(priors$correlation), ncol(priors$correlation))

  # Check that correlation is specified correctly
  cor_matrix <- priors$correlation
  price_idx <- which(rownames(cor_matrix) == "price")
  weight_idx <- which(rownames(cor_matrix) == "weight")

  expect_equal(cor_matrix[price_idx, weight_idx], 0.3)
  expect_equal(cor_matrix[weight_idx, price_idx], 0.3) # Symmetric
})

test_that("Categorical correlations work correctly", {
  profiles <- setup_priors_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = rand_spec(
      "n",
      mean = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      sd = c("Gala" = 0.1, "Honeycrisp" = 0.15),
      correlations = list(
        cor_spec(
          with = "type",
          level = "Gala",
          with_level = "Honeycrisp",
          value = 0.4
        )
      )
    ),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    weight = 0.05,
    n_draws = 100
  )

  validate_priors_structure(priors, profiles)

  # Should have correlation matrix
  expect_false(is.null(priors$correlation))

  # Check categorical correlation
  cor_matrix <- priors$correlation
  gala_idx <- which(rownames(cor_matrix) == "typeGala")
  honeycrisp_idx <- which(rownames(cor_matrix) == "typeHoneycrisp")

  expect_equal(cor_matrix[gala_idx, honeycrisp_idx], 0.4)
})

test_that("Mixed type correlations work correctly", {
  profiles <- setup_priors_test_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = rand_spec(
      "n",
      mean = -0.1,
      sd = 0.05,
      correlations = list(
        cor_spec(with = "type", with_level = "Gala", value = 0.25)
      )
    ),
    type = rand_spec(
      "n",
      mean = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      sd = c("Gala" = 0.1, "Honeycrisp" = 0.15)
    ),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    weight = 0.05,
    n_draws = 100
  )

  validate_priors_structure(priors, profiles)

  # Check mixed correlation (continuous with categorical level)
  cor_matrix <- priors$correlation
  price_idx <- which(rownames(cor_matrix) == "price")
  gala_idx <- which(rownames(cor_matrix) == "typeGala")

  expect_equal(cor_matrix[price_idx, gala_idx], 0.25)
})

# =============================================================================
# INTERACTION TESTS
# =============================================================================

test_that("Simple interactions work correctly", {
  profiles <- setup_priors_test_profiles()

  interactions <- list(
    int_spec(between = c("price", "weight"), value = 0.1),
    int_spec(between = c("price", "type"), with_level = "Gala", value = 0.15)
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    weight = 0.05,
    interactions = interactions
  )

  validate_priors_structure(priors, profiles)
  validate_interactions(priors, interactions)

  # Check that interaction parameters are in parameter vector
  expect_true(any(grepl(":", names(priors$pars))))
})

test_that("Categorical-categorical interactions work", {
  profiles <- setup_priors_test_profiles()

  interactions <- list(
    int_spec(
      between = c("type", "freshness"),
      level = "Gala",
      with_level = "Average",
      value = 0.3
    )
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    weight = 0.05,
    interactions = interactions
  )

  validate_priors_structure(priors, profiles)
  validate_interactions(priors, interactions)
})

test_that("Complex interaction combinations work", {
  profiles <- setup_priors_test_profiles()

  interactions <- list(
    # Continuous * continuous
    int_spec(between = c("price", "weight"), value = 0.08),
    # Continuous * categorical
    int_spec(between = c("price", "type"), with_level = "Gala", value = 0.12),
    int_spec(
      between = c("price", "type"),
      with_level = "Honeycrisp",
      value = 0.15
    ),
    # Categorical * categorical
    int_spec(
      between = c("type", "freshness"),
      level = "Gala",
      with_level = "Average",
      value = 0.2
    )
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    weight = 0.05,
    interactions = interactions
  )

  validate_priors_structure(priors, profiles)
  validate_interactions(priors, interactions)

  # Should have multiple interaction terms in parameter vector
  interaction_params <- names(priors$pars)[grepl(":", names(priors$pars))]
  expect_true(length(interaction_params) >= length(interactions))
})

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("Input validation works correctly", {
  profiles <- setup_simple_profiles()

  # Invalid profiles object
  expect_error(
    cbc_priors("not_profiles", price = -0.1),
    "profiles must be a cbc_profiles object"
  )

  # Missing attribute specifications
  expect_error(
    cbc_priors(profiles, price = -0.1), # Missing quality
    "Missing prior specifications for attributes"
  )

  # Extra attribute specifications
  expect_error(
    cbc_priors(
      profiles,
      price = -0.1,
      quality = c("High" = 0.5),
      fake_attr = 0.1
    ),
    "Prior specifications provided for non-existent attributes"
  )

  # Wrong number of levels for categorical
  expect_error(
    cbc_priors(profiles, price = -0.1, quality = c(0.1, 0.2, 0.3)), # Too many
    "Incorrect number of values"
  )

  # Invalid level names
  expect_error(
    cbc_priors(profiles, price = -0.1, quality = c("Medium" = 0.5)), # Invalid level
    "Invalid level.*provided"
  )
})

test_that("Random parameter validation works", {
  profiles <- setup_simple_profiles()

  # Invalid distribution
  expect_error(
    cbc_priors(
      profiles,
      price = rand_spec("invalid", mean = -0.1, sd = 0.05),
      quality = c("High" = 0.5)
    ),
    "dist must be one of"
  )

  # Mismatched names for categorical random parameters
  expect_error(
    cbc_priors(
      profiles,
      price = -0.1,
      quality = rand_spec("n", mean = c("High" = 0.5), sd = c("Medium" = 0.1))
    ), # Wrong name
    "Names for mean and sd must match"
  )
})

test_that("Interaction validation works", {
  profiles <- setup_priors_test_profiles()

  # Interaction with non-existent attribute
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.1,
      type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      freshness = c("Average" = 0.1, "Excellent" = 0.4),
      weight = 0.05,
      interactions = list(
        int_spec(between = c("price", "fake_attr"), value = 0.1)
      )
    ),
    "Interaction references non-existent attributes"
  )

  # Interaction with random parameter
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = rand_spec("n", mean = -0.1, sd = 0.05), # Random
      type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      freshness = c("Average" = 0.1, "Excellent" = 0.4),
      weight = 0.05,
      interactions = list(
        int_spec(between = c("price", "weight"), value = 0.1) # price is random
      )
    ),
    "Interactions with random parameters not supported"
  )

  # Missing level specification for categorical interaction
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.1,
      type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
      freshness = c("Average" = 0.1, "Excellent" = 0.4),
      weight = 0.05,
      interactions = list(
        int_spec(between = c("type", "freshness"), value = 0.1) # Missing levels
      )
    ),
    "must specify both level and with_level"
  )
})

test_that("Correlation validation works", {
  profiles <- setup_simple_profiles()

  # Correlation with non-random parameter
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = rand_spec(
        "n",
        mean = -0.1,
        sd = 0.05,
        correlations = list(cor_spec(with = "quality", value = 0.3))
      ),
      quality = c("High" = 0.5) # Not random
    ),
    "it must be a random parameter"
  )

  # Invalid correlation value
  expect_error(
    cor_spec(with = "quality", value = 1.5), # > 1
    "Correlation value must be between -1 and 1"
  )

  expect_error(
    cor_spec(with = "quality", value = -1.5), # < -1
    "Correlation value must be between -1 and 1"
  )
})

# =============================================================================
# INTEGRATION AND COMPATIBILITY TESTS
# =============================================================================

test_that("Priors work with different profile structures", {
  # Test with different numbers of levels
  profiles_binary <- cbc_profiles(
    price = c(1, 2),
    choice = c("A", "B")
  )

  priors_binary <- cbc_priors(
    profiles = profiles_binary,
    price = -0.1,
    choice = c("B" = 0.5)
  )

  validate_priors_structure(priors_binary, profiles_binary)

  # Test with many levels
  profiles_many <- cbc_profiles(
    rating = c(1, 2, 3, 4, 5, 6, 7),
    category = c("A", "B")
  )

  priors_many <- cbc_priors(
    profiles = profiles_many,
    rating = -0.1,
    category = c("B" = 0.3)
  )

  validate_priors_structure(priors_many, profiles_many)
})

test_that("Priors work with cbc_design integration", {
  profiles <- setup_simple_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    quality = c("High" = 0.5)
  )

  # Should work with design creation
  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 2,
    n_q = 4,
    n_resp = 10
  )

  expect_s3_class(design, "cbc_design")
})

test_that("Parameter draws have correct statistical properties", {
  profiles <- setup_simple_profiles()

  priors <- cbc_priors(
    profiles = profiles,
    price = rand_spec("n", mean = -0.1, sd = 0.05),
    quality = c("High" = 0.5),
    n_draws = 1000 # Large sample for statistical test
  )

  # Check that draws have approximately correct mean and sd
  price_draws <- priors$par_draws[, "price"]

  expect_lt(abs(mean(price_draws) - (-0.1)), 0.01) # Mean should be close
  expect_lt(abs(sd(price_draws) - 0.05), 0.01) # SD should be close

  # Should be approximately normal
  # Shapiro-Wilk test (small sample) or visual check for larger samples
  if (length(price_draws) <= 5000) {
    p_value <- shapiro.test(price_draws)$p.value
    expect_gt(p_value, 0.01) # Don't reject normality at 1% level
  }
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================

test_that("Prior creation completes in reasonable time", {
  profiles <- setup_priors_test_profiles()

  # Simple priors should be very fast
  expect_lt(
    system.time({
      cbc_priors(
        profiles = profiles,
        price = -0.1,
        type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
        freshness = c("Average" = 0.1, "Excellent" = 0.4),
        weight = 0.05
      )
    })[["elapsed"]],
    1 # Should complete in under 1 second
  )

  # Random priors with draws should still be reasonably fast
  expect_lt(
    system.time({
      cbc_priors(
        profiles = profiles,
        price = rand_spec("n", mean = -0.1, sd = 0.05),
        type = rand_spec(
          "n",
          mean = c("Gala" = 0.2, "Honeycrisp" = 0.3),
          sd = c("Gala" = 0.1, "Honeycrisp" = 0.15)
        ),
        freshness = c("Average" = 0.1, "Excellent" = 0.4),
        weight = 0.05,
        n_draws = 500
      )
    })[["elapsed"]],
    3 # Should complete in under 3 seconds
  )
})

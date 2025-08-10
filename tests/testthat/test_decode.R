context("Testing cbc_decode()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

# Create shared test fixtures
setup_decode_test_data <- function() {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c("Poor", "Average", "Excellent")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4)
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 3,
    n_q = 4,
    n_resp = 20
  )

  choices <- cbc_choices(design, priors)

  list(
    profiles = profiles,
    priors = priors,
    design = design,
    choices = choices
  )
}

# Test data with no-choice (should not be decodable)
setup_nochoice_test_data <- function() {
  profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    quality = c("High" = 0.5),
    no_choice = -1.0
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10,
    no_choice = TRUE
  )

  choices <- cbc_choices(design, priors)

  list(
    profiles = profiles,
    priors = priors,
    design = design,
    choices = choices
  )
}

# =============================================================================
# VALIDATION HELPER FUNCTIONS
# =============================================================================

# Validate decoded data structure
validate_decoded_structure <- function(decoded_data, original_data, profiles) {
  # Should preserve basic structure
  expect_s3_class(decoded_data, class(original_data))
  expect_equal(nrow(decoded_data), nrow(original_data))

  # Should have same ID columns
  id_cols <- c("profileID", "respID", "qID", "altID", "obsID")
  if ("blockID" %in% names(original_data)) {
    id_cols <- c(id_cols, "blockID")
  }
  if ("choice" %in% names(original_data)) {
    id_cols <- c(id_cols, "choice")
  }

  for (id_col in id_cols) {
    if (id_col %in% names(original_data)) {
      expect_true(id_col %in% names(decoded_data))
      expect_identical(decoded_data[[id_col]], original_data[[id_col]])
    }
  }

  # Should not be dummy coded anymore
  expect_false(attr(decoded_data, "is_dummy_coded") %||% TRUE)

  # Should have categorical variables restored
  categorical_structure <- attr(decoded_data, "categorical_structure")
  if (!is.null(categorical_structure)) {
    categorical_vars <- names(categorical_structure)[
      sapply(categorical_structure, function(x) x$is_categorical)
    ]

    for (var in categorical_vars) {
      if (var %in% names(decoded_data)) {
        expect_true(
          is.factor(decoded_data[[var]]) || is.character(decoded_data[[var]])
        )

        # Check that levels match expected levels
        expected_levels <- categorical_structure[[var]]$levels
        actual_levels <- if (is.factor(decoded_data[[var]])) {
          levels(decoded_data[[var]])
        } else {
          unique(decoded_data[[var]])
        }
        expect_setequal(actual_levels, expected_levels)
      }
    }
  }
}

# Validate that dummy columns are removed
validate_dummy_removal <- function(decoded_data, original_profiles) {
  # Get original categorical attributes (exclude profileID and numeric columns)
  attr_names <- names(original_profiles)
  categorical_attrs <- attr_names[
    sapply(original_profiles, function(x) !is.numeric(x)) &
      attr_names != "profileID"
  ]

  # Check that dummy-coded columns are removed
  for (attr in categorical_attrs) {
    if (attr %in% names(original_profiles)) {
      levels <- unique(original_profiles[[attr]])
      if (length(levels) > 1) {
        # Dummy columns would be like "attrLevel"
        for (level in levels[-1]) {
          # Skip reference level
          dummy_col <- paste0(attr, level)
          expect_false(dummy_col %in% names(decoded_data))
        }
      }
    }
  }
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Basic design decoding works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_decode_test_data()
  design <- test_data$design

  # Should start as dummy coded
  expect_true(attr(design, "is_dummy_coded") %||% TRUE)

  # Decode the design
  decoded_design <- cbc_decode(design)

  validate_decoded_structure(decoded_design, design, test_data$profiles)
  validate_dummy_removal(decoded_design, test_data$profiles)

  # Should have categorical variables
  expect_true("type" %in% names(decoded_design))
  expect_true("freshness" %in% names(decoded_design))
  expect_true(
    is.factor(decoded_design$type) || is.character(decoded_design$type)
  )
  expect_true(
    is.factor(decoded_design$freshness) ||
      is.character(decoded_design$freshness)
  )

  # Should not have dummy columns
  expect_false("typeGala" %in% names(decoded_design))
  expect_false("typeHoneycrisp" %in% names(decoded_design))
  expect_false("freshnessAverage" %in% names(decoded_design))
  expect_false("freshnessExcellent" %in% names(decoded_design))
})

test_that("Choice data decoding works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_decode_test_data()
  choices <- test_data$choices

  # Should start as dummy coded
  expect_true(attr(choices, "is_dummy_coded") %||% TRUE)

  # Decode the choices
  decoded_choices <- cbc_decode(choices)

  validate_decoded_structure(decoded_choices, choices, test_data$profiles)
  validate_dummy_removal(decoded_choices, test_data$profiles)

  # Should preserve choice column
  expect_true("choice" %in% names(decoded_choices))
  expect_identical(decoded_choices$choice, choices$choice)

  # Should have categorical variables
  expect_true("type" %in% names(decoded_choices))
  expect_true("freshness" %in% names(decoded_choices))
})

test_that("Already decoded data returns unchanged with message", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_decode_test_data()
  design <- test_data$design

  # First decode
  decoded_design <- cbc_decode(design)

  # Second decode should return same data with message
  expect_message(
    decoded_again <- cbc_decode(decoded_design),
    "Data is already in categorical format"
  )

  # Should be identical
  expect_identical(decoded_design, decoded_again)
})

test_that("Continuous variables are preserved correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_decode_test_data()
  design <- test_data$design

  decoded_design <- cbc_decode(design)

  # Price should remain numeric and unchanged
  expect_true("price" %in% names(decoded_design))
  expect_type(decoded_design$price, "double")
  expect_identical(decoded_design$price, design$price)
})

test_that("Factor level ordering is preserved", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create profiles with specific ordering
  profiles <- cbc_profiles(
    priority = c("High", "Medium", "Low"), # Will become factor with this level order
    category = c("A", "B", "C")
  )

  priors <- cbc_priors(
    profiles = profiles,
    priority = c("Medium" = 0.2, "Low" = -0.3), # High is reference
    category = c("B" = 0.1, "C" = 0.2)
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  decoded_design <- cbc_decode(design)

  # Should preserve original factor level ordering
  expect_equal(levels(decoded_design$priority), c("High", "Medium", "Low"))
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("No-choice data cannot be decoded", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_nochoice_test_data()
  design <- test_data$design
  choices <- test_data$choices

  # Should error for design with no-choice
  expect_error(
    cbc_decode(design),
    "Cannot convert data with no-choice option"
  )

  # Should error for choices with no-choice
  expect_error(
    cbc_decode(choices),
    "Cannot convert data with no-choice option"
  )
})

test_that("Invalid input objects are rejected", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Invalid object type
  expect_error(
    cbc_decode("not_a_design"),
    "Input must be a cbc_design or cbc_choices object"
  )

  expect_error(
    cbc_decode(data.frame(x = 1:5)),
    "Input must be a cbc_design or cbc_choices object"
  )
})

test_that("Missing categorical structure is handled gracefully", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_decode_test_data()
  design <- test_data$design

  # Remove categorical structure attribute
  attr(design, "categorical_structure") <- NULL

  expect_warning(
    decoded <- cbc_decode(design),
    "No categorical structure information found"
  )

  # Should return original data
  expect_identical(decoded, design)
})

test_that("Data without dummy coding is handled correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create design that's not dummy coded
  profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  design <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  # Manually set as not dummy coded
  attr(design, "is_dummy_coded") <- FALSE

  expect_message(
    decoded <- cbc_decode(design),
    "Data is already in categorical format"
  )

  expect_identical(decoded, design)
})

# =============================================================================
# EDGE CASES AND SPECIAL SCENARIOS
# =============================================================================

test_that("Binary categorical variables work correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Test with minimal 2-level categorical (valid edge case)
  profiles <- cbc_profiles(
    price = c(1, 2),
    available = c("Yes", "No") # Binary categorical
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    available = c("No" = -0.5) # "Yes" is reference
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  decoded_design <- cbc_decode(design)

  # Should have the binary categorical variable
  expect_true("available" %in% names(decoded_design))
  expect_true(
    is.factor(decoded_design$available) ||
      is.character(decoded_design$available)
  )

  # Should have both levels represented
  unique_levels <- unique(decoded_design$available)
  expect_setequal(as.character(unique_levels), c("Yes", "No"))

  # Should not have dummy column
  expect_false("availableNo" %in% names(decoded_design))
})

test_that("Mixed data types are handled correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create profiles with various data types
  profiles <- cbc_profiles(
    price = c(1.5, 2.0, 2.5), # Numeric
    available = c(TRUE, FALSE), # Logical
    brand = c("A", "B", "C") # Character
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    available = c("TRUE" = 0.5), # Note: logical becomes character in dummy coding
    brand = c("B" = 0.2, "C" = 0.3)
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  decoded_design <- cbc_decode(design)

  # Check data types are appropriate
  expect_type(decoded_design$price, "double")
  expect_true(
    is.factor(decoded_design$available) || is.logical(decoded_design$available)
  )
  expect_true(
    is.factor(decoded_design$brand) || is.character(decoded_design$brand)
  )
})

test_that("Large datasets decode efficiently", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create larger dataset
  profiles <- cbc_profiles(
    price = c(1, 2, 3, 4, 5),
    type = c("A", "B", "C", "D"),
    quality = c("Low", "Medium", "High")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.1, "C" = 0.2, "D" = 0.3),
    quality = c("Medium" = 0.1, "High" = 0.3)
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 3,
    n_q = 10,
    n_resp = 100 # Larger dataset
  )

  # Should complete reasonably quickly
  expect_lt(
    system.time({
      decoded_design <- cbc_decode(design)
    })[["elapsed"]],
    2 # Should complete in under 2 seconds
  )

  validate_decoded_structure(decoded_design, design, profiles)
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Decode works with different design methods", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.3)
  )

  methods <- c("random", "shortcut")

  for (method in methods) {
    design <- cbc_design(
      profiles = profiles,
      priors = if (method == "random") NULL else priors,
      method = method,
      n_alts = 2,
      n_q = 3,
      n_resp = 10
    )

    decoded_design <- cbc_decode(design)

    validate_decoded_structure(decoded_design, design, profiles)
    expect_false(attr(decoded_design, "is_dummy_coded") %||% TRUE)
  }
})

test_that("Decode works with blocked designs", {
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

  design <- cbc_design(
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

  decoded_design <- cbc_decode(design)

  validate_decoded_structure(decoded_design, design, profiles)

  # Should preserve blockID
  expect_true("blockID" %in% names(decoded_design))
  expect_identical(decoded_design$blockID, design$blockID)
})

test_that("Roundtrip compatibility works", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Test that we can decode and the result is sensible
  # (Can't easily test encode->decode roundtrip since we don't have encode function)

  test_data <- setup_decode_test_data()
  design <- test_data$design

  decoded_design <- cbc_decode(design)

  # The decoded design should have the same profileID mapping
  # Let's verify by checking that profiles with same profileID have same attributes
  for (pid in unique(decoded_design$profileID)) {
    if (pid != 0) {
      # Skip no-choice
      rows_with_pid <- decoded_design[decoded_design$profileID == pid, ]

      # All rows with same profileID should have same attribute values
      if (nrow(rows_with_pid) > 1) {
        expect_true(all(rows_with_pid$price == rows_with_pid$price[1]))
        expect_true(all(rows_with_pid$type == rows_with_pid$type[1]))
        expect_true(all(rows_with_pid$freshness == rows_with_pid$freshness[1]))
      }
    }
  }
})

# =============================================================================
# ATTRIBUTE PRESERVATION TESTS
# =============================================================================

test_that("All attributes are preserved during decoding", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_decode_test_data()
  design <- test_data$design

  # Store original attributes
  original_attrs <- attributes(design)

  decoded_design <- cbc_decode(design)
  decoded_attrs <- attributes(decoded_design)

  # Should preserve most attributes (except is_dummy_coded which changes)
  important_attrs <- c(
    "design_params",
    "design_summary",
    "categorical_structure"
  )

  for (attr_name in important_attrs) {
    if (attr_name %in% names(original_attrs)) {
      expect_true(attr_name %in% names(decoded_attrs))
      # Don't test exact equality since some might be legitimately modified
    }
  }

  # is_dummy_coded should be updated
  expect_false(attr(decoded_design, "is_dummy_coded") %||% TRUE)
})

test_that("Class inheritance is preserved", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_decode_test_data()
  design <- test_data$design
  choices <- test_data$choices

  decoded_design <- cbc_decode(design)
  decoded_choices <- cbc_decode(choices)

  # Should preserve original classes
  expect_s3_class(decoded_design, "cbc_design")
  expect_s3_class(decoded_design, "data.frame")

  expect_s3_class(decoded_choices, "cbc_choices")
  expect_s3_class(decoded_choices, "data.frame")
})

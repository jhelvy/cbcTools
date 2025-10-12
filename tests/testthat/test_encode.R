context("Testing cbc_encode()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

setup_encode_test_data <- function() {
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
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("No changes when no arguments provided", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  expect_equal(attr(design, "encoding"), "standard")

  expect_message(
    unchanged <- cbc_encode(design),
    "No encoding or reference levels specified"
  )

  expect_identical(unchanged, design)
})

test_that("Explicit dummy encoding works", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  encoded <- cbc_encode(design, coding = "dummy")

  expect_equal(attr(encoded, "encoding"), "dummy")
  expect_s3_class(encoded, "cbc_design")
  expect_equal(nrow(encoded), nrow(design))

  expect_true("typeGala" %in% names(encoded))
  expect_true("typeHoneycrisp" %in% names(encoded))
  expect_true("freshnessAverage" %in% names(encoded))
  expect_true("freshnessExcellent" %in% names(encoded))
  expect_false("type" %in% names(encoded))
  expect_false("freshness" %in% names(encoded))

  expect_true("price" %in% names(encoded))
})

test_that("Explicit effects encoding works", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  encoded <- cbc_encode(design, coding = "effects")

  expect_equal(attr(encoded, "encoding"), "effects")
  expect_true("typeGala" %in% names(encoded))
  expect_true("typeHoneycrisp" %in% names(encoded))

  expect_true(any(encoded$typeGala == -1))
  expect_true(any(encoded$typeHoneycrisp == -1))
})

test_that("Choice data encoding works", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  choices <- test_data$choices

  encoded <- cbc_encode(choices, coding = "dummy")

  expect_s3_class(encoded, "cbc_choices")
  expect_equal(attr(encoded, "encoding"), "dummy")
  expect_true("choice" %in% names(encoded))
  expect_identical(encoded$choice, choices$choice)
})

test_that("Already encoded data returns with message", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  dummy_design <- cbc_encode(design, coding = "dummy")

  expect_message(
    dummy_again <- cbc_encode(dummy_design, coding = "dummy"),
    "Data is already in 'dummy' encoding"
  )

  expect_identical(dummy_design, dummy_again)
})

test_that("Continuous variables preserved in all encodings", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  dummy_design <- cbc_encode(design, coding = "dummy")
  effects_design <- cbc_encode(design, coding = "effects")

  expect_true("price" %in% names(dummy_design))
  expect_true("price" %in% names(effects_design))
  expect_identical(dummy_design$price, design$price)
  expect_identical(effects_design$price, design$price)
})

# =============================================================================
# REFERENCE LEVEL TESTS
# =============================================================================

test_that("Custom reference levels work with ref_levels argument", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  encoded <- cbc_encode(
    design,
    coding = "dummy",
    ref_levels = list(type = "Gala")
  )

  expect_true("typeFuji" %in% names(encoded))
  expect_true("typeHoneycrisp" %in% names(encoded))
  expect_false("typeGala" %in% names(encoded))

  categorical_structure <- attr(encoded, "categorical_structure")
  expect_equal(categorical_structure$type$reference_level, "Gala")
})

test_that("Multiple reference levels can be updated", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  encoded <- cbc_encode(
    design,
    coding = "dummy",
    ref_levels = list(
      type = "Honeycrisp",
      freshness = "Excellent"
    )
  )

  expect_true("typeFuji" %in% names(encoded))
  expect_true("typeGala" %in% names(encoded))
  expect_false("typeHoneycrisp" %in% names(encoded))

  expect_true("freshnessPoor" %in% names(encoded))
  expect_true("freshnessAverage" %in% names(encoded))
  expect_false("freshnessExcellent" %in% names(encoded))
})

test_that("ref_levels can be updated without changing encoding", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  dummy_design <- cbc_encode(design, coding = "dummy")

  updated <- cbc_encode(
    dummy_design,
    ref_levels = list(type = "Gala")
  )

  expect_equal(attr(updated, "encoding"), "dummy")
  expect_true("typeFuji" %in% names(updated))
  expect_false("typeGala" %in% names(updated))
})

# =============================================================================
# NO-CHOICE TESTS
# =============================================================================

test_that("No-choice designs encode correctly", {
  skip_on_cran()
  test_data <- setup_nochoice_test_data()
  design <- test_data$design

  encoded <- cbc_encode(design, coding = "dummy")

  expect_true("no_choice" %in% names(encoded))
  expect_equal(attr(encoded, "encoding"), "dummy")

  no_choice_rows <- encoded$no_choice == 1
  expect_true(all(encoded$qualityHigh[no_choice_rows] == 0))
  expect_true(all(encoded$price[no_choice_rows] == 0))
})

test_that("No-choice roundtrip works", {
  skip_on_cran()
  test_data <- setup_nochoice_test_data()
  design <- test_data$design

  encoded <- cbc_encode(design, coding = "dummy")
  decoded <- cbc_encode(encoded, coding = "standard")

  expect_equal(attr(decoded, "encoding"), "standard")
  expect_true("quality" %in% names(decoded))

  no_choice_rows <- decoded$no_choice == 1
  expect_true(all(is.na(decoded$quality[no_choice_rows])))
  expect_true(all(is.na(decoded$price[no_choice_rows])))
})

# =============================================================================
# ROUNDTRIP TESTS
# =============================================================================

test_that("Standard -> Dummy -> Standard roundtrip works", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  dummy_design <- cbc_encode(design, coding = "dummy")
  roundtrip <- cbc_encode(dummy_design, coding = "standard")

  expect_equal(attr(roundtrip, "encoding"), "standard")
  expect_true("type" %in% names(roundtrip))
  expect_true("freshness" %in% names(roundtrip))
  expect_identical(roundtrip$price, design$price)

  categorical_structure <- attr(roundtrip, "categorical_structure")
  expect_equal(
    categorical_structure$type$levels,
    categorical_structure$type$levels
  )
})

test_that("Standard -> Effects -> Standard roundtrip works", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  effects_design <- cbc_encode(design, coding = "effects")
  roundtrip <- cbc_encode(effects_design, coding = "standard")

  expect_equal(attr(roundtrip, "encoding"), "standard")
  expect_true("type" %in% names(roundtrip))
  expect_identical(roundtrip$price, design$price)
})

test_that("Dummy -> Effects -> Dummy roundtrip works", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  dummy_design <- cbc_encode(design, coding = "dummy")
  effects_design <- cbc_encode(dummy_design, coding = "effects")
  roundtrip <- cbc_encode(effects_design, coding = "dummy")

  expect_equal(attr(roundtrip, "encoding"), "dummy")
  expect_true("typeGala" %in% names(roundtrip))

  expect_true(all(roundtrip$typeGala %in% c(0, 1)))
  expect_true(all(roundtrip$typeHoneycrisp %in% c(0, 1)))
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("Invalid input objects are rejected", {
  skip_on_cran()

  expect_error(
    cbc_encode("not_a_design"),
    "Input must be a data.frame or cbc_design/cbc_choices object"
  )

  expect_error(
    cbc_encode(data.frame(x = 1:5)),
    "Missing required ID columns: profileID, qID, altID, obsID"
  )
})

test_that("Invalid coding argument is rejected", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  expect_error(
    cbc_encode(design, coding = "invalid"),
    "coding must be one of: 'standard', 'dummy', 'effects'"
  )
})

test_that("Invalid ref_levels argument is rejected", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  expect_error(
    cbc_encode(design, ref_levels = "not_a_list"),
    "ref_levels must be a named list"
  )

  expect_error(
    cbc_encode(design, ref_levels = list("unnamed")),
    "ref_levels must be a named list"
  )

  expect_error(
    cbc_encode(design, ref_levels = list(invalid_attr = "value")),
    "Attribute 'invalid_attr' not found in categorical structure"
  )

  expect_error(
    cbc_encode(design, ref_levels = list(type = "InvalidLevel")),
    "Level 'InvalidLevel' not found in attribute 'type'"
  )
})

# =============================================================================
# EDGE CASES
# =============================================================================

test_that("Binary categorical variables work correctly", {
  skip_on_cran()
  profiles <- cbc_profiles(
    price = c(1, 2),
    available = c("Yes", "No")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    available = c("No" = -0.5)
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "random",
    n_alts = 2,
    n_q = 3,
    n_resp = 10
  )

  encoded <- cbc_encode(design, coding = "dummy")

  expect_true("availableNo" %in% names(encoded))
  expect_false("availableYes" %in% names(encoded))
})

test_that("Large datasets encode efficiently", {
  skip_on_cran()
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
    n_resp = 100
  )

  expect_lt(
    system.time({
      encoded <- cbc_encode(design, coding = "dummy")
    })[["elapsed"]],
    2
  )

  expect_equal(attr(encoded, "encoding"), "dummy")
})

# =============================================================================
# ATTRIBUTE PRESERVATION TESTS
# =============================================================================

test_that("Attributes preserved during encoding", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design

  original_attrs <- attributes(design)

  encoded <- cbc_encode(design, coding = "dummy")
  encoded_attrs <- attributes(encoded)

  important_attrs <- c(
    "design_params",
    "categorical_structure"
  )

  for (attr_name in important_attrs) {
    if (attr_name %in% names(original_attrs)) {
      expect_true(attr_name %in% names(encoded_attrs))
    }
  }
})

test_that("Class inheritance preserved", {
  skip_on_cran()
  test_data <- setup_encode_test_data()
  design <- test_data$design
  choices <- test_data$choices

  encoded_design <- cbc_encode(design, coding = "dummy")
  encoded_choices <- cbc_encode(choices, coding = "dummy")

  expect_s3_class(encoded_design, "cbc_design")
  expect_s3_class(encoded_design, "data.frame")

  expect_s3_class(encoded_choices, "cbc_choices")
  expect_s3_class(encoded_choices, "data.frame")
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Encoding works with different design methods", {
  skip_on_cran()
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B")
  )

  methods <- c("random", "shortcut")

  for (method in methods) {
    design <- cbc_design(
      profiles = profiles,
      method = method,
      n_alts = 2,
      n_q = 3,
      n_resp = 10
    )

    encoded <- cbc_encode(design, coding = "dummy")

    expect_equal(attr(encoded, "encoding"), "dummy")
    expect_s3_class(encoded, "cbc_design")
  }
})

test_that("Encoding works with blocked designs", {
  skip_on_cran()
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

  encoded <- cbc_encode(design, coding = "dummy")

  expect_equal(attr(encoded, "encoding"), "dummy")
  expect_true("blockID" %in% names(encoded))
  expect_identical(encoded$blockID, design$blockID)
})

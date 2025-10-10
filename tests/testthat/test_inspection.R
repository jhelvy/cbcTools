context("Testing cbc_inspect()")

# =============================================================================
# TEST SETUP AND FIXTURES
# =============================================================================

# Create shared test fixtures
setup_inspection_test_data <- function() {
  profiles <- cbc_profiles(
    price = c(1, 2, 3, 4),
    type = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c("Poor", "Average", "Excellent"),
    weight = c(0.5, 1.0, 1.5)
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("Gala" = 0.2, "Honeycrisp" = 0.3),
    freshness = c("Average" = 0.1, "Excellent" = 0.4),
    weight = 0.05
  )

  design <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "shortcut",
    n_alts = 3,
    n_q = 6,
    n_resp = 50
  )

  choices <- cbc_choices(design, priors)

  list(
    profiles = profiles,
    priors = priors,
    design = design,
    choices = choices
  )
}

# Create optimal design for D-error testing
setup_optimal_inspection_data <- function() {
  skip_if_not_installed("idefix")

  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B"),
    quality = c("Low", "High")
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = -0.1,
    type = c("B" = 0.3),
    quality = c("High" = 0.5)
  )

  design <- cbc_design(
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

  list(
    profiles = profiles,
    priors = priors,
    design = design
  )
}

# Create design with special features
setup_special_features_data <- function() {
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
    n_q = 4,
    n_resp = 20,
    no_choice = TRUE
  )

  list(
    profiles = profiles,
    priors = priors,
    design = design
  )
}

# =============================================================================
# VALIDATION HELPER FUNCTIONS
# =============================================================================

# Validate inspection object structure
validate_inspection_structure <- function(inspection, expected_sections) {
  # Basic class and structure
  expect_s3_class(inspection, "cbc_inspection")
  expect_s3_class(inspection, "list")

  # Required metadata
  expect_true("sections_requested" %in% names(inspection))
  expect_true("verbose" %in% names(inspection))
  expect_true("design_info" %in% names(inspection))

  # Should have requested sections
  for (section in expected_sections) {
    expect_true(section %in% names(inspection))
  }

  # Metadata should be reasonable
  expect_type(inspection$sections_requested, "character")
  expect_type(inspection$verbose, "logical")
  expect_type(inspection$design_info, "list")
}

# Validate structure section content
validate_structure_section <- function(structure_data) {
  expect_type(structure_data, "list")

  # Required fields
  required_fields <- c(
    "method",
    "created_at",
    "generation_time",
    "n_resp",
    "n_q",
    "n_alts",
    "n_blocks",
    "n_choice_sets",
    "n_profiles_used",
    "n_profiles_available",
    "profile_usage_rate",
    "features",
    "optimization_attempts"
  )

  for (field in required_fields) {
    expect_true(field %in% names(structure_data))
  }

  # Validate data types and reasonableness
  expect_type(structure_data$method, "character")
  expect_s3_class(structure_data$created_at, "POSIXct")
  expect_type(structure_data$generation_time, "double")
  expect_true(structure_data$generation_time >= 0)

  expect_type(structure_data$n_resp, "double")
  expect_true(structure_data$n_resp >= 1)

  expect_type(structure_data$n_q, "double")
  expect_true(structure_data$n_q >= 1)

  expect_type(structure_data$n_alts, "double")
  expect_true(structure_data$n_alts >= 2)

  expect_type(structure_data$profile_usage_rate, "double")
  expect_true(
    structure_data$profile_usage_rate >= 0 &&
      structure_data$profile_usage_rate <= 1
  )
}

# Validate efficiency section content
validate_efficiency_section <- function(efficiency_data) {
  expect_type(efficiency_data, "list")

  # Required fields
  expect_true("method" %in% names(efficiency_data))
  expect_type(efficiency_data$method, "character")

  # D-errors may be present depending on method
  if (
    "d_error_prior" %in%
      names(efficiency_data) &&
      !is.null(efficiency_data$d_error_prior)
  ) {
    expect_type(efficiency_data$d_error_prior, "double")
    expect_true(is.finite(efficiency_data$d_error_prior))
    expect_true(efficiency_data$d_error_prior > 0)
  }

  if (
    "d_error_null" %in%
      names(efficiency_data) &&
      !is.null(efficiency_data$d_error_null)
  ) {
    expect_type(efficiency_data$d_error_null, "double")
    expect_true(is.finite(efficiency_data$d_error_null))
    expect_true(efficiency_data$d_error_null > 0)
  }

  # Balance and overlap scores
  if (
    "balance_score" %in%
      names(efficiency_data) &&
      !is.na(efficiency_data$balance_score)
  ) {
    expect_type(efficiency_data$balance_score, "double")
    expect_true(
      efficiency_data$balance_score >= 0 && efficiency_data$balance_score <= 1
    )
  }

  if (
    "overlap_score" %in%
      names(efficiency_data) &&
      !is.na(efficiency_data$overlap_score)
  ) {
    expect_type(efficiency_data$overlap_score, "double")
    expect_true(efficiency_data$overlap_score >= 0)
  }
}

# Validate balance section content
validate_balance_section <- function(balance_data) {
  expect_type(balance_data, "list")

  # Required fields
  expect_true("individual_counts" %in% names(balance_data))
  expect_true("balance_metrics" %in% names(balance_data))
  expect_true("overall_balance" %in% names(balance_data))

  # Individual counts should be a list of tables
  expect_type(balance_data$individual_counts, "list")
  for (attr_counts in balance_data$individual_counts) {
    expect_s3_class(attr_counts, "table")
  }

  # Balance metrics should be a list
  expect_type(balance_data$balance_metrics, "list")

  # Overall balance should be reasonable
  expect_type(balance_data$overall_balance, "double")
  expect_true(
    balance_data$overall_balance >= 0 && balance_data$overall_balance <= 1
  )
}

# Validate overlap section content
validate_overlap_section <- function(overlap_data) {
  expect_type(overlap_data, "list")

  # Required fields
  expect_true("overlap_counts" %in% names(overlap_data))
  expect_true("overlap_metrics" %in% names(overlap_data))
  expect_true("overall_overlap" %in% names(overlap_data))

  # Overlap counts should be a list
  expect_type(overlap_data$overlap_counts, "list")

  # Overlap metrics should be a list
  expect_type(overlap_data$overlap_metrics, "list")

  # Overall overlap should be reasonable
  expect_type(overlap_data$overall_overlap, "double")
  expect_true(overlap_data$overall_overlap >= 0)
}

# Validate encoding section content
validate_encoding_section <- function(encoding_data) {
  expect_type(encoding_data, "list")

  # Required fields
  expect_true("encoding" %in% names(encoding_data))
  expect_true("no_choice" %in% names(encoding_data))

  expect_type(encoding_data$encoding, "character")
  expect_true(encoding_data$encoding %in% c("standard", "dummy", "effects"))
  expect_type(encoding_data$no_choice, "logical")

  # Categorical variables info
  if (
    "categorical_variables" %in%
      names(encoding_data) &&
      !is.null(encoding_data$categorical_variables)
  ) {
    expect_type(encoding_data$categorical_variables, "character")
  }
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("Basic inspection with all sections works", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  inspection <- cbc_inspect(design)

  validate_inspection_structure(
    inspection,
    c("structure", "efficiency", "balance", "overlap", "encoding")
  )

  # Should have default sections
  expect_setequal(
    inspection$sections_requested,
    c("structure", "efficiency", "balance", "overlap", "encoding")
  )
  expect_false(inspection$verbose)

  # Validate each section
  validate_structure_section(inspection$structure)
  validate_efficiency_section(inspection$efficiency)
  validate_balance_section(inspection$balance)
  validate_overlap_section(inspection$overlap)
  validate_encoding_section(inspection$encoding)
})

test_that("Individual section inspection works", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  # Test structure only
  inspection_structure <- cbc_inspect(design, sections = "structure")

  validate_inspection_structure(inspection_structure, "structure")
  expect_equal(inspection_structure$sections_requested, "structure")
  expect_false("efficiency" %in% names(inspection_structure))
  validate_structure_section(inspection_structure$structure)

  # Test efficiency only
  inspection_efficiency <- cbc_inspect(design, sections = "efficiency")

  validate_inspection_structure(inspection_efficiency, "efficiency")
  expect_equal(inspection_efficiency$sections_requested, "efficiency")
  validate_efficiency_section(inspection_efficiency$efficiency)

  # Test balance only
  inspection_balance <- cbc_inspect(design, sections = "balance")

  validate_inspection_structure(inspection_balance, "balance")
  expect_equal(inspection_balance$sections_requested, "balance")
  validate_balance_section(inspection_balance$balance)
})

test_that("Multiple specific sections work", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  inspection <- cbc_inspect(design, sections = c("structure", "balance"))

  validate_inspection_structure(inspection, c("structure", "balance"))
  expect_setequal(inspection$sections_requested, c("structure", "balance"))

  # Should have requested sections
  expect_true("structure" %in% names(inspection))
  expect_true("balance" %in% names(inspection))

  # Should not have other sections
  expect_false("efficiency" %in% names(inspection))
  expect_false("overlap" %in% names(inspection))
  expect_false("encoding" %in% names(inspection))
})

test_that("Verbose mode works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  # Test verbose mode
  inspection_verbose <- cbc_inspect(design, verbose = TRUE)

  validate_inspection_structure(
    inspection_verbose,
    c("structure", "efficiency", "balance", "overlap", "encoding")
  )
  expect_true(inspection_verbose$verbose)

  # Test non-verbose mode
  inspection_normal <- cbc_inspect(design, verbose = FALSE)

  validate_inspection_structure(
    inspection_normal,
    c("structure", "efficiency", "balance", "overlap", "encoding")
  )
  expect_false(inspection_normal$verbose)
})

# =============================================================================
# DESIGN TYPE SPECIFIC TESTS
# =============================================================================

test_that("Optimal design inspection includes D-errors", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_optimal_inspection_data()
  design <- test_data$design

  inspection <- cbc_inspect(design, sections = "efficiency")

  validate_inspection_structure(inspection, "efficiency")
  validate_efficiency_section(inspection$efficiency)

  # Should have D-error information
  expect_true(!is.null(inspection$efficiency$d_error_null))
  expect_true(!is.null(inspection$efficiency$d_error_prior))

  # D-errors should be positive
  expect_true(inspection$efficiency$d_error_null > 0)
  expect_true(inspection$efficiency$d_error_prior > 0)
})

test_that("Random design inspection handles missing D-errors", {
  skip_on_cran() # Skip on CRAN due to computation time
  profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  design_random <- cbc_design(
    profiles = profiles,
    method = "random",
    n_alts = 2,
    n_q = 4,
    n_resp = 20
  )

  inspection <- cbc_inspect(design_random, sections = "efficiency")

  validate_inspection_structure(inspection, "efficiency")
  validate_efficiency_section(inspection$efficiency)

  # D-errors should be NULL or NA for random designs
  expect_true(
    is.null(inspection$efficiency$d_error_null) ||
      is.na(inspection$efficiency$d_error_null)
  )
  expect_true(
    is.null(inspection$efficiency$d_error_prior) ||
      is.na(inspection$efficiency$d_error_prior)
  )
})

test_that("No-choice design inspection works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_special_features_data()
  design <- test_data$design

  inspection <- cbc_inspect(design)

  validate_inspection_structure(
    inspection,
    c("structure", "efficiency", "balance", "overlap", "encoding")
  )

  # Should indicate no-choice in structure and encoding
  expect_true(inspection$encoding$no_choice)
  expect_true("No-choice option" %in% inspection$structure$features)
})

test_that("Blocked design inspection works correctly", {
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

  inspection <- cbc_inspect(design_blocked, sections = "structure")

  validate_inspection_structure(inspection, "structure")
  validate_structure_section(inspection$structure)

  # Should show block information
  expect_equal(inspection$structure$n_blocks, 2)
})

# =============================================================================
# BALANCE AND OVERLAP ANALYSIS TESTS
# =============================================================================

test_that("Balance analysis produces reasonable results", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  inspection <- cbc_inspect(design, sections = "balance")

  validate_balance_section(inspection$balance)

  # Check that we have the expected dummy-coded attribute names
  balance_names <- names(inspection$balance$individual_counts)

  # Continuous attributes should be present as-is
  expect_true("price" %in% balance_names)
  expect_true("weight" %in% balance_names)

  # Categorical attributes should be dummy-coded
  expect_true(any(grepl("^type", balance_names)))
  expect_true(any(grepl("^freshness", balance_names)))

  # Balance scores should be reasonable for all attributes
  for (attr in balance_names) {
    if (attr %in% names(inspection$balance$balance_metrics)) {
      balance_score <- inspection$balance$balance_metrics[[attr]]$balance_score
      expect_true(
        balance_score >= 0 && balance_score <= 1,
        info = paste("Invalid balance score for", attr)
      )
    }
  }
})

test_that("Overlap analysis produces reasonable results", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design
  inspection <- cbc_inspect(design, sections = "overlap")
  validate_overlap_section(inspection$overlap)

  # Check that we have the expected dummy-coded attribute names
  overlap_names <- names(inspection$overlap$overlap_counts)

  # Continuous attributes should be present as-is
  expect_true("price" %in% overlap_names)
  expect_true("weight" %in% overlap_names)

  # Overlap scores should be reasonable for all attributes
  for (attr in overlap_names) {
    if (attr %in% names(inspection$overlap$overlap_metrics)) {
      overlap_rate <- inspection$overlap$overlap_metrics[[
        attr
      ]]$complete_overlap_rate
      expect_true(
        overlap_rate >= 0 && overlap_rate <= 1,
        info = paste("Invalid overlap rate for", attr)
      )
    }
  }
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("Input validation works correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()

  # Invalid design object
  expect_error(
    cbc_inspect("not_a_design"),
    "Input must be a data.frame or cbc_design/cbc_choices object"
  )

  expect_error(
    cbc_inspect(data.frame(x = 1:5)),
    "Missing required ID columns: profileID, qID, altID, obsID"
  )

  # Invalid sections
  expect_error(
    cbc_inspect(test_data$design, sections = "invalid_section"),
    "Invalid sections"
  )

  expect_error(
    cbc_inspect(test_data$design, sections = c("structure", "invalid")),
    "Invalid sections"
  )
})

# =============================================================================
# PRINT METHOD TESTS
# =============================================================================

test_that("Print method works correctly", {
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  inspection <- cbc_inspect(design)

  # Should print without error
  expect_output(print(inspection), "DESIGN SUMMARY")
  expect_output(print(inspection), "STRUCTURE")
  expect_output(print(inspection), "SUMMARY METRICS")
  expect_output(print(inspection), "VARIABLE ENCODING")
  expect_output(print(inspection), "ATTRIBUTE BALANCE")
  expect_output(print(inspection), "ATTRIBUTE OVERLAP")
})

test_that("Print method shows only requested sections", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  # Test structure only
  inspection_structure <- cbc_inspect(design, sections = "structure")
  output_structure <- capture.output(print(inspection_structure))

  expect_true(any(grepl("STRUCTURE", output_structure)))
  expect_false(any(grepl("ATTRIBUTE BALANCE", output_structure)))

  # Test balance only
  inspection_balance <- cbc_inspect(design, sections = "balance")
  output_balance <- capture.output(print(inspection_balance))

  expect_true(any(grepl("ATTRIBUTE BALANCE", output_balance)))
  expect_false(any(grepl("STRUCTURE", output_balance)))
})

test_that("Verbose print mode shows additional details", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  # Compare verbose vs normal output
  inspection_normal <- cbc_inspect(design, verbose = FALSE)
  inspection_verbose <- cbc_inspect(design, verbose = TRUE)

  output_normal <- capture.output(print(inspection_normal))
  output_verbose <- capture.output(print(inspection_verbose))

  # Verbose output should be longer
  expect_gt(length(output_verbose), length(output_normal))
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Inspection works with different design methods", {
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

  methods <- c("random", "shortcut", "minoverlap")

  for (method in methods) {
    design <- cbc_design(
      profiles = profiles,
      priors = if (method == "random") NULL else priors,
      method = method,
      n_alts = 2,
      n_q = 4,
      n_resp = 20
    )

    inspection <- cbc_inspect(design)

    validate_inspection_structure(
      inspection,
      c("structure", "efficiency", "balance", "overlap", "encoding")
    )
    expect_equal(inspection$structure$method, method)
  }
})

test_that("Inspection works with choice data", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  choices <- test_data$choices

  # cbc_inspect should work with choice data too (inherits from design)
  inspection <- cbc_inspect(choices)

  validate_inspection_structure(
    inspection,
    c("structure", "efficiency", "balance", "overlap", "encoding")
  )

  # Should have choice column information
  expect_true("choice" %in% names(choices))
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================

test_that("Inspection completes in reasonable time", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  # Should complete quickly
  expect_lt(
    system.time({
      cbc_inspect(design)
    })[["elapsed"]],
    3 # Should complete in under 3 seconds
  )
})

test_that("Large design inspection is efficient", {
  skip_on_cran() # Skip on CRAN due to computation time
  # Create larger design
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

  design_large <- cbc_design(
    profiles = profiles,
    priors = priors,
    method = "shortcut",
    n_alts = 3,
    n_q = 10,
    n_resp = 100 # Larger design
  )

  # Should still complete reasonably quickly
  expect_lt(
    system.time({
      cbc_inspect(design_large)
    })[["elapsed"]],
    5 # Should complete in under 5 seconds
  )
})

# =============================================================================
# DESIGN INFO METADATA TESTS
# =============================================================================

test_that("Design info metadata is populated correctly", {
  skip_on_cran() # Skip on CRAN due to computation time
  test_data <- setup_inspection_test_data()
  design <- test_data$design

  inspection <- cbc_inspect(design)

  design_info <- inspection$design_info

  # Should have basic design information
  expect_true("method" %in% names(design_info))
  expect_true("n_choice_sets" %in% names(design_info))
  expect_true("profiles_used" %in% names(design_info))
  expect_true("profiles_available" %in% names(design_info))

  # Values should be reasonable
  expect_type(design_info$method, "character")
  expect_type(design_info$n_choice_sets, "double")
  expect_true(design_info$n_choice_sets > 0)
  expect_true(design_info$profiles_used <= design_info$profiles_available)
})

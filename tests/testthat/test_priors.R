context("Testing cbc_priors()")

test_that("Basic prior specification works", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C"),
    size = c("Small", "Medium", "Large")
  )

  expect_no_error(
    cbc_priors(
      profiles = profiles,
      price = -0.5,
      type = c("B" = 0.2, "C" = 0.3),
      size = c(0.1, 0.2)
    )
  )
})

test_that("Invalid profiles input is caught", {
  # Not a data frame
  expect_error(
    cbc_priors(
      profiles = list(price = c(1, 2, 3)),
      price = -0.5
    ),
    "'profiles' must be a data frame created by cbc_profiles()"
  )

  # Missing profileID
  expect_error(
    cbc_priors(
      profiles = data.frame(price = c(1, 2, 3)),
      price = -0.5
    ),
    "'profiles' must be a data frame created by cbc_profiles()"
  )
})

test_that("Parameter validation works", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C")
  )

  # Missing attributes
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.5  # missing type
    ),
    "Missing prior specifications for attributes: type"
  )

  # Extra attributes
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.5,
      type = c(0.1, 0.2),
      color = 0.3  # non-existent attribute
    ),
    "Prior specifications provided for non-existent attributes: color"
  )

  # Wrong number of coefficients for categorical
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.5,
      type = c(0.1)  # should be 2 values for 3 levels
    ),
    "Prior for categorical attribute 'type' must have 2 values"
  )

  # Invalid level names
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.5,
      type = c("D" = 0.1, "E" = 0.2)  # invalid levels
    ),
    "Invalid levels specified for attribute 'type': D, E"
  )
})

test_that("Random parameter validation works", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C")
  )

  # Invalid distribution
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = rand(
        dist = "invalid",
        mean = -0.5,
        sd = 0.1
      ),
      type = c(0.1, 0.2)
    ),
    'dist must be one of "n" (normal), "ln" (log-normal), or "cn" (censored normal)'
  )

  # Inconsistent length for continuous attribute SD
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = rand(
        dist = "n",
        mean = -0.5,
        sd = c(0.1, 0.2)  # should be single value
      ),
      type = c(0.1, 0.2)
    ),
    "SD for continuous attribute 'price' must be a single value"
  )

  # Mismatched named levels between mean and SD
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.5,
      type = rand(
        dist = "n",
        mean = c("A" = 0.2, "B" = 0.3),
        sd = c("C" = 0.2, "B" = 0.3)
      )
    ),
    "For attribute 'type', mean and sd must use the same levels"
  )

  # Named in mean but unnamed in SD
  expect_error(
    cbc_priors(
      profiles = profiles,
      price = -0.5,
      type = rand(
        dist = "n",
        mean = c("A" = 0.2, "B" = 0.3),
        sd = c(0.2, 0.3)
      )
    ),
    "For attribute 'type', if either mean or sd has named levels, both must have named levels"
  )
})

test_that("Random parameter specifications work", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C")
  )

  # Test that mixed fixed and random parameters work
  priors <- cbc_priors(
    profiles = profiles,
    price = rand(
      dist = "n",
      mean = -0.5,
      sd = 0.1
    ),
    type = c(0.1, 0.2)  # fixed parameter
  )

  expect_equal(priors$modelType, "mxl")
  expect_true("price" %in% names(priors$param_specs))
  expect_equal(priors$param_specs$price$type, "random")
  expect_equal(priors$param_specs$type$type, "fixed")

  # Test that all parameters can be random
  priors <- cbc_priors(
    profiles = profiles,
    price = rand(
      dist = "n",
      mean = -0.5,
      sd = 0.1
    ),
    type = rand(
      dist = "n",
      mean = c(0.1, 0.2),
      sd = c(0.2, 0.3)
    )
  )

  expect_equal(priors$modelType, "mxl")
  expect_true(all(c("price", "type") %in% names(priors$param_specs)))
  expect_true(all(sapply(priors$param_specs, function(x) x$type == "random")))
})

test_that("Correlation matrix works", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C")
  )

  correlation <- matrix(
    c(1.0, 0.3,
      0.3, 1.0),
    nrow = 2
  )

  priors <- cbc_priors(
    profiles = profiles,
    price = rand(
      dist = "n",
      mean = -0.5,
      sd = 0.1
    ),
    type = rand(
      dist = "n",
      mean = c(0.1, 0.2),
      sd = c(0.2, 0.3)
    ),
    correlation = correlation
  )

  expect_equal(priors$correlation, correlation)
})
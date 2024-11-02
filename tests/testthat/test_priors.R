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
      mean = list(
        price = -0.5,
        type = c("B" = 0.2, "C" = 0.3),
        size = c(0.1, 0.2)
      )
    )
  )
})

test_that("Invalid profiles input is caught", {
  # Not a data frame
  expect_error(
    cbc_priors(
      profiles = list(price = c(1, 2, 3)),
      mean = list(price = -0.5)
    ),
    "'profiles' must be a data frame created by cbc_profiles()"
  )

  # Missing profileID
  expect_error(
    cbc_priors(
      profiles = data.frame(price = c(1, 2, 3)),
      mean = list(price = -0.5)
    ),
    "'profiles' must be a data frame created by cbc_profiles()"
  )
})

test_that("Mean parameter validation works", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C")
  )

  # Mean must be a list
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = c(price = -0.5)
    ),
    "'mean' must be a named list of prior specifications"
  )

  # Missing attributes
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(price = -0.5)  # missing type
    ),
    "Missing prior specifications for attributes: type"
  )

  # Extra attributes
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(
        price = -0.5,
        type = c(0.1, 0.2),
        color = 0.3  # non-existent attribute
      )
    ),
    "Prior specifications provided for non-existent attributes: color"
  )

  # Wrong number of coefficients for categorical
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(
        price = -0.5,
        type = c(0.1)  # should be 2 values for 3 levels
      )
    ),
    "Prior for categorical attribute 'type' must have 2 values"
  )

  # Invalid level names
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(
        price = -0.5,
        type = c("D" = 0.1, "E" = 0.2)  # invalid levels
      )
    ),
    "Invalid levels specified for attribute 'type': D, E"
  )
})

test_that("SD parameter validation works", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C")
  )

  # SD must be a list
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(price = -0.5, type = c(0.1, 0.2)),
      sd = c(price = 0.1)
    ),
    "sd must be a named list"
  )

  # Non-existent parameters in SD
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(price = -0.5, type = c(0.1, 0.2)),
      sd = list(color = 0.1)
    ),
    "SD specified for non-existent parameters: color"
  )

  # Inconsistent length for continuous attribute
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(price = -0.5, type = c(0.1, 0.2)),
      sd = list(price = c(0.1, 0.2))
    ),
    "SD for continuous attribute 'price' must be a single value"
  )
})

test_that("Mean and SD level consistency is enforced", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B", "C")
  )

  # Mismatched named levels
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(
        price = -0.5,
        type = c("A" = 0.2, "B" = 0.3)
      ),
      sd = list(
        price = 0.1,
        type = c("C" = 0.2, "B" = 0.3)
      )
    ),
    "For attribute 'type', mean and sd must use the same levels"
  )

  # Named in mean but unnamed in sd
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(
        price = -0.5,
        type = c("A" = 0.2, "B" = 0.3)
      ),
      sd = list(
        price = 0.1,
        type = c(0.2, 0.3)
      )
    ),
    "For attribute 'type', if either mean or sd has named levels, both must have named levels"
  )

  # Unnamed but different lengths
  expect_error(
    cbc_priors(
      profiles = profiles,
      mean = list(
        price = -0.5,
        type = c(0.2, 0.3)
      ),
      sd = list(
        price = 0.1,
        type = c(0.2, 0.3, 0.4)
      )
    ),
    "For attribute 'type', sd must have same length as mean specification \\(2 values\\)"
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
    mean = list(
      price = -0.5,
      type = c(0.1, 0.2)
    ),
    sd = list(price = 0.1)  # only price is random
  )

  expect_equal(priors$model$modelType, "mxl")
  expect_true("price" %in% names(priors$sd))
  expect_false("type" %in% names(priors$sd))

  # Test that all parameters can be random
  priors <- cbc_priors(
    profiles = profiles,
    mean = list(
      price = -0.5,
      type = c(0.1, 0.2)
    ),
    sd = list(
      price = 0.1,
      type = c(0.2, 0.3)
    )
  )

  expect_equal(priors$model$modelType, "mxl")
  expect_true(all(c("price", "type") %in% names(priors$sd)))
})
context("Testing cbc_profiles()")

test_that("Basic profile creation works", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B")
  )

  expect_s3_class(profiles, "cbc_profiles")
  expect_s3_class(profiles, "data.frame")
  expect_equal(nrow(profiles), 6)  # 3 * 2 = 6 combinations
  expect_equal(ncol(profiles), 3)  # profileID + price + type
  expect_named(profiles, c("profileID", "price", "type"))
})

test_that("Profile IDs are sequential", {
  profiles <- cbc_profiles(
    price = c(1, 2),
    quality = c("Low", "High")
  )

  expect_equal(profiles$profileID, 1:4)
  expect_true(all(diff(profiles$profileID) == 1))
})

test_that("Full factorial expansion works correctly", {
  profiles <- cbc_profiles(
    price = c(1, 2),
    type = c("A", "B"),
    quality = c("Low", "High")
  )

  expect_equal(nrow(profiles), 8)  # 2 * 2 * 2 = 8

  # Check all combinations are present
  expect_true(all(c(1, 2) %in% profiles$price))
  expect_true(all(c("A", "B") %in% profiles$type))
  expect_true(all(c("Low", "High") %in% profiles$quality))
})

test_that("Factor levels are preserved correctly", {
  profiles <- cbc_profiles(
    type = c("Fuji", "Gala", "Honeycrisp"),
    quality = c("Poor", "Average", "Excellent")
  )

  expect_true(is.factor(profiles$type))
  expect_true(is.factor(profiles$quality))
  expect_equal(levels(profiles$type), c("Fuji", "Gala", "Honeycrisp"))
  expect_equal(levels(profiles$quality), c("Poor", "Average", "Excellent"))
})

test_that("Mixed data types work", {
  profiles <- cbc_profiles(
    price = c(1.5, 2.0, 2.5),
    available = c(TRUE, FALSE),
    brand = c("X", "Y", "Z")
  )

  expect_type(profiles$price, "double")
  expect_type(profiles$available, "logical")
  expect_s3_class(profiles$brand, "factor")
  expect_equal(nrow(profiles), 18)  # 3 * 2 * 3 = 18
})

test_that("Single attribute works", {
  profiles <- cbc_profiles(price = c(1, 2, 3, 4, 5))

  expect_equal(nrow(profiles), 5)
  expect_equal(ncol(profiles), 2)  # profileID + price
  expect_named(profiles, c("profileID", "price"))
})

test_that("Attribute metadata is stored correctly", {
  profiles <- cbc_profiles(
    price = c(1, 2, 3),
    type = c("A", "B"),
    available = c(TRUE, FALSE)
  )

  attr_info <- attr(profiles, "attribute_info")
  expect_type(attr_info, "list")
  expect_named(attr_info, c("price", "type", "available"))

  # Check continuous attribute
  expect_equal(attr_info$price$type, "continuous")
  expect_equal(attr_info$price$n_levels, 3)

  # Check categorical attribute
  expect_equal(attr_info$type$type, "categorical")
  expect_equal(attr_info$type$levels, c("A", "B"))
})

test_that("Original count is tracked", {
  profiles <- cbc_profiles(
    price = c(1, 2),
    type = c("A", "B", "C")
  )

  expect_equal(attr(profiles, "original_count"), 6)
})

test_that("Input validation works", {
  # Test unnamed vectors
  expect_error(
    cbc_profiles(c(1, 2, 3)),
    "must be a named vector"
  )
})

test_that("Logical variables work correctly", {
  profiles <- cbc_profiles(
    price = c(1, 2),
    premium = c(TRUE, FALSE)
  )

  expect_type(profiles$premium, "logical")
  expect_equal(sort(unique(profiles$premium)), c(FALSE, TRUE))
})

test_that("Large factorial works", {
  profiles <- cbc_profiles(
    attr1 = c(1, 2, 3, 4),
    attr2 = c("A", "B", "C"),
    attr3 = c("X", "Y")
  )

  expect_equal(nrow(profiles), 24)  # 4 * 3 * 2 = 24
  expect_equal(length(unique(profiles$profileID)), 24)
})

test_that("Empty attribute levels cause error", {
  expect_error(
    cbc_profiles(price = c())
  )
})

context("Testing cbc_restrict()")

test_that("Restrict profiles based on a single attribute", {
  profiles <- cbcTools::cbc_profiles(
    price = c(1, 1.5, 2),
    type = c("Fuji", "Gala")
  )
  restricted_profiles <- cbcTools::cbc_restrict(profiles, type = "Fuji")
  expected_profiles <- profiles[profiles$type == "Fuji", ]
  expect_identical(restricted_profiles, expected_profiles)
})

test_that("Restrict profiles based on multiple attributes", {
  profiles <- cbcTools::cbc_profiles(
    price = c(1, 1.5, 2),
    type = c("Fuji", "Gala"),
    freshness = c("Poor", "Excellent")
  )
  restricted_profiles <- cbcTools::cbc_restrict(profiles, type = "Fuji", freshness = "Excellent")
  expected_profiles <- profiles[profiles$type == "Fuji" & profiles$freshness == "Excellent", ]
  expect_identical(restricted_profiles, expected_profiles)
})

test_that("Restrict profiles with no matching attributes", {
  profiles <- cbcTools::cbc_profiles(
    price = c(1, 1.5, 2),
    type = c("Fuji", "Gala"),
    freshness = c("Poor", "Excellent")
  )
  restricted_profiles <- cbcTools::cbc_restrict(profiles, type = "Nonexistent")
  expected_profiles <- profiles[FALSE, ]
  expect_identical(restricted_profiles, expected_profiles)
})
context("Testing cbc_restrict()")

test_that("Restrict profiles based on a single restriction", {
  profiles <- cbcTools::cbc_profiles(
    price = c(1, 1.5, 2),
    type = c("Fuji", "Gala")
  )
  restricted_profiles <- cbcTools::cbc_restrict(
    profiles,
    list(type = "Fuji", price = 1.5)
  )
  expected_profiles <- as.data.frame(
     tibble::tribble(
         ~profileID, ~price,  ~type,
                 1L,      1, "Fuji",
                 2L,      2, "Fuji",
                 3L,      1, "Gala",
                 4L,    1.5, "Gala",
                 5L,      2, "Gala"
    )
  )
  expected_profiles$type <- factor(
    expected_profiles$type, levels = c('Fuji', 'Gala'))
  expect_identical(restricted_profiles, expected_profiles)
})


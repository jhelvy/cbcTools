context("Testing cbc_profiles()")

test_that("Full factorial set of profiles created with numeric and character levels", {
  profiles <- cbcTools::cbc_profiles(
    price     = c(1, 1.5, 2),
    type      = c("Fuji", "Gala"),
    freshness = c("Poor", "Excellent")
  )
  comparison <-
     as.data.frame(
         tibble::tribble(
             ~profileID, ~price,  ~type,  ~freshness,
                     1L,      1, "Fuji",      "Poor",
                     2L,    1.5, "Fuji",      "Poor",
                     3L,      2, "Fuji",      "Poor",
                     4L,      1, "Gala",      "Poor",
                     5L,    1.5, "Gala",      "Poor",
                     6L,      2, "Gala",      "Poor",
                     7L,      1, "Fuji", "Excellent",
                     8L,    1.5, "Fuji", "Excellent",
                     9L,      2, "Fuji", "Excellent",
                    10L,      1, "Gala", "Excellent",
                    11L,    1.5, "Gala", "Excellent",
                    12L,      2, "Gala", "Excellent"
         )
     )
  comparison$type <- factor(comparison$type, levels = c('Fuji', 'Gala'))
  comparison$freshness <- factor(comparison$freshness, levels = c('Poor', 'Excellent'))
  expect_identical(profiles, comparison)
})

test_that("Full factorial set of profiles created with logical levels", {
    profiles <- cbcTools::cbc_profiles(
        price     = c(1, 1.5, 2),
        available = c(TRUE, FALSE)
    )
    comparison <-
        as.data.frame(
            tibble::tribble(
                ~profileID, ~price, ~available,
                        1L,      1,       TRUE,
                        2L,    1.5,       TRUE,
                        3L,      2,       TRUE,
                        4L,      1,      FALSE,
                        5L,    1.5,      FALSE,
                        6L,      2,      FALSE
                )

        )
    expect_identical(profiles, comparison)
})

test_that("Full factorial set of profiles created with at least 3 character levels", {
    profiles <- cbcTools::cbc_profiles(
        price     = c(1, 1.5, 2),
        freshness = c("Poor", "Average", "Excellent")
    )
    comparison <-
        as.data.frame(
            tibble::tribble(
                ~profileID, ~price,  ~freshness,
                1L,      1,      "Poor",
                2L,    1.5,      "Poor",
                3L,      2,      "Poor",
                4L,      1,   "Average",
                5L,    1.5,   "Average",
                6L,      2,   "Average",
                7L,      1, "Excellent",
                8L,    1.5, "Excellent",
                9L,      2, "Excellent"
            )
        )
    comparison$freshness <- factor(comparison$freshness, levels = c(
        'Poor', 'Average', 'Excellent'))
    expect_identical(profiles, comparison)
})

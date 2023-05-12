context("Testing cbc_restrict()")

test_that("Restrict profiles based on a single restriction", {
  profiles <- cbcTools::cbc_profiles(
    price = c(1, 1.5, 2),
    type = c("Fuji", "Gala")
  )
  restricted_profiles <- cbcTools::cbc_restrict(
    profiles,
    type == "Fuji" & price == 1.5
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

test_that("Restrict profiles based on two restrictions", {
    profiles <- cbcTools::cbc_profiles(
        price = c(1, 1.5, 2),
        type = c("Fuji", "Gala")
    )
    restricted_profiles <- cbcTools::cbc_restrict(
        profiles,
        type == "Fuji" & price == 1.5,
        type == "Gala" & price == 2
    )
    expected_profiles <- as.data.frame(
        tibble::tribble(
            ~profileID, ~price,  ~type,
            1L,      1, "Fuji",
            2L,      2, "Fuji",
            3L,      1, "Gala",
            4L,    1.5, "Gala"
        )
    )
    expected_profiles$type <- factor(
        expected_profiles$type, levels = c('Fuji', 'Gala'))
    expect_identical(restricted_profiles, expected_profiles)
})

test_that("Restrict profiles based on a complex set of restrictions", {
    profiles <- cbcTools::cbc_profiles(
        price     = c(1, 1.5, 2),
        freshness = c('Poor', 'Average', 'Excellent'),
        type = c("Fuji", "Gala", "Honeycrisp")
    )
    restricted_profiles <- cbcTools::cbc_restrict(
        profiles,
        type == "Fuji" & price == 2,
        type == "Gala" & price == 1,
        type == "Honeycrisp" & price == 1,
        type == "Honeycrisp" & freshness == "Poor"
    )
    expected_profiles <- as.data.frame(
        tibble::tribble(
            ~profileID, ~price,  ~freshness,   ~type,
            1L,      1,      "Poor",       "Fuji",
            2L,    1.5,      "Poor",       "Fuji",
            3L,      1,   "Average",       "Fuji",
            4L,    1.5,   "Average",       "Fuji",
            5L,      1, "Excellent",       "Fuji",
            6L,    1.5, "Excellent",       "Fuji",
            7L,    1.5,      "Poor",       "Gala",
            8L,      2,      "Poor",       "Gala",
            9L,    1.5,   "Average",       "Gala",
            10L,      2,   "Average",       "Gala",
            11L,    1.5, "Excellent",       "Gala",
            12L,      2, "Excellent",       "Gala",
            13L,    1.5,   "Average", "Honeycrisp",
            14L,      2,   "Average", "Honeycrisp",
            15L,    1.5, "Excellent", "Honeycrisp",
            16L,      2, "Excellent", "Honeycrisp"
        )
    )
    expected_profiles$freshness <- factor(
        expected_profiles$freshness, levels = c('Poor', 'Average', 'Excellent'))
    expected_profiles$type <- factor(
        expected_profiles$type, levels = c('Fuji', 'Gala', 'Honeycrisp'))
    expect_identical(restricted_profiles, expected_profiles)
})

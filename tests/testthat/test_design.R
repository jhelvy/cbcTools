context("Testing cbc_design()")

test_that(
  "cbc_design() stops if the number of alternatives per observation is larger than the number of unique profiles", {
  profiles <- cbcTools::cbc_profiles(
    a = c(0, 1),
    b = c("Yes", "No")
  )
  expect_error(
    design <- cbcTools::cbc_design(
      profiles = profiles,
      n_resp = 4,
      n_alts = 5,
      n_q = 4
    )
  )
})

test_that(
  "cbc_design() stops if the number of questions per respondent is larger than the number of unique sets of choice sets", {
    profiles <- cbcTools::cbc_profiles(
      a = c(0, 1),
      b = c("Yes", "No")
    )
    expect_error(
      design <- cbcTools::cbc_design(
        profiles = profiles,
        n_resp = 4,
        n_alts = 2,
        n_q = 7
      )
    )
  })

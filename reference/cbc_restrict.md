# Obtain a restricted set of profiles

This function returns a restricted set of profiles as a data frame.

## Usage

``` r
cbc_restrict(profiles, ...)
```

## Arguments

- profiles:

  A data frame of class `cbc_profiles` created using the
  [`cbc_profiles()`](https://jhelvy.github.io/cbcTools/reference/cbc_profiles.md)
  function.

- ...:

  Any number of restricted pairs of attribute levels, defined as pairs
  of logical expressions separated by commas. For example, the
  restriction `type == 'Fuji' & freshness == 'Poor'` will eliminate
  profiles such that `"Fuji"` type apples will never be shown with
  `"Poor"` freshness.

## Value

A restricted set of profiles as a data frame with class `cbc_profiles`.

## Examples

``` r
library(cbcTools)

# Generate all profiles for a simple conjoint experiment about apples
profiles <- cbc_profiles(
  price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
  type      = c("Fuji", "Gala", "Honeycrisp"),
  freshness = c('Poor', 'Average', 'Excellent')
)

# Obtain a restricted subset of profiles based on pairs of logical
# expressions. The example below contains the following restrictions:

# - `"Gala"` apples will not be shown with the prices `1.5`, `2.5`, & `3.5`.
# - `"Honeycrisp"` apples will not be shown with prices less than `2`.
# - `"Honeycrisp"` apples will not be shown with the `"Poor"` freshness.
# - `"Fuji"` apples will not be shown with the `"Excellent"` freshness.

profiles_restricted <- cbc_restrict(
    profiles,
    type == "Gala" & price %in% c(1.5, 2.5, 3.5),
    type == "Honeycrisp" & price > 2,
    type == "Honeycrisp" & freshness == "Poor",
    type == "Fuji" & freshness == "Excellent"
)
```

# Make a data frame of all combinations of attribute levels

This function creates a data frame of of all possible combinations of
attribute levels.

## Usage

``` r
cbc_profiles(...)
```

## Arguments

- ...:

  Any number of named vectors defining each attribute and their levels,
  e.g. `price = c(1, 2, 3)`. Separate each vector by a comma.

## Value

A data frame of all possible combinations of attribute levels with class
`cbc_profiles`.

## Examples

``` r
library(cbcTools)

# Generate all profiles for a simple conjoint experiment about apples
profiles <- cbc_profiles(
  price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
  type      = c("Fuji", "Gala", "Honeycrisp"),
  freshness = c('Poor', 'Average', 'Excellent')
)
```

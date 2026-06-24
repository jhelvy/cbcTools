# Simulate choices for a survey design

Simulate choices for a survey design, either randomly or according to a
utility model defined by user-provided prior parameters. When priors are
provided, choices are simulated using the same probability computation
framework as used in cbc_design() for consistency.

## Usage

``` r
cbc_choices(design, priors = NULL)
```

## Arguments

- design:

  A `cbc_design` object created by
  [`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md)

- priors:

  A `cbc_priors` object created by
  [`cbc_priors()`](https://jhelvy.github.io/cbcTools/reference/cbc_priors.md),
  or `NULL` (default) for random choices.

## Value

Returns the input design with an additional `choice` column identifying
the simulated choices.

## Examples

``` r
library(cbcTools)

# Create profiles and design
profiles <- cbc_profiles(
  price = c(1, 2, 3),
  type = c("A", "B", "C"),
  quality = c("Low", "High")
)

design <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 4
)

# Simulate random choices (default)
choices_random <- cbc_choices(design)

# Create priors and simulate utility-based choices
priors <- cbc_priors(
  profiles = profiles,
  price = -0.1,
  type = c(0.5, 0.2),  # vs reference level
  quality = 0.3
)

choices_utility <- cbc_choices(design, priors = priors)
```

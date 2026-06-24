# Variable Encoding

This article demonstrates how to convert between different encoding
schemes for categorical variables in choice-based conjoint designs using
the
[`cbc_encode()`](https://jhelvy.github.io/cbcTools/reference/cbc_encode.md)
function.

## Overview

Choice-based conjoint data can use different encoding schemes for
categorical variables:

- **Standard encoding**: Categorical variables represented as factors or
  characters
- **Dummy coding**: Binary indicators with a reference category (all
  zeros)
- **Effects coding**: Coded as -1, 0, or 1 to ensure coefficients sum to
  zero

The
[`cbc_encode()`](https://jhelvy.github.io/cbcTools/reference/cbc_encode.md)
function allows you to convert between these encodings and customize
reference levels.

## Basic Encoding Conversion

### Creating a Design

Let’s start by creating a simple design:

``` r

library(cbcTools)

# Create profiles
profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c("Fuji", "Gala", "Honeycrisp"),
  freshness = c("Poor", "Average", "Excellent")
)

# Create design (uses standard encoding by default)
design <- cbc_design(
  profiles = profiles,
  n_alts = 3,
  n_q = 6,
  n_resp = 100,
  method = "random"
)

head(design)
#> Design method: random
#> Encoding: standard
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price       type freshness
#> 1        45      1   1     1     1   3.0 Honeycrisp Excellent
#> 2        23      1   1     2     1   2.0       Gala   Average
#> 3        12      1   1     3     1   1.5 Honeycrisp      Poor
#> 4        37      1   2     1     2   1.5       Gala Excellent
#> 5        38      1   2     2     2   2.0       Gala Excellent
#> 6        31      1   2     3     2   1.0       Fuji Excellent
```

By default, designs are created with **standard encoding** where
categorical variables remain as factors.

### Converting to Dummy Coding

Convert to dummy coding for model estimation:

``` r

design_dummy <- cbc_encode(design, coding = "dummy")
head(design_dummy)
#> Design method: random
#> Encoding: dummy
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price typeGala typeHoneycrisp
#> 1        45      1   1     1     1   3.0        0              1
#> 2        23      1   1     2     1   2.0        1              0
#> 3        12      1   1     3     1   1.5        0              1
#> 4        37      1   2     1     2   1.5        1              0
#> 5        38      1   2     2     2   2.0        1              0
#> 6        31      1   2     3     2   1.0        0              0
#>   freshnessAverage freshnessExcellent
#> 1                0                  1
#> 2                1                  0
#> 3                0                  0
#> 4                0                  1
#> 5                0                  1
#> 6                0                  1
```

Notice that: - The `type` variable is replaced with `typeGala` and
`typeHoneycrisp` - The `freshness` variable is replaced with
`freshnessAverage` and `freshnessExcellent` - `Fuji` and `Poor` are the
reference levels (represented when dummy variables = 0) - Continuous
variables like `price` remain unchanged

### Converting to Effects Coding

Effects coding uses -1 for the reference level:

``` r

design_effects <- cbc_encode(design, coding = "effects")
head(design_effects)
#> Design method: random
#> Encoding: effects
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price typeGala typeHoneycrisp
#> 1        45      1   1     1     1   3.0        0              1
#> 2        23      1   1     2     1   2.0        1              0
#> 3        12      1   1     3     1   1.5        0              1
#> 4        37      1   2     1     2   1.5        1              0
#> 5        38      1   2     2     2   2.0        1              0
#> 6        31      1   2     3     2   1.0       -1             -1
#>   freshnessAverage freshnessExcellent
#> 1                0                  1
#> 2                1                  0
#> 3               -1                 -1
#> 4                0                  1
#> 5                0                  1
#> 6                0                  1
```

In effects coding: - Non-reference levels are coded as 0 or 1 (same as
dummy) - Reference level rows have -1 for all level indicators - This
ensures coefficients sum to zero

### Converting Back to Standard

Convert back to categorical variables:

``` r

design_standard <- cbc_encode(design_dummy, coding = "standard")
head(design_standard)
#> Design method: random
#> Encoding: standard
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price       type freshness
#> 1        45      1   1     1     1   3.0 Honeycrisp Excellent
#> 2        23      1   1     2     1   2.0       Gala   Average
#> 3        12      1   1     3     1   1.5 Honeycrisp      Poor
#> 4        37      1   2     1     2   1.5       Gala Excellent
#> 5        38      1   2     2     2   2.0       Gala Excellent
#> 6        31      1   2     3     2   1.0       Fuji Excellent
```

## Customizing Reference Levels

By default, the first level of each categorical variable is used as the
reference. You can specify different reference levels using the
`ref_levels` argument.

### Setting Custom References

``` r

# Use "Honeycrisp" as reference for type, "Excellent" for freshness
design_custom <- cbc_encode(
  design,
  coding = "dummy",
  ref_levels = list(
    type = "Honeycrisp",
    freshness = "Excellent"
  )
)

head(design_custom)
#> Design method: random
#> Encoding: dummy
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price typeFuji typeGala freshnessPoor
#> 1        45      1   1     1     1   3.0        0        0             0
#> 2        23      1   1     2     1   2.0        0        1             0
#> 3        12      1   1     3     1   1.5        0        0             1
#> 4        37      1   2     1     2   1.5        0        1             0
#> 5        38      1   2     2     2   2.0        0        1             0
#> 6        31      1   2     3     2   1.0        1        0             0
#>   freshnessAverage
#> 1                0
#> 2                1
#> 3                0
#> 4                0
#> 5                0
#> 6                0
```

Now `Honeycrisp` and `Excellent` are the reference categories.

### Updating References Without Changing Encoding

You can update reference levels while keeping the current encoding:

``` r

# Start with dummy coding
design_dummy <- cbc_encode(design, coding = "dummy")

# Update reference levels only (keeps dummy coding)
design_updated <- cbc_encode(
  design_dummy,
  ref_levels = list(type = "Gala")
)

head(design_updated)
#> Design method: random
#> Encoding: dummy
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price typeFuji typeHoneycrisp
#> 1        45      1   1     1     1   3.0        0              1
#> 2        23      1   1     2     1   2.0        0              0
#> 3        12      1   1     3     1   1.5        0              1
#> 4        37      1   2     1     2   1.5        0              0
#> 5        38      1   2     2     2   2.0        0              0
#> 6        31      1   2     3     2   1.0        1              0
#>   freshnessAverage freshnessExcellent
#> 1                0                  1
#> 2                1                  0
#> 3                0                  0
#> 4                0                  1
#> 5                0                  1
#> 6                0                  1
```

## Working with No-Choice Options

When using designs with no-choice options, you should convert to dummy
coding before power analysis or model estimation:

``` r

# Create profiles
profiles_nc <- cbc_profiles(
  price = c(1, 2, 3),
  quality = c("Low", "High")
)

# Create priors including no-choice
priors_nc <- cbc_priors(
  profiles = profiles_nc,
  price = -0.1,
  quality = c("High" = 0.5),
  no_choice = -1.5
)

# Create design with no-choice
design_nc <- cbc_design(
  profiles = profiles_nc,
  priors = priors_nc,
  n_alts = 2,
  n_q = 4,
  n_resp = 50,
  no_choice = TRUE,
  method = "random"
)

# Simulate choices
choices_nc <- cbc_choices(design_nc, priors_nc)

head(choices_nc)
#> CBC Choice Data
#> ===============
#> Encoding: dummy
#> Observations: 2 choice tasks
#> Alternatives per task: 3
#> Total choices made: 2
#> 
#> Simulation method: utility_based
#> Priors: Used for utility-based simulation
#> Simulated at: 2026-06-24 10:41:50
#> 
#> Choice rates by alternative:
#>   Alt 1: 50.0% (1 choices)
#>   Alt 2: 50.0% (1 choices)
#>   Alt 3: 0.0% (0 choices)
#> 
#> No-choice rate: 0.0%
#> 
#> 💡 Use cbc_encode(choices, 'standard') to view categorical format
#> 
#> First few rows:
#>   profileID respID qID altID obsID price no_choice qualityHigh choice
#> 1         2      1   1     1     1     2         0           0      0
#> 2         6      1   1     2     1     3         0           1      1
#> 3         0      1   1     3     1     0         1           0      0
#> 4         6      1   2     1     2     3         0           1      1
#> 5         1      1   2     2     2     1         0           0      0
#> 6         0      1   2     3     2     0         1           0      0
```

For modeling or power analysis with no-choice data, convert to dummy or
effects coding:

``` r

# Convert to dummy coding for power analysis
choices_dummy <- cbc_encode(choices_nc, coding = "dummy")

# Run power analysis
power_result <- cbc_power(
  data = choices_dummy,
  n_breaks = 5
)

power_result
```

## Use Cases

### For Model Estimation

While it is not required for the `logitr` package, encoding the data
into dummy or effects coding can be helpful when estimating models for
easier interpretation or simply greater control over which levels are
included in the model:

``` r

library(logitr)

# Convert to dummy coding
choices_dummy <- cbc_encode(choices, coding = "dummy")

# Estimate model
model <- logitr(
  data = choices_dummy,
  outcome = "choice",
  obsID = "obsID",
  pars = c("price", "typeGala", "typeHoneycrisp",
           "freshnessAverage", "freshnessExcellent")
)
```

### For Data Inspection

It is generally easier to inspect your data when using standard
encoding:

``` r

# Work with categorical variables
choices_standard <- design

# Filter for chosen alternatives
chosen <- choices_standard[sample(1:nrow(choices_standard), 100), ]

# Examine choice frequencies by category
table(chosen$type)
#> 
#>       Fuji       Gala Honeycrisp 
#>         45         25         30
table(chosen$freshness)
#> 
#>      Poor   Average Excellent 
#>        33        37        30

# Use cbc_inspect
cbc_inspect(choices_standard, sections = 'balance')
#> DESIGN SUMMARY
#> =========================
#> 
#> ATTRIBUTE BALANCE
#> =================
#> Overall balance score: 0.972 (higher is better)
#> 
#> Individual attribute level counts:
#> 
#> price:
#> 
#>   1 1.5   2 2.5   3 
#> 372 362 348 362 356 
#>   Balance score: 0.976 (higher is better)
#> 
#> type:
#> 
#>       Fuji       Gala Honeycrisp 
#>        605        612        583 
#>   Balance score: 0.975 (higher is better)
#> 
#> freshness:
#> 
#>      Poor   Average Excellent 
#>       624       593       583 
#>   Balance score: 0.966 (higher is better)
```

### For Power Analysis

You can use either encoding, but results differ:

``` r

# Dummy coding: estimates for each level
power_dummy <- cbc_power(
  cbc_encode(choices, coding = "dummy"),
  n_breaks = 5
)

# Standard coding: estimates categorical effect
power_standard <- cbc_power(
  cbc_encode(choices, coding = "standard"),
  pars = c("price", "type", "freshness"),
  n_breaks = 5
)
```

# Encode categorical variables in a CBC design

This function converts categorical variables between different coding
schemes. Standard coding keeps categorical variables as-is (factor or
character). Dummy coding uses a reference category (all zeros) with
indicator variables for other levels. Effects coding uses -1 for the
reference category to ensure coefficients sum to zero.

## Usage

``` r
cbc_encode(data, coding = NULL, ref_levels = NULL)
```

## Arguments

- data:

  A `cbc_design` or `cbc_choices` object

- coding:

  Character. Type of encoding: "standard", "dummy", or "effects". If
  NULL and ref_levels is NULL, data is returned unchanged. If NULL and
  ref_levels is specified, the current encoding is maintained.

- ref_levels:

  Named list specifying reference levels for categorical variables. For
  example: `list(powertrain = "Gasoline", brand = "A")`. If NULL
  (default), uses the first level of each categorical variable as
  reference.

## Value

The input object with specified encoding applied

## Examples

``` r
library(cbcTools)

# Create profiles with categorical variables
profiles <- cbc_profiles(
  price = c(10, 20, 30),
  quality = c("Low", "Medium", "High"),
  brand = c("A", "B")
)

# Create design (defaults to standard coding)
design <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 4
)

# Convert to dummy coding
design_dummy <- cbc_encode(design, coding = "dummy")
head(design_dummy)
#> Design method: random
#> Encoding: dummy
#> Structure: 100 respondents × 4 questions × 2 alternatives
#> Profile usage: 18/18 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price qualityMedium qualityHigh brandB
#> 1         9      1   1     1     1    30             0           1      0
#> 2        11      1   1     2     1    20             0           0      1
#> 3        16      1   2     1     2    10             0           1      1
#> 4        10      1   2     2     2    10             0           0      1
#> 5         4      1   3     1     3    10             1           0      0
#> 6         9      1   3     2     3    30             0           1      0

# Convert to effects coding
design_effects <- cbc_encode(design, coding = "effects")
head(design_effects)
#> Design method: random
#> Encoding: effects
#> Structure: 100 respondents × 4 questions × 2 alternatives
#> Profile usage: 18/18 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price qualityMedium qualityHigh brandB
#> 1         9      1   1     1     1    30             0           1     -1
#> 2        11      1   1     2     1    20            -1          -1      1
#> 3        16      1   2     1     2    10             0           1      1
#> 4        10      1   2     2     2    10            -1          -1      1
#> 5         4      1   3     1     3    10             1           0     -1
#> 6         9      1   3     2     3    30             0           1     -1

# Convert back to standard
design_standard <- cbc_encode(design_dummy, coding = "standard")
head(design_standard)
#> Design method: random
#> Encoding: standard
#> Structure: 100 respondents × 4 questions × 2 alternatives
#> Profile usage: 18/18 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price quality brand
#> 1         9      1   1     1     1    30    High     A
#> 2        11      1   1     2     1    20     Low     B
#> 3        16      1   2     1     2    10    High     B
#> 4        10      1   2     2     2    10     Low     B
#> 5         4      1   3     1     3    10  Medium     A
#> 6         9      1   3     2     3    30    High     A

# Custom reference levels with dummy coding
design_dummy2 <- cbc_encode(
  design,
  coding = "dummy",
  ref_levels = list(quality = "Medium", brand = "B")
)
head(design_dummy2)
#> Design method: random
#> Encoding: dummy
#> Structure: 100 respondents × 4 questions × 2 alternatives
#> Profile usage: 18/18 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price qualityLow qualityHigh brandA
#> 1         9      1   1     1     1    30          0           1      1
#> 2        11      1   1     2     1    20          1           0      0
#> 3        16      1   2     1     2    10          0           1      0
#> 4        10      1   2     2     2    10          1           0      0
#> 5         4      1   3     1     3    10          0           0      1
#> 6         9      1   3     2     3    30          0           1      1

# Update reference levels without changing encoding
design_updated <- cbc_encode(
  design_dummy,
  ref_levels = list(quality = "High")
)
head(design_updated)
#> Design method: random
#> Encoding: dummy
#> Structure: 100 respondents × 4 questions × 2 alternatives
#> Profile usage: 18/18 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price qualityLow qualityMedium brandB
#> 1         9      1   1     1     1    30          0             0      0
#> 2        11      1   1     2     1    20          1             0      1
#> 3        16      1   2     1     2    10          0             0      1
#> 4        10      1   2     2     2    10          1             0      1
#> 5         4      1   3     1     3    10          0             1      0
#> 6         9      1   3     2     3    30          0             0      0
```

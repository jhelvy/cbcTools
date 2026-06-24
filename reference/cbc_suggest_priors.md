# Suggest prior specifications for CBC models

Provides automated suggestions for prior specifications based on
attribute scales and desired effect sizes. This function analyzes your
profile attributes and generates starting values for priors that can be
refined based on domain knowledge.

## Usage

``` r
cbc_suggest_priors(
  profiles,
  effect_size = c("moderate", "small", "large"),
  categorical_effect = NULL,
  direction = NULL
)
```

## Arguments

- profiles:

  A data frame of profiles created by
  [`cbc_profiles()`](https://jhelvy.github.io/cbcTools/reference/cbc_profiles.md)

- effect_size:

  Character. Desired effect size: `"small"` (0.1-0.3), `"moderate"`
  (0.3-1.0, default), or `"large"` (1.0-3.0). Controls the utility range
  for continuous attributes.

- categorical_effect:

  Numeric. Effect size increment for categorical levels. Defaults to
  `0.1` for small, `0.2` for moderate, and `0.5` for large effect sizes.
  Categorical levels are spaced evenly from 0 to
  `categorical_effect * (n_levels - 1)`.

- direction:

  Named list mapping attribute names to `"positive"` or `"negative"` to
  specify the direction of the effect. Useful for attributes like price
  where you expect a negative relationship. Defaults to `NULL` (positive
  direction for all attributes).

## Value

Invisibly returns a named list of suggested prior values. The function
primarily prints formatted output to the console.

## Details

This function analyzes each attribute in your profiles and suggests
prior parameter values scaled to produce the desired effect size. For
continuous attributes, coefficients are scaled so that the full
attribute range produces a utility contribution matching the target
effect size. For categorical attributes, levels are assigned evenly
spaced coefficients with the first level as the reference.

These suggestions are **starting points** that should be refined based
on:

- Domain knowledge about attribute importance

- Previous research or pilot studies

- Expected relative importance between attributes

- Sign considerations (e.g., negative coefficients for cost)

The function prints a formatted output with:

- Copy-paste ready code for
  [`cbc_priors()`](https://jhelvy.github.io/cbcTools/reference/cbc_priors.md)

- Explanation of suggested values

- Expected utility ranges

- Notes and recommendations

## Examples

``` r
library(cbcTools)

# Create example profiles
profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

# Get moderate effect size suggestions (default)
cbc_suggest_priors(profiles)
#> 
#> ========================================
#> Suggested Priors (moderate effects)
#> ========================================
#> 
#> Copy-paste this into your code:
#> 
#> priors <- cbc_priors(
#>   profiles,
#>   price = 0.325000,
#>   type = c('Gala' = 0.20, 'Honeycrisp' = 0.40),
#>   freshness = c('Average' = 0.20, 'Excellent' = 0.40)
#> )
#> 
#> Explanation:
#> ----------------------------------------
#>   price: Range [1.00, 3.00] -> suggested 0.325000 (utility range: ±0.65) 
#>   type: 3 levels -> suggested c(Gala = 0.20, Honeycrisp = 0.40) with 'Fuji' as reference 
#>   freshness: 3 levels -> suggested c(Average = 0.20, Excellent = 0.40) with 'Poor' as reference 
#> 
#> Notes:
#> - These are STARTING suggestions based on attribute scales
#> - Adjust based on your domain knowledge and research goals
#> - Continuous attributes are scaled to produce moderate effects
#> - For price/cost attributes, consider using negative coefficients
#> - The total utility range should stay roughly between -5 and +5
#>   to avoid numerical overflow during optimization
#> 

# Get small effect size suggestions
cbc_suggest_priors(profiles, effect_size = "small")
#> 
#> ========================================
#> Suggested Priors (small effects)
#> ========================================
#> 
#> Copy-paste this into your code:
#> 
#> priors <- cbc_priors(
#>   profiles,
#>   price = 0.100000,
#>   type = c('Gala' = 0.10, 'Honeycrisp' = 0.20),
#>   freshness = c('Average' = 0.10, 'Excellent' = 0.20)
#> )
#> 
#> Explanation:
#> ----------------------------------------
#>   price: Range [1.00, 3.00] -> suggested 0.100000 (utility range: ±0.20) 
#>   type: 3 levels -> suggested c(Gala = 0.10, Honeycrisp = 0.20) with 'Fuji' as reference 
#>   freshness: 3 levels -> suggested c(Average = 0.10, Excellent = 0.20) with 'Poor' as reference 
#> 
#> Notes:
#> - These are STARTING suggestions based on attribute scales
#> - Adjust based on your domain knowledge and research goals
#> - Continuous attributes are scaled to produce small effects
#> - For price/cost attributes, consider using negative coefficients
#> - The total utility range should stay roughly between -5 and +5
#>   to avoid numerical overflow during optimization
#> 

# Get large effect size suggestions
cbc_suggest_priors(profiles, effect_size = "large")
#> 
#> ========================================
#> Suggested Priors (large effects)
#> ========================================
#> 
#> Copy-paste this into your code:
#> 
#> priors <- cbc_priors(
#>   profiles,
#>   price = 1.000000,
#>   type = c('Gala' = 0.50, 'Honeycrisp' = 1.00),
#>   freshness = c('Average' = 0.50, 'Excellent' = 1.00)
#> )
#> 
#> Explanation:
#> ----------------------------------------
#>   price: Range [1.00, 3.00] -> suggested 1.000000 (utility range: ±2.00) 
#>   type: 3 levels -> suggested c(Gala = 0.50, Honeycrisp = 1.00) with 'Fuji' as reference 
#>   freshness: 3 levels -> suggested c(Average = 0.50, Excellent = 1.00) with 'Poor' as reference 
#> 
#> Notes:
#> - These are STARTING suggestions based on attribute scales
#> - Adjust based on your domain knowledge and research goals
#> - Continuous attributes are scaled to produce large effects
#> - For price/cost attributes, consider using negative coefficients
#> - The total utility range should stay roughly between -5 and +5
#>   to avoid numerical overflow during optimization
#> 

# Specify direction for price (should be negative)
suggestions <- cbc_suggest_priors(
  profiles,
  effect_size = "moderate",
  direction = list(price = "negative")
)
#> 
#> ========================================
#> Suggested Priors (moderate effects)
#> ========================================
#> 
#> Copy-paste this into your code:
#> 
#> priors <- cbc_priors(
#>   profiles,
#>   price = -0.325000,
#>   type = c('Gala' = 0.20, 'Honeycrisp' = 0.40),
#>   freshness = c('Average' = 0.20, 'Excellent' = 0.40)
#> )
#> 
#> Explanation:
#> ----------------------------------------
#>   price: Range [1.00, 3.00] -> suggested -0.325000 (utility range: ±0.65) 
#>   type: 3 levels -> suggested c(Gala = 0.20, Honeycrisp = 0.40) with 'Fuji' as reference 
#>   freshness: 3 levels -> suggested c(Average = 0.20, Excellent = 0.40) with 'Poor' as reference 
#> 
#> Notes:
#> - These are STARTING suggestions based on attribute scales
#> - Adjust based on your domain knowledge and research goals
#> - Continuous attributes are scaled to produce moderate effects
#> - For price/cost attributes, consider using negative coefficients
#> - The total utility range should stay roughly between -5 and +5
#>   to avoid numerical overflow during optimization
#> 

# Use suggestions as a starting point
# Then adjust based on domain knowledge
priors <- cbc_priors(
  profiles,
  price = -0.5,  # Adjusted from suggestion
  type = c("Gala" = 0.3, "Honeycrisp" = 0.8),
  freshness = c("Average" = 0.4, "Excellent" = 0.9)
)
```

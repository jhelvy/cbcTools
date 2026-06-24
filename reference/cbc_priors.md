# Create prior specifications for CBC models

Creates a standardized prior specification object for use in CBC
analysis functions like
[`cbc_choices()`](https://jhelvy.github.io/cbcTools/reference/cbc_choices.md)
and
[`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md).
Supports both fixed and random parameters, with flexible specification
of categorical variable levels and interaction terms between fixed
parameters.

## Usage

``` r
cbc_priors(
  profiles,
  no_choice = NULL,
  n_draws = 100,
  draw_type = "halton",
  interactions = NULL,
  ...
)
```

## Arguments

- profiles:

  A data frame of profiles created by
  [`cbc_profiles()`](https://jhelvy.github.io/cbcTools/reference/cbc_profiles.md)

- no_choice:

  Prior specification for no-choice alternative. Can be:

  - A single numeric value for fixed no-choice utility

  - A
    [`rand_spec()`](https://jhelvy.github.io/cbcTools/reference/rand_spec.md)
    object for random no-choice utility

  - `NULL` if no no-choice option (default)

- n_draws:

  Number of draws for DB-error calculation if using Bayesian priors.
  Defaults to `100`

- draw_type:

  Specify the draw type as a character: `"halton"` (the default) or
  `"sobol"` (recommended for models with more than 5 random parameters).

- interactions:

  A list of interaction specifications created by
  [`int_spec()`](https://jhelvy.github.io/cbcTools/reference/int_spec.md).
  Only interactions between fixed (non-random) parameters are supported.
  Each interaction must specify the appropriate level(s) for categorical
  variables. Defaults to `NULL` (no interactions).

- ...:

  Named arguments specifying priors for each attribute:

  - For fixed parameters:

    - Continuous variables: provide a single numeric value

    - Categorical variables: provide either:

      - An unnamed vector of values one less than the number of levels
        (dummy coding)

      - A named vector mapping levels to coefficients (remaining level
        becomes reference)

  - For random parameters: use
    [`rand_spec()`](https://jhelvy.github.io/cbcTools/reference/rand_spec.md)
    to specify distribution, parameters, and correlations

## Value

A structured prior specification object including parameter draws for
random coefficients and interaction terms. This object contains:

- `pars`: Vector of mean parameter values

- `par_draws`: Matrix of parameter draws (if random parameters
  specified)

- `correlation`: Correlation matrix for random parameters (if
  applicable)

- `interactions`: List of interaction specifications

- `attrs`: Detailed attribute information

- Additional metadata for validation and compatibility checking

## Details

### Fixed vs Random Parameters

**Fixed parameters** assume all respondents have the same preference
coefficients. Specify these as simple numeric values.

**Random parameters** assume preference coefficients vary across
respondents according to a specified distribution. Use
[`rand_spec()`](https://jhelvy.github.io/cbcTools/reference/rand_spec.md)
to define the distribution type, mean, and standard deviation.

### Categorical Variable Specification

For categorical variables, you can specify priors in two ways:

1.  **Unnamed vector**: Provide coefficients for all levels except the
    first (which becomes the reference level). Order matters and should
    match the natural order of levels.

2.  **Named vector**: Explicitly map coefficient values to specific
    levels. Any level not specified becomes the reference level.

### Interaction Terms

Use the `interactions` parameter with
[`int_spec()`](https://jhelvy.github.io/cbcTools/reference/int_spec.md)
to include interaction effects between attributes. Only interactions
between fixed parameters are supported. For categorical variables
involved in interactions, you must specify the relevant levels.

### No-Choice Options

When including a no-choice alternative, provide a `no_choice` parameter.
This can be either a fixed numeric value or a
[`rand_spec()`](https://jhelvy.github.io/cbcTools/reference/rand_spec.md)
for random no-choice utility.

## Examples

``` r
library(cbcTools)

# Create profiles for examples
profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

# Example 1: Simple fixed priors
priors_fixed <- cbc_priors(
  profiles = profiles,
  price = -0.25, # Negative = prefer lower prices
  type = c(0.5, 1.0), # "Fuji" is reference level
  freshness = c(0.6, 1.2) # "Poor" reference level
)

# Example 2: Named categorical priors (more explicit)
priors_named <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c("Gala" = 0.5, "Honeycrisp" = 1.0),  # "Fuji" is reference
  freshness = c("Average" = 0.6, "Excellent" = 1.2)  # "Poor" is reference
)

# Example 3: Random parameters - normal distributions for "price" and "freshness"
priors_random <- cbc_priors(
  profiles = profiles,
  price = rand_spec(
    dist = "n",
    mean = -0.25,
    sd = 0.1
  ),
  type = c(0.5, 1.0),
  freshness = rand_spec(
    dist = "n",
    mean = c(0.6, 1.2),
    sd = c(0.1, 0.1)
  )
)

# Example 4: Correlated random parameters
priors_correlated <- cbc_priors(
  profiles = profiles,
  price = rand_spec(
    dist = "n",
    mean = -0.1,
    sd = 0.05,
    correlations = list(
      cor_spec(
        with = "type",
        with_level = "Honeycrisp",
        value = 0.3
      )
    )
  ),
  type = rand_spec(
    dist = "n",
    mean = c("Gala" = 0.1, "Honeycrisp" = 0.2),
    sd = c("Gala" = 0.05, "Honeycrisp" = 0.1)
  ),
  freshness = c(0.1, 0.2)
)

# Example 5: With interaction terms
priors_interactions <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c("Fuji" = 0.5, "Honeycrisp" = 1.0),
  freshness = c("Average" = 0.6, "Excellent" = 1.2),
  interactions = list(
    # Price sensitivity varies by apple type
    int_spec(
      between = c("price", "type"),
      with_level = "Fuji",
      value = 0.1
    ),
    int_spec(
      between = c("price", "type"),
      with_level = "Honeycrisp",
      value = 0.2
    ),
    # Type preferences vary by freshness
    int_spec(
      between = c("type", "freshness"),
      level = "Honeycrisp",
      with_level = "Excellent",
      value = 0.3
    )
  )
)

# Example 6: Including no-choice option
priors_nochoice_fixed <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c(0.5, 1.0),
  freshness = c(0.6, 1.2),
  no_choice = -0.5 # Negative values make no-choice less attractive
)

# Example 7: Random no-choice
priors_nochoice_random <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c(0.5, 1.0),
  freshness = c(0.6, 1.2),
  no_choice = rand_spec(dist = "n", mean = -0.5, sd = 0.2)
)

# View the priors
priors_fixed
#> CBC Prior Specifications:
#> 
#> price:
#>   Continuous variable
#>   Levels: 1, 1.5, 2, 2.5, 3 
#>   Fixed parameter
#>     Coefficient: -0.25
#> 
#> type:
#>   Categorical variable
#>   Levels: Fuji, Gala, Honeycrisp 
#>   Reference level: Fuji
#>   Fixed parameter
#>     Gala: 0.5
#>     Honeycrisp: 1
#> 
#> freshness:
#>   Categorical variable
#>   Levels: Poor, Average, Excellent 
#>   Reference level: Poor
#>   Fixed parameter
#>     Average: 0.6
#>     Excellent: 1.2
#> 
priors_random
#> CBC Prior Specifications:
#> 
#> price:
#>   Continuous variable
#>   Levels: 1, 1.5, 2, 2.5, 3 
#>   Random - Normal distribution
#>     Mean: -0.25
#>     SD:   0.1
#> 
#> type:
#>   Categorical variable
#>   Levels: Fuji, Gala, Honeycrisp 
#>   Reference level: Fuji
#>   Fixed parameter
#>     Gala: 0.5
#>     Honeycrisp: 1
#> 
#> freshness:
#>   Categorical variable
#>   Levels: Poor, Average, Excellent 
#>   Reference level: Poor
#>   Random - Normal distribution
#>     Average:
#>       Mean: 0.6
#>       SD:   0.1
#>     Excellent:
#>       Mean: 1.2
#>       SD:   0.1
#> 
#> Correlation Matrix:
#>                    price freshnessAverage freshnessExcellent
#> price                  1                0                  0
#> freshnessAverage       0                1                  0
#> freshnessExcellent     0                0                  1
```

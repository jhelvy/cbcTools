# Simulating Choices

Choice simulation converts experimental designs into realistic choice
data by predicting how respondents would answer choice questions. This
is essential for testing designs, conducting power analyses, and
validating experimental assumptions before data collection. This article
shows how to use
[`cbc_choices()`](https://jhelvy.github.io/cbcTools/reference/cbc_choices.md)
to simulate choice patterns.

Before starting, let’s define some basic profiles and a basic random
design to work with:

``` r

library(cbcTools)

profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

design <- cbc_design(
  profiles = profiles,
  method = "random",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

design
#> Design method: random
#> Encoding: standard
#> Structure: 100 respondents × 6 questions × 2 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price       type freshness
#> 1        31      1   1     1     1   1.0       Fuji Excellent
#> 2        15      1   1     2     1   3.0 Honeycrisp      Poor
#> 3        14      1   2     1     2   2.5 Honeycrisp      Poor
#> 4         3      1   2     2     2   2.0       Fuji      Poor
#> 5        42      1   3     1     3   1.5 Honeycrisp Excellent
#> 6        43      1   3     2     3   2.0 Honeycrisp Excellent
#> ... and 1194 more rows
```

## Choice Simulation Approaches

[`cbc_choices()`](https://jhelvy.github.io/cbcTools/reference/cbc_choices.md)
supports two simulation approaches:

1.  **Random simulation**: Each alternative has equal probability of
    being chosen
2.  **Utility-based simulation**: Choice probabilities based on
    multinomial logit model with specified priors

### Random Choices

Without priors, choices are simulated randomly with equal probabilities:

``` r

# Random choice simulation (default)
choices_random <- cbc_choices(design)

head(choices_random)
#> CBC Choice Data
#> ===============
#> Encoding: standard
#> Observations: 3 choice tasks
#> Alternatives per task: 2
#> Total choices made: 3
#> 
#> Simulation method: random
#> Priors: None (random choices)
#> Simulated at: 2026-06-24 10:41:32
#> 
#> Choice rates by alternative:
#>   Alt 1: 33.3% (1 choices)
#>   Alt 2: 66.7% (2 choices)
#> 
#> First few rows:
#>   profileID respID qID altID obsID price       type freshness choice
#> 1        31      1   1     1     1   1.0       Fuji Excellent      0
#> 2        15      1   1     2     1   3.0 Honeycrisp      Poor      1
#> 3        14      1   2     1     2   2.5 Honeycrisp      Poor      0
#> 4         3      1   2     2     2   2.0       Fuji      Poor      1
#> 5        42      1   3     1     3   1.5 Honeycrisp Excellent      1
#> 6        43      1   3     2     3   2.0 Honeycrisp Excellent      0

# Check choice distribution
table(choices_random$choice, choices_random$altID)
#>    
#>       1   2
#>   0 294 306
#>   1 306 294
```

Random simulation is useful for:

- Quick testing of design structure
- Conservative power analysis (worst-case scenario)
- Baseline comparisons

### Utility-Based Choices

With priors, choices follow realistic utility-based patterns:

``` r

# Create priors for utility-based simulation
priors <- cbc_priors(
  profiles = profiles,
  price = -0.25, # Negative preference for higher prices
  type = c(0.5, 1), # Gala and Honeycrisp preferred over Fuji
  freshness = c(0.6, 1.2) # Average and Excellent preferred over Poor
)

# Utility-based choice simulation
choices_utility <- cbc_choices(design, priors = priors)

head(choices_utility)
#> CBC Choice Data
#> ===============
#> Encoding: standard
#> Observations: 3 choice tasks
#> Alternatives per task: 2
#> Total choices made: 3
#> 
#> Simulation method: utility_based
#> Priors: Used for utility-based simulation
#> Simulated at: 2026-06-24 10:41:33
#> 
#> Choice rates by alternative:
#>   Alt 1: 66.7% (2 choices)
#>   Alt 2: 33.3% (1 choices)
#> 
#> First few rows:
#>   profileID respID qID altID obsID price       type freshness choice
#> 1        31      1   1     1     1   1.0       Fuji Excellent      1
#> 2        15      1   1     2     1   3.0 Honeycrisp      Poor      0
#> 3        14      1   2     1     2   2.5 Honeycrisp      Poor      0
#> 4         3      1   2     2     2   2.0       Fuji      Poor      1
#> 5        42      1   3     1     3   1.5 Honeycrisp Excellent      1
#> 6        43      1   3     2     3   2.0 Honeycrisp Excellent      0
```

### Choice Data Format

The simulated choice data includes all design columns plus a `choice`
column:

``` r

head(choices_utility)
#> CBC Choice Data
#> ===============
#> Encoding: standard
#> Observations: 3 choice tasks
#> Alternatives per task: 2
#> Total choices made: 3
#> 
#> Simulation method: utility_based
#> Priors: Used for utility-based simulation
#> Simulated at: 2026-06-24 10:41:33
#> 
#> Choice rates by alternative:
#>   Alt 1: 66.7% (2 choices)
#>   Alt 2: 33.3% (1 choices)
#> 
#> First few rows:
#>   profileID respID qID altID obsID price       type freshness choice
#> 1        31      1   1     1     1   1.0       Fuji Excellent      1
#> 2        15      1   1     2     1   3.0 Honeycrisp      Poor      0
#> 3        14      1   2     1     2   2.5 Honeycrisp      Poor      0
#> 4         3      1   2     2     2   2.0       Fuji      Poor      1
#> 5        42      1   3     1     3   1.5 Honeycrisp Excellent      1
#> 6        43      1   3     2     3   2.0 Honeycrisp Excellent      0
```

## Advanced Simulation Options

### Designs with No-Choice

For designs with no-choice options, specify no-choice priors:

``` r

# Create design with no-choice option
design_nochoice <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  no_choice = TRUE,
  method = "random"
)

# Create priors including no-choice utility
priors_nochoice <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c(0.5, 1.0),
  freshness = c(0.6, 1.2),
  no_choice = -0.5 # Negative = no-choice less attractive
)

# Simulate choices
choices_nochoice <- cbc_choices(
  design_nochoice,
  priors = priors_nochoice
)

# Examine no-choice rates
nochoice_rate <- mean(choices_nochoice$choice[choices_nochoice$no_choice == 1])
cat("No-choice selection rate:", round(nochoice_rate * 100, 1), "%\n")
#> No-choice selection rate: 13.3 %
```

### Random Parameters (Mixed Logit)

Simulate heterogeneous preferences using random parameters:

``` r

# Create priors with random parameters
priors_random <- cbc_priors(
  profiles = profiles,
  price = rand_spec(dist = "n", mean = -0.1, sd = 0.05),
  type = rand_spec(dist = "n", mean = c(0.1, 0.2), sd = c(0.05, 0.1)),
  freshness = c(0.1, 0.2), # Keep some parameters fixed
  n_draws = 100
)

# Simulate choices with preference heterogeneity
choices_mixed <- cbc_choices(design, priors = priors_random)
```

### Interaction Effects

Include interaction effects in choice simulation:

``` r

# Create priors with interactions
priors_interactions <- cbc_priors(
  profiles = profiles,
  price = -0.1,
  type = c("Fuji" = 0.5, "Gala" = 1),
  freshness = c("Average" = 0.6, "Excellent" = 1.2),
  interactions = list(
    # Price sensitivity varies by apple type
    int_spec(
      between = c("price", "type"),
      with_level = "Fuji",
      value = 0.5
    ),
    int_spec(
      between = c("price", "type"),
      with_level = "Gala",
      value = 0.2
    )
  )
)

# Simulate choices with interaction effects
choices_interactions <- cbc_choices(
  design,
  priors = priors_interactions
)
```

## Validating Choice Patterns

### Overall Choice Frequencies

Based on the priors used, we expect:

- **Lower prices preferred** (negative price coefficient)
- **Honeycrisp \> Gala \> Fuji** (type coefficients: 0.2 \> 0.1 \> 0)
- **Excellent \> Average \> Poor** (freshness coefficients: 0.2 \> 0.1
  \> 0)

Examine aggregate choice patterns to validate simulation:

``` r

# Convert to standard encoding to get categorical variables
choices_standard <- cbc_encode(choices_utility, coding = "standard")

# Aggregate attribute choices across all respondents
choices <- choices_standard

# Price choices
price_choices <- aggregate(choice ~ price, data = choices, sum)
price_choices$prop <- price_choices$choice / sum(price_choices$choice)
print(price_choices)
#>   price choice      prop
#> 1   1.0    123 0.2050000
#> 2   1.5    119 0.1983333
#> 3   2.0    121 0.2016667
#> 4   2.5    107 0.1783333
#> 5   3.0    130 0.2166667

# Type choices
type_choices <- aggregate(choice ~ type, data = choices, sum)
type_choices$prop <- type_choices$choice / sum(type_choices$choice)
print(type_choices)
#>         type choice  prop
#> 1       Fuji    171 0.285
#> 2       Gala    213 0.355
#> 3 Honeycrisp    216 0.360

# Freshness choices
freshness_choices <- aggregate(choice ~ freshness, data = choices, sum)
freshness_choices$prop <- freshness_choices$choice /
  sum(freshness_choices$choice)
print(freshness_choices)
#>   freshness choice      prop
#> 1      Poor    145 0.2416667
#> 2   Average    210 0.3500000
#> 3 Excellent    245 0.4083333
```

### Respondent Heterogeneity

For random parameter models, examine variation across respondents:

``` r

# Create dataset with only chosen alternatives
# Convert to dummy coding to more easily select individual levels
chosen_alts <- choices_mixed[choices_mixed$choice == 1, ] |> 
  cbc_encode('dummy')

# Mean attribute levels chosen by each respondent
resp_means <- aggregate(
  cbind(
    price,
    typeGala,
    typeHoneycrisp,
    freshnessAverage,
    freshnessExcellent
  ) ~
    respID,
  data = chosen_alts,
  mean
)

# Look at variation across respondents
cat("Price variation across respondents:\n")
#> Price variation across respondents:
cat("Mean:", round(mean(resp_means$price), 2), "\n")
#> Mean: 2.01
cat("SD:", round(sd(resp_means$price), 2), "\n")
#> SD: 0.28

cat("\nHoneycrisp choice rate variation:\n")
#> 
#> Honeycrisp choice rate variation:
cat("Mean:", round(mean(resp_means$typeHoneycrisp), 2), "\n")
#> Mean: 0.32
cat("SD:", round(sd(resp_means$typeHoneycrisp), 2), "\n")
#> SD: 0.2
```

## Design Consistency

### Using Consistent Priors

For D-optimal designs created with priors, use the same priors for
choice simulation:

``` r

# Create D-optimal design with priors
design_optimal <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "stochastic"
)

# Use SAME priors for choice simulation
choices_consistent <- cbc_choices(
  design_optimal,
  priors = priors
)
```

### Prior Consistency Warnings

cbcTools warns when different priors are used:

``` r

# Create different priors
different_priors <- cbc_priors(
  profiles = profiles,
  price = -0.2, # Different from design optimization
  type = c(0.2, 0.4),
  freshness = c(0.2, 0.4)
)

# This will generate a warning about inconsistent priors
choices_inconsistent <- cbc_choices(
  design_optimal,
  priors = different_priors
)
```

## Best Practices

### Prior Specification

- **Use realistic priors**: Base on literature, pilot studies, or expert
  judgment
- **Match design priors**: Use same priors for design optimization and
  choice simulation  
- **Test multiple scenarios**: Simulate under optimistic and
  conservative assumptions
- **Include heterogeneity**: Use random parameters when appropriate

### Validation Steps

1.  **Check choice counts**: Verify one choice per question
2.  **Examine patterns**: Ensure choices align with prior expectations
3.  **Test extremes**: Simulate with very strong/weak preferences
4.  **Compare methods**: Test different simulation approaches

## Next Steps

After simulating choices:

1.  **Conduct power analysis** using
    [`cbc_power()`](https://jhelvy.github.io/cbcTools/reference/cbc_power.md)
    to determine sample size requirements
2.  **Compare designs** by simulating choices for different design
    methods
3.  **Validate assumptions** by checking if simulated patterns match
    expectations
4.  **Refine priors** based on simulation results before data collection

For details on power analysis, see the [Power
Analysis](https://jhelvy.github.io/cbcTools/articles/power.Rmd)
vignette.

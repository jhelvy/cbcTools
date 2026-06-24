# Generate survey designs for choice experiments (Updated Implementation)

This function creates experimental designs for choice-based conjoint
experiments using multiple design approaches including optimization and
frequency-based methods.

## Usage

``` r
cbc_design(
  profiles,
  method = "random",
  priors = NULL,
  n_alts,
  n_q,
  n_resp = 100,
  n_blocks = 1,
  n_cores = NULL,
  no_choice = FALSE,
  label = NULL,
  balance_by = NULL,
  randomize_questions = TRUE,
  randomize_alts = TRUE,
  remove_dominant = FALSE,
  dominance_types = c("total", "partial"),
  dominance_threshold = 0.8,
  max_dominance_attempts = 50,
  max_iter = 50,
  n_start = 5,
  include_probs = FALSE,
  use_idefix = TRUE
)
```

## Arguments

- profiles:

  A data frame of class `cbc_profiles` created using
  [`cbc_profiles()`](https://jhelvy.github.io/cbcTools/reference/cbc_profiles.md)

- method:

  Choose the design method: "random", "shortcut", "minoverlap",
  "balanced", "stochastic", "modfed", or "cea". Defaults to "random"

- priors:

  A `cbc_priors` object created by
  [`cbc_priors()`](https://jhelvy.github.io/cbcTools/reference/cbc_priors.md),
  or NULL for random/shortcut designs

- n_alts:

  Number of alternatives per choice question

- n_q:

  Number of questions per respondent (or per block)

- n_resp:

  Number of respondents (for random/shortcut designs) or 1 (for
  optimized designs that get repeated)

- n_blocks:

  Number of blocks in the design. Defaults to 1

- n_cores:

  Number of cores to use for parallel processing in the design search.
  Defaults to NULL, in which case it is set to the number of available
  cores minus 1.

- no_choice:

  Include a "no choice" option? Defaults to FALSE

- label:

  The name of the variable to use in a "labeled" design. Defaults to
  NULL

- balance_by:

  Character vector of attribute names to balance sampling across.
  Ensures balanced representation across levels of specified attributes.
  Only compatible with "random", "shortcut", "minoverlap", and
  "balanced" methods. Cannot be used with labeled designs or D-optimal
  methods ("stochastic", "modfed", "cea"). Defaults to NULL

- randomize_questions:

  Randomize question order for each respondent? Defaults to TRUE
  (optimized methods only)

- randomize_alts:

  Randomize alternative order within questions? Defaults to TRUE
  (optimized methods only)

- remove_dominant:

  Remove choice sets with dominant alternatives? Defaults to FALSE

- dominance_types:

  Types of dominance to check: "total" and/or "partial"

- dominance_threshold:

  Threshold for total dominance detection. Defaults to 0.8

- max_dominance_attempts:

  Maximum attempts to replace dominant choice sets. Defaults to 50.

- max_iter:

  Maximum iterations for optimized designs. Defaults to 50

- n_start:

  Number of random starts for optimized designs. Defaults to 5

- include_probs:

  Include predicted probabilities in resulting design? Requires
  `priors`. Defaults to `FALSE`

- use_idefix:

  If `TRUE` (the default), the idefix package will be used to find
  optimal designs, which is faster. Only valid with `"cea"` and
  `"modfed"` methods.

## Value

A `cbc_design` object containing the experimental design

## Details

### Design Methods

The `method` argument determines the design approach used:

- `"random"`: Creates designs by randomly sampling profiles for each
  respondent independently

- `"shortcut"`: Frequency-based greedy algorithm that balances attribute
  level usage

- `"minoverlap"`: Greedy algorithm that minimizes attribute overlap
  within choice sets

- `"balanced"`: Greedy algorithm that maximizes overall attribute
  balance across the design

- `"stochastic"`: Stochastic profile swapping with D-error optimization
  (first improvement found)

- `"modfed"`: Modified Fedorov algorithm with exhaustive profile
  swapping for D-error optimization

- `"cea"`: Coordinate Exchange Algorithm with attribute-by-attribute
  D-error optimization

### Method Compatibility

The table below summarizes method compatibility with design features:

|  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|
| Method | No choice? | Labeled designs? | Restricted profiles? | balance_by? | Blocking? | Interactions? | Dominance removal? |
| "random" | Yes | Yes | Yes | Yes | No | Yes | Yes |
| "shortcut" | Yes | Yes | Yes | Yes | No | No | Yes |
| "minoverlap" | Yes | Yes | Yes | Yes | No | No | Yes |
| "balanced" | Yes | Yes | Yes | Yes | No | No | Yes |
| "stochastic" | Yes | Yes | Yes | No | Yes | Yes | Yes |
| "modfed" | Yes | Yes | Yes | No | Yes | Yes | Yes |
| "cea" | Yes | Yes | No | No | Yes | Yes | Yes |

### Design Quality Assurance

All methods ensure the following criteria are met:

1.  No duplicate profiles within any choice set

2.  No duplicate choice sets within any respondent

3.  If `remove_dominant = TRUE`, choice sets with dominant alternatives
    are eliminated (optimization methods only)

### Balanced Sampling with balance_by

The `balance_by` argument enables balanced sampling across specified
attributes, solving the problem of attribute-specific features that
create imbalanced designs. For example, consider an experiment on
alternative vehicle powertrains with a "powertrain" attribute for gas
and electric vehicles. If you had an "electric_vehicle_range" attribute,
it should be 0 for non-electric powertrains, but using restrictions can
lead to over-representation of electric vehicles. Using
`balance_by = "powertrain"` ensures that each choice question samples
proportionally from gas and electric powertrains, maintaining balance
even when electric vehicles have additional attributes.

Multiple attributes can be balanced simultaneously using
`balance_by = c("attr1", "attr2")`, which creates groups based on unique
combinations of the specified attributes.

### Method Details

#### Random Method

Creates designs where each respondent sees completely independent,
randomly generated choice sets.

#### Greedy Methods (shortcut, minoverlap, balanced)

These methods use frequency-based algorithms that make locally optimal
choices:

- **Shortcut**: Balances attribute level usage within questions and
  across the overall design

- **Minoverlap**: Minimizes attribute overlap within choice sets while
  allowing some overlap for balance

- **Balanced**: Maximizes overall attribute balance, prioritizing level
  distribution over overlap reduction

These methods provide good level balance without requiring priors or
D-error calculations and offer fast execution suitable for large
designs.

#### D-Error Optimization Methods (stochastic, modfed, cea)

These methods minimize D-error to create statistically efficient
designs:

- **Stochastic**: Random profile sampling with first improvement
  acceptance

- **Modfed**: Exhaustive profile testing for best improvement (slower
  but thorough)

- **CEA**: Coordinate exchange testing attribute levels individually
  (requires full factorial profiles)

### idefix Integration

When `use_idefix = TRUE` (the default), the function leverages the
highly optimized algorithms from the idefix package for 'cea' and
'modfed' design generation methods. This can provide significant speed
improvements, especially for larger problems.

Key benefits of idefix integration:

- Faster optimization algorithms with C++ implementation

- Better handling of large candidate sets

- Optimized parallel processing

- Advanced blocking capabilities for multi-block designs

## Examples

``` r
library(cbcTools)

# Create profiles for an apple choice experiment
profiles <- cbc_profiles(
    price = c(1, 1.5, 2, 2.5, 3),
    type = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c("Poor", "Average", "Excellent")
)

# Basic random design
design_random <- cbc_design(
    profiles = profiles,
    n_alts = 3,
    n_q = 6,
    n_resp = 100
)

head(design_random)
#> Design method: random
#> Encoding: standard
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price       type freshness
#> 1         7      1   1     1     1   1.5       Gala      Poor
#> 2        22      1   1     2     1   1.5       Gala   Average
#> 3        29      1   1     3     1   2.5 Honeycrisp   Average
#> 4        22      1   2     1     2   1.5       Gala   Average
#> 5        29      1   2     2     2   2.5 Honeycrisp   Average
#> 6        21      1   2     3     2   1.0       Gala   Average

# Inspect design
cbc_inspect(design_random)
#> DESIGN SUMMARY
#> =========================
#> 
#> STRUCTURE
#> ================
#> Method: random
#> Created: 2026-06-24 10:41:24
#> Respondents: 100
#> Questions per respondent: 6
#> Alternatives per question: 3
#> Total choice sets: 600
#> Profile usage: 45/45 (100.0%)
#> 
#> SUMMARY METRICS
#> =================
#> D-error calculation not available for this design
#> Overall balance score: 0.975 (higher is better)
#> Overall overlap score: 0.074 (lower is better)
#> 
#> VARIABLE ENCODING
#> =================
#> Format: Standard (categorical) (type, freshness)
#> 💡 Use cbc_encode() to convert to dummy or effects coding
#> 
#> ATTRIBUTE BALANCE
#> =================
#> Overall balance score: 0.975 (higher is better)
#> 
#> Individual attribute level counts:
#> 
#> price:
#> 
#>   1 1.5   2 2.5   3 
#> 357 355 371 365 352 
#>   Balance score: 0.979 (higher is better)
#> 
#> type:
#> 
#>       Fuji       Gala Honeycrisp 
#>        583        616        601 
#>   Balance score: 0.973 (higher is better)
#> 
#> freshness:
#> 
#>      Poor   Average Excellent 
#>       614       604       582 
#>   Balance score: 0.973 (higher is better)
#> 
#> ATTRIBUTE OVERLAP
#> =================
#> Overall overlap score: 0.074 (lower is better)
#> 
#> Counts of attribute overlap:
#> (# of questions with N unique levels)
#> 
#> price: Continuous variable
#>   Questions by # unique levels:
#>   1 (complete overlap):   3.5%  (21 / 600 questions)
#>   2 (partial overlap):   50.8%  (305 / 600 questions)
#>   3 (partial overlap):   45.7%  (274 / 600 questions)
#>   4 (partial overlap):    0.0%  (0 / 600 questions)
#>   5 (no overlap):         0.0%  (0 / 600 questions)
#>   Average unique levels per question: 2.42
#> 
#> type: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):  10.3%  (62 / 600 questions)
#>   2 (partial overlap):   67.2%  (403 / 600 questions)
#>   3 (no overlap):        22.5%  (135 / 600 questions)
#>   Average unique levels per question: 2.12
#> 
#> freshness: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):   8.5%  (51 / 600 questions)
#>   2 (partial overlap):   67.2%  (403 / 600 questions)
#>   3 (no overlap):        24.3%  (146 / 600 questions)
#>   Average unique levels per question: 2.16
#> 
#> 

# Greedy design with balanced frequency
design_balanced <- cbc_design(
    profiles = profiles,
    method = "balanced",
    n_alts = 3,
    n_q = 6,
    n_resp = 100
)
#> Generating balanced design for 100 respondents using 3 cores...

# Design with priors using D-optimal method
priors <- cbc_priors(
    profiles = profiles,
    price = -0.25,
    type = c("Gala" = 0.5, "Honeycrisp" = 1.0),
    freshness = c("Average" = 0.6, "Excellent" = 1.2)
)

design_optimal <- cbc_design(
    profiles = profiles,
    method = "stochastic",
    priors = priors,
    n_alts = 3,
    n_q = 6,
    n_resp = 100,
    n_start = 3
)
#> Stochastic design will be optimized into 1 design block, then allocated across 100 respondents
#> Running 3 design searches using 3 cores...
#> 
#> D-error results from all starts:
#> Start 3: 0.895676   (Best)
#> Start 2: 0.903040 
#> Start 1: 0.927361 

# Compare designs
cbc_compare(
    "Random" = design_random,
    "Balanced" = design_balanced,
    "D-optimal" = design_optimal
)
#> CBC Design Comparison
#> =====================
#> Designs compared: 3
#> Metrics: structure, efficiency, balance, overlap
#> Sorted by: d_error (ascending)
#> 
#> Structure
#> =====================
#>     Design     Method respondents questions
#>  D-optimal stochastic         100         6
#>     Random     random         100         6
#>   Balanced   balanced         100         6
#>  Alternatives Blocks Profile Usage
#>             3      1 (17/45) 37.8%
#>             3      1  (45/45) 100%
#>             3      1  (45/45) 100%
#>  No Choice Labeled?
#>         No       No
#>         No       No
#>         No       No
#> 
#> Design Metrics
#> =====================
#>     Design     Method D-Error (Null) D-Error (Prior) Balance Overlap
#>  D-optimal stochastic       0.822674        0.895676   0.837   0.000
#>     Random     random             NA              NA   0.975   0.074
#>   Balanced   balanced             NA              NA   0.995   0.000
#> 
#> Interpretation:
#> - D-Error: Lower is better (design efficiency)
#> - Balance: Higher is better (level distribution)
#> - Overlap: Lower is better (attribute variation)
#> - Profile Usage: Higher means more profiles used
#> 
#> Best performers:
#> - D-Error: D-optimal (0.895676)
#> - Balance: Balanced (0.995)
#> - Overlap: D-optimal (0.000)
#> - Profile Usage: Random (100.0%)
#> 
#> Use summary() for detailed information on any one design.
```

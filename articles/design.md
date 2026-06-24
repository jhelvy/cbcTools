# Generating Designs

Once you have a set of profiles and (optionally) priors, you can
generate a choice-based conjoint (CBC) survey design using the
[`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md)
function. This article covers all the design methods available, their
features, and how to customize designs for specific research needs.

Before starting, let’s define some basic profiles and priors to work
with:

``` r

library(cbcTools)

profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

priors <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c('Gala' = 0.5, 'Honeycrisp' = 1.0),
  freshness = c('Average' = 0.6, 'Excellent' = 1.2)
)
```

## Design Basics

The
[`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md)
function generates a data frame with an encoded experiment design
formatted as one row per alternative. Choice questions are defined by
sets of rows with the same `obsID`. Let’s start with a simple example (a
random design):

``` r

design <- cbc_design(
  profiles = profiles,
  n_alts = 2, # Alternatives per question
  n_q = 6, # Questions per respondent
  n_resp = 100 # Number of respondents
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

### Understanding the Design Structure

The design data frame contains several types of columns that help
organize the experiment:

#### ID Columns

These columns identify the structure of your experiment:

- **`profileID`**: Unique identifier for each profile (combination of
  attribute levels), that corresponds to the IDs in `profiles`
- **`respID`**: Respondent ID (1 to `n_resp`)
- **`qID`**: Question number within each respondent (1 to `n_q`)
- **`altID`**: Alternative number within each question (1 to `n_alts`)
- **`obsID`**: Unique identifier for each choice question across all
  respondents

#### Attribute Columns

The remaining columns represent your experimental attributes. By
default, categorical attributes use **standard** coding, meaning the
level names are directly used.

#### Converting Encoding

You can change the design encoding to **dummy** coding or **effects**
coding using the
[`cbc_encode()`](https://jhelvy.github.io/cbcTools/reference/cbc_encode.md)
function (see the [Variable
Encoding](https://jhelvy.github.io/cbcTools/articles/encoding.md)
article for more details.):

``` r

design_dummy <- cbc_encode(design, coding = 'dummy')

design_dummy
#> Design method: random
#> Encoding: dummy
#> Structure: 100 respondents × 6 questions × 2 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price typeGala typeHoneycrisp
#> 1        31      1   1     1     1   1.0        0              0
#> 2        15      1   1     2     1   3.0        0              1
#> 3        14      1   2     1     2   2.5        0              1
#> 4         3      1   2     2     2   2.0        0              0
#> 5        42      1   3     1     3   1.5        0              1
#> 6        43      1   3     2     3   2.0        0              1
#>   freshnessAverage freshnessExcellent
#> 1                0                  1
#> 2                0                  0
#> 3                0                  0
#> 4                0                  0
#> 5                0                  1
#> 6                0                  1
#> ... and 1194 more rows
```

In dummy coding, **continuous attributes** (like `price`) appear as-is,
but **categorical attributes** (like `type` and `freshness`) are split
into multiple binary columns.

For example, for `type`, we have the following columns:

- `typeGala` = 1 if type is “Gala”, 0 otherwise
- `typeHoneycrisp` = 1 if type is “Honeycrisp”, 0 otherwise

Here the reference level (“Fuji”) is represented when both dummy
variables equal 0.

## Design Methods

The
[`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md)
function supports several design generation methods, each with different
strengths and use cases:

### Method Comparison Table

| Method | Speed | Efficiency | No Choice | Labeled | Restrictions | Balanced Sampling | Blocking | Interactions |
|:---|:---|:---|:--:|:--:|:--:|:--:|:--:|:--:|
| `"random"` | Fast | Low | ✓ | ✓ | ✓ | ✓ | ✗ | ✓ |
| `"shortcut"` | Fast | Medium | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ |
| `"minoverlap"` | Fast | Medium | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ |
| `"balanced"` | Fast | Medium | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ |
| `"stochastic"` | Slow | High | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ |
| `"modfed"` | Medium | High | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ |
| `"cea"` | Medium | High | ✓ | ✓ | ✗ | ✗ | ✓ | ✓ |

All design methods ensure:

1.  **No duplicate profiles** within any choice set
2.  **No duplicate choice sets** within any respondent
3.  **Dominance removal** (if enabled) eliminates choice sets with
    dominant alternatives (requires priors)

### `"random"` Method

The `"random"` method is the default and creates designs by randomly
sampling profiles for each respondent independently. This ensures
maximum diversity but may be less statistically efficient.

``` r

design_random <- cbc_design(
  profiles = profiles,
  method = "random",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

# Quick inspection
cbc_inspect(design_random, sections = "structure")
#> DESIGN SUMMARY
#> =========================
#> 
#> STRUCTURE
#> ================
#> Method: random
#> Created: 2026-06-24 10:41:38
#> Respondents: 100
#> Questions per respondent: 6
#> Alternatives per question: 2
#> Total choice sets: 600
#> Profile usage: 45/45 (100.0%)
```

**When to use:**

- Large sample sizes where efficiency matters less
- Want maximum diversity across respondents
- No strong prior assumptions about parameters
- Uncertain whether interactions might be important
- Quick prototyping or testing

### Frequency-Based Methods

The `"shortcut"`, `"minoverlap"`, and `"balanced"` methods use greedy
algorithms to balance attribute level frequencies and minimize overlap.
While they prioritize different metrics, they often can result in
similar solutions. Each method has a different objective:

- The `"shortcut"` method balances attribute level frequencies while
  avoiding duplicate profiles within questions.
- The `"minoverlap"` method prioritizes minimizing attribute overlap
  within choice questions.
- The `"balanced"` method optimizes both frequency balance and pairwise
  attribute interactions.

``` r

design_shortcut <- cbc_design(
  profiles = profiles,
  method = "shortcut",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

design_minoverlap <- cbc_design(
  profiles = profiles,
  method = "minoverlap",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

design_balanced <- cbc_design(
  profiles = profiles,
  method = "balanced",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)
```

### D-Optimal Methods

These methods minimize D-error to create statistically efficient
designs. They require more computation but produce higher-quality
designs, especially with good priors.

Unlike the previous methods, these methods identify a single d-optimal
design and then repeat that design across each respondent. In contrast,
the other methods create a unique design for each respondent.

Each method has a different approach:

- The `"stochastic"` method uses random profile swapping to minimize the
  d-error, accepting the first improvement found. This is a faster
  algorithm as a compromise between speed and exhaustiveness.
- The `"modfed"` (Modified Fedorov) method exhaustively tests all
  possible profile swaps for each position. It is slower than other
  methods though more thorough.
- The `"cea"` (Coordinate Exchange Algorithm) method optimizes
  attribute-by-attribute, testing all possible levels for each
  attribute. It is faster than `"modfed"`, though requires all possible
  profiles and cannot accept restricted profile sets.

For the `"modfed"` and `"cea"` methods, designs are by default created
using the much faster algorithms in the
[{idefix}](https://github.com/traets/idefix) package. You can use the
(slower) {cbcTools} versions of these methods by setting
`use_idefix = FALSE`.

For the examples below, we have `n_start = 1`, meaning it will only run
one design search (which is faster), but you may want to run a longer
search by increasing `n_start`. The best design across all starts is
chosen.

``` r

design_stochastic <- cbc_design(
  profiles = profiles,
  method = "stochastic",
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  n_start = 1 # Number of random starting points
)

design_modfed <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "modfed",
  n_start = 1
)

design_cea <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "cea",
  n_start = 1
)
```

Notice also that in the examples above we provided the `priors` to each
design. This will optimize the design around these assumed priors by
minimizing the $`D_p`$-error. If you are uncertain what the true
parameters are, you can omit the `priors` argument and the algorithms
will minimize the $`D_0`$-error. See the [Computing
D-error](https://jhelvy.github.io/cbcTools/articles/d_error.md) page for
more details on how these errors are computed.

## Comparing Designs

You can compare the results of different designs using the
[`cbc_compare()`](https://jhelvy.github.io/cbcTools/reference/cbc_compare.md)
function. This provides a comprehensive overview of differences in
structure as well as common metrics such as D-error, overlap, and
balance.

``` r

cbc_compare(
  "Random" = design_random,
  "Shortcut" = design_shortcut,
  "Min Overlap" = design_minoverlap,
  "Balanced" = design_balanced,
  "Stochastic" = design_stochastic,
  "Modfed" = design_modfed,
  "CEA" = design_cea
)
#> CBC Design Comparison
#> =====================
#> Designs compared: 7
#> Metrics: structure, efficiency, balance, overlap
#> Sorted by: d_error (ascending)
#> 
#> Structure
#> =====================
#>       Design     Method respondents questions
#>       Modfed     modfed         100         6
#>          CEA        cea         100         6
#>   Stochastic stochastic         100         6
#>       Random     random         100         6
#>     Shortcut   shortcut         100         6
#>  Min Overlap minoverlap         100         6
#>     Balanced   balanced         100         6
#>  Alternatives Blocks Profile Usage
#>             2      1 (10/45) 22.2%
#>             2      1 (11/45) 24.4%
#>             2      1 (12/45) 26.7%
#>             2      1  (45/45) 100%
#>             2      1  (45/45) 100%
#>             2      1  (45/45) 100%
#>             2      1  (45/45) 100%
#>  No Choice Labeled?
#>         No       No
#>         No       No
#>         No       No
#>         No       No
#>         No       No
#>         No       No
#>         No       No
#> 
#> Design Metrics
#> =====================
#>       Design     Method D-Error (Null) D-Error (Prior) Balance Overlap
#>       Modfed     modfed       0.850283        1.023292   1.000   0.000
#>          CEA        cea       0.858174        1.043690   0.867   0.000
#>   Stochastic stochastic       1.203841        1.485359   0.759   0.056
#>       Random     random             NA              NA   0.965   0.277
#>     Shortcut   shortcut             NA              NA   0.993   0.000
#>  Min Overlap minoverlap             NA              NA   0.959   0.000
#>     Balanced   balanced             NA              NA   0.986   0.000
#> 
#> Interpretation:
#> - D-Error: Lower is better (design efficiency)
#> - Balance: Higher is better (level distribution)
#> - Overlap: Lower is better (attribute variation)
#> - Profile Usage: Higher means more profiles used
#> 
#> Best performers:
#> - D-Error: Modfed (1.023292)
#> - Balance: Modfed (1.000)
#> - Overlap: Modfed (0.000)
#> - Profile Usage: Random (100.0%)
#> 
#> Use summary() for detailed information on any one design.
```

## Design Features

### No-Choice Option

Add a “no-choice” alternative to allow respondents to opt out by
including the argument `no_choice = TRUE`. If you are using priors in
your design (optional), then you must also provide a `no_choice` value
in your priors:

``` r

# For D-optimal methods, must include no_choice in priors
priors_nochoice <- cbc_priors(
  profiles = profiles,
  price = -0.1,
  type = c(0.1, 0.2),
  freshness = c(0.1, 0.2),
  no_choice = -0.5 # Negative value makes no-choice less attractive
)

design_nochoice <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  no_choice = TRUE,
  priors = priors_nochoice,
  method = "stochastic"
)

head(design_nochoice)
#> Design method: stochastic
#> Encoding: standard
#> Structure: 100 respondents × 6 questions × 2 alternatives
#> Profile usage: 11/45 (24.4%)
#> D-error: 0.834125
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 
#> First few rows of design:
#>   profileID blockID respID qID altID obsID price       type freshness no_choice
#> 1        43       1      1   1     1     1     2 Honeycrisp Excellent         0
#> 2        25       1      1   1     2     1     3       Gala   Average         0
#> 3         0       1      1   1     3     1    NA       <NA>      <NA>         1
#> 4        40       1      1   2     1     2     3       Gala Excellent         0
#> 5        41       1      1   2     2     2     1 Honeycrisp Excellent         0
#> 6         0       1      1   2     3     2    NA       <NA>      <NA>         1
```

Notice that the “no_choice” row contains `NA` values for all other
attributes. This is the default behavior when using standard encoding.
When used for modeling, the design must be converted to either “dummy”
or “effects” coding for proper handling of the “no_choice” attribute:

``` r

design_nochoice |> 
  cbc_encode(coding = 'dummy')
#> Design method: stochastic
#> Encoding: dummy
#> Structure: 100 respondents × 6 questions × 2 alternatives
#> Profile usage: 11/45 (24.4%)
#> D-error: 0.834125
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 💡 Use cbc_encode(design, 'standard') to view categorical format
#> 
#> First few rows of design:
#>   profileID blockID respID qID altID obsID price no_choice typeGala
#> 1        43       1      1   1     1     1     2         0        0
#> 2        25       1      1   1     2     1     3         0        1
#> 3         0       1      1   1     3     1     0         1        0
#> 4        40       1      1   2     1     2     3         0        1
#> 5        41       1      1   2     2     2     1         0        0
#> 6         0       1      1   2     3     2     0         1        0
#>   typeHoneycrisp freshnessAverage freshnessExcellent
#> 1              1                0                  1
#> 2              0                1                  0
#> 3              0                0                  0
#> 4              0                0                  1
#> 5              1                0                  1
#> 6              0                0                  0
#> ... and 1794 more rows
```

With “dummy” coding, the “no_choice” row contains `0` for all other
attributes.

### Labeled Designs

Create “labeled” or “alternative-specific” designs where one attribute
serves as a label using the `label` argument. With a labeled design, the
attribute chosen to use as the label will always have the same ordering
in each choice set:

``` r

design_labeled <- cbc_design(
  profiles = profiles,
  n_alts = 3, # Will be overridden to match number of type levels
  n_q = 6,
  n_resp = 100,
  label = "type", # Use 'type' attribute as labels
  method = "random"
)

head(design_labeled)
#> Design method: random
#> Encoding: standard
#> Structure: 100 respondents × 6 questions × 3 alternatives
#> Profile usage: 45/45 (100.0%)
#> 
#> 💡 Use cbc_inspect() for a more detailed summary
#> 
#> First few rows of design:
#>   profileID respID qID altID obsID price       type freshness
#> 1        20      1   1     1     1   3.0       Fuji   Average
#> 2        21      1   1     2     1   1.0       Gala   Average
#> 3        14      1   1     3     1   2.5 Honeycrisp      Poor
#> 4         5      1   2     1     2   3.0       Fuji      Poor
#> 5        38      1   2     2     2   2.0       Gala Excellent
#> 6        44      1   2     3     2   2.5 Honeycrisp Excellent
```

### Balanced Sampling

When working with attribute-specific features (attributes that only
apply to certain levels of another attribute), you may encounter balance
problems. For example, electric vehicles have an all-electric driving
range attribute, but gas and hybrid vehicles do not. Using restrictions
to handle this can lead to over-representation of certain attribute
levels.

The `balance_by` argument solves this by ensuring balanced sampling
across specified attributes. Here’s an example with vehicle powertrains
and electric range:

``` r

# Create profiles with attribute-specific feature
profiles_vehicles <- cbc_profiles(
  price = c(15, 20, 25), # Price in $1,000s
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  powertrain = c('gas', 'hybrid', 'electric'),
  range_electric = c(0, 100, 150, 200, 250) # EV driving range (miles)
) |> 
  # Restrict non-electric powertrains from having range_electric other than 0
  cbc_restrict(
    (powertrain == 'electric') & (range_electric == 0),
    (powertrain != 'electric') & (range_electric != 0)
  )

# Check the balance problem - electric vehicles are over-represented
table(profiles_vehicles$powertrain)
#> 
#>      gas   hybrid electric 
#>        9        9       36
```

Without `balance_by`, the restricted profiles lead to unbalanced
designs:

``` r

# Design without balance_by - electric powertrains over-represented
design_unbalanced <- cbc_design(
  profiles = profiles_vehicles,
  n_alts = 3,
  n_q = 8,
  n_resp = 100,
  method = "random"
)

# Check powertrain distribution - electric is over-represented
table(design_unbalanced$powertrain)
#> 
#>      gas   hybrid electric 
#>      389      382     1629
```

Using `balance_by` ensures balanced sampling across powertrains:

``` r

# Design with balance_by - balanced powertrain representation
design_balanced <- cbc_design(
  profiles = profiles_vehicles,
  n_alts = 3,
  n_q = 8,
  n_resp = 100,
  method = "random",
  balance_by = "powertrain" # Balance across powertrain levels
)

# Check improved powertrain distribution - now powertrain is balanced
table(design_balanced$powertrain)
#> 
#>      gas   hybrid electric 
#>      777      798      825
```

You can also balance across multiple attributes simultaneously:

``` r

# Balance across combinations of powertrain and price
design_multi_balance <- cbc_design(
  profiles = profiles_vehicles,
  n_alts = 3,
  n_q = 8,
  n_resp = 50,
  method = "random",
  balance_by = c("powertrain", "price")
)

# Check improved powertrain distribution - now powertrain and price are balanced
table(design_multi_balance$powertrain)
#> 
#>      gas   hybrid electric 
#>      407      422      371
table(design_multi_balance$price)
#> 
#>  15  20  25 
#> 381 419 400
```

> **Note**: The `balance_by` argument cannot be used simultaneously with
> `label`. Choose one approach based on your design needs.

> **Note**: The `balance_by` feature is only available for `"random"`,
> `"shortcut"`, `"minoverlap"`, and `"balanced"` methods. D-optimal
> methods (`"stochastic"`, `"modfed"`, `"cea"`) prioritize statistical
> efficiency over balance and do not support this feature.

### Blocking

For D-optimal methods, create multiple design blocks to reduce
respondent burden using the `n_blocks` argument. In the example below,
two blocks are created with each block containing `n_q = 6` questions:

``` r

design_blocked <- cbc_design(
  profiles = profiles,
  method = "stochastic",
  priors = priors,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  n_blocks = 2 # Create 2 different design blocks
)

# Check block allocation
table(design_blocked$blockID)
#> 
#>   1   2 
#> 600 600
```

The way blocking works is that a single design is created with
`n_q*n_blocks` questions, then those questions are allocated into blocks
with `n_q` questions per block. For the `"modfed"` and `"cea"` methods,
the blocking is handled using the internal logic of the
[{idefix}](https://github.com/traets/idefix) package, which allocates
questions to the blocks in a balanced way. You can use the (slower)
{cbcTools} versions of these methods by setting `use_idefix = FALSE`.
For the `"stochastic"` method, questions are randomly allocated to
blocks.

### Dominance Removal

Remove choice sets where one alternative dominates others based on
parameter preferences. There are two forms of dominance removal:

1.  **Total dominance**: Occurs when one alternative has such a high
    predicted choice probability (based on the prior coefficients) that
    it would be chosen by virtually all respondents. This creates choice
    sets with little information value since the outcome is
    predetermined. The `dominance_threshold` parameter controls this -
    alternatives with choice probabilities above this threshold (e.g.,
    0.8 = 80%) are considered dominant.
2.  **Partial dominance**: Occurs when one alternative is superior to
    all others across every individual attribute component of the
    utility function (again, based on prior coefficients). For example,
    if Alternative A has higher partial utilities than Alternative B for
    every single attribute (price, type, freshness), then A partially
    dominates B regardless of the overall choice probability. This type
    of dominance is detected by comparing the attribute-level
    contributions to utility.

Both forms of dominance create unrealistic choice scenarios that provide
less information about respondent preferences, so removing them
generally improves design quality.

``` r

design_no_dominance <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "stochastic",
  remove_dominant = TRUE,
  dominance_types = c("total", "partial"),
  dominance_threshold = 0.8
)
```

### Interactions

Include interaction effects in D-optimal designs by specifying them in
your prior model. Interactions capture how the effect of one attribute
depends on the level of another attribute. The design optimization then
accounts for these interaction terms when minimizing D-error.

Interactions are specified via the priors defined by
[`cbc_priors()`](https://jhelvy.github.io/cbcTools/reference/cbc_priors.md).
For example:

``` r

# Create priors with interactions
priors_interactions <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c("Fuji" = 0.5, "Gala" = 1.0),
  freshness = c(0.6, 1.2),
  interactions = list(
    # Price is less negative (less price sensitive) for Fuji apples
    int_spec(
      between = c("price", "type"),
      with_level = "Fuji",
      value = 0.5
    ),
    # Price is slightly less negative for Gala apples
    int_spec(
      between = c("price", "type"),
      with_level = "Gala",
      value = 0.2
    )
    # Honeycrisp uses reference level (no additional interaction term)
  )
)

design_interactions <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors_interactions,
  method = "stochastic"
)
```

When you include interactions in the prior model, the design
optimization:

1.  **Accounts for interaction parameters** when computing choice
    probabilities
2.  **Optimizes profile combinations** that provide information about
    both main effects AND interactions
3.  **Creates choice sets** that help distinguish between different
    interaction effects

This leads to more efficient designs when interaction effects truly
exist in your population, but can reduce efficiency for estimating main
effects if interactions are misspecified or don’t actually exist.

See the [Specifying
Priors](https://jhelvy.github.io/cbcTools/articles/priors.md) article
for more details and options on defining priors with interactions.

## Comprehensive Design Inspection

Use
[`cbc_inspect()`](https://jhelvy.github.io/cbcTools/reference/cbc_inspect.md)
for detailed design analysis:

``` r

# Detailed inspection of the stochastic design
cbc_inspect(
  design_stochastic,
  sections = "all"
)
#> DESIGN SUMMARY
#> =========================
#> 
#> STRUCTURE
#> ================
#> Method: stochastic
#> Created: 2026-06-24 10:41:40
#> Respondents: 100
#> Questions per respondent: 6
#> Alternatives per question: 2
#> Total choice sets: 600
#> Profile usage: 12/45 (26.7%)
#> 
#> SUMMARY METRICS
#> =================
#> D-error (with priors): 1.485359
#> D-error (null model): 1.203841
#> (Lower values indicate more efficient designs)
#> 
#> Overall balance score: 0.759 (higher is better)
#> Overall overlap score: 0.056 (lower is better)
#> 
#> VARIABLE ENCODING
#> =================
#> Format: Standard (categorical) (type, freshness)
#> 💡 Use cbc_encode() to convert to dummy or effects coding
#> 
#> ATTRIBUTE BALANCE
#> =================
#> Overall balance score: 0.759 (higher is better)
#> 
#> Individual attribute level counts:
#> 
#> price:
#> 
#>   1 1.5   2 2.5   3 
#> 300 200 200 400 100 
#>   Balance score: 0.678 (higher is better)
#> 
#> type:
#> 
#>       Fuji       Gala Honeycrisp 
#>        500        400        300 
#>   Balance score: 0.800 (higher is better)
#> 
#> freshness:
#> 
#>      Poor   Average Excellent 
#>       300       500       400 
#>   Balance score: 0.800 (higher is better)
#> 
#> ATTRIBUTE OVERLAP
#> =================
#> Overall overlap score: 0.056 (lower is better)
#> 
#> Counts of attribute overlap:
#> (# of questions with N unique levels)
#> 
#> price: Continuous variable
#>   Questions by # unique levels:
#>   1 (complete overlap):  16.7%  (100 / 600 questions)
#>   2 (partial overlap):   83.3%  (500 / 600 questions)
#>   3 (partial overlap):    0.0%  (0 / 600 questions)
#>   4 (partial overlap):    0.0%  (0 / 600 questions)
#>   5 (no overlap):         0.0%  (0 / 600 questions)
#>   Average unique levels per question: 1.83
#> 
#> type: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):   0.0%  (0 / 600 questions)
#>   2 (partial overlap):  100.0%  (600 / 600 questions)
#>   3 (no overlap):         0.0%  (0 / 600 questions)
#>   Average unique levels per question: 2.00
#> 
#> freshness: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):   0.0%  (0 / 600 questions)
#>   2 (partial overlap):  100.0%  (600 / 600 questions)
#>   3 (no overlap):         0.0%  (0 / 600 questions)
#>   Average unique levels per question: 2.00
```

### Customizing Optimization

The
[`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md)
function offers many customization options:

``` r

# Advanced stochastic design with custom settings
design_advanced <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 8,
  n_resp = 300,
  n_blocks = 2,
  priors = priors,
  method = "stochastic",
  n_start = 10, # More starting points for better optimization
  max_iter = 100, # More iterations per start
  n_cores = 4, # Parallel processing
  remove_dominant = TRUE,
  dominance_threshold = 0.9,
  randomize_questions = TRUE,
  randomize_alts = TRUE
)
```

## Next Steps

After generating your design:

1.  **Inspect the design** using
    [`cbc_inspect()`](https://jhelvy.github.io/cbcTools/reference/cbc_inspect.md)
    to understand its properties
2.  **Simulate choices** using
    [`cbc_choices()`](https://jhelvy.github.io/cbcTools/reference/cbc_choices.md)
    to test the design
3.  **Conduct power analysis** using
    [`cbc_power()`](https://jhelvy.github.io/cbcTools/reference/cbc_power.md)
    to determine sample size requirements
4.  **Compare alternatives** using
    [`cbc_compare()`](https://jhelvy.github.io/cbcTools/reference/cbc_compare.md)
    to choose the best design

For more details on these next steps, see:

- The [Simulating
  Choices](https://jhelvy.github.io/cbcTools/articles/choices.md)
  vignette
- The [Power
  Analysis](https://jhelvy.github.io/cbcTools/articles/power.md)
  vignette

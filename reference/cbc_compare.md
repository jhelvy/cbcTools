# Compare multiple choice experiment designs

This function compares multiple CBC designs across key quality metrics
including D-error, balance, overlap, and structural characteristics.
Useful for evaluating different design methods or parameter settings.

## Usage

``` r
cbc_compare(..., metrics = "all", sort_by = "d_error", ascending = NULL)
```

## Arguments

- ...:

  Any number of `cbc_design` objects to compare, separated by commas.
  Can be named for clearer output (e.g.,
  `random = design1, stochastic = design2`).

- metrics:

  Character vector specifying which metrics to compare. Options:
  "structure", "efficiency", "balance", "overlap", or "all" (default).
  Can specify multiple: `c("efficiency", "balance")`

- sort_by:

  Character. Metric to sort designs by. Options: "d_error" (default),
  "balance", "overlap", "profiles_used", "generation_time", or "none"

- ascending:

  Logical. If TRUE, sort in ascending order (lower is better). If FALSE,
  sort in descending order (higher is better). Default depends on
  metric.

## Value

A `cbc_comparison` object containing comparison results, printed in a
formatted table.

## Examples

``` r
library(cbcTools)

# Create profiles
profiles <- cbc_profiles(
  price = c(1, 2, 3),
  type = c("A", "B", "C"),
  quality = c("Low", "High")
)

# Create different designs
design_random <- cbc_design(
  profiles = profiles,
  method = "random",
  n_alts = 2, n_q = 4
)

design_stochastic <- cbc_design(
  profiles = profiles,
  method = "stochastic",
  n_alts = 2, n_q = 4
)
#> Stochastic design will be optimized into 1 design block, then allocated across 100 respondents
#> Running 5 design searches using 3 cores...
#> 
#> D-error results from all starts:
#> Start 1: 1.788854   (Best)
#> Start 1: 1.788854   (Best)
#> Start 2: 2.000000 
#> Start 2: 2.000000 
#> Start 5: Inf 

# Compare designs
cbc_compare(design_random, design_stochastic)
#> CBC Design Comparison
#> =====================
#> Designs compared: 2
#> Metrics: structure, efficiency, balance, overlap
#> Sorted by: d_error (ascending)
#> 
#> Structure
#> =====================
#>    Design     Method respondents questions
#>  Design 2 stochastic         100         4
#>  Design 1     random         100         4
#>  Alternatives Blocks Profile Usage
#>             2      1  (8/18) 44.4%
#>             2      1  (18/18) 100%
#>  No Choice Labeled?
#>         No       No
#>         No       No
#> 
#> Design Metrics
#> =====================
#>    Design     Method D-Error (Null) D-Error (Prior) Balance Overlap
#>  Design 2 stochastic       1.788854              NA   0.732   0.333
#>  Design 1     random             NA              NA   0.959   0.343
#> 
#> Interpretation:
#> - D-Error: Lower is better (design efficiency)
#> - Balance: Higher is better (level distribution)
#> - Overlap: Lower is better (attribute variation)
#> - Profile Usage: Higher means more profiles used
#> 
#> Best performers:
#> - Balance: Design 1 (0.959)
#> - Overlap: Design 2 (0.333)
#> - Profile Usage: Design 1 (100.0%)
#> 
#> Use summary() for detailed information on any one design.

# Named comparison with specific metrics
cbc_compare(
  Random = design_random,
  Stochastic = design_stochastic,
  metrics = c("efficiency", "balance"),
  sort_by = "d_error"
)
#> CBC Design Comparison
#> =====================
#> Designs compared: 2
#> Metrics: efficiency, balance
#> Sorted by: d_error (ascending)
#> 
#> Design Metrics
#> =====================
#>      Design     Method D-Error (Null) D-Error (Prior) Balance
#>  Stochastic stochastic       1.788854              NA   0.732
#>      Random     random             NA              NA   0.959
#> 
#> Interpretation:
#> - D-Error: Lower is better (design efficiency)
#> - Balance: Higher is better (level distribution)
#> 
#> Best performers:
#> - Balance: Random (0.959)
#> 
#> Use summary() for detailed information on any one design.
```

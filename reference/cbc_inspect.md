# Comprehensive design quality inspection

This function provides detailed inspection of choice experiment designs
across multiple dimensions including design structure, efficiency
metrics, attribute balance, overlap patterns, and variable encoding.

## Usage

``` r
cbc_inspect(design, sections = "all", verbose = FALSE)
```

## Arguments

- design:

  A `cbc_design` or `cbc_choices` object created by
  [`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md)

- sections:

  Character vector specifying which sections to show. Options:
  "structure", "efficiency", "balance", "overlap", "encoding", or "all"
  (default). Can specify multiple: `c("balance", "overlap")`

- verbose:

  Logical. If TRUE, shows additional technical details. If FALSE
  (default), shows simplified output.

## Value

A `cbc_inspection` object containing the inspection results

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

# Inspect all sections (default) - prints automatically
cbc_inspect(design)
#> DESIGN SUMMARY
#> =========================
#> 
#> STRUCTURE
#> ================
#> Method: random
#> Created: 2026-06-24 10:41:26
#> Respondents: 100
#> Questions per respondent: 4
#> Alternatives per question: 2
#> Total choice sets: 400
#> Profile usage: 18/18 (100.0%)
#> 
#> SUMMARY METRICS
#> =================
#> D-error calculation not available for this design
#> Overall balance score: 0.969 (higher is better)
#> Overall overlap score: 0.365 (lower is better)
#> 
#> VARIABLE ENCODING
#> =================
#> Format: Standard (categorical) (type, quality)
#> 💡 Use cbc_encode() to convert to dummy or effects coding
#> 
#> ATTRIBUTE BALANCE
#> =================
#> Overall balance score: 0.969 (higher is better)
#> 
#> Individual attribute level counts:
#> 
#> price:
#> 
#>   1   2   3 
#> 276 256 268 
#>   Balance score: 0.964 (higher is better)
#> 
#> type:
#> 
#>   A   B   C 
#> 276 272 252 
#>   Balance score: 0.954 (higher is better)
#> 
#> quality:
#> 
#>  Low High 
#>  397  403 
#>   Balance score: 0.990 (higher is better)
#> 
#> ATTRIBUTE OVERLAP
#> =================
#> Overall overlap score: 0.365 (lower is better)
#> 
#> Counts of attribute overlap:
#> (# of questions with N unique levels)
#> 
#> price: Continuous variable
#>   Questions by # unique levels:
#>   1 (complete overlap):  34.5%  (138 / 400 questions)
#>   2 (partial overlap):   65.5%  (262 / 400 questions)
#>   3 (no overlap):         0.0%  (0 / 400 questions)
#>   Average unique levels per question: 1.66
#> 
#> type: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):  29.2%  (117 / 400 questions)
#>   2 (partial overlap):   70.8%  (283 / 400 questions)
#>   3 (no overlap):         0.0%  (0 / 400 questions)
#>   Average unique levels per question: 1.71
#> 
#> quality: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):  45.8%  (183 / 400 questions)
#>   2 (no overlap):        54.2%  (217 / 400 questions)
#>   Average unique levels per question: 1.54
#> 
#> 

# Store results for later use
inspection <- cbc_inspect(design, sections = "balance")
inspection  # prints the same output
#> DESIGN SUMMARY
#> =========================
#> 
#> ATTRIBUTE BALANCE
#> =================
#> Overall balance score: 0.969 (higher is better)
#> 
#> Individual attribute level counts:
#> 
#> price:
#> 
#>   1   2   3 
#> 276 256 268 
#>   Balance score: 0.964 (higher is better)
#> 
#> type:
#> 
#>   A   B   C 
#> 276 272 252 
#>   Balance score: 0.954 (higher is better)
#> 
#> quality:
#> 
#>  Low High 
#>  397  403 
#>   Balance score: 0.990 (higher is better)
#> 

# Verbose output with technical details
cbc_inspect(design, verbose = TRUE)
#> DESIGN SUMMARY
#> =========================
#> 
#> STRUCTURE
#> ================
#> Method: random
#> Created: 2026-06-24 10:41:26
#> Generation time: 0.143 seconds
#> Respondents: 100
#> Questions per respondent: 4
#> Alternatives per question: 2
#> Total choice sets: 400
#> Profile usage: 18/18 (100.0%)
#> Optimization attempts: 2
#> 
#> SUMMARY METRICS
#> =================
#> D-error calculation not available for this design
#> Overall balance score: 0.969 (higher is better)
#> Overall overlap score: 0.365 (lower is better)
#>   Profiles used: 18/18
#> 
#> VARIABLE ENCODING
#> =================
#> Format: Standard (categorical) (type, quality)
#> 
#> Categorical variable details:
#>   type: A, B, C (reference: A)
#>   quality: Low, High (reference: Low)
#> 💡 Use cbc_encode() to convert to dummy or effects coding
#> 
#> ATTRIBUTE BALANCE
#> =================
#> Overall balance score: 0.969 (higher is better)
#> 
#> Individual attribute level counts:
#> 
#> price:
#> 
#>   1   2   3 
#> 276 256 268 
#>   Balance score: 0.964 (higher is better), CV: 0.038 (lower is better)
#> 
#> type:
#> 
#>   A   B   C 
#> 276 272 252 
#>   Balance score: 0.954 (higher is better), CV: 0.048 (lower is better)
#> 
#> quality:
#> 
#>  Low High 
#>  397  403 
#>   Balance score: 0.990 (higher is better), CV: 0.011 (lower is better)
#> 
#> ATTRIBUTE OVERLAP
#> =================
#> Overall overlap score: 0.365 (lower is better)
#> 
#> Counts of attribute overlap:
#> (# of questions with N unique levels)
#> 
#> price: Continuous variable
#>   Unique levels:  1, 2, 3 
#>   Questions by # unique levels:
#>   1 (complete overlap):  34.5%  (138 / 400 questions)
#>   2 (partial overlap):   65.5%  (262 / 400 questions)
#>   3 (no overlap):         0.0%  (0 / 400 questions)
#>   Average unique levels per question: 1.66
#> 
#> type: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):  29.2%  (117 / 400 questions)
#>   2 (partial overlap):   70.8%  (283 / 400 questions)
#>   3 (no overlap):         0.0%  (0 / 400 questions)
#>   Average unique levels per question: 1.71
#> 
#> quality: Categorical variable
#>   Questions by # unique levels:
#>   1 (complete overlap):  45.8%  (183 / 400 questions)
#>   2 (no overlap):        54.2%  (217 / 400 questions)
#>   Average unique levels per question: 1.54
#> 
#> 
```

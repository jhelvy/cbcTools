# Generating Profiles

## Generating all possible profiles

The first step in designing an experiment is to define the attributes
and levels for your experiment and then generate all of the `profiles`
of each possible combination of those attributes and levels.

For example, let’s say you’re designing a conjoint experiment about
apples and you want to include `price`, `type`, and `freshness` as
attributes. You can obtain all of the possible profiles for these
attributes using the
[`cbc_profiles()`](https://jhelvy.github.io/cbcTools/reference/cbc_profiles.md)
function:

``` r

library(cbcTools)

profiles <- cbc_profiles(
  price     = seq(1, 5, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

profiles
#> CBC Profiles
#> ============
#> price       : Continuous (9 levels, range: 1.00-5.00)
#> type        : Categorical (3 levels: Fuji, Gala, Honeycrisp)
#> freshness   : Categorical (3 levels: Poor, Average, Excellent)
#> 
#> Profiles: 81
#> First few rows:
#>   profileID price type freshness
#> 1         1   1.0 Fuji      Poor
#> 2         2   1.5 Fuji      Poor
#> 3         3   2.0 Fuji      Poor
#> 4         4   2.5 Fuji      Poor
#> 5         5   3.0 Fuji      Poor
#> 6         6   3.5 Fuji      Poor
#> ... and 75 more rows
```

## Restricted profiles

Depending on the context of your survey, you may wish to eliminate some
profiles before designing your conjoint survey (e.g., some profile
combinations may be illogical or unrealistic).

> **CAUTION: including restrictions in your designs can substantially
> reduce the statistical power of your design, so use them cautiously
> and avoid them if possible**.

If you do wish to restrict some attribute level combinations, you can do
so using the
[`cbc_restrict()`](https://jhelvy.github.io/cbcTools/reference/cbc_restrict.md)
function, which takes the full set of `profiles` along with any number
of restricted pairs of attribute levels, defined as pairs of logical
expressions separated by commas. The example below includes the
following restrictions (these are arbitrary and just for illustration
purposes):

- `"Gala"` apples will not be shown with the prices `1.5`, `2.5`, and
  `3.5`.
- `"Honeycrisp"` apples will not be shown with prices less than `2`.
- `"Fuji"` apples will not be shown with the `"Excellent"` freshness.

With these restrictions, there are now only 57 profiles compared to 81
without them:

``` r

restricted_profiles <- cbc_restrict(
    profiles,
    type == "Gala" & price %in% c(1.5, 2.5, 3.5),
    type == "Honeycrisp" & price < 2,
    type == "Fuji" & freshness == "Excellent"
)

dim(restricted_profiles)
#> [1] 57  4
```

Once you have your profiles, you can use them to generate an experiment
design with
[`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md).
See the [Generating
Designs](https://jhelvy.github.io/cbcTools/articles/design.md) article
for more details.

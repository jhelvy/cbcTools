
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbcTools <a href='https://jhelvy.github.io/cbcTools/'><img src='man/figures/logo.png' align="right" style="height:139px;"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cbcTools)](https://CRAN.R-project.org/package=cbcTools)
<!-- badges: end -->

This package provides a set of tools for designing surveys and
conducting power analyses for choice-based conjoint survey experiments
in R. Each function in the package begins with `cbc_` and supports a
step in the following process for designing and analyzing surveys:

![](man/figures/process.png)

## Installation

The current version is not yet on CRAN, but you can install it from
Github using the {remotes} library:

``` r
# install.packages("remotes")
remotes::install_github("jhelvy/cbcTools")
```

Load the library with:

``` r
library(cbcTools)
```

## Make survey designs

### Generating profiles

The first step in designing an experiment is to define the attributes
and levels for your experiment and then generate all of the `profiles`
of each possible combination of those attributes and levels. For
example, let’s say you’re designing a conjoint experiment about apples
and you want to include `price`, `type`, and `freshness` as attributes.
You can obtain all of the possible profiles for these attributes using
the `cbc_profiles()` function:

``` r
profiles <- cbc_profiles(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Excellent', 'Average', 'Poor')
)

nrow(profiles)
#> [1] 63
head(profiles)
#>   profileID price type freshness
#> 1         1   1.0 Fuji Excellent
#> 2         2   1.5 Fuji Excellent
#> 3         3   2.0 Fuji Excellent
#> 4         4   2.5 Fuji Excellent
#> 5         5   3.0 Fuji Excellent
#> 6         6   3.5 Fuji Excellent
tail(profiles)
#>    profileID price       type freshness
#> 58        58   1.5 Honeycrisp      Poor
#> 59        59   2.0 Honeycrisp      Poor
#> 60        60   2.5 Honeycrisp      Poor
#> 61        61   3.0 Honeycrisp      Poor
#> 62        62   3.5 Honeycrisp      Poor
#> 63        63   4.0 Honeycrisp      Poor
```

Depending on the context of your survey, you may wish to eliminate or
modify some profiles before designing your conjoint survey (e.g., some
profile combinations may be illogical or unrealistic), though doing so
could significantly impact your ability to identify effects. As a
result, it is recommended that you avoid eliminating profiles if
possible.

If you do wish to set some levels conditional on those of other
attributes, you can do so by setting each level of an attribute to a
list that defines these constraints. In the example below, the `type`
attribute has constraints such that only certain price levels will be
shown for each level. In addition, for the `"Honeycrisp"` level, only
two of the three `freshness` levels are included: `"Excellent"` and
`"Average"`. Note that both the other attributes (`price` and
`freshness`) should contain all of the possible levels:

``` r
profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
  freshness = c("Excellent", "Average", "Poor"),
  type = list(
    "Fuji" = list(
        price = c(2, 2.5, 3)
    ),
    "Gala" = list(
        price = c(1, 1.5, 2)
    ),
    "Honeycrisp" = list(
        price = c(2.5, 3, 3.5, 4, 4.5, 5),
        freshness = c("Excellent", "Average")
    )
  )
)

nrow(profiles)
#> [1] 30
head(profiles)
#>   profileID price freshness type
#> 1         1   2.0 Excellent Fuji
#> 2         2   2.5 Excellent Fuji
#> 3         3   3.0 Excellent Fuji
#> 4         4   2.0   Average Fuji
#> 5         5   2.5   Average Fuji
#> 6         6   3.0   Average Fuji
tail(profiles)
#>    profileID price freshness       type
#> 25        25   2.5   Average Honeycrisp
#> 26        26   3.0   Average Honeycrisp
#> 27        27   3.5   Average Honeycrisp
#> 28        28   4.0   Average Honeycrisp
#> 29        29   4.5   Average Honeycrisp
#> 30        30   5.0   Average Honeycrisp
```

When these constraints you can see that there are fewer profiles
available (only 30 compared to 63 without constraints). **WARNING:
including hard constraints in your designs can substantially reduce the
statistical power of your design, so use them cautiously and avoid them
if possible**.

### Generating random designs

Once a set of profiles is obtained, a randomized conjoint survey can
then be generated using the `cbc_design()` function:

``` r
design <- cbc_design(
  profiles = profiles,
  n_resp   = 300, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6    # Number of questions per respondent
)

dim(design)  # View dimensions
#> [1] 5400    8
head(design) # Preview first 6 rows
#>   respID qID altID obsID profileID price       type freshness
#> 1      1   1     1     1        14   4.0       Gala Excellent
#> 2      1   1     2     1         5   3.0       Fuji Excellent
#> 3      1   1     3     1        34   3.5       Gala   Average
#> 4      1   2     1     2        32   2.5       Gala   Average
#> 5      1   2     2     2         9   1.5       Gala Excellent
#> 6      1   2     3     2        63   4.0 Honeycrisp      Poor
```

For now, the `cbc_design()` function only generates a randomized design.
Other packages, such as the [{idefix}](https://github.com/traets/idefix)
package, are able to generate other types of designs, such as
D-efficient designs. The randomized design simply samples from the set
of `profiles`. It also ensures that no two alternatives are the same in
any choice question.

The resulting `design` data frame includes the following columns:

-   `respID`: Identifies each survey respondent.
-   `qID`: Identifies the choice question answered by the respondent.
-   `altID`:Identifies the alternative in any one choice observation.
-   `obsID`: Identifies each unique choice observation across all
    respondents.
-   `profileID`: Identifies the profile in `profiles`.

### Labeled designs (a.k.a. “alternative-specific” designs)

You can also make a “labeled” design (also known as
“alternative-specific” design) where the levels of one attribute is used
as a label by setting the `label` argument to that attribute. This by
definition sets the number of alternatives in each question to the
number of levels in the chosen attribute, so the `n_alts` argument is
overridden. Here is an example labeled survey using the `type` attribute
as the label:

``` r
design_labeled <- cbc_design(
  profiles  = profiles,
  n_resp    = 300, # Number of respondents
  n_alts    = 3,   # Number of alternatives per question
  n_q       = 6,   # Number of questions per respondent
  label     = "type" # Set the "type" attribute as the label
)

dim(design_labeled)
#> [1] 5400    8
head(design_labeled)
#>   respID qID altID obsID profileID price       type freshness
#> 1      1   1     1     1         2   1.5       Fuji Excellent
#> 2      1   1     2     1        11   2.5       Gala Excellent
#> 3      1   1     3     1        40   3.0 Honeycrisp   Average
#> 4      1   2     1     2        47   3.0       Fuji      Poor
#> 5      1   2     2     2         9   1.5       Gala Excellent
#> 6      1   2     3     2        59   2.0 Honeycrisp      Poor
```

In the above example, you can see in the first six rows of the survey
that the `type` attribute is always fixed to be the same order, ensuring
that each level in the `type` attribute will always be shown in each
choice question.

### Adding a “no choice” option (a.k.a. “outside good”)

You can include a “no choice” (also known as “outside good” option in
your survey by setting `no_choice = TRUE`. If included, all categorical
attributes will be dummy-coded to appropriately dummy-code the “no
choice” alternative.

``` r
design_nochoice <- cbc_design(
  profiles  = profiles,
  n_resp    = 300, # Number of respondents
  n_alts    = 3, # Number of alternatives per question
  n_q       = 6, # Number of questions per respondent
  no_choice = TRUE
)

dim(design_nochoice)
#> [1] 7200   13
head(design_nochoice)
#>   respID qID altID obsID profileID price type_Fuji type_Gala type_Honeycrisp
#> 1      1   1     1     1        30   1.5         0         1               0
#> 2      1   1     2     1        14   4.0         0         1               0
#> 3      1   1     3     1        24   2.0         1         0               0
#> 4      1   1     4     1         0   0.0         0         0               0
#> 5      1   2     1     2        26   3.0         1         0               0
#> 6      1   2     2     2        35   4.0         0         1               0
#>   freshness_Excellent freshness_Average freshness_Poor no_choice
#> 1                   0                 1              0         0
#> 2                   1                 0              0         0
#> 3                   0                 1              0         0
#> 4                   0                 0              0         1
#> 5                   0                 1              0         0
#> 6                   0                 1              0         0
```

## Inspecting survey designs

The {cbcTools} package includes some functions to quickly inspect some
basic metrics of a design.

The `cbc_balance()` function prints out a summary of the counts of each
level for each attribute across all choice questions as well as the
two-way counts across all pairs of attributes for a given design:

``` r
cbc_balance(design)
#> ==============================
#> Attribute counts:
#> 
#> price:
#> 
#>   1 1.5   2 2.5   3 3.5   4 
#> 748 753 742 818 775 786 778 
#> 
#> type:
#> 
#>       Fuji       Gala Honeycrisp 
#>       1771       1794       1835 
#> 
#> freshness:
#> 
#> Excellent   Average      Poor 
#>      1802      1809      1789 
#> 
#> ==============================
#> Pairwise attribute counts:
#> 
#> price & type:
#>      
#>       Fuji Gala Honeycrisp
#>   1    253  233        262
#>   1.5  241  264        248
#>   2    249  252        241
#>   2.5  257  276        285
#>   3    248  278        249
#>   3.5  262  242        282
#>   4    261  249        268
#> 
#> price & freshness:
#>      
#>       Excellent Average Poor
#>   1         269     225  254
#>   1.5       249     248  256
#>   2         258     242  242
#>   2.5       274     282  262
#>   3         249     267  259
#>   3.5       234     272  280
#>   4         269     273  236
#> 
#> type & freshness:
#>             
#>              Excellent Average Poor
#>   Fuji             598     575  598
#>   Gala             579     631  584
#>   Honeycrisp       625     603  607
```

The `cbc_overlap()` function prints out a summary of the amount of
“overlap” across attributes within the choice questions. For example,
for each attribute, the count under `"1"` is the number of choice
questions in which the same level was shown across all alternatives for
that attribute (because there was only one level shown). Likewise, the
count under `"2"` is the number of choice questions in which only two
unique levels of that attribute were shown, and so on:

``` r
cbc_overlap(design)
#> ==============================
#> Counts of attribute overlap:
#> (# of questions with N unique levels)
#> 
#> price:
#> 
#>    1    2    3 
#>   24  627 1149 
#> 
#> type:
#> 
#>    1    2    3 
#>  185 1166  449 
#> 
#> freshness:
#> 
#>    1    2    3 
#>  161 1224  415
```

## Simulating choices

You can simulate choices for a given `design` using the `cbc_choices()`
function. By default, random choices are simulated:

``` r
data <- cbc_choices(
  design = design,
  obsID  = "obsID"
)

head(data)
#>   respID qID altID obsID profileID price       type freshness choice
#> 1      1   1     1     1        14   4.0       Gala Excellent      1
#> 2      1   1     2     1         5   3.0       Fuji Excellent      0
#> 3      1   1     3     1        34   3.5       Gala   Average      0
#> 4      1   2     1     2        32   2.5       Gala   Average      0
#> 5      1   2     2     2         9   1.5       Gala Excellent      1
#> 6      1   2     3     2        63   4.0 Honeycrisp      Poor      0
```

You can also pass a list of prior parameters to define a utility model
that will be used to simulate choices. In the example below, the choices
are simulated using a utility model with the following parameters:

-   1 continuous parameter for `price`
-   2 categorical parameters for `type` (first level is reference)
-   2 categorical parameters for `freshness` (first level is reference)

``` r
data <- cbc_choices(
  design = design,
  obsID = "obsID",
  priors = list(
    price     = 0.1,
    type      = c(0.1, 0.2),
    freshness = c(0.1, -0.2)
  )
)
```

If you wish to include a prior model with an interaction, you can do so
inside the `priors` list. For example, here is the same example as above
but with an interaction between `price` and `type` added:

``` r
data <- cbc_choices(
  design = design,
  obsID = "obsID",
  priors = list(
    price = 0.1,
    type = c(0.1, 0.2),
    freshness = c(0.1, -0.2),
    `price*type` = c(0.1, 0.5)
  )
)
```

Finally, you can also simulate data for a mixed logit specification
where parameters follow a normal or log-normal distribution across the
population. In the example below, the `randN()` function is used to
specify the `type` attribute with 2 random normal discrete parameters
with a specified mean (`mu`) and standard deviation (`sigma`):

``` r
data <- cbc_choices(
  design = design,
  obsID = "obsID",
  priors = list(
    price = 0.1,
    type = randN(mu = c(0.1, 0.2), sigma = c(0.5, 1)),
    freshness = c(0.1, -0.2)
  )
)
```

## Conducting a power analysis

The simulated choice data can be used to conduct a power analysis by
estimating the same model multiple times with incrementally increasing
sample sizes. As the sample size increases, the estimated coefficient
standard errors should decrease (i.e. coefficient estimates become more
precise). The `cbc_power()` function achieves this by partitioning the
choice data into multiple sizes (defined by the `nbreaks` argument) and
then estimating a user-defined choice model on each data subset. In the
example below, 10 different sample sizes are used. All models are
estimated using the [{logitr}](https://jhelvy.github.io/logitr) package:

``` r
results <- cbc_power(
  data    = data,
  pars    = c("price", "type", "freshness"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6
)

head(results)
#>   sampleSize             coef          est         se
#> 1         30            price -0.054967595 0.09331697
#> 2         30         typeGala  0.241460837 0.23571546
#> 3         30   typeHoneycrisp -0.025891938 0.23294214
#> 4         30 freshnessAverage  0.282635623 0.22742947
#> 5         30    freshnessPoor  0.200574674 0.22775789
#> 6         60            price  0.001842514 0.06471784
tail(results)
#>    sampleSize             coef          est         se
#> 45        270    freshnessPoor  0.021202810 0.07357262
#> 46        300            price  0.036521461 0.02863566
#> 47        300         typeGala -0.146216522 0.07066389
#> 48        300   typeHoneycrisp -0.119684914 0.06911106
#> 49        300 freshnessAverage  0.079206886 0.06993513
#> 50        300    freshnessPoor  0.006282582 0.06983651
```

The `results` object is a data frame containing the coefficient
estimates and standard errors for each sample size. You can quickly
visualize the outcome to identify a required sample size for a desired
level of parameter precision by using the `plot()` method:

``` r
plot(results)
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="672" />

## Piping it all together!

One of the convenient features of how the package is written is that the
object generated in each step is used as the first argument to the
function for the next step. Thus, just like in the overall program
diagram, the functions can be piped together:

![](man/figures/process.png)

``` r
cbc_profiles(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Excellent', 'Average', 'Poor')
) |>
cbc_design(
  n_resp   = 300, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6    # Number of questions per respondent
) |>
cbc_choices(
  obsID = "obsID",
  priors = list(
    price     = 0.1,
    type      = c(0.1, 0.2),
    freshness = c(0.1, -0.2)
  )
) |>
cbc_power(
    pars    = c("price", "type", "freshness"),
    outcome = "choice",
    obsID   = "obsID",
    nbreaks = 10,
    n_q     = 6
) |>
plot()
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="672" />

## Author, Version, and License Information

-   Author: *John Paul Helveston* <https://www.jhelvy.com/>
-   Date First Written: *October 23, 2020*
-   License:
    [MIT](https://github.com/jhelvy/cbcTools/blob/master/LICENSE.md)

## Citation Information

If you use this package for in a publication, I would greatly appreciate
it if you cited it - you can get the citation by typing
`citation("cbcTools")` into R:

``` r
citation("cbcTools")
#> 
#> To cite cbcTools in publications use:
#> 
#>   John Paul Helveston (2022). cbcTools: Tools For Designing Conjoint
#>   Survey Experiments.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {cbcTools: Tools For Designing Choice-Based Conjoint Survey Experiments},
#>     author = {John Paul Helveston},
#>     year = {2022},
#>     note = {R package version 0.0.2},
#>     url = {https://jhelvy.github.io/cbcTools/},
#>   }
```

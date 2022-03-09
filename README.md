
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbcTools

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cbcTools)](https://CRAN.R-project.org/package=cbcTools)
<!-- badges: end -->

This package contains tools for designing surveys and conducting power
analyses for choice based conjoint survey experiments in R.

## Installation

The current version is not yet on CRAN, but you can install it from
Github using the {remotes} library:

``` r
# install.packages("remotes")
remotes::install_github("jhelvy/cbcTools")
```

Load the library with:

``` r
library("cbcTools")
```

## Make survey designs

The first step in designing an experiment is to define the attributes
and levels for your experiment. Many of the functions in {cbcTools} are
more convenient to use if you define these as a separate object. For
example, let’s say you’re designing a conjoint experiment about apples.
You might have the following attributes and levels:

``` r
levels <- list(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Excellent', 'Average', 'Poor')
)
```

With these levels defined, you can then obtain all of the `profiles` of
each possible combination of the attributes and levels using the
`cbc_profiles()` function:

``` r
profiles <- cbc_profiles(levels)
head(profiles)
#>   profileID price type freshness
#> 1         1   1.0 Fuji Excellent
#> 2         2   1.5 Fuji Excellent
#> 3         3   2.0 Fuji Excellent
#> 4         4   2.5 Fuji Excellent
#> 5         5   3.0 Fuji Excellent
#> 6         6   3.5 Fuji Excellent
```

Depending on the context of your survey, you may wish to eliminate or
modify some profiles before designing your conjoint survey (e.g., some
profile combinations may be illogical or unrealistic), though doing so
could significantly impact your ability to identify effects. As a
result, it is recommended that you avoid eliminating profiles if
possible.

With these profiles, a randomized conjoint survey can then be generated
using the `cbc_design()` function:

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
#> 1      1   1     1     1         5   3.0       Fuji Excellent
#> 2      1   1     2     1        18   2.5 Honeycrisp Excellent
#> 3      1   1     3     1        45   2.0       Fuji      Poor
#> 4      1   2     1     2         1   1.0       Fuji Excellent
#> 5      1   2     2     2         4   2.5       Fuji Excellent
#> 6      1   2     3     2        49   4.0       Fuji      Poor
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
#> 1      1   1     1     1        28   4.0       Fuji   Average
#> 2      1   1     2     1        30   1.5       Gala   Average
#> 3      1   1     3     1        36   1.0 Honeycrisp   Average
#> 4      1   2     1     2        47   3.0       Fuji      Poor
#> 5      1   2     2     2        54   3.0       Gala      Poor
#> 6      1   2     3     2        41   3.5 Honeycrisp   Average
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
#>       respID qID altID obsID profileID price type_Fuji type_Gala
#> 1          1   1     1     1        47   3.0         1         0
#> 2          1   1     2     1         7   4.0         1         0
#> 3          1   1     3     1        18   2.5         0         0
#> 11000      1   1     4     1         0   0.0         0         0
#> 4          1   2     1     2        39   2.5         0         0
#> 5          1   2     2     2        14   4.0         0         1
#>       type_Honeycrisp freshness_Excellent freshness_Average freshness_Poor
#> 1                   0                   0                 0              1
#> 2                   0                   1                 0              0
#> 3                   1                   1                 0              0
#> 11000               0                   0                 0              0
#> 4                   1                   0                 1              0
#> 5                   0                   1                 0              0
#>       no_choice
#> 1             0
#> 2             0
#> 3             0
#> 11000         1
#> 4             0
#> 5             0
```

## Inspect survey design

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
#> 780 780 730 723 793 787 807 
#> 
#> type:
#> 
#>       Fuji       Gala Honeycrisp 
#>       1837       1842       1721 
#> 
#> freshness:
#> 
#> Excellent   Average      Poor 
#>      1775      1791      1834 
#> 
#> ==============================
#> Pairwise attribute counts:
#> 
#> price & type:
#>      
#>       Fuji Gala Honeycrisp
#>   1    254  286        240
#>   1.5  267  266        247
#>   2    251  243        236
#>   2.5  238  240        245
#>   3    284  259        250
#>   3.5  283  263        241
#>   4    260  285        262
#> 
#> price & freshness:
#>      
#>       Excellent Average Poor
#>   1         272     259  249
#>   1.5       234     295  251
#>   2         239     260  231
#>   2.5       251     233  239
#>   3         267     243  283
#>   3.5       224     257  306
#>   4         288     244  275
#> 
#> type & freshness:
#>             
#>              Excellent Average Poor
#>   Fuji             611     628  598
#>   Gala             620     594  628
#>   Honeycrisp       544     569  608
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
#>   23  615 1162 
#> 
#> type:
#> 
#>    1    2    3 
#>  181 1198  421 
#> 
#> freshness:
#> 
#>    1    2    3 
#>  171 1201  428
```

## Simulate choices

You can simulate choices for a given `design` using the `cbc_choices()`
function. By default, random choices are simulated:

``` r
data <- cbc_choices(
  design = design,
  obsID  = "obsID"
)

head(data)
#>   respID qID altID obsID profileID price       type freshness choice
#> 1      1   1     1     1         5   3.0       Fuji Excellent      0
#> 2      1   1     2     1        18   2.5 Honeycrisp Excellent      1
#> 3      1   1     3     1        45   2.0       Fuji      Poor      0
#> 4      1   2     1     2         1   1.0       Fuji Excellent      1
#> 5      1   2     2     2         4   2.5       Fuji Excellent      0
#> 6      1   2     3     2        49   4.0       Fuji      Poor      0
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

## Conduct a power analysis

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
    nbreaks = 10,
    n_q     = 6,
    data    = data,
    pars    = c("price", "type", "freshness"),
    outcome = "choice",
    obsID   = "obsID"
)

head(results)
#>   sampleSize             coef          est         se
#> 1         30            price  0.040168091 0.09065817
#> 2         30         typeGala  0.200132877 0.22043473
#> 3         30   typeHoneycrisp  0.302388043 0.22559634
#> 4         30 freshnessAverage  0.089309743 0.22409682
#> 5         30    freshnessPoor  0.187173728 0.22217364
#> 6         60            price -0.008665727 0.06194921
tail(results)
#>    sampleSize             coef          est         se
#> 45        270    freshnessPoor  0.006020976 0.07251988
#> 46        300            price  0.003517079 0.02817057
#> 47        300         typeGala -0.022768295 0.06992489
#> 48        300   typeHoneycrisp  0.048709964 0.07047344
#> 49        300 freshnessAverage -0.091429629 0.07048173
#> 50        300    freshnessPoor  0.010679873 0.06919329
```

The `results` object is a data frame containing the coefficient
estimates and standard errors for each sample size. You can quickly
visualize the outcome to identify a required sample size for a desired
level of parameter precision by using the `plot()` method:

``` r
plot(results)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="672" />

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
#>   John Paul Helveston (2021). cbcTools: Tools For Designing Conjoint
#>   Survey Experiments.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {cbcTools: Tools For Designing Choice-Based Conjoint Survey Experiments},
#>     author = {John Paul Helveston},
#>     year = {2021},
#>     note = {R package version 0.0.1},
#>     url = {https://jhelvy.github.io/cbcTools/},
#>   }
```

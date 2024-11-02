
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbcTools <a href='https://jhelvy.github.io/cbcTools/'><img src='man/figures/logo.png' align="right" style="height:139px;"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cbcTools)](https://CRAN.R-project.org/package=cbcTools)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/cbcTools)](https://cran.r-project.org/package=cbcTools)
<!-- badges: end -->

Functions for designing surveys and conducting power analyses for
choice-based conjoint survey experiments in R. Each function in the
package begins with `cbc_` and supports a step in the following process
for designing and analyzing surveys:

![](man/figures/program_diagram.png)

# Usage

View the [usage](https://jhelvy.github.io/cbcTools/articles/usage.html)
page for details on how to use **cbcTools**.

# Installation

You can install the latest version of {cbcTools} from CRAN:

``` r
install.packages("cbcTools")
```

or you can install the development version of {cbcTools} from
[GitHub](https://github.com/jhelvy/cbcTools):

``` r
# install.packages("remotes")
remotes::install_github("jhelvy/cbcTools")
```

Load the library with:

``` r
library(cbcTools)
```

# Author, Version, and License Information

- Author: *John Paul Helveston* <https://www.jhelvy.com/>
- Date First Written: *October 23, 2020*
- License:
  [MIT](https://github.com/jhelvy/cbcTools/blob/master/LICENSE.md)

# Citation Information

If you use this package for in a publication, I would greatly appreciate
it if you cited it - you can get the citation by typing
`citation("cbcTools")` into R:

``` r
citation("cbcTools")
#> To cite cbcTools in publications use:
#> 
#>   Helveston JP (2023). _cbcTools: Design and Evaluate Choice-Based
#>   Conjoint Survey Experiments_. R package,
#>   <https://jhelvy.github.io/cbcTools/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {{cbcTools}: Design and Evaluate Choice-Based Conjoint Survey Experiments},
#>     author = {John Paul Helveston},
#>     year = {2023},
#>     note = {R package},
#>     url = {https://jhelvy.github.io/cbcTools/},
#>   }
```

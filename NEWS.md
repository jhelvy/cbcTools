# cbcTools (development version)

# cbcTools 0.3.0

- Bug fix: modify code in `cbc_design()` to avoid duplicate choice sets for the same respondents; addresses #7.
- Bug fix: modify code in `cbc_design()` to allow Bayesian D-efficient designs with restricted profile sets; addresses #10 and #9.

# cbcTools 0.2.0

- Added a startup message when the package is loaded.
- Updates for compatibility with logitr version 1.0.1.
- Updated DESCRIPTION and CITATION to remove redundancy in title.
- Updated documentation of returned values in several functions.

# cbcTools 0.1.0

- Added initial integration with {idefix} packages for Bayesian D-efficient designs

# cbcTools 0.0.5

- Updates for compatibility with logitr version 0.8.0.

# cbcTools 0.0.4

- Updates for compatibility with logitr version 0.7.0.

# cbcTools 0.0.3

- Modified the argument of `cbc_profiles()` to `...` so that the user no longer needs to create a separate list to define the attributes and levels.
- Modified the arguments for the `randN()` and `randLN()` functions to `mean` and `sd`.
- Improved printing of counts in `cbc_balance()` and `cbc_overlap()`.
- Updated names of random parameter models to match that of future logitr v0.6.0.
- Updated documentation and examples for all functions.
- Adding piping example to readme.

# cbcTools 0.0.2

- Added support for conditional levels in `cbc_profiles()`

# cbcTools 0.0.1

- Added a `NEWS.md` file to track changes to the package.

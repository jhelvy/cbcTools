# cbcTools (development version)

# cbcTools 0.5.0

- Further revisions to the `method` argument in the `cbc_design()` function.
- Added the `"random"` and `"dopt"` methods.
- Added restrictions so that orthogonal designs cannot use the `label` argument or restricted profile sets (as either of these would result in a non-orthogonal design).

# cbcTools 0.4.0

- Adjustments made to the `method` argument in the `cbc_design()` function in preparation for potentially adding new design methods.
- Added the `"orthogonal"` option for generating orthogonal designs.

# cbcTools 0.3.4

- Another small bug fix in `cbc_design()` related to #16 where factor level ordering for categorical variables were being mis-ordered.
- Updated how the `method` argument is handled by default in `cbc_design()` to be more flexible (anticipating other methods in the future).
- Added `keep_db_error` arg to `cbc_design()`.

# cbcTools 0.3.3

- Bug fix in `cbc_design()` where factor level ordering for categorical variables were being mis-ordered.
- Added additional input check for appropriate `priors` in `cbc_design()`.

# cbcTools 0.3.2

- Modify how restrictions are defined in the `cbc_restrict()` function to allow users to provide expressions.

# cbcTools 0.3.1

- Add `cbc_restrict()` function to improve UI for adding restrictions to profiles.
- Remove previous approach to including restrictions in `cbc_profiles()`.
- Add new test cases

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

# Package index

## Profiles

Functions for generating attribute profiles.

- [`cbc_profiles()`](https://jhelvy.github.io/cbcTools/reference/cbc_profiles.md)
  : Make a data frame of all combinations of attribute levels
- [`cbc_restrict()`](https://jhelvy.github.io/cbcTools/reference/cbc_restrict.md)
  : Obtain a restricted set of profiles

## Priors

Functions for generating prior (assumed) coefficients.

- [`cbc_priors()`](https://jhelvy.github.io/cbcTools/reference/cbc_priors.md)
  : Create prior specifications for CBC models
- [`cbc_suggest_priors()`](https://jhelvy.github.io/cbcTools/reference/cbc_suggest_priors.md)
  : Suggest prior specifications for CBC models
- [`rand_spec()`](https://jhelvy.github.io/cbcTools/reference/rand_spec.md)
  : Create a random parameter specification
- [`cor_spec()`](https://jhelvy.github.io/cbcTools/reference/cor_spec.md)
  : Create a correlation specification for random parameters
- [`int_spec()`](https://jhelvy.github.io/cbcTools/reference/int_spec.md)
  : Create an interaction specification for fixed parameters

## Design

Functions for generating and comparing survey designs.

- [`cbc_design()`](https://jhelvy.github.io/cbcTools/reference/cbc_design.md)
  : Generate survey designs for choice experiments (Updated
  Implementation)
- [`cbc_compare()`](https://jhelvy.github.io/cbcTools/reference/cbc_compare.md)
  : Compare multiple choice experiment designs

## Inspect

Functions for inspecting and encoding survey designs.

- [`cbc_inspect()`](https://jhelvy.github.io/cbcTools/reference/cbc_inspect.md)
  : Comprehensive design quality inspection
- [`cbc_encode()`](https://jhelvy.github.io/cbcTools/reference/cbc_encode.md)
  : Encode categorical variables in a CBC design

## Simulate Choice

Functions for simulating choices.

- [`cbc_choices()`](https://jhelvy.github.io/cbcTools/reference/cbc_choices.md)
  : Simulate choices for a survey design

## Power Analysis

Functions for conducting power analysis.

- [`cbc_power()`](https://jhelvy.github.io/cbcTools/reference/cbc_power.md)
  : Estimate power analysis for choice experiment designs
- [`plot_compare_power()`](https://jhelvy.github.io/cbcTools/reference/plot_compare_power.md)
  : Compare power across multiple designs

## Methods

Various S3 methods.

- [`print(`*`<cbc_profiles>`*`)`](https://jhelvy.github.io/cbcTools/reference/print.cbc_profiles.md)
  : Print method for cbc_profiles objects
- [`print(`*`<cbc_priors>`*`)`](https://jhelvy.github.io/cbcTools/reference/print.cbc_priors.md)
  : Print method for cbc_priors objects
- [`print(`*`<cbc_design>`*`)`](https://jhelvy.github.io/cbcTools/reference/print.cbc_design.md)
  : Concise print method for cbc_design objects
- [`print(`*`<cbc_choices>`*`)`](https://jhelvy.github.io/cbcTools/reference/print.cbc_choices.md)
  : Print method for cbc_choices objects
- [`print(`*`<cbc_power>`*`)`](https://jhelvy.github.io/cbcTools/reference/print.cbc_power.md)
  : Print method for cbc_power objects
- [`plot(`*`<cbc_power>`*`)`](https://jhelvy.github.io/cbcTools/reference/plot.cbc_power.md)
  : Plot method for cbc_power objects
- [`summary(`*`<cbc_power>`*`)`](https://jhelvy.github.io/cbcTools/reference/summary.cbc_power.md)
  : Summary method for cbc_power objects
- [`print(`*`<cbc_inspection>`*`)`](https://jhelvy.github.io/cbcTools/reference/print.cbc_inspection.md)
  : Print method for cbc_inspection objects
- [`print(`*`<cbc_comparison>`*`)`](https://jhelvy.github.io/cbcTools/reference/print.cbc_comparison.md)
  : Print method for cbc_comparison objects

## Depreciated

Depreciated functions.

- [`cbc_decode()`](https://jhelvy.github.io/cbcTools/reference/cbc_decode.md)
  : Convert dummy-coded CBC data back to categorical format

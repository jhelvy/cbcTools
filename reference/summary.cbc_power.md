# Summary method for cbc_power objects

Summary method for cbc_power objects

## Usage

``` r
# S3 method for class 'cbc_power'
summary(object, power_threshold = 0.8, ...)
```

## Arguments

- object:

  A cbc_power object

- power_threshold:

  Minimum power threshold to report sample size requirements

- ...:

  Additional arguments

## Value

Returns the input `cbc_power` object invisibly (class: c("cbc_power",
"list")). This function is called for its side effect of printing a
detailed summary to the console showing sample size requirements for
achieving specified power thresholds for each parameter, including exact
power levels and standard errors at the required sample sizes.

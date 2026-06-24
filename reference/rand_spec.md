# Create a random parameter specification

Create a random parameter specification

## Usage

``` r
rand_spec(dist = "n", mean, sd, correlations = NULL)
```

## Arguments

- dist:

  Character. Distribution type: "n" for normal, "ln" for log-normal, or
  "cn" for censored normal

- mean:

  Numeric. Mean parameter value(s)

- sd:

  Numeric. Standard deviation parameter value(s)

- correlations:

  List of correlation specifications created by cor_spec()

## Value

A random parameter specification list

# Compare power across multiple designs

Compare power across multiple designs

## Usage

``` r
plot_compare_power(..., type = "power", power_threshold = 0.8)
```

## Arguments

- ...:

  Named cbc_power objects to compare

- type:

  Type of plot: "power" for power curves or "se" for standard error
  curves

- power_threshold:

  Power threshold for horizontal reference line (only for power plots).
  Defaults to 0.8

## Value

A ggplot object comparing power curves

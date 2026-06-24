# User-friendly wrapper to validate design with error messages

User-friendly wrapper to validate design with error messages

## Usage

``` r
validate_or_reconstruct(
  data,
  require_class = FALSE,
  allow_choices = TRUE,
  context = "this function"
)
```

## Arguments

- data:

  Input data

- require_class:

  Whether to require proper class

- allow_choices:

  Whether to allow cbc_choices objects

- context:

  Character describing where this is being called from (for error
  messages)

## Value

Validated/reconstructed data, or stops with error

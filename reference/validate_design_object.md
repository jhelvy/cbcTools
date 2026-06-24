# Validate and potentially reconstruct a CBC design object

This internal function checks if data is a valid CBC design/choices
object or can be treated as one. If the data has lost its class but has
the right structure (ID columns), it attempts to reconstruct minimal
attributes.

## Usage

``` r
validate_design_object(data, require_class = FALSE, allow_choices = TRUE)
```

## Arguments

- data:

  Input data (cbc_design, cbc_choices, or data.frame)

- require_class:

  Logical. If TRUE, strictly requires proper class. If FALSE, allows
  data.frames with correct structure.

- allow_choices:

  Logical. If TRUE, allows cbc_choices objects. Default TRUE.

## Value

A list with: valid (logical), data (potentially reconstructed), message
(character or NULL)

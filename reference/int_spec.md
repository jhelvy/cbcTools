# Create an interaction specification for fixed parameters

Create an interaction specification for fixed parameters

## Usage

``` r
int_spec(between, value, level = NULL, with_level = NULL)
```

## Arguments

- between:

  Character vector of length 2 specifying the two attributes to interact

- value:

  Numeric. Interaction coefficient value

- level:

  Character. For categorical variables, specific level of first
  attribute

- with_level:

  Character. For categorical variables, specific level of second
  attribute

## Value

An interaction specification list

## Examples

``` r
# Continuous * continuous interaction
int_spec(between = c("price", "weight"), value = 0.1)
#> $attr1
#> [1] "price"
#> 
#> $attr2
#> [1] "weight"
#> 
#> $value
#> [1] 0.1
#> 
#> $level
#> NULL
#> 
#> $with_level
#> NULL
#> 
#> attr(,"class")
#> [1] "cbc_interaction"

# Continuous * categorical interactions (must specify categorical level)
int_spec(between = c("price", "type"), with_level = "Fuji", value = 0.15)
#> $attr1
#> [1] "price"
#> 
#> $attr2
#> [1] "type"
#> 
#> $value
#> [1] 0.15
#> 
#> $level
#> NULL
#> 
#> $with_level
#> [1] "Fuji"
#> 
#> attr(,"class")
#> [1] "cbc_interaction"
int_spec(between = c("price", "type"), with_level = "Gala", value = 0.05)
#> $attr1
#> [1] "price"
#> 
#> $attr2
#> [1] "type"
#> 
#> $value
#> [1] 0.05
#> 
#> $level
#> NULL
#> 
#> $with_level
#> [1] "Gala"
#> 
#> attr(,"class")
#> [1] "cbc_interaction"

# Categorical * categorical interactions (must specify both levels)
int_spec(between = c("type", "freshness"),
         level = "Fuji", with_level = "Poor", value = -0.2)
#> $attr1
#> [1] "type"
#> 
#> $attr2
#> [1] "freshness"
#> 
#> $value
#> [1] -0.2
#> 
#> $level
#> [1] "Fuji"
#> 
#> $with_level
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "cbc_interaction"
```

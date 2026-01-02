# Generate Power Table

Creates a table of power values across effect sizes and sample sizes

## Usage

``` r
ssm_power_table(
  effects = c(0.1, 0.15, 0.2, 0.25),
  ns = c(50, 100, 150, 200, 300),
  type = c("amplitude", "elevation"),
  alpha = 0.05
)
```

## Arguments

- effects:

  Vector of effect sizes

- ns:

  Vector of sample sizes

- type:

  Either "amplitude" or "elevation"

- alpha:

  Significance level (default: 0.05)

## Value

A data frame of class `ssm_power_table` with power values

## Examples

``` r
ssm_power_table(effects = c(0.10, 0.15, 0.20),
                ns = c(50, 100, 150, 200),
                type = "amplitude")
#> 
#> SSM Power Table (amplitude)
#> ======================================== 
#>  Effect  n=50 n=100 n=150 n=200
#>    0.10 0.532 0.786 0.910 0.964
#>    0.15 0.827 0.978 0.998 1.000
#>    0.20 0.964 0.999 1.000 1.000
```

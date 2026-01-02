# Required Sample Size for Two-Group Elevation Comparison

Calculates sample size per group for comparing elevations between groups

## Usage

``` r
ssm_sample_size_elevation_diff(
  effect,
  power = 0.8,
  alpha = 0.05,
  k = 0.6,
  ratio = 1
)
```

## Arguments

- effect:

  Expected elevation difference

- power:

  Target power (default: 0.80)

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant (default: 0.60)

- ratio:

  Allocation ratio n2/n1 (default: 1)

## Value

An object of class `ssm_sample_size`

## Examples

``` r
ssm_sample_size_elevation_diff(effect = 0.11, power = 0.80)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           elevation_difference (two_sample)
#> Effect size:    0.110
#> Target power:   0.80
#> Alpha:          0.050
#> Test:           two-sided
#> ---------------------------
#> Required n1:    468
#> Required n2:    468
#> Total N:        936
#> Achieved power: 0.8008
```

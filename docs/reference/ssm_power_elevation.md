# Power for Single-Sample Elevation Test

Calculates power for testing H0: elevation = 0 vs H1: elevation != 0

## Usage

``` r
ssm_power_elevation(effect, n, alpha = 0.05, k = 0.6, one_sided = FALSE)
```

## Arguments

- effect:

  Expected absolute elevation

- n:

  Sample size

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant for elevation SE (default: 0.60)

- one_sided:

  Use one-sided test? (default: FALSE)

## Value

An object of class `ssm_power`

## Examples

``` r
ssm_power_elevation(effect = 0.11, n = 200)
#> 
#> SSM Power Analysis
#> ==================
#> Type:        elevation (single_sample)
#> Effect size: 0.110
#> Sample size: 200
#> Alpha:       0.050
#> Test:        two-sided
#> ------------------
#> Power:       0.737
```

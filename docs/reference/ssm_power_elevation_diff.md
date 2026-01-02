# Power for Two-Group Elevation Difference

Calculates power for comparing elevations between two independent groups

## Usage

``` r
ssm_power_elevation_diff(effect, n1, n2 = n1, alpha = 0.05, k = 0.6)
```

## Arguments

- effect:

  Expected elevation difference

- n1:

  Sample size for group 1

- n2:

  Sample size for group 2 (default: same as n1)

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant (default: 0.60)

## Value

An object of class `ssm_power`

## Examples

``` r
ssm_power_elevation_diff(effect = 0.11, n1 = 200, n2 = 200)
#> 
#> SSM Power Analysis
#> ==================
#> Type:        elevation_difference (two_sample)
#> Effect size: 0.110
#> Sample size: n1=200, n2=200
#> Alpha:       0.050
#> Test:        two-sided
#> ------------------
#> Power:       0.450
```

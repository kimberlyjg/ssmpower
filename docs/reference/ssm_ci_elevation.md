# Confidence Interval for Elevation

Calculates asymptotic confidence interval for elevation

## Usage

``` r
ssm_ci_elevation(elevation, n, conf_level = 0.95, k = 0.6)
```

## Arguments

- elevation:

  Point estimate of elevation

- n:

  Sample size

- conf_level:

  Confidence level (default: 0.95)

- k:

  Scaling constant (default: 0.60)

## Value

Named numeric vector with lower and upper bounds

## Examples

``` r
ssm_ci_elevation(elevation = 0.15, n = 200)
#>      lower      upper 
#> 0.06684577 0.23315423 
#> attr(,"conf_level")
#> [1] 0.95
```

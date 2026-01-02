# Confidence Interval for Amplitude

Calculates asymptotic confidence interval for amplitude (truncated at 0)

## Usage

``` r
ssm_ci_amplitude(amplitude, n, conf_level = 0.95, k = 0.41)
```

## Arguments

- amplitude:

  Point estimate of amplitude

- n:

  Sample size

- conf_level:

  Confidence level (default: 0.95)

- k:

  Scaling constant (default: 0.41)

## Value

Named numeric vector with lower and upper bounds

## Examples

``` r
ssm_ci_amplitude(amplitude = 0.25, n = 200)
#>     lower     upper 
#> 0.1931779 0.3068221 
#> attr(,"conf_level")
#> [1] 0.95
```

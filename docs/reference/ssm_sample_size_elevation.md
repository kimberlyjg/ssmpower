# Required Sample Size for Elevation Test

Calculates sample size needed to achieve target power for elevation test

## Usage

``` r
ssm_sample_size_elevation(
  effect,
  power = 0.8,
  alpha = 0.05,
  k = 0.6,
  one_sided = FALSE
)
```

## Arguments

- effect:

  Expected absolute elevation

- power:

  Target power (default: 0.80)

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant (default: 0.60)

- one_sided:

  Use one-sided test? (default: FALSE)

## Value

An object of class `ssm_sample_size`

## Examples

``` r
ssm_sample_size_elevation(effect = 0.11, power = 0.80)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           elevation (single_sample)
#> Effect size:    0.110
#> Target power:   0.80
#> Alpha:          0.050
#> Test:           two-sided
#> ---------------------------
#> Required n:     234
#> Achieved power: 0.8008
```

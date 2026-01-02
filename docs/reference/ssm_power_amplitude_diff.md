# Power for Two-Group Amplitude Difference

Calculates power for comparing amplitudes between two independent groups

## Usage

``` r
ssm_power_amplitude_diff(effect, n1, n2 = n1, alpha = 0.05, k = 0.41)
```

## Arguments

- effect:

  Expected amplitude difference

- n1:

  Sample size for group 1

- n2:

  Sample size for group 2 (default: same as n1)

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant (default: 0.41)

## Value

An object of class `ssm_power`

## Examples

``` r
# Power to detect medium difference with 100 per group
ssm_power_amplitude_diff(effect = 0.16, n1 = 100, n2 = 100)
#> 
#> SSM Power Analysis
#> ==================
#> Type:        amplitude_difference (two_sample)
#> Effect size: 0.160
#> Sample size: n1=100, n2=100
#> Alpha:       0.050
#> Test:        two-sided
#> ------------------
#> Power:       0.788
```

# Power for Single-Sample Amplitude Test

Calculates statistical power for testing H0: amplitude = 0 vs H1:
amplitude \> 0

## Usage

``` r
ssm_power_amplitude(effect, n, alpha = 0.05, k = 0.41, one_sided = TRUE)
```

## Arguments

- effect:

  Expected amplitude (effect size)

- n:

  Sample size

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant for SE (default: 0.41, empirically derived)

- one_sided:

  Logical; use one-sided test? (default: TRUE, appropriate for
  amplitude)

## Value

An object of class `ssm_power` containing power and input parameters

## References

Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
interpersonal construct validation: Methodological advances in the
circumplex Structural Summary Approach. *Assessment, 24*, 3-23.

## Examples

``` r
# Power to detect medium amplitude with n=100
ssm_power_amplitude(effect = 0.16, n = 100)
#> 
#> SSM Power Analysis
#> ==================
#> Type:        amplitude (single_sample)
#> Effect size: 0.160
#> Sample size: 100
#> Alpha:       0.050
#> Test:        one-sided
#> ------------------
#> Power:       0.988

# Power curve across sample sizes
sapply(c(50, 100, 150, 200), function(n) ssm_power_amplitude(0.15, n)$power)
#> [1] 0.8269350 0.9779786 0.9977153 0.9997915
```

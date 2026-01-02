# Required Sample Size for Two-Group Amplitude Comparison

Calculates sample size per group for comparing amplitudes between groups

## Usage

``` r
ssm_sample_size_amplitude_diff(
  effect,
  power = 0.8,
  alpha = 0.05,
  k = 0.41,
  ratio = 1
)
```

## Arguments

- effect:

  Expected amplitude difference

- power:

  Target power (default: 0.80)

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant (default: 0.41)

- ratio:

  Allocation ratio n2/n1 (default: 1 for balanced)

## Value

An object of class `ssm_sample_size`

## Examples

``` r
# Balanced design
ssm_sample_size_amplitude_diff(effect = 0.16, power = 0.80)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           amplitude_difference (two_sample)
#> Effect size:    0.160
#> Target power:   0.80
#> Alpha:          0.050
#> Test:           two-sided
#> ---------------------------
#> Required n1:    104
#> Required n2:    104
#> Total N:        208
#> Achieved power: 0.8035

# Unbalanced (2:1 ratio)
ssm_sample_size_amplitude_diff(effect = 0.16, power = 0.80, ratio = 2)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           amplitude_difference (two_sample)
#> Effect size:    0.160
#> Target power:   0.80
#> Alpha:          0.050
#> Test:           two-sided
#> ---------------------------
#> Required n1:    78
#> Required n2:    155
#> Total N:        233
#> Achieved power: 0.8026
```

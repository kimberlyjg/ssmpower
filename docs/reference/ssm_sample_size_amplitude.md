# Required Sample Size for Amplitude Test

Calculates sample size needed to achieve target power for amplitude test

## Usage

``` r
ssm_sample_size_amplitude(
  effect,
  power = 0.8,
  alpha = 0.05,
  k = 0.41,
  one_sided = TRUE
)
```

## Arguments

- effect:

  Expected amplitude

- power:

  Target power (default: 0.80)

- alpha:

  Significance level (default: 0.05)

- k:

  Scaling constant (default: 0.41)

- one_sided:

  Use one-sided test? (default: TRUE)

## Value

An object of class `ssm_sample_size` containing:

- n:

  Required sample size (ceiling)

- n_exact:

  Exact calculated sample size

- achieved_power:

  Actual power with ceiling n

- target_power:

  Requested power level

## Examples

``` r
# Sample size for medium effect with 80% power
ssm_sample_size_amplitude(effect = 0.16, power = 0.80)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           amplitude (single_sample)
#> Effect size:    0.160
#> Target power:   0.80
#> Alpha:          0.050
#> Test:           one-sided
#> ---------------------------
#> Required n:     41
#> Achieved power: 0.8034

# Sample size for small effect with 90% power
ssm_sample_size_amplitude(effect = 0.10, power = 0.90)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           amplitude (single_sample)
#> Effect size:    0.100
#> Target power:   0.90
#> Alpha:          0.050
#> Test:           one-sided
#> ---------------------------
#> Required n:     144
#> Achieved power: 0.9001
```

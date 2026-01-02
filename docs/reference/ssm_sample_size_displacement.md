# Sample Size for Displacement Comparison

Calculate required sample size for detecting a displacement difference
in SSM analysis.

## Usage

``` r
ssm_sample_size_displacement(
  delta_diff,
  amplitude,
  power = 0.8,
  alpha = 0.05,
  k_delta = K_DISPLACEMENT,
  two_group = TRUE
)
```

## Arguments

- delta_diff:

  Expected displacement difference in degrees.

- amplitude:

  Expected amplitude (required).

- power:

  Desired power (default: 0.80).

- alpha:

  Significance level (default: 0.05).

- k_delta:

  Scaling constant in degrees (default: 31).

- two_group:

  Logical; TRUE for two-group comparison (default).

## Value

A list containing:

- n:

  Required sample size (per group for two-group designs)

- achieved_power:

  Actual power at recommended n

- target_power:

  Requested power level

- delta_diff:

  Expected displacement difference

- amplitude:

  Expected amplitude

- se:

  Expected standard error at recommended n

## Details

Formula: \\n = 2 \times (k\_\delta/a)^2 \times (z\_\alpha + z\_\beta)^2
/ \Delta\delta^2\\

IMPORTANT: Sample size depends heavily on amplitude. When amplitude is
small, required sample sizes become prohibitively large because
displacement precision degrades rapidly as amplitude approaches zero.

## Examples

``` r
# Typical case: medium amplitude, 30° difference
ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.25)
#> $n
#> [1] 269
#> 
#> $achieved_power
#> [1] 0.801186
#> 
#> $target_power
#> [1] 0.8
#> 
#> $delta_diff
#> [1] 30
#> 
#> $amplitude
#> [1] 0.25
#> 
#> $alpha
#> [1] 0.05
#> 
#> $k_delta
#> [1] 31
#> 
#> $se
#> [1] 10.69204
#> 
#> $two_group
#> [1] TRUE
#> 
#> $type
#> [1] "displacement"
#> 
#> $design
#> [1] "two_sample"
#> 

# Low amplitude requires much larger samples
ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.15)
#> $n
#> [1] 745
#> 
#> $achieved_power
#> [1] 0.8000192
#> 
#> $target_power
#> [1] 0.8
#> 
#> $delta_diff
#> [1] 30
#> 
#> $amplitude
#> [1] 0.15
#> 
#> $alpha
#> [1] 0.05
#> 
#> $k_delta
#> [1] 31
#> 
#> $se
#> [1] 10.70797
#> 
#> $two_group
#> [1] TRUE
#> 
#> $type
#> [1] "displacement"
#> 
#> $design
#> [1] "two_sample"
#> 

# High amplitude is very efficient
ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.40)
#> $n
#> [1] 105
#> 
#> $achieved_power
#> [1] 0.8008949
#> 
#> $target_power
#> [1] 0.8
#> 
#> $delta_diff
#> [1] 30
#> 
#> $amplitude
#> [1] 0.4
#> 
#> $alpha
#> [1] 0.05
#> 
#> $k_delta
#> [1] 31
#> 
#> $se
#> [1] 10.69602
#> 
#> $two_group
#> [1] TRUE
#> 
#> $type
#> [1] "displacement"
#> 
#> $design
#> [1] "two_sample"
#> 
```

# Power for Displacement Comparison

Calculate statistical power for detecting a displacement difference in
SSM analysis.

## Usage

``` r
ssm_power_displacement(
  delta_diff,
  amplitude,
  n,
  alpha = 0.05,
  k_delta = K_DISPLACEMENT,
  two_group = TRUE
)
```

## Arguments

- delta_diff:

  Expected displacement difference in degrees.

- amplitude:

  Expected amplitude (required - precision depends on this).

- n:

  Sample size (per group for two-group designs).

- alpha:

  Significance level (default: 0.05).

- k_delta:

  Scaling constant in degrees (default: 31).

- two_group:

  Logical; TRUE for two-group comparison (default), FALSE for
  single-sample test against a hypothesized value.

## Value

A list containing:

- power:

  Statistical power

- delta_diff:

  Expected displacement difference

- amplitude:

  Expected amplitude

- n:

  Sample size

- se:

  Standard error of displacement (difference)

- alpha:

  Significance level

- k_delta:

  Scaling constant used

- two_group:

  Whether two-group design

- type:

  Parameter type ("displacement")

- design:

  Study design ("two_sample" or "single_sample")

## Details

The key insight is that displacement precision is INVERSELY PROPORTIONAL
to amplitude: \\SE(\delta) = k\_\delta / (a \times \sqrt{n})\\

When amplitude approaches zero, displacement becomes undefined - there
is no peak to locate. This formula was derived using the delta method
(Oehlert, 1992) applied to the arctangent transformation of the Fourier
coefficients.

## Examples

``` r
# Power for detecting 30° difference with amplitude = 0.25, n = 100 per group
ssm_power_displacement(delta_diff = 30, amplitude = 0.25, n = 100)
#> $power
#> [1] 0.4017156
#> 
#> $delta_diff
#> [1] 30
#> 
#> $amplitude
#> [1] 0.25
#> 
#> $n
#> [1] 100
#> 
#> $se
#> [1] 17.53625
#> 
#> $alpha
#> [1] 0.05
#> 
#> $k_delta
#> [1] 31
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

# Low amplitude makes displacement imprecise
ssm_power_displacement(delta_diff = 30, amplitude = 0.10, n = 100)
#> $power
#> [1] 0.1051301
#> 
#> $delta_diff
#> [1] 30
#> 
#> $amplitude
#> [1] 0.1
#> 
#> $n
#> [1] 100
#> 
#> $se
#> [1] 43.84062
#> 
#> $alpha
#> [1] 0.05
#> 
#> $k_delta
#> [1] 31
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

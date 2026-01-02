# Power for Two-Group Displacement Difference

Convenience wrapper for two-group displacement comparisons with unequal
sample sizes.

## Usage

``` r
ssm_power_displacement_diff(
  delta_diff,
  amplitude,
  n1,
  n2 = n1,
  alpha = 0.05,
  k_delta = K_DISPLACEMENT
)
```

## Arguments

- delta_diff:

  Expected displacement difference in degrees.

- amplitude:

  Expected amplitude.

- n1:

  Sample size for group 1.

- n2:

  Sample size for group 2 (default: n1).

- alpha:

  Significance level (default: 0.05).

- k_delta:

  Scaling constant in degrees (default: 31).

## Value

A list containing power and related statistics.

## Examples

``` r
# Unequal group sizes
ssm_power_displacement_diff(delta_diff = 30, amplitude = 0.25, n1 = 100, n2 = 150)
#> $power
#> [1] 0.4658202
#> 
#> $delta_diff
#> [1] 30
#> 
#> $amplitude
#> [1] 0.25
#> 
#> $n1
#> [1] 100
#> 
#> $n2
#> [1] 150
#> 
#> $se
#> [1] 16.00833
#> 
#> $alpha
#> [1] 0.05
#> 
#> $k_delta
#> [1] 31
#> 
#> $type
#> [1] "displacement_difference"
#> 
#> $design
#> [1] "two_sample"
#> 
```

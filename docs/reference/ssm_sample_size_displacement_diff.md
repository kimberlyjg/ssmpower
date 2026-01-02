# Sample Size for Two-Group Displacement Difference

Convenience wrapper with support for unequal allocation.

## Usage

``` r
ssm_sample_size_displacement_diff(
  delta_diff,
  amplitude,
  power = 0.8,
  alpha = 0.05,
  k_delta = K_DISPLACEMENT,
  ratio = 1
)
```

## Arguments

- delta_diff:

  Expected displacement difference in degrees.

- amplitude:

  Expected amplitude.

- power:

  Desired power (default: 0.80).

- alpha:

  Significance level (default: 0.05).

- k_delta:

  Scaling constant in degrees (default: 31).

- ratio:

  Allocation ratio n2/n1 (default: 1 for equal groups).

## Value

A list containing n1, n2, n_total, and achieved_power.

## Examples

``` r
# Equal allocation
ssm_sample_size_displacement_diff(delta_diff = 30, amplitude = 0.25)
#> $n1
#> [1] 269
#> 
#> $n2
#> [1] 269
#> 
#> $n_total
#> [1] 538
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
#> $ratio
#> [1] 1
#> 
#> $type
#> [1] "displacement_difference"
#> 
#> $design
#> [1] "two_sample"
#> 

# 2:1 allocation ratio
ssm_sample_size_displacement_diff(delta_diff = 30, amplitude = 0.25, ratio = 2)
#> $n1
#> [1] 202
#> 
#> $n2
#> [1] 403
#> 
#> $n_total
#> [1] 605
#> 
#> $achieved_power
#> [1] 0.8013471
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
#> $ratio
#> [1] 2
#> 
#> $type
#> [1] "displacement_difference"
#> 
#> $design
#> [1] "two_sample"
#> 
```

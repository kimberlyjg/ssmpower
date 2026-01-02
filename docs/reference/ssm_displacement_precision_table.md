# Displacement Precision Table

Generate a table showing displacement standard errors across
combinations of amplitude and sample size.

## Usage

``` r
ssm_displacement_precision_table(
  amplitudes = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.4),
  sample_sizes = c(50, 100, 200, 500),
  k_delta = K_DISPLACEMENT
)
```

## Arguments

- amplitudes:

  Vector of amplitude values (default: 0.10 to 0.40).

- sample_sizes:

  Vector of sample sizes (default: 50, 100, 200, 500).

- k_delta:

  Scaling constant (default: 31).

## Value

A data frame with SE(displacement) for each combination.

## Examples

``` r
ssm_displacement_precision_table()
#>   Amplitude n = 50 n = 100 n = 200 n = 500
#> 1      0.10   43.8    31.0    21.9    13.9
#> 2      0.15   29.2    20.7    14.6     9.2
#> 3      0.20   21.9    15.5    11.0     6.9
#> 4      0.25   17.5    12.4     8.8     5.5
#> 5      0.30   14.6    10.3     7.3     4.6
#> 6      0.40   11.0     7.8     5.5     3.5
```

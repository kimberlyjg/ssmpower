# Comprehensive SSM Analysis with Bootstrap CIs and Power

Performs full SSM analysis including parameter estimation, bootstrap
confidence intervals, effect size interpretation, and post-hoc power

## Usage

``` r
ssm_analyze(
  external_var,
  octant_scores,
  angles = seq(0, 315, by = 45),
  n_boot = 2000,
  conf_level = 0.95
)
```

## Arguments

- external_var:

  Numeric vector of external variable scores

- octant_scores:

  Matrix or data frame with 8 columns (octant scores)

- angles:

  Vector of octant angles in degrees (default: 0, 45, ..., 315)

- n_boot:

  Number of bootstrap resamples (default: 2000)

- conf_level:

  Confidence level for CIs (default: 0.95)

## Value

An object of class `ssm_analysis` containing:

- elevation:

  Elevation point estimate

- amplitude:

  Amplitude point estimate

- displacement:

  Displacement in degrees

- ci_elevation:

  Bootstrap CI for elevation

- ci_amplitude:

  Bootstrap CI for amplitude

- ci_displacement:

  Bootstrap CI for displacement

- effect_label_amplitude:

  Effect size category for amplitude

- effect_label_elevation:

  Effect size category for elevation

- n:

  Sample size (complete cases)

- correlations:

  Octant correlations

## Details

Missing data are handled by complete-case analysis. A message is printed
if cases are removed, and a warning is issued if the resulting sample
size is below 30.

## Examples

``` r
# Simulate example data
set.seed(42)
n <- 200
external <- rnorm(n)
octants <- matrix(rnorm(n * 8), ncol = 8)
octants[, 1] <- octants[, 1] + 0.3 * external
octants[, 2] <- octants[, 2] + 0.2 * external

result <- ssm_analyze(external, octants, n_boot = 500)
print(result)
#> 
#> ==============================================================
#>                     SSM ANALYSIS RESULTS
#> ==============================================================
#> Sample size: N = 200 | Bootstrap samples: 500
#> --------------------------------------------------------------
#> 
#> PARAMETER ESTIMATES
#> -------------------
#>   Elevation:     0.060  [0.012, 0.111]  (Small)
#>   Amplitude:     0.073  [0.023, 0.152]  (< Small)
#>   Displacement:  351.2 deg [  1.5 deg, 358.8 deg]
#> 
#> OCTANT CORRELATIONS
#> -------------------
#>   PA (  0 deg):  0.223
#>   BC ( 45 deg):  0.133
#>   DE ( 90 deg): -0.061
#>   FG (135 deg):  0.033
#>   HI (180 deg):  0.020
#>   JK (225 deg):  0.038
#>   LM (270 deg):  0.032
#>   NO (315 deg):  0.060
#> 
#> ==============================================================
```

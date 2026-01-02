# Print Sample Size Quick Reference Guide

Displays a formatted table of sample size requirements for common
scenarios

## Usage

``` r
ssm_sample_size_guide(power = 0.8)
```

## Arguments

- power:

  Target power level (default: 0.80)

## Value

Invisibly returns a data frame with the values

## Examples

``` r
ssm_sample_size_guide()
#> 
#> ============================================================
#>          SSM SAMPLE SIZE QUICK REFERENCE GUIDE
#>                   (Power = 80%, alpha = .05)
#> ============================================================
#> 
#> Effect Size Benchmarks (Zimmermann & Wright, 2017)
#> ------------------------------------------------------------
#> Amplitude:  Small = 0.10  |  Medium = 0.16  |  Large = 0.23
#> Elevation:  Small = 0.02  |  Medium = 0.11  |  Large = 0.27
#> 
#> SINGLE-SAMPLE: Test if parameter differs from 0
#> ------------------------------------------------------------
#> Amplitude:  n =  104 (small) | n =  41 (medium) | n =  20 (large)
#> Elevation:  n = 7064 (small) | n = 234 (medium) | n =  39 (large)
#> 
#> TWO-GROUP: Test if parameters differ between groups
#> ------------------------------------------------------------
#> Amplitude:  n =  264/grp (small) | n = 104/grp (med) | n =  50/grp (lg)
#> ============================================================
#> 
ssm_sample_size_guide(power = 0.90)
#> 
#> ============================================================
#>          SSM SAMPLE SIZE QUICK REFERENCE GUIDE
#>                   (Power = 90%, alpha = .05)
#> ============================================================
#> 
#> Effect Size Benchmarks (Zimmermann & Wright, 2017)
#> ------------------------------------------------------------
#> Amplitude:  Small = 0.10  |  Medium = 0.16  |  Large = 0.23
#> Elevation:  Small = 0.02  |  Medium = 0.11  |  Large = 0.27
#> 
#> SINGLE-SAMPLE: Test if parameter differs from 0
#> ------------------------------------------------------------
#> Amplitude:  n =  144 (small) | n =  57 (medium) | n =  28 (large)
#> Elevation:  n = 9457 (small) | n = 313 (medium) | n =  52 (large)
#> 
#> TWO-GROUP: Test if parameters differ between groups
#> ------------------------------------------------------------
#> Amplitude:  n =  354/grp (small) | n = 138/grp (med) | n =  67/grp (lg)
#> ============================================================
#> 
```

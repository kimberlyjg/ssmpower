# ssmpower: Power Analysis for the Structural Summary Method
<!-- badges: start -->
[![R-CMD-check](https://github.com/kimberlyjg/ssmpower/workflows/R-CMD-check/badge.svg)](https://github.com/kimberlyjg/ssmpower/actions)
<!-- badges: end -->

## Overview

**ssmpower** provides power analysis and sample size planning for Structural Summary Method (SSM) analyses of circumplex data. It implements the methods described in:

> Gilbert, K. J. (2025). Powering the circumplex: A practical guide to sample size for the structural summary method.

## Installation
```r
# Install from GitHub (current version)
devtools::install_github("kimberlyjg/ssmpower")

# Or from CRAN (after acceptance)
# install.packages("ssmpower")
```

## Quick Start
```r
library(ssmpower)

# How many participants do I need?
ssm_sample_size_amplitude(effect = 0.16, power = 0.80)
#> n = 41 for 80% power

# What's my power with N = 100?
ssm_power_amplitude(effect = 0.16, n = 100)
#> Power = 0.95

# Print a quick reference guide
ssm_sample_size_guide()

# Launch the interactive web app
run_app()
```

## Interactive Web App

Don't want to install R? Use the **free online calculator**:

🌐 **https://kjgilbert01.shinyapps.io/shiny/**

## Effect Size Benchmarks

Based on Zimmermann & Wright (2017):

| Parameter | Small | Medium | Large |
|-----------|-------|--------|-------|
| Amplitude | 0.10  | 0.16   | 0.23  |
| Elevation | 0.02  | 0.11   | 0.27  |

## Sample Sizes for 80% Power

| Design | Parameter | Small | Medium | Large |
|--------|-----------|-------|--------|-------|
| Single sample | Amplitude | 104 | 41 | 20 |
| Single sample | Elevation | 7064 | 234 | 39 |
| Two groups (per group) | Amplitude | 264 | 104 | 50 |

## NEW: Displacement Power Analysis (Study 2)

Version 1.1.0 adds power analysis for angular displacement (θ). The key insight:

**Displacement precision depends on amplitude:**

$$SE(\delta) = \frac{31°}{a \times \sqrt{n}}$$

When amplitude is low (< 0.15), displacement becomes unreliable regardless of sample size.
```r
# Sample size for 30° displacement difference
ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.25)
#> n = 54 per group

# Precision reference table
ssm_displacement_precision_table()
```

### Sample Sizes for Displacement (80% Power, Two-Group)

| Amplitude | Δδ = 20° | Δδ = 30° | Δδ = 45° |
|-----------|----------|----------|----------|
| 0.20      | 188      | 84       | 38       |
| 0.25      | 120      | 54       | 24       |
| 0.30      | 84       | 38       | 17       |

## Main Functions

### Amplitude & Elevation (Study 1)

| Function | Purpose |
|----------|---------|
| `ssm_power_amplitude()` | Calculate power for amplitude tests |
| `ssm_power_elevation()` | Calculate power for elevation tests |
| `ssm_sample_size_amplitude()` | Determine N for amplitude tests |
| `ssm_sample_size_elevation()` | Determine N for elevation tests |
| `ssm_power_amplitude_diff()` | Power for two-group amplitude comparisons |
| `ssm_sample_size_amplitude_diff()` | Sample size for two-group comparisons |

### Displacement (Study 2)

| Function | Purpose |
|----------|---------|
| `ssm_power_displacement()` | Power for displacement comparisons |
| `ssm_sample_size_displacement()` | Sample size for displacement tests |
| `ssm_displacement_precision_table()` | SE reference by amplitude × n |

### Utilities

| Function | Purpose |
|----------|---------|
| `ssm_analyze()` | Full SSM analysis with bootstrap CIs |
| `ssm_sample_size_guide()` | Print quick reference table |
| `run_app()` | Launch interactive Shiny calculator |

## Citation

If you use this package, please cite:
```
Gilbert, K. J. (2025). Powering the circumplex: A practical guide to sample 
size for the structural summary method.

Gilbert, K. J. (2025). ssmpower: Power Analysis for the Structural Summary 
Method. R package version 1.1.0. https://github.com/kimberlyjg/ssmpower
```

## References

- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation. *Assessment, 24*, 3-23.
- Wright, A. G. C., Pincus, A. L., Conroy, D. E., & Hilsenroth, M. J. (2009). Integrating methods to optimize circumplex description. *Assessment, 16*, 212-224.

## License

MIT
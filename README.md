# ssmpower: Power Analysis for the Structural Summary Method

<!-- badges: start -->
[![R-CMD-check](https://github.com/kimberlyjg/ssmpower/workflows/R-CMD-check/badge.svg)](https://github.com/kimberlyjg/ssmpower/actions)
<!-- badges: end -->

## Overview

**ssmpower** provides power analysis and sample size planning for Structural Summary Method (SSM) analyses of circumplex data. It implements the methods described in:

> Gilbert, K. J. (2026). Power analysis for the Structural Summary Method. *Manuscript in preparation.*

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

## Main Functions

| Function | Purpose |
|----------|---------|
| `ssm_power_amplitude()` | Calculate power for amplitude tests |
| `ssm_power_elevation()` | Calculate power for elevation tests |
| `ssm_sample_size_amplitude()` | Determine N for amplitude tests |
| `ssm_sample_size_elevation()` | Determine N for elevation tests |
| `ssm_power_amplitude_diff()` | Power for two-group amplitude comparisons |
| `ssm_sample_size_amplitude_diff()` | Sample size for two-group comparisons |
| `ssm_analyze()` | Full SSM analysis with bootstrap CIs |
| `ssm_sample_size_guide()` | Print quick reference table |
| `run_app()` | Launch interactive Shiny calculator |

## Citation

If you use this package, please cite:
```
Gilbert, K. J. (2026). Power analysis for the Structural Summary Method. 
Manuscript in preparation.

Gilbert, K. J. (2026). ssmpower: Power Analysis for the Structural Summary 
Method. R package version 1.0.0. https://github.com/kimberlyjg/ssmpower
```

## References

- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation. *Assessment, 24*, 3-23.
- Wright, A. G. C., Pincus, A. L., Conroy, D. E., & Hilsenroth, M. J. (2009). Integrating methods to optimize circumplex description. *Assessment, 16*, 212-224.

## License

MIT

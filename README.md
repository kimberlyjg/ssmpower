# ssmpower

<!-- badges: start -->
<!-- badges: end -->

**ssmpower** provides power analysis and sample size planning tools for Structural Summary Method (SSM) analyses of circumplex data.

## Installation

You can install ssmpower from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("kimberlyjg/ssmpower")
```

## Overview

The package implements closed-form power functions for SSM parameters based on:

- **Zimmermann & Wright (2017)**: Bootstrap methodology and effect size benchmarks
- **Gurtman (1992)**: Foundational SSM methodology
- **Cohen (1988, 1992)**: Power analysis framework

### Effect Size Benchmarks

Based on 433 published SSM analyses (Zimmermann & Wright, 2017):

| Parameter | Small (25th %ile) | Medium (50th %ile) | Large (75th %ile) |
|-----------|-------------------|--------------------|--------------------|
| Amplitude | 0.10 | 0.16 | 0.23 |
| Elevation | 0.02 | 0.11 | 0.27 |

### Scaling Constants

Empirically derived from bootstrap simulations:

- **Amplitude**: k = 0.41
- **Elevation**: k = 0.60

## Quick Start

```r
library(ssmpower)

# How much power do I have?
ssm_power_amplitude(effect = 0.16, n = 100)

# How many participants do I need?
ssm_sample_size_amplitude(effect = 0.16, power = 0.80)

# Quick reference guide
ssm_sample_size_guide()

# Launch interactive Shiny app
run_app()
```

## Interactive Shiny App

The package includes an interactive Shiny web application for power analysis:

```r
# Launch the app
ssmpower::run_app()
```

The app provides:
- **Power Calculator**: Calculate power with real-time visualization
- **Sample Size Calculator**: Determine required N with preset effect sizes
- **Power Tables**: Generate customizable power tables with heatmaps
- **Quick Reference**: Summary of benchmarks, formulas, and references

Requires: shiny, bslib, ggplot2, DT (installed automatically or via `install.packages()`)

## Main Functions

### Power Functions

- `ssm_power_amplitude()` - Power for single-sample amplitude test
- `ssm_power_elevation()` - Power for single-sample elevation test
- `ssm_power_amplitude_diff()` - Power for two-group amplitude comparison
- `ssm_power_elevation_diff()` - Power for two-group elevation comparison

### Sample Size Functions

- `ssm_sample_size_amplitude()` - Required n for amplitude test
- `ssm_sample_size_elevation()` - Required n for elevation test
- `ssm_sample_size_amplitude_diff()` - Required n per group for amplitude comparison
- `ssm_sample_size_elevation_diff()` - Required n per group for elevation comparison

### Analysis Functions

- `ssm_analyze()` - Comprehensive SSM analysis with bootstrap CIs and post-hoc power
- `ssm_parameters()` - Calculate SSM parameters from octant scores

### Utility Functions

- `ssm_ci_amplitude()` - Confidence interval for amplitude
- `ssm_ci_elevation()` - Confidence interval for elevation
- `ssm_effect_label()` - Interpret effect size magnitude
- `ssm_sample_size_guide()` - Print quick reference table
- `ssm_power_table()` - Generate power table

## Example: Full Analysis

```r
# Simulate data
set.seed(42)
n <- 250
external <- rnorm(n)
octants <- matrix(rnorm(n * 8), ncol = 8)
for (i in 1:8) {
  octants[, i] <- octants[, i] + 0.3 * external
}

# Run analysis
result <- ssm_analyze(external, octants, n_boot = 1000)
print(result)
```

## Sample Size Quick Reference (80% Power)

| Test Type | Small | Medium | Large |
|-----------|-------|--------|-------|
| Single-sample amplitude | 104 | 41 | 20 |
| Single-sample elevation | 7064 | 234 | 39 |
| Two-group amplitude | 264/group | 104/group | 50/group |

## Citation

If you use this package, please cite:

Gilbert, K. (2025). *ssmpower: Power Analysis for the Structural Summary Method*. R package version 1.0.0. https://github.com/kimberlyjg/ssmpower

```bibtex
@Manual{,
  title = {ssmpower: Power Analysis for the Structural Summary Method},
  author = {Kimberly Gilbert},
  year = {2025},
  note = {R package version 1.0.0},
  url = {https://github.com/kimberlyjg/ssmpower},
}
```

For the underlying SSM methodology, also cite:

Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation: Methodological advances in the circumplex Structural Summary Approach. *Assessment, 24*, 3-23. https://doi.org/10.1177/1073191116656437

## License

MIT

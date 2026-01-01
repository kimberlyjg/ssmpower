# ssmpower

<!-- badges: start -->
[![R-CMD-check](https://github.com/kimberlyjg/ssmpower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kimberlyjg/ssmpower/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Power Analysis for the Structural Summary Method**

The `ssmpower` package provides tools for power analysis and sample size planning 
for Structural Summary Method (SSM) analyses of circumplex data.

```r
## Shiny App

Try the interactive web app: https://kjgilbert01.shinyapps.io/ssm-power-app/
```

```r
The project associated with this R package is https://osf.io/63wyf/
```

## Installation

```r
# Install from GitHub
remotes::install_github("kimberlyjg/ssmpower")
```

## Quick Start

```r
library(ssmpower)

# How much power do I have?
ssm_power_amplitude(effect = 0.16, n = 100)
#> Power: 0.928

# How many participants do I need?
ssm_sample_size_amplitude(effect = 0.16, power = 0.80)
#> Required n: 41

# Quick reference
ssm_sample_size_guide()
```

## Effect Size Benchmarks

From Zimmermann & Wright (2017), based on 433 published SSM analyses:

| Parameter | Small | Medium | Large |
|-----------|-------|--------|-------|
| Amplitude | 0.10  | 0.16   | 0.23  |
| Elevation | 0.02  | 0.11   | 0.27  |

## Sample Size Requirements (80% Power)

| Analysis Type | Small Effect | Medium Effect | Large Effect |
|---------------|--------------|---------------|--------------|
| Amplitude (single) | 104 | 41 | 20 |
| Elevation (single) | 7,059 | 185 | 31 |
| Amplitude (two-group) | 264/group | 82/group | 40/group |

## Key Functions

### Power Calculation
- `ssm_power_amplitude()` - Single-sample amplitude test
- `ssm_power_elevation()` - Single-sample elevation test  
- `ssm_power_amplitude_diff()` - Two-group amplitude comparison
- `ssm_power_elevation_diff()` - Two-group elevation comparison

### Sample Size Planning
- `ssm_sample_size_amplitude()` - Required n for amplitude
- `ssm_sample_size_elevation()` - Required n for elevation
- `ssm_sample_size_amplitude_diff()` - Required n per group
- `ssm_sample_size_guide()` - Quick reference table

### Analysis
- `ssm_parameters()` - Calculate SSM parameters from octants
- `ssm_analyze()` - Full analysis with bootstrap CIs

## Technical Details

The power functions use empirically-derived scaling constants:
- **Amplitude:** k = 0.41
- **Elevation:** k = 0.60

These allow SE estimation as: `SE = k / sqrt(n)`

### Important Notes from Simulation Validation

Monte Carlo validation (5,000 simulations per scenario, following Morris et al., 2019 
ADEMP framework) revealed:

**Power estimates:**
- The power formula is slightly **conservative** for small and medium effects 
  (empirical power ≈ 2-17% higher than predicted)
- For large effects at small n, predictions are very accurate
- **Sample sizes from this package will yield at least the targeted power**

**Confidence intervals:**
- Normal-approximation 95% CIs achieve approximately **90% coverage** (not 95%)
- This occurs because: (1) amplitude estimates have positive bias, and (2) 
  model SE slightly underestimates empirical SE
- For critical applications, consider using `ssm_analyze()` with bootstrap CIs

See the [simulation validation vignette](vignettes/simulation-validation.Rmd) for 
full Monte Carlo verification.

## Citation

If you use this package, please cite:

> Gilbert, K. J. (2025). ssmpower: Power Analysis for the SSM. R package version 1.0.0, https://kimberlyjg/ssmpower.

@Manual{,
  title = {ssmpower: Power Analysis for the SSM},
  author = {Kimerly Gilbert},
  year = {2025},
  note = {R package version 1.0.0},
  url = {https://kimberlyjg/ssmpower},
}

## References

- Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Lawrence Erlbaum.
- Gurtman, M. B. (1992). Construct validity of interpersonal personality measures. *JPSP, 63*, 105-118.
- Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation studies to evaluate statistical methods. *Statistics in Medicine, 38*, 2074-2102.
- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation. *Assessment, 24*, 3-23.

## License

MIT

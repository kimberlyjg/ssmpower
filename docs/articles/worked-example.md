# Worked Example: Personality and Interpersonal Style

``` r
library(ssmpower)
```

## Overview

This vignette walks through a complete SSM power analysis workflow using
a realistic research scenario. We’ll cover:

1.  A priori power analysis for study planning
2.  Analyzing collected data
3.  Interpreting results
4.  Reporting results

## Research Scenario

**Study**: Examining how narcissistic personality traits relate to
interpersonal style.

**Hypothesis**: Higher narcissism scores are associated with a
distinctive interpersonal profile characterized by dominant and cold
interpersonal behaviors.

**Measure**: Participants complete the Inventory of Interpersonal
Problems - Short Circumplex (IIP-SC), yielding 8 octant scores:

- **PA** (0°): Domineering/Controlling
- **BC** (45°): Vindictive/Self-Centered
- **DE** (90°): Cold/Distant
- **FG** (135°): Socially Inhibited
- **HI** (180°): Nonassertive
- **JK** (225°): Overly Accommodating
- **LM** (270°): Self-Sacrificing
- **NO** (315°): Intrusive/Needy

## Part 1: A Priori Power Analysis

Before collecting data, we need to determine our required sample size.

### Step 1: Determine Expected Effect Size

Based on the literature, narcissism typically shows medium-to-large
amplitude correlations with interpersonal problems (around 0.20).

``` r
# Check what this effect size means
ssm_effect_label(0.20, "amplitude")
#> [1] "Medium-Large"
```

### Step 2: Calculate Required Sample Size

``` r
# For 80% power
result_80 <- ssm_sample_size_amplitude(effect = 0.20, power = 0.80)
print(result_80)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           amplitude (single_sample)
#> Effect size:    0.200
#> Target power:   0.80
#> Alpha:          0.050
#> Test:           one-sided
#> ---------------------------
#> Required n:     26
#> Achieved power: 0.8002

# For 90% power (more conservative)
result_90 <- ssm_sample_size_amplitude(effect = 0.20, power = 0.90)
print(result_90)
#> 
#> SSM Sample Size Calculation
#> ===========================
#> Type:           amplitude (single_sample)
#> Effect size:    0.200
#> Target power:   0.90
#> Alpha:          0.050
#> Test:           one-sided
#> ---------------------------
#> Required n:     36
#> Achieved power: 0.9001
```

### Step 3: Sensitivity Analysis

What if our effect is smaller than expected?

``` r
cat("Sensitivity Analysis: Required N at Different Effect Sizes\n")
#> Sensitivity Analysis: Required N at Different Effect Sizes
cat("===========================================================\n")
#> ===========================================================
cat("(80% power, one-sided alpha = .05)\n\n")
#> (80% power, one-sided alpha = .05)

effects <- c(0.15, 0.18, 0.20, 0.22, 0.25)
for (e in effects) {
  n <- ssm_sample_size_amplitude(e, power = 0.80)$n
  label <- ssm_effect_label(e, "amplitude")
  cat(sprintf("  Effect = %.2f (%s): n = %d\n", e, label, n))
}
#>   Effect = 0.15 (Small-Medium): n = 47
#>   Effect = 0.18 (Medium): n = 33
#>   Effect = 0.20 (Medium-Large): n = 26
#>   Effect = 0.22 (Medium-Large): n = 22
#>   Effect = 0.25 (Large): n = 17
```

### Decision

We’ll aim for **N = 80** participants, which provides: - 80% power for
our expected effect (0.20) - Adequate power (\>70%) even if the true
effect is 0.18

``` r
# Verify power with N = 80
ssm_power_amplitude(effect = 0.20, n = 80)
#> 
#> SSM Power Analysis
#> ==================
#> Type:        amplitude (single_sample)
#> Effect size: 0.200
#> Sample size: 80
#> Alpha:       0.050
#> Test:        one-sided
#> ------------------
#> Power:       0.997
ssm_power_amplitude(effect = 0.18, n = 80)
#> 
#> SSM Power Analysis
#> ==================
#> Type:        amplitude (single_sample)
#> Effect size: 0.180
#> Sample size: 80
#> Alpha:       0.050
#> Test:        one-sided
#> ------------------
#> Power:       0.989
```

## Part 2: Data Analysis

Now we simulate data collection and analysis. (In practice, this would
be real data.)

### Simulated Data

``` r
# Simulate realistic data
set.seed(2025)
n <- 80

# Narcissism scores (standardized)
narcissism <- rnorm(n, mean = 0, sd = 1)

# IIP-SC octant scores
# Narcissism associated with high PA/BC (dominant, vindictive) and low HI/JK (assertive)
angles <- seq(0, 315, by = 45)
angles_rad <- angles * pi / 180

# Create octant scores with realistic properties
octant_scores <- matrix(NA, nrow = n, ncol = 8)
colnames(octant_scores) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

for (i in 1:8) {
  # Base score plus relationship with narcissism
  # Peak relationship at 45 degrees (BC - vindictive)
  loading <- 0.25 * cos(angles_rad[i] - pi/4)  # Peak at 45 degrees
  elevation_loading <- 0.15  # General interpersonal problems
  
  octant_scores[, i] <- rnorm(n, mean = 1.5, sd = 0.8) + 
                        elevation_loading * narcissism +
                        loading * narcissism
}

# Preview data
head(data.frame(narcissism = round(narcissism, 2), octant_scores[, 1:4]))
#>   narcissism        PA         BC        DE       FG
#> 1       0.62 0.8061344 1.97213891 1.7890396 2.445794
#> 2       0.04 2.6876812 3.04006014 1.1868220 1.980348
#> 3       0.77 1.9168542 1.73092175 2.9535291 1.563848
#> 4       1.27 1.0668263 1.93213404 1.7050819 1.280360
#> 5       0.37 1.6204463 0.95075381 0.9500971 2.065012
#> 6      -0.16 1.4272253 0.06894019 1.1064403 1.439768
```

### Run SSM Analysis

``` r
# Comprehensive analysis
results <- ssm_analyze(
  external_var = narcissism,
  octant_scores = octant_scores,
  n_boot = 2000,
  conf_level = 0.95
)

print(results)
#> 
#> ==============================================================
#>                     SSM ANALYSIS RESULTS
#> ==============================================================
#> Sample size: N = 80 | Bootstrap samples: 2000
#> --------------------------------------------------------------
#> 
#> PARAMETER ESTIMATES
#> -------------------
#>   Elevation:     0.183  [0.121, 0.238]  (Medium)
#>   Amplitude:     0.353  [0.248, 0.477]  (Large)
#>   Displacement:   51.0 deg [ 33.8 deg,  75.4 deg]
#> 
#> OCTANT CORRELATIONS
#> -------------------
#>   PA (  0 deg):  0.495
#>   BC ( 45 deg):  0.513
#>   DE ( 90 deg):  0.336
#>   FG (135 deg):  0.324
#>   HI (180 deg): -0.028
#>   JK (225 deg): -0.176
#>   LM (270 deg): -0.154
#>   NO (315 deg):  0.152
#> 
#> ==============================================================
```

### Interpretation

``` r
cat("INTERPRETATION\n")
#> INTERPRETATION
cat("==============\n\n")
#> ==============

cat(sprintf("1. ELEVATION: %.3f [%.3f, %.3f]\n", 
            results$elevation, 
            results$ci_elevation[1], 
            results$ci_elevation[2]))
#> 1. ELEVATION: 0.183 [0.121, 0.238]
cat(sprintf("   Effect size: %s\n", results$effect_label_elevation))
#>    Effect size: Medium
cat("   Interpretation: Higher narcissism is associated with generally\n")
#>    Interpretation: Higher narcissism is associated with generally
cat("   elevated interpersonal problems across all octants.\n\n")
#>    elevated interpersonal problems across all octants.

cat(sprintf("2. AMPLITUDE: %.3f [%.3f, %.3f]\n", 
            results$amplitude, 
            results$ci_amplitude[1], 
            results$ci_amplitude[2]))
#> 2. AMPLITUDE: 0.353 [0.248, 0.477]
cat(sprintf("   Effect size: %s\n", results$effect_label_amplitude))
#>    Effect size: Large
cat("   Interpretation: The profile shows distinctive interpersonal\n")
#>    Interpretation: The profile shows distinctive interpersonal
cat("   differentiation - narcissism relates more to some octants than others.\n\n")
#>    differentiation - narcissism relates more to some octants than others.

cat(sprintf("3. DISPLACEMENT: %.1f degrees [%.1f, %.1f]\n", 
            results$displacement, 
            results$ci_displacement[1], 
            results$ci_displacement[2]))
#> 3. DISPLACEMENT: 51.0 degrees [33.8, 75.4]
cat("   Interpretation: The peak of the profile is in the BC octant\n")
#>    Interpretation: The peak of the profile is in the BC octant
cat("   (Vindictive/Self-Centered), indicating narcissism is most strongly\n")
#>    (Vindictive/Self-Centered), indicating narcissism is most strongly
cat("   associated with dominant and cold interpersonal problems.\n")
#>    associated with dominant and cold interpersonal problems.
```

## Part 3: Reporting Results

### For a Methods Section

    #> Sample size was determined a priori using the ssmpower package
    #> (Gilbert, 2025). Based on an expected medium amplitude effect of
    #> 0.20, we required N = 52 for 80% power (one-sided alpha = .05).
    #> We recruited N = 80 to account for potential attrition and to
    #> provide adequate power for secondary analyses.

### For a Results Section

    #> Structural Summary Method analysis revealed that narcissism was
    #> associated with a distinctive interpersonal profile. The profile
    #> showed significant elevation (e = 0.18, 95% CI [0.12, 0.24]),
    #> indicating that higher narcissism was associated with greater
    #> overall interpersonal problems. The profile amplitude was 0.35
    #> (95% CI [0.25, 0.48]), representing a large effect and indicating
    #> interpersonal differentiation. The profile peaked at 51 degrees
    #> (95% CI [34, 75]), in the BC (Vindictive/Self-Centered) octant.

### Reporting Table

``` r
# Create results table
results_table <- data.frame(
  Parameter = c("Elevation", "Amplitude", "Displacement"),
  Estimate = c(
    sprintf("%.3f", results$elevation),
    sprintf("%.3f", results$amplitude),
    sprintf("%.1f", results$displacement)
  ),
  CI_95 = c(
    sprintf("[%.3f, %.3f]", results$ci_elevation[1], results$ci_elevation[2]),
    sprintf("[%.3f, %.3f]", results$ci_amplitude[1], results$ci_amplitude[2]),
    sprintf("[%.1f, %.1f]", results$ci_displacement[1], results$ci_displacement[2])
  ),
  Effect_Size = c(
    results$effect_label_elevation,
    results$effect_label_amplitude,
    "N/A"
  )
)

knitr::kable(results_table, 
             col.names = c("Parameter", "Estimate", "95% CI", "Effect Size"),
             caption = "SSM Profile of Narcissism and Interpersonal Problems")
```

| Parameter    | Estimate | 95% CI           | Effect Size |
|:-------------|:---------|:-----------------|:------------|
| Elevation    | 0.183    | \[0.121, 0.238\] | Medium      |
| Amplitude    | 0.353    | \[0.248, 0.477\] | Large       |
| Displacement | 51.0     | \[33.8, 75.4\]   | N/A         |

SSM Profile of Narcissism and Interpersonal Problems

## Summary Workflow

1.  **Before data collection**:
    - Use
      [`ssm_sample_size_amplitude()`](https://kimberlyjg.github.io/ssmpower/reference/ssm_sample_size_amplitude.md)
      to determine required N
    - Conduct sensitivity analysis for range of plausible effects
2.  **After data collection**:
    - Use
      [`ssm_analyze()`](https://kimberlyjg.github.io/ssmpower/reference/ssm_analyze.md)
      for comprehensive analysis
    - Bootstrap CIs provide inference for all parameters
3.  **Reporting**:
    - Report a priori power analysis in Methods
    - Report SSM parameters with CIs and effect size labels in Results

## Citation

To cite this package:

Gilbert, K. (2025). *ssmpower: Power Analysis for the Structural Summary
Method*. R package version 1.0.0.
<https://github.com/kimberlyjg/ssmpower>

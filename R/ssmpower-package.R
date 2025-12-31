#' ssmpower: Power Analysis for the Structural Summary Method
#'
#' The ssmpower package provides tools for power analysis and sample size
#' planning for Structural Summary Method (SSM) analyses of circumplex data.
#'
#' @section Main Functions:
#'
#' **Power Calculation:**
#' - [ssm_power_amplitude()]: Power for single-sample amplitude test
#' - [ssm_power_elevation()]: Power for single-sample elevation test
#' - [ssm_power_amplitude_diff()]: Power for two-group amplitude comparison
#' - [ssm_power_elevation_diff()]: Power for two-group elevation comparison
#'
#' **Sample Size Planning:**
#' - [ssm_sample_size_amplitude()]: Required n for amplitude test
#' - [ssm_sample_size_elevation()]: Required n for elevation test
#' - [ssm_sample_size_amplitude_diff()]: Required n per group for amplitude comparison
#' - [ssm_sample_size_elevation_diff()]: Required n per group for elevation comparison
#' - [ssm_sample_size_guide()]: Quick reference table
#'
#' **Analysis:**
#' - [ssm_parameters()]: Calculate SSM parameters from octant scores
#' - [ssm_analyze()]: Full analysis with bootstrap CIs and power
#'
#' **Utilities:**
#' - [ssm_ci_amplitude()], [ssm_ci_elevation()]: Confidence intervals
#' - [ssm_effect_label()]: Interpret effect sizes
#' - [ssm_power_table()]: Generate power tables
#'
#' @section Scaling Constants:
#' The package uses empirically-derived scaling constants for standard errors:
#' - Amplitude: k = 0.41
#' - Elevation: k = 0.60
#'
#' These were derived from bootstrap simulations with N = 1,101 participants.
#'
#' @section Effect Size Benchmarks:
#' From Zimmermann & Wright (2017), based on 433 published SSM analyses:
#'
#' **Amplitude:**
#' - Small (25th percentile): 0.10
#' - Medium (50th percentile): 0.16
#' - Large (75th percentile): 0.23
#'
#' **Elevation:**
#' - Small (25th percentile): 0.02
#' - Medium (50th percentile): 0.11
#' - Large (75th percentile): 0.27
#'
#' @references
#' Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
#' interpersonal construct validation: Methodological advances in the
#' circumplex Structural Summary Approach. *Assessment, 24*, 3-23.
#' \doi{10.1177/1073191116656441}
#'
#' Gurtman, M. B. (1992). Construct validity of interpersonal personality
#' measures: The interpersonal circumplex as a nomological net.
#' *Journal of Personality and Social Psychology, 63*, 105-118.
#' \doi{10.1037/0022-3514.63.1.105}
#'
#' Cohen, J. (1988). *Statistical power analysis for the behavioral sciences*
#' (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @docType package
#' @name ssmpower-package
#' @aliases ssmpower
"_PACKAGE"

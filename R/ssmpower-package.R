#' ssmpower: Power Analysis for the Structural Summary Method
#'
#' The ssmpower package provides power analysis and sample size planning tools
#' for Structural Summary Method (SSM) analyses of circumplex data.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{ssm_power_amplitude}}}{Power for single-sample amplitude test}
#'   \item{\code{\link{ssm_power_elevation}}}{Power for single-sample elevation test}
#'   \item{\code{\link{ssm_power_amplitude_diff}}}{Power for two-group amplitude comparison}
#'   \item{\code{\link{ssm_power_elevation_diff}}}{Power for two-group elevation comparison}
#'   \item{\code{\link{ssm_sample_size_amplitude}}}{Sample size for amplitude test}
#'   \item{\code{\link{ssm_sample_size_elevation}}}{Sample size for elevation test}
#'   \item{\code{\link{ssm_sample_size_amplitude_diff}}}{Sample size for amplitude comparison}
#'   \item{\code{\link{ssm_sample_size_elevation_diff}}}{Sample size for elevation comparison}
#'   \item{\code{\link{ssm_analyze}}}{Comprehensive SSM analysis with bootstrap CIs}
#' }
#'
#' @section Utility Functions:
#' \describe{
#'   \item{\code{\link{ssm_parameters}}}{Calculate SSM parameters from octant scores}
#'   \item{\code{\link{ssm_ci_amplitude}}}{Confidence interval for amplitude}
#'   \item{\code{\link{ssm_ci_elevation}}}{Confidence interval for elevation}
#'   \item{\code{\link{ssm_effect_label}}}{Interpret effect size magnitude}
#'   \item{\code{\link{ssm_sample_size_guide}}}{Print quick reference guide}
#'   \item{\code{\link{ssm_power_table}}}{Generate power table}
#'   \item{\code{\link{run_app}}}{Launch interactive Shiny application}
#' }
#'
#' @section Effect Size Benchmarks:
#' Based on Zimmermann & Wright (2017), from 433 published SSM analyses:
#' \itemize{
#'   \item Amplitude: Small = 0.10, Medium = 0.16, Large = 0.23
#'   \item Elevation: Small = 0.02, Medium = 0.11, Large = 0.27
#' }
#'
#' @section Scaling Constants:
#' Empirically derived from bootstrap simulations:
#' \itemize{
#'   \item Amplitude: k = 0.41
#'   \item Elevation: k = 0.60
#' }
#'
#' @references
#' Cohen, J. (1988). \emph{Statistical power analysis for the behavioral
#' sciences} (2nd ed.). Lawrence Erlbaum.
#'
#' Gurtman, M. B. (1992). Construct validity of interpersonal personality
#' measures: The interpersonal circumplex as a nomological net.
#' \emph{Journal of Personality and Social Psychology, 63}, 105-118.
#'
#' Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
#' interpersonal construct validation: Methodological advances in the
#' circumplex Structural Summary Approach. \emph{Assessment, 24}, 3-23.
#' \doi{10.1177/1073191116656437}
#'
#' @docType package
#' @name ssmpower-package
#' @aliases ssmpower
"_PACKAGE"

#' Power for Single-Sample Amplitude Test
#'
#' Calculates statistical power for testing H0: amplitude = 0 vs H1: amplitude > 0.
#' Uses a normal approximation with empirically-derived scaling constant.
#'
#' @param effect Expected amplitude (effect size). Must be positive.
#' @param n Sample size. Must be positive.
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant for standard error. Default is 0.41, empirically
#'   derived from bootstrap simulations.
#' @param one_sided Logical; use one-sided test? Default is TRUE, which is

#'   appropriate for amplitude since it is bounded at 0.
#'
#' @return An object of class `ssm_power` containing:
#' \describe{
#'   \item{power}{Calculated statistical power}
#'   \item{effect}{Expected effect size}
#'   \item{n}{Sample size}
#'   \item{alpha}{Significance level}
#'   \item{k}{Scaling constant used}
#'   \item{se}{Standard error}
#'   \item{one_sided}{Whether one-sided test was used}
#'   \item{type}{Type of analysis ("amplitude")}
#'   \item{design}{Study design ("single_sample")}
#' }
#'
#' @details
#' Power is calculated as:
#' \deqn{Power = \Phi\left(\frac{a\sqrt{n}}{k} - z_{1-\alpha}\right)}
#'
#' where \eqn{a} is the expected amplitude, \eqn{n} is sample size,
#' \eqn{k = 0.41} is the scaling constant, and \eqn{\Phi} is the standard
#' normal CDF.
#'
#' @references
#' Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
#' interpersonal construct validation: Methodological advances in the
#' circumplex Structural Summary Approach. *Assessment, 24*, 3-23.
#'
#' @seealso [ssm_sample_size_amplitude()] for sample size calculation,
#'   [ssm_power_amplitude_diff()] for two-group comparisons
#'
#' @examples
#' # Power to detect medium amplitude with n = 100
#' ssm_power_amplitude(effect = 0.16, n = 100)
#'
#' # Power curve across sample sizes
#' sapply(c(50, 100, 150, 200), function(n) ssm_power_amplitude(0.15, n)$power)
#'
#' @export
ssm_power_amplitude <- function(effect, n, alpha = 0.05, k = 0.41, one_sided = TRUE) {

 # Input validation
 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }
 if (!is.numeric(n) || length(n) != 1 || n <= 0) {
   stop("n must be a single positive number")
 }
 if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
   stop("alpha must be a single number between 0 and 1")
 }

 # Calculate SE and z-scores
 se <- k / sqrt(n)
 z_effect <- effect / se

 if (one_sided) {
   z_crit <- stats::qnorm(1 - alpha)
   power <- stats::pnorm(z_effect - z_crit)
 } else {
   z_crit <- stats::qnorm(1 - alpha / 2)
   power <- stats::pnorm(z_effect - z_crit) + stats::pnorm(-z_effect - z_crit)
 }

 # Return structured object
 result <- list(
   power = power,
   effect = effect,
   n = n,
   alpha = alpha,
   k = k,
   se = se,
   one_sided = one_sided,
   type = "amplitude",
   design = "single_sample"
 )
 class(result) <- "ssm_power"
 return(result)
}


#' Power for Two-Group Amplitude Difference
#'
#' Calculates power for comparing amplitudes between two independent groups.
#'
#' @param effect Expected amplitude difference between groups. Must be positive.
#' @param n1 Sample size for group 1.
#' @param n2 Sample size for group 2. Default is same as n1 (balanced design).
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant. Default is 0.41.
#'
#' @return An object of class `ssm_power` with design = "two_sample".
#'
#' @details
#' The standard error for the difference is:
#' \deqn{SE_{diff} = k\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}}
#'
#' A two-sided test is always used for group comparisons.
#'
#' @seealso [ssm_sample_size_amplitude_diff()] for sample size calculation
#'
#' @examples
#' # Power to detect medium difference with 100 per group
#' ssm_power_amplitude_diff(effect = 0.16, n1 = 100, n2 = 100)
#'
#' # Unbalanced design
#' ssm_power_amplitude_diff(effect = 0.16, n1 = 150, n2 = 75)
#'
#' @export
ssm_power_amplitude_diff <- function(effect, n1, n2 = n1, alpha = 0.05, k = 0.41) {

 # Input validation
 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }
 if (!is.numeric(n1) || length(n1) != 1 || n1 <= 0) {
   stop("n1 must be a single positive number")
 }
 if (!is.numeric(n2) || length(n2) != 1 || n2 <= 0) {
   stop("n2 must be a single positive number")
 }

 # SE for difference between independent groups
 se_diff <- k * sqrt(1 / n1 + 1 / n2)
 z_effect <- effect / se_diff
 z_crit <- stats::qnorm(1 - alpha / 2)  # Two-sided for group comparison

 power <- stats::pnorm(z_effect - z_crit) + stats::pnorm(-z_effect - z_crit)

 result <- list(
   power = power,
   effect = effect,
   n1 = as.integer(n1),
   n2 = as.integer(n2),
   alpha = alpha,
   k = k,
   se = se_diff,
   one_sided = FALSE,
   type = "amplitude_difference",
   design = "two_sample"
 )
 class(result) <- "ssm_power"
 return(result)
}


#' Power for Single-Sample Elevation Test
#'
#' Calculates power for testing H0: elevation = 0 vs H1: elevation != 0.
#'
#' @param effect Expected absolute elevation. Must be positive.
#' @param n Sample size.
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant for elevation SE. Default is 0.60, empirically
#'   derived from bootstrap simulations.
#' @param one_sided Logical; use one-sided test? Default is FALSE since
#'   elevation can be positive or negative.
#'
#' @return An object of class `ssm_power`.
#'
#' @details
#' Elevation uses a larger scaling constant (k = 0.60) than amplitude (k = 0.41),
#' meaning elevation estimates have higher variance and require larger samples.
#'
#' @examples
#' # Power to detect medium elevation with n = 200
#' ssm_power_elevation(effect = 0.11, n = 200)
#'
#' @export
ssm_power_elevation <- function(effect, n, alpha = 0.05, k = 0.60, one_sided = FALSE) {

 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }
 if (!is.numeric(n) || length(n) != 1 || n <= 0) {
   stop("n must be a single positive number")
 }

 se <- k / sqrt(n)
 z_effect <- effect / se

 if (one_sided) {
   z_crit <- stats::qnorm(1 - alpha)
   power <- stats::pnorm(z_effect - z_crit)
 } else {
   z_crit <- stats::qnorm(1 - alpha / 2)
   power <- stats::pnorm(z_effect - z_crit) + stats::pnorm(-z_effect - z_crit)
 }

 result <- list(
   power = power,
   effect = effect,
   n = n,
   alpha = alpha,
   k = k,
   se = se,
   one_sided = one_sided,
   type = "elevation",
   design = "single_sample"
 )
 class(result) <- "ssm_power"
 return(result)
}


#' Power for Two-Group Elevation Difference
#'
#' Calculates power for comparing elevations between two independent groups.
#'
#' @param effect Expected elevation difference between groups.
#' @param n1 Sample size for group 1.
#' @param n2 Sample size for group 2. Default is same as n1.
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant. Default is 0.60.
#'
#' @return An object of class `ssm_power` with design = "two_sample".
#'
#' @examples
#' ssm_power_elevation_diff(effect = 0.11, n1 = 200, n2 = 200)
#'
#' @export
ssm_power_elevation_diff <- function(effect, n1, n2 = n1, alpha = 0.05, k = 0.60) {

 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }

 se_diff <- k * sqrt(1 / n1 + 1 / n2)
 z_effect <- effect / se_diff
 z_crit <- stats::qnorm(1 - alpha / 2)

 power <- stats::pnorm(z_effect - z_crit) + stats::pnorm(-z_effect - z_crit)

 result <- list(
   power = power,
   effect = effect,
   n1 = as.integer(n1),
   n2 = as.integer(n2),
   alpha = alpha,
   k = k,
   se = se_diff,
   one_sided = FALSE,
   type = "elevation_difference",
   design = "two_sample"
 )
 class(result) <- "ssm_power"
 return(result)
}


#' @export
print.ssm_power <- function(x, ...) {
 cat("\nSSM Power Analysis\n")
 cat("==================\n")
 cat(sprintf("Type:        %s (%s)\n", x$type, x$design))
 cat(sprintf("Effect size: %.3f\n", x$effect))
 if (x$design == "two_sample") {
   cat(sprintf("Sample size: n1 = %d, n2 = %d\n", x$n1, x$n2))
 } else {
   cat(sprintf("Sample size: %d\n", x$n))
 }
 cat(sprintf("Alpha:       %.3f\n", x$alpha))
 cat(sprintf("Test:        %s\n", ifelse(x$one_sided, "one-sided", "two-sided")))
 cat("------------------\n")
 cat(sprintf("Power:       %.3f\n", x$power))
 invisible(x)
}

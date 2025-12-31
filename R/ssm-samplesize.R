#' Required Sample Size for Amplitude Test
#'
#' Calculates sample size needed to achieve target power for a single-sample
#' amplitude test (H0: amplitude = 0).
#'
#' @param effect Expected amplitude. Must be positive.
#' @param power Target power. Default is 0.80.
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant. Default is 0.41.
#' @param one_sided Logical; use one-sided test? Default is TRUE.
#'
#' @return An object of class `ssm_sample_size` containing:
#' \describe{
#'   \item{n}{Required sample size (ceiling of exact value)}
#'   \item{n_exact}{Exact calculated sample size}
#'   \item{achieved_power}{Actual power achieved with ceiling n}
#'   \item{target_power}{Requested power level}
#'   \item{effect}{Expected effect size}
#'   \item{alpha}{Significance level}
#'   \item{k}{Scaling constant used}
#'   \item{one_sided}{Whether one-sided test was used}
#'   \item{type}{Type of analysis}
#'   \item{design}{Study design}
#' }
#'
#' @details
#' Sample size is calculated as:
#' \deqn{n = \left[\frac{k(z_{1-\alpha} + z_{1-\beta})}{a}\right]^2}
#'
#' where \eqn{a} is the expected amplitude, \eqn{k} is the scaling constant,
#' and \eqn{\beta = 1 - power}.
#'
#' @seealso [ssm_power_amplitude()] for power calculation,
#'   [ssm_sample_size_guide()] for quick reference table
#'
#' @examples
#' # Sample size for medium effect with 80% power
#' ssm_sample_size_amplitude(effect = 0.16, power = 0.80)
#'
#' # Sample size for small effect with 90% power
#' ssm_sample_size_amplitude(effect = 0.10, power = 0.90)
#'
#' @export
ssm_sample_size_amplitude <- function(effect, power = 0.80, alpha = 0.05,
                                     k = 0.41, one_sided = TRUE) {

 # Input validation
 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }
 if (!is.numeric(power) || length(power) != 1 || power <= 0 || power >= 1) {
   stop("power must be a single number between 0 and 1")
 }

 # Calculate required n
 z_beta <- stats::qnorm(power)
 if (one_sided) {
   z_alpha <- stats::qnorm(1 - alpha)
 } else {
   z_alpha <- stats::qnorm(1 - alpha / 2)
 }

 n_exact <- (k * (z_alpha + z_beta) / effect)^2
 n_ceiling <- ceiling(n_exact)

 # Verify achieved power
 achieved_power <- ssm_power_amplitude(effect, n_ceiling, alpha, k, one_sided)$power

 result <- list(
   n = n_ceiling,
   n_exact = n_exact,
   achieved_power = achieved_power,
   target_power = power,
   effect = effect,
   alpha = alpha,
   k = k,
   one_sided = one_sided,
   type = "amplitude",
   design = "single_sample"
 )
 class(result) <- "ssm_sample_size"
 return(result)
}


#' Required Sample Size for Elevation Test
#'
#' Calculates sample size needed to achieve target power for a single-sample
#' elevation test (H0: elevation = 0).
#'
#' @param effect Expected absolute elevation. Must be positive.
#' @param power Target power. Default is 0.80.
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant. Default is 0.60.
#' @param one_sided Logical; use one-sided test? Default is FALSE.
#'
#' @return An object of class `ssm_sample_size`.
#'
#' @details
#' Note that elevation uses a larger scaling constant (k = 0.60) than
#' amplitude (k = 0.41). This means detecting small elevations requires
#' substantially larger samples than detecting small amplitudes.
#'
#' @examples
#' # Sample size for medium elevation effect
#' ssm_sample_size_elevation(effect = 0.11, power = 0.80)
#'
#' # Small elevation effects require very large samples
#' ssm_sample_size_elevation(effect = 0.02, power = 0.80)
#'
#' @export
ssm_sample_size_elevation <- function(effect, power = 0.80, alpha = 0.05,
                                     k = 0.60, one_sided = FALSE) {

 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }
 if (!is.numeric(power) || length(power) != 1 || power <= 0 || power >= 1) {
   stop("power must be a single number between 0 and 1")
 }

 z_beta <- stats::qnorm(power)
 if (one_sided) {
   z_alpha <- stats::qnorm(1 - alpha)
 } else {
   z_alpha <- stats::qnorm(1 - alpha / 2)
 }

 n_exact <- (k * (z_alpha + z_beta) / effect)^2
 n_ceiling <- ceiling(n_exact)
 achieved_power <- ssm_power_elevation(effect, n_ceiling, alpha, k, one_sided)$power

 result <- list(
   n = n_ceiling,
   n_exact = n_exact,
   achieved_power = achieved_power,
   target_power = power,
   effect = effect,
   alpha = alpha,
   k = k,
   one_sided = one_sided,
   type = "elevation",
   design = "single_sample"
 )
 class(result) <- "ssm_sample_size"
 return(result)
}


#' Required Sample Size for Two-Group Amplitude Comparison
#'
#' Calculates sample size needed per group to detect a difference in
#' amplitudes between two independent groups.
#'
#' @param effect Expected amplitude difference between groups.
#' @param power Target power. Default is 0.80.
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant. Default is 0.41.
#' @param ratio Allocation ratio n2/n1. Default is 1 for balanced design.
#'
#' @return An object of class `ssm_sample_size` with additional elements
#'   `n1`, `n2`, and `n_total`.
#'
#' @details
#' For unequal allocation with ratio \eqn{r = n_2/n_1}:
#' \deqn{n_1 = \frac{k^2(z_{\alpha/2} + z_{1-\beta})^2(1 + 1/r)}{effect^2}}
#'
#' Balanced designs (ratio = 1) are most efficient. Very unbalanced designs
#' require substantially more total participants.
#'
#' @examples
#' # Balanced design
#' ssm_sample_size_amplitude_diff(effect = 0.16, power = 0.80)
#'
#' # Unbalanced (2:1 ratio)
#' ssm_sample_size_amplitude_diff(effect = 0.16, power = 0.80, ratio = 2)
#'
#' @export
ssm_sample_size_amplitude_diff <- function(effect, power = 0.80, alpha = 0.05,
                                          k = 0.41, ratio = 1) {

 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }
 if (!is.numeric(power) || length(power) != 1 || power <= 0 || power >= 1) {
   stop("power must be a single number between 0 and 1")
 }
 if (!is.numeric(ratio) || length(ratio) != 1 || ratio <= 0) {
   stop("ratio must be a single positive number")
 }

 z_beta <- stats::qnorm(power)
 z_alpha <- stats::qnorm(1 - alpha / 2)  # Two-sided

 # For unequal allocation: SE = k * sqrt(1/n1 + 1/n2) = k * sqrt((1 + 1/ratio)/n1)
 # Solving: n1 = k^2 * (z_alpha + z_beta)^2 * (1 + 1/ratio) / effect^2
 n1 <- (k * (z_alpha + z_beta) / effect)^2 * (1 + 1 / ratio)
 n2 <- n1 * ratio

 n1_ceiling <- ceiling(n1)
 n2_ceiling <- ceiling(n2)

 achieved_power <- ssm_power_amplitude_diff(effect, n1_ceiling, n2_ceiling, alpha, k)$power

 result <- list(
   n1 = n1_ceiling,
   n2 = n2_ceiling,
   n_total = n1_ceiling + n2_ceiling,
   n1_exact = n1,
   achieved_power = achieved_power,
   target_power = power,
   effect = effect,
   alpha = alpha,
   k = k,
   ratio = ratio,
   one_sided = FALSE,
   type = "amplitude_difference",
   design = "two_sample"
 )
 class(result) <- "ssm_sample_size"
 return(result)
}


#' Required Sample Size for Two-Group Elevation Comparison
#'
#' Calculates sample size needed per group to detect a difference in
#' elevations between two independent groups.
#'
#' @param effect Expected elevation difference between groups.
#' @param power Target power. Default is 0.80.
#' @param alpha Significance level. Default is 0.05.
#' @param k Scaling constant. Default is 0.60.
#' @param ratio Allocation ratio n2/n1. Default is 1 for balanced design.
#'
#' @return An object of class `ssm_sample_size`.
#'
#' @examples
#' ssm_sample_size_elevation_diff(effect = 0.11, power = 0.80)
#'
#' @export
ssm_sample_size_elevation_diff <- function(effect, power = 0.80, alpha = 0.05,
                                          k = 0.60, ratio = 1) {

 if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
   stop("effect must be a single positive number")
 }

 z_beta <- stats::qnorm(power)
 z_alpha <- stats::qnorm(1 - alpha / 2)

 n1 <- (k * (z_alpha + z_beta) / effect)^2 * (1 + 1 / ratio)
 n2 <- n1 * ratio

 n1_ceiling <- ceiling(n1)
 n2_ceiling <- ceiling(n2)

 achieved_power <- ssm_power_elevation_diff(effect, n1_ceiling, n2_ceiling, alpha, k)$power

 result <- list(
   n1 = n1_ceiling,
   n2 = n2_ceiling,
   n_total = n1_ceiling + n2_ceiling,
   n1_exact = n1,
   achieved_power = achieved_power,
   target_power = power,
   effect = effect,
   alpha = alpha,
   k = k,
   ratio = ratio,
   one_sided = FALSE,
   type = "elevation_difference",
   design = "two_sample"
 )
 class(result) <- "ssm_sample_size"
 return(result)
}


#' @export
print.ssm_sample_size <- function(x, ...) {
 cat("\nSSM Sample Size Calculation\n")
 cat("===========================\n")
 cat(sprintf("Type:           %s (%s)\n", x$type, x$design))
 cat(sprintf("Effect size:    %.3f\n", x$effect))
 cat(sprintf("Target power:   %.2f\n", x$target_power))
 cat(sprintf("Alpha:          %.3f\n", x$alpha))
 cat(sprintf("Test:           %s\n", ifelse(x$one_sided, "one-sided", "two-sided")))
 cat("---------------------------\n")
 if (x$design == "two_sample") {
   cat(sprintf("Required n1:    %d\n", x$n1))
   cat(sprintf("Required n2:    %d\n", x$n2))
   cat(sprintf("Total N:        %d\n", x$n_total))
 } else {
   cat(sprintf("Required n:     %d\n", x$n))
 }
 cat(sprintf("Achieved power: %.4f\n", x$achieved_power))
 invisible(x)
}

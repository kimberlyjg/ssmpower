#' @title Power Analysis for SSM Displacement
#' @name displacement
#' @description Functions for power analysis and sample size planning for
#'   SSM angular displacement comparisons.
#'
#' @details
#' Displacement precision is inversely proportional to amplitude:
#' \deqn{SE(\delta) = k_\delta / (a \times \sqrt{n})}
#'
#' where \eqn{k_\delta \approx 31°} based on empirical validation across
#' multiple circumplex measures and independent samples (total N = 2,106).
#'
#' When amplitude is low (< 0.10), displacement becomes essentially
#' indeterminate regardless of sample size.
#'
#' @references
#' Gilbert, K. J. (2025). Powering the circumplex: A practical guide to
#'   sample size for the structural summary method.
#'
#' Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
#'   interpersonal construct validation. Assessment, 24(1), 3-23.
#'
#' @importFrom stats pnorm qnorm reshape
NULL

#' Scaling constant for displacement (in degrees)
#' @export
K_DISPLACEMENT <- 31

#' Power for Displacement Comparison
#'
#' Calculate statistical power for detecting a displacement difference
#' in SSM analysis.
#'
#' @param delta_diff Expected displacement difference in degrees.
#' @param amplitude Expected amplitude (required - precision depends on this).
#' @param n Sample size (per group for two-group designs).
#' @param alpha Significance level (default: 0.05).
#' @param k_delta Scaling constant in degrees (default: 31).
#' @param two_group Logical; TRUE for two-group comparison (default),
#'   FALSE for single-sample test against a hypothesized value.
#'
#' @return A list containing:
#'   \item{power}{Statistical power}
#'   \item{delta_diff}{Expected displacement difference}
#'   \item{amplitude}{Expected amplitude}
#'   \item{n}{Sample size}
#'   \item{se}{Standard error of displacement (difference)}
#'   \item{alpha}{Significance level}
#'   \item{k_delta}{Scaling constant used}
#'   \item{two_group}{Whether two-group design}
#'   \item{type}{Parameter type ("displacement")}
#'   \item{design}{Study design ("two_sample" or "single_sample")}
#'
#' @details
#' The key insight is that displacement precision is INVERSELY PROPORTIONAL
#' to amplitude: \eqn{SE(\delta) = k_\delta / (a \times \sqrt{n})}
#'
#' When amplitude approaches zero, displacement becomes undefined - there
#' is no peak to locate. This formula was derived using the delta method
#' (Oehlert, 1992) applied to the arctangent transformation of the
#' Fourier coefficients.
#'
#' @examples
#' # Power for detecting 30° difference with amplitude = 0.25, n = 100 per group
#' ssm_power_displacement(delta_diff = 30, amplitude = 0.25, n = 100)
#'
#' # Low amplitude makes displacement imprecise
#' ssm_power_displacement(delta_diff = 30, amplitude = 0.10, n = 100)
#'
#' @export
ssm_power_displacement <- function(delta_diff, amplitude, n, alpha = 0.05,
                                    k_delta = K_DISPLACEMENT, two_group = TRUE) {

  # Input validation
  if (amplitude < 0.05) {
    warning("Amplitude < 0.05: displacement is essentially undefined")
    return(list(
      power = NA,
      delta_diff = delta_diff,
      amplitude = amplitude,
      n = n,
      se = NA,
      alpha = alpha,
      k_delta = k_delta,
      two_group = two_group,
      type = "displacement",
      design = ifelse(two_group, "two_sample", "single_sample"),
      message = "Amplitude too low for displacement inference"
    ))
  }

  if (amplitude < 0.10) {
    warning("Amplitude < 0.10: displacement estimates will be highly unstable")
  }

  if (delta_diff <= 0) stop("delta_diff must be positive")
  if (n <= 0) stop("n must be positive")

  z_alpha <- qnorm(1 - alpha / 2)

  if (two_group) {
    # Two independent groups: SE_diff = k_delta * sqrt(2/n) / a
    se_diff <- k_delta * sqrt(2 / n) / amplitude
  } else {
    # Single sample: SE = k_delta / (a * sqrt(n))
    se_diff <- k_delta / (amplitude * sqrt(n))
  }

  z_effect <- delta_diff / se_diff
  power <- pnorm(z_effect - z_alpha) + pnorm(-z_effect - z_alpha)

  list(
    power = power,
    delta_diff = delta_diff,
    amplitude = amplitude,
    n = n,
    se = se_diff,
    alpha = alpha,
    k_delta = k_delta,
    two_group = two_group,
    type = "displacement",
    design = ifelse(two_group, "two_sample", "single_sample")
  )
}


#' Power for Two-Group Displacement Difference
#'
#' Convenience wrapper for two-group displacement comparisons with
#' unequal sample sizes.
#'
#' @param delta_diff Expected displacement difference in degrees.
#' @param amplitude Expected amplitude.
#' @param n1 Sample size for group 1.
#' @param n2 Sample size for group 2 (default: n1).
#' @param alpha Significance level (default: 0.05).
#' @param k_delta Scaling constant in degrees (default: 31).
#'
#' @return A list containing power and related statistics.
#'
#' @examples
#' # Unequal group sizes
#' ssm_power_displacement_diff(delta_diff = 30, amplitude = 0.25, n1 = 100, n2 = 150)
#'
#' @export
ssm_power_displacement_diff <- function(delta_diff, amplitude, n1, n2 = n1,
                                         alpha = 0.05, k_delta = K_DISPLACEMENT) {

  if (amplitude < 0.05) {
    warning("Amplitude < 0.05: displacement is essentially undefined")
    return(list(power = NA, message = "Amplitude too low"))
  }

  if (delta_diff <= 0) stop("delta_diff must be positive")

  z_alpha <- qnorm(1 - alpha / 2)
  se_diff <- k_delta * sqrt(1/n1 + 1/n2) / amplitude
  z_effect <- delta_diff / se_diff
  power <- pnorm(z_effect - z_alpha) + pnorm(-z_effect - z_alpha)

  list(
    power = power,
    delta_diff = delta_diff,
    amplitude = amplitude,
    n1 = n1,
    n2 = n2,
    se = se_diff,
    alpha = alpha,
    k_delta = k_delta,
    type = "displacement_difference",
    design = "two_sample"
  )
}


#' Sample Size for Displacement Comparison
#'
#' Calculate required sample size for detecting a displacement difference
#' in SSM analysis.
#'
#' @param delta_diff Expected displacement difference in degrees.
#' @param amplitude Expected amplitude (required).
#' @param power Desired power (default: 0.80).
#' @param alpha Significance level (default: 0.05).
#' @param k_delta Scaling constant in degrees (default: 31).
#' @param two_group Logical; TRUE for two-group comparison (default).
#'
#' @return A list containing:
#'   \item{n}{Required sample size (per group for two-group designs)}
#'   \item{achieved_power}{Actual power at recommended n}
#'   \item{target_power}{Requested power level}
#'   \item{delta_diff}{Expected displacement difference}
#'   \item{amplitude}{Expected amplitude}
#'   \item{se}{Expected standard error at recommended n}
#'
#' @details
#' Formula: \eqn{n = 2 \times (k_\delta/a)^2 \times (z_\alpha + z_\beta)^2 / \Delta\delta^2}
#'
#' IMPORTANT: Sample size depends heavily on amplitude. When amplitude is small,
#' required sample sizes become prohibitively large because displacement precision
#' degrades rapidly as amplitude approaches zero.
#'
#' @examples
#' # Typical case: medium amplitude, 30° difference
#' ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.25)
#'
#' # Low amplitude requires much larger samples
#' ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.15)
#'
#' # High amplitude is very efficient
#' ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.40)
#'
#' @export
ssm_sample_size_displacement <- function(delta_diff, amplitude, power = 0.80,
                                          alpha = 0.05, k_delta = K_DISPLACEMENT,
                                          two_group = TRUE) {

  if (amplitude < 0.10) {
    warning("Amplitude < 0.10: displacement is unreliable regardless of sample size. ",
            "Consider focusing on elevation instead, or verify amplitude >= 0.15 in prior research.")
    if (amplitude < 0.05) {
      return(list(
        n = NA,
        achieved_power = NA,
        target_power = power,
        delta_diff = delta_diff,
        amplitude = amplitude,
        message = "Amplitude too low for meaningful displacement inference"
      ))
    }
  }

  if (delta_diff <= 0) stop("delta_diff must be positive")
  if (power <= 0 || power >= 1) stop("power must be between 0 and 1")

  z_beta <- qnorm(power)
  z_alpha <- qnorm(1 - alpha / 2)

  if (two_group) {
    # n per group = 2 * (k_delta/a)^2 * (z_alpha + z_beta)^2 / delta_diff^2
    n <- 2 * (k_delta / amplitude)^2 * (z_alpha + z_beta)^2 / delta_diff^2
  } else {
    n <- (k_delta / amplitude)^2 * (z_alpha + z_beta)^2 / delta_diff^2
  }

  n <- ceiling(n)

  # Calculate achieved power
  achieved <- ssm_power_displacement(delta_diff, amplitude, n, alpha,
                                      k_delta, two_group)

  list(
    n = n,
    achieved_power = achieved$power,
    target_power = power,
    delta_diff = delta_diff,
    amplitude = amplitude,
    alpha = alpha,
    k_delta = k_delta,
    se = achieved$se,
    two_group = two_group,
    type = "displacement",
    design = ifelse(two_group, "two_sample", "single_sample")
  )
}


#' Sample Size for Two-Group Displacement Difference
#'
#' Convenience wrapper with support for unequal allocation.
#'
#' @param delta_diff Expected displacement difference in degrees.
#' @param amplitude Expected amplitude.
#' @param power Desired power (default: 0.80).
#' @param alpha Significance level (default: 0.05).
#' @param k_delta Scaling constant in degrees (default: 31).
#' @param ratio Allocation ratio n2/n1 (default: 1 for equal groups).
#'
#' @return A list containing n1, n2, n_total, and achieved_power.
#'
#' @examples
#' # Equal allocation
#' ssm_sample_size_displacement_diff(delta_diff = 30, amplitude = 0.25)
#'
#' # 2:1 allocation ratio
#' ssm_sample_size_displacement_diff(delta_diff = 30, amplitude = 0.25, ratio = 2)
#'
#' @export
ssm_sample_size_displacement_diff <- function(delta_diff, amplitude, power = 0.80,
                                               alpha = 0.05, k_delta = K_DISPLACEMENT,
                                               ratio = 1) {

  if (amplitude < 0.10) {
    warning("Amplitude < 0.10: displacement is unreliable regardless of sample size")
    if (amplitude < 0.05) return(list(n1 = NA, n2 = NA, n_total = NA))
  }

  if (delta_diff <= 0) stop("delta_diff must be positive")

  z_beta <- qnorm(power)
  z_alpha <- qnorm(1 - alpha / 2)

  # General formula with unequal allocation
  n1_exact <- (k_delta / amplitude)^2 * (z_alpha + z_beta)^2 * (1 + 1/ratio) / delta_diff^2
  n2_exact <- n1_exact * ratio

  n1 <- ceiling(n1_exact)
  n2 <- ceiling(n2_exact)

  achieved <- ssm_power_displacement_diff(delta_diff, amplitude, n1, n2,
                                           alpha, k_delta)

  list(
    n1 = n1,
    n2 = n2,
    n_total = n1 + n2,
    achieved_power = achieved$power,
    target_power = power,
    delta_diff = delta_diff,
    amplitude = amplitude,
    alpha = alpha,
    k_delta = k_delta,
    ratio = ratio,
    type = "displacement_difference",
    design = "two_sample"
  )
}


#' Displacement Precision Table
#'
#' Generate a table showing displacement standard errors across
#' combinations of amplitude and sample size.
#'
#' @param amplitudes Vector of amplitude values (default: 0.10 to 0.40).
#' @param sample_sizes Vector of sample sizes (default: 50, 100, 200, 500).
#' @param k_delta Scaling constant (default: 31).
#'
#' @return A data frame with SE(displacement) for each combination.
#'
#' @examples
#' ssm_displacement_precision_table()
#'
#' @export
ssm_displacement_precision_table <- function(
    amplitudes = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.40),
    sample_sizes = c(50, 100, 200, 500),
    k_delta = K_DISPLACEMENT) {

  result <- expand.grid(Amplitude = amplitudes, n = sample_sizes)
  result$SE_degrees <- k_delta / (result$Amplitude * sqrt(result$n))
  result$SE_degrees <- round(result$SE_degrees, 1)

  # Reshape to wide format
  wide <- reshape(result,
                  idvar = "Amplitude",
                  timevar = "n",
                  direction = "wide")

  names(wide) <- c("Amplitude", paste0("n = ", sample_sizes))
  wide
}
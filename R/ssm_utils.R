#' Confidence Interval for Amplitude
#'
#' Calculates asymptotic confidence interval for amplitude (truncated at 0)
#'
#' @param amplitude Point estimate of amplitude
#' @param n Sample size
#' @param conf_level Confidence level (default: 0.95)
#' @param k Scaling constant (default: 0.41)
#'
#' @return Named numeric vector with lower and upper bounds
#'
#' @examples
#' ssm_ci_amplitude(amplitude = 0.25, n = 200)
#'
#' @export
ssm_ci_amplitude <- function(amplitude, n, conf_level = 0.95, k = 0.41) {

  se <- k / sqrt(n)
  z <- stats::qnorm(1 - (1 - conf_level)/2)

  lower <- max(0, amplitude - z * se)
  upper <- amplitude + z * se

  ci <- c(lower = lower, upper = upper)
  attr(ci, "conf_level") <- conf_level
  return(ci)
}


#' Confidence Interval for Elevation
#'
#' Calculates asymptotic confidence interval for elevation
#'
#' @param elevation Point estimate of elevation
#' @param n Sample size
#' @param conf_level Confidence level (default: 0.95)
#' @param k Scaling constant (default: 0.60)
#'
#' @return Named numeric vector with lower and upper bounds
#'
#' @examples
#' ssm_ci_elevation(elevation = 0.15, n = 200)
#'
#' @export
ssm_ci_elevation <- function(elevation, n, conf_level = 0.95, k = 0.60) {

  se <- k / sqrt(n)
  z <- stats::qnorm(1 - (1 - conf_level)/2)

  lower <- elevation - z * se
  upper <- elevation + z * se

  ci <- c(lower = lower, upper = upper)
  attr(ci, "conf_level") <- conf_level
  return(ci)
}


#' Effect Size Label
#'
#' Interprets effect size based on Zimmermann and Wright (2017) benchmarks
#'
#' @param effect Effect size value
#' @param type Either "amplitude" or "elevation"
#'
#' @return Character label describing effect size magnitude
#'
#' @details
#' Benchmarks from Zimmermann & Wright (2017) based on 433 SSM analyses:
#' \itemize{
#'   \item Amplitude: Small = 0.10, Medium = 0.16, Large = 0.23
#'   \item Elevation: Small = 0.02, Medium = 0.11, Large = 0.27
#' }
#'
#' @examples
#' ssm_effect_label(0.12, "amplitude")
#' ssm_effect_label(0.25, "elevation")
#'
#' @export
ssm_effect_label <- function(effect, type = c("amplitude", "elevation")) {

  type <- match.arg(type)
  effect <- abs(effect)

  if (type == "amplitude") {
    if (effect < 0.10) return("< Small")
    if (effect < 0.13) return("Small")
    if (effect < 0.16) return("Small-Medium")
    if (effect < 0.20) return("Medium")
    if (effect < 0.23) return("Medium-Large")
    return("Large")
  } else {
    if (effect < 0.02) return("< Small")
    if (effect < 0.07) return("Small")
    if (effect < 0.11) return("Small-Medium")
    if (effect < 0.19) return("Medium")
    if (effect < 0.27) return("Medium-Large")
    return("Large")
  }
}


#' Print Sample Size Quick Reference Guide
#'
#' Displays a formatted table of sample size requirements for common scenarios
#'
#' @param power Target power level (default: 0.80)
#'
#' @return Invisibly returns a data frame with the values
#'
#' @examples
#' ssm_sample_size_guide()
#' ssm_sample_size_guide(power = 0.90)
#'
#' @export
ssm_sample_size_guide <- function(power = 0.80) {

  cat("\n")
  cat("============================================================\n")
  cat("         SSM SAMPLE SIZE QUICK REFERENCE GUIDE\n")
  cat(sprintf("                  (Power = %.0f%%, alpha = .05)\n", power * 100))
  cat("============================================================\n")
  cat("\n")
  cat("Effect Size Benchmarks (Zimmermann & Wright, 2017)\n")
  cat("------------------------------------------------------------\n")
  cat("Amplitude:  Small = 0.10  |  Medium = 0.16  |  Large = 0.23\n")
  cat("Elevation:  Small = 0.02  |  Medium = 0.11  |  Large = 0.27\n")
  cat("\n")
  cat("SINGLE-SAMPLE: Test if parameter differs from 0\n")
  cat("------------------------------------------------------------\n")

  amp_small <- ssm_sample_size_amplitude(0.10, power)$n
  amp_med <- ssm_sample_size_amplitude(0.16, power)$n
  amp_large <- ssm_sample_size_amplitude(0.23, power)$n

  elev_small <- ssm_sample_size_elevation(0.02, power)$n
  elev_med <- ssm_sample_size_elevation(0.11, power)$n
  elev_large <- ssm_sample_size_elevation(0.27, power)$n

  cat(sprintf("Amplitude:  n = %4d (small) | n = %3d (medium) | n = %3d (large)\n",
              amp_small, amp_med, amp_large))
  cat(sprintf("Elevation:  n = %4d (small) | n = %3d (medium) | n = %3d (large)\n",
              elev_small, elev_med, elev_large))

  cat("\n")
  cat("TWO-GROUP: Test if parameters differ between groups\n")
  cat("------------------------------------------------------------\n")

  amp_diff_small <- ssm_sample_size_amplitude_diff(0.10, power)$n1
  amp_diff_med <- ssm_sample_size_amplitude_diff(0.16, power)$n1
  amp_diff_large <- ssm_sample_size_amplitude_diff(0.23, power)$n1

  cat(sprintf("Amplitude:  n = %4d/grp (small) | n = %3d/grp (med) | n = %3d/grp (lg)\n",
              amp_diff_small, amp_diff_med, amp_diff_large))
  cat("============================================================\n")
  cat("\n")

  df <- data.frame(
    Type = c("Amplitude", "Amplitude", "Amplitude", "Elevation", "Elevation", "Elevation"),
    Effect = c("Small", "Medium", "Large", "Small", "Medium", "Large"),
    Value = c(0.10, 0.16, 0.23, 0.02, 0.11, 0.27),
    N_single = c(amp_small, amp_med, amp_large, elev_small, elev_med, elev_large)
  )
  invisible(df)
}


#' Generate Power Table
#'
#' Creates a table of power values across effect sizes and sample sizes
#'
#' @param effects Vector of effect sizes
#' @param ns Vector of sample sizes
#' @param type Either "amplitude" or "elevation"
#' @param alpha Significance level (default: 0.05)
#'
#' @return A data frame of class \code{ssm_power_table} with power values
#'
#' @examples
#' ssm_power_table(effects = c(0.10, 0.15, 0.20),
#'                 ns = c(50, 100, 150, 200),
#'                 type = "amplitude")
#'
#' @export
ssm_power_table <- function(effects = c(0.10, 0.15, 0.20, 0.25),
                            ns = c(50, 100, 150, 200, 300),
                            type = c("amplitude", "elevation"),
                            alpha = 0.05) {

  type <- match.arg(type)
  power_func <- if (type == "amplitude") ssm_power_amplitude else ssm_power_elevation

  power_mat <- matrix(NA, nrow = length(effects), ncol = length(ns))
  rownames(power_mat) <- paste0("a=", effects)
  colnames(power_mat) <- paste0("n=", ns)

  for (i in seq_along(effects)) {
    for (j in seq_along(ns)) {
      power_mat[i, j] <- round(power_func(effects[i], ns[j], alpha)$power, 3)
    }
  }

  result <- as.data.frame(power_mat)
  result <- cbind(Effect = effects, result)

  class(result) <- c("ssm_power_table", "data.frame")
  attr(result, "type") <- type
  return(result)
}


#' @export
print.ssm_power_table <- function(x, ...) {
  type <- attr(x, "type")
  cat(sprintf("\nSSM Power Table (%s)\n", type))
  cat(paste(rep("=", 40), collapse = ""), "\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

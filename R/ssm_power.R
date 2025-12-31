#' Power for Single-Sample Amplitude Test
#'
#' Calculates statistical power for testing H0: amplitude = 0 vs H1: amplitude > 0
#'
#' @param effect Expected amplitude (effect size)
#' @param n Sample size
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant for SE (default: 0.41, empirically derived)
#' @param one_sided Logical; use one-sided test? (default: TRUE, appropriate for amplitude)
#'
#' @return An object of class \code{ssm_power} containing power and input parameters
#'
#' @references
#' Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal
#' construct validation: Methodological advances in the circumplex Structural
#' Summary Approach. \emph{Assessment, 24}, 3-23.
#'
#' @examples
#' # Power to detect medium amplitude with n=100
#' ssm_power_amplitude(effect = 0.16, n = 100)
#'
#' # Power curve across sample sizes
#' sapply(c(50, 100, 150, 200), function(n) ssm_power_amplitude(0.15, n)$power)
#'
#' @export
ssm_power_amplitude <- function(effect, n, alpha = 0.05, k = 0.41, one_sided = TRUE) {

  if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
    stop("effect must be a single positive number")
  }
  if (!is.numeric(n) || length(n) != 1 || n <= 0) {
    stop("n must be a single positive number")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }

  se <- k / sqrt(n)
  z_effect <- effect / se

  if (one_sided) {
    z_crit <- stats::qnorm(1 - alpha)
    power <- stats::pnorm(z_effect - z_crit)
  } else {
    z_crit <- stats::qnorm(1 - alpha/2)
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
    type = "amplitude",
    design = "single_sample"
  )
  class(result) <- "ssm_power"
  return(result)
}


#' Power for Two-Group Amplitude Difference
#'
#' Calculates power for comparing amplitudes between two independent groups
#'
#' @param effect Expected amplitude difference
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2 (default: same as n1)
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant (default: 0.41)
#'
#' @return An object of class \code{ssm_power}
#'
#' @examples
#' # Power to detect medium difference with 100 per group
#' ssm_power_amplitude_diff(effect = 0.16, n1 = 100, n2 = 100)
#'
#' @export
ssm_power_amplitude_diff <- function(effect, n1, n2 = n1, alpha = 0.05, k = 0.41) {

  if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
    stop("effect must be a single positive number")
  }
  if (!is.numeric(n1) || length(n1) != 1 || n1 <= 0) {
    stop("n1 must be a single positive number")
  }
  if (!is.numeric(n2) || length(n2) != 1 || n2 <= 0) {
    stop("n2 must be a single positive number")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }

  se_diff <- k * sqrt(1/n1 + 1/n2)
  z_effect <- effect / se_diff
  z_crit <- stats::qnorm(1 - alpha/2)

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
#' Calculates power for testing H0: elevation = 0 vs H1: elevation != 0
#'
#' @param effect Expected absolute elevation
#' @param n Sample size
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant for elevation SE (default: 0.60)
#' @param one_sided Use one-sided test? (default: FALSE)
#'
#' @return An object of class \code{ssm_power}
#'
#' @examples
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
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }

  se <- k / sqrt(n)
  z_effect <- effect / se

  if (one_sided) {
    z_crit <- stats::qnorm(1 - alpha)
    power <- stats::pnorm(z_effect - z_crit)
  } else {
    z_crit <- stats::qnorm(1 - alpha/2)
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
#' Calculates power for comparing elevations between two independent groups
#'
#' @param effect Expected elevation difference
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2 (default: same as n1)
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant (default: 0.60)
#'
#' @return An object of class \code{ssm_power}
#'
#' @examples
#' ssm_power_elevation_diff(effect = 0.11, n1 = 200, n2 = 200)
#'
#' @export
ssm_power_elevation_diff <- function(effect, n1, n2 = n1, alpha = 0.05, k = 0.60) {

  if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
    stop("effect must be a single positive number")
  }
  if (!is.numeric(n1) || length(n1) != 1 || n1 <= 0) {
    stop("n1 must be a single positive number")
  }
  if (!is.numeric(n2) || length(n2) != 1 || n2 <= 0) {
    stop("n2 must be a single positive number")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }

  se_diff <- k * sqrt(1/n1 + 1/n2)
  z_effect <- effect / se_diff
  z_crit <- stats::qnorm(1 - alpha/2)

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
    cat(sprintf("Sample size: n1=%d, n2=%d\n", x$n1, x$n2))
  } else {
    cat(sprintf("Sample size: %d\n", x$n))
  }
  cat(sprintf("Alpha:       %.3f\n", x$alpha))
  cat(sprintf("Test:        %s\n", ifelse(x$one_sided, "one-sided", "two-sided")))
  cat("------------------\n")
  cat(sprintf("Power:       %.3f\n", x$power))
  invisible(x)
}

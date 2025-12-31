#' Required Sample Size for Amplitude Test
#'
#' Calculates sample size needed to achieve target power for amplitude test
#'
#' @param effect Expected amplitude
#' @param power Target power (default: 0.80)
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant (default: 0.41)
#' @param one_sided Use one-sided test? (default: TRUE)
#'
#' @return An object of class \code{ssm_sample_size} containing:
#' \describe{
#'   \item{n}{Required sample size (ceiling)}
#'   \item{n_exact}{Exact calculated sample size}
#'   \item{achieved_power}{Actual power with ceiling n}
#'   \item{target_power}{Requested power level}
#' }
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

  if (!is.numeric(effect) || length(effect) != 1 || effect <= 0) {
    stop("effect must be a single positive number")
  }
  if (!is.numeric(power) || length(power) != 1 || power <= 0 || power >= 1) {
    stop("power must be a single number between 0 and 1")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }

  z_beta <- stats::qnorm(power)
  if (one_sided) {
    z_alpha <- stats::qnorm(1 - alpha)
  } else {
    z_alpha <- stats::qnorm(1 - alpha/2)
  }

  n_exact <- (k * (z_alpha + z_beta) / effect)^2
  n_ceiling <- ceiling(n_exact)

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
#' Calculates sample size needed to achieve target power for elevation test
#'
#' @param effect Expected absolute elevation
#' @param power Target power (default: 0.80)
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant (default: 0.60)
#' @param one_sided Use one-sided test? (default: FALSE)
#'
#' @return An object of class \code{ssm_sample_size}
#'
#' @examples
#' ssm_sample_size_elevation(effect = 0.11, power = 0.80)
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
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }

  z_beta <- stats::qnorm(power)
  if (one_sided) {
    z_alpha <- stats::qnorm(1 - alpha)
  } else {
    z_alpha <- stats::qnorm(1 - alpha/2)
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
#' Calculates sample size per group for comparing amplitudes between groups
#'
#' @param effect Expected amplitude difference
#' @param power Target power (default: 0.80)
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant (default: 0.41)
#' @param ratio Allocation ratio n2/n1 (default: 1 for balanced)
#'
#' @return An object of class \code{ssm_sample_size}
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
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }
  if (!is.numeric(ratio) || length(ratio) != 1 || ratio <= 0) {
    stop("ratio must be a single positive number")
  }

  z_beta <- stats::qnorm(power)
  z_alpha <- stats::qnorm(1 - alpha/2)

  n1 <- (k * (z_alpha + z_beta) / effect)^2 * (1 + 1/ratio)
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
#' Calculates sample size per group for comparing elevations between groups
#'
#' @param effect Expected elevation difference
#' @param power Target power (default: 0.80)
#' @param alpha Significance level (default: 0.05)
#' @param k Scaling constant (default: 0.60)
#' @param ratio Allocation ratio n2/n1 (default: 1)
#'
#' @return An object of class \code{ssm_sample_size}
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
  if (!is.numeric(power) || length(power) != 1 || power <= 0 || power >= 1) {
    stop("power must be a single number between 0 and 1")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1")
  }
  if (!is.numeric(ratio) || length(ratio) != 1 || ratio <= 0) {
    stop("ratio must be a single positive number")
  }

  z_beta <- stats::qnorm(power)
  z_alpha <- stats::qnorm(1 - alpha/2)

  n1 <- (k * (z_alpha + z_beta) / effect)^2 * (1 + 1/ratio)
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

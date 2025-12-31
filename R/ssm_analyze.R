#' Comprehensive SSM Analysis with Bootstrap CIs and Power
#'
#' Performs full SSM analysis including parameter estimation, bootstrap
#' confidence intervals, effect size interpretation, and post-hoc power
#'
#' @param external_var Numeric vector of external variable scores
#' @param octant_scores Matrix or data frame with 8 columns (octant scores)
#' @param angles Vector of octant angles in degrees (default: 0, 45, ..., 315)
#' @param n_boot Number of bootstrap resamples (default: 2000)
#' @param conf_level Confidence level for CIs (default: 0.95)
#'
#' @return An object of class \code{ssm_analysis} containing:
#' \describe{
#'   \item{elevation}{Elevation point estimate}
#'   \item{amplitude}{Amplitude point estimate}
#'   \item{displacement}{Displacement in degrees}
#'   \item{ci_elevation}{Bootstrap CI for elevation}
#'   \item{ci_amplitude}{Bootstrap CI for amplitude}
#'   \item{ci_displacement}{Bootstrap CI for displacement}
#'   \item{effect_label_amplitude}{Effect size category for amplitude}
#'   \item{effect_label_elevation}{Effect size category for elevation}
#'   \item{n}{Sample size (complete cases)}
#'   \item{correlations}{Octant correlations}
#' }
#'
#' @details
#' Missing data are handled by complete-case analysis. A message is printed
#' if cases are removed, and a warning is issued if the resulting sample
#' size is below 30.
#'
#' @examples
#' # Simulate example data
#' set.seed(42)
#' n <- 200
#' external <- rnorm(n)
#' octants <- matrix(rnorm(n * 8), ncol = 8)
#' octants[, 1] <- octants[, 1] + 0.3 * external
#' octants[, 2] <- octants[, 2] + 0.2 * external
#'
#' result <- ssm_analyze(external, octants, n_boot = 500)
#' print(result)
#'
#' @export
ssm_analyze <- function(external_var, octant_scores,
                        angles = seq(0, 315, by = 45),
                        n_boot = 2000,
                        conf_level = 0.95) {

  if (!is.numeric(external_var)) {
    stop("external_var must be numeric")
  }
  octant_scores <- as.matrix(octant_scores)
  if (ncol(octant_scores) != 8) {
    stop("octant_scores must have 8 columns")
  }
  if (length(external_var) != nrow(octant_scores)) {
    stop("external_var and octant_scores must have the same number of observations")
  }

  # Handle missing data: use complete cases only
  complete_cases <- stats::complete.cases(external_var, octant_scores)
  n_original <- length(external_var)
  external_var <- external_var[complete_cases]
  octant_scores <- octant_scores[complete_cases, ]
  n <- length(external_var)

  if (n < n_original) {
    message(sprintf("Note: %d cases with missing data removed (%d complete cases used)",
                    n_original - n, n))
  }
  if (n < 30) {
    warning("Sample size after removing missing data is very small (n < 30)")
  }

  cors <- sapply(1:8, function(i) stats::cor(external_var, octant_scores[, i]))
  params <- ssm_parameters(cors, angles)

  boot_results <- matrix(NA, nrow = n_boot, ncol = 3)
  colnames(boot_results) <- c("elevation", "amplitude", "displacement")

  set.seed(123)
  for (b in 1:n_boot) {
    idx <- sample(n, replace = TRUE)
    boot_cors <- sapply(1:8, function(i) {
      stats::cor(external_var[idx], octant_scores[idx, i])
    })
    boot_params <- ssm_parameters(boot_cors, angles)
    boot_results[b, ] <- c(boot_params$elevation,
                           boot_params$amplitude,
                           boot_params$displacement_deg)
  }

  alpha <- 1 - conf_level
  ci_elevation <- stats::quantile(boot_results[, "elevation"], c(alpha/2, 1 - alpha/2))
  ci_amplitude <- stats::quantile(boot_results[, "amplitude"], c(alpha/2, 1 - alpha/2))
  ci_displacement <- stats::quantile(boot_results[, "displacement"], c(alpha/2, 1 - alpha/2))

  label_amplitude <- ssm_effect_label(params$amplitude, "amplitude")
  label_elevation <- ssm_effect_label(abs(params$elevation), "elevation")

  result <- list(
    elevation = params$elevation,
    amplitude = params$amplitude,
    displacement = params$displacement_deg,
    ci_elevation = ci_elevation,
    ci_amplitude = ci_amplitude,
    ci_displacement = ci_displacement,
    effect_label_amplitude = label_amplitude,
    effect_label_elevation = label_elevation,
    n = n,
    n_boot = n_boot,
    conf_level = conf_level,
    correlations = cors,
    angles = angles
  )

  class(result) <- "ssm_analysis"
  return(result)
}


#' @export
print.ssm_analysis <- function(x, digits = 3, ...) {
  cat("\n")
  cat("==============================================================\n")
  cat("                    SSM ANALYSIS RESULTS\n")
  cat("==============================================================\n")
  cat(sprintf("Sample size: N = %d | Bootstrap samples: %d\n", x$n, x$n_boot))
  cat("--------------------------------------------------------------\n\n")

  cat("PARAMETER ESTIMATES\n")
  cat("-------------------\n")
  cat(sprintf("  Elevation:    %6.3f  [%5.3f, %5.3f]  (%s)\n",
              x$elevation, x$ci_elevation[1], x$ci_elevation[2], x$effect_label_elevation))
  cat(sprintf("  Amplitude:    %6.3f  [%5.3f, %5.3f]  (%s)\n",
              x$amplitude, x$ci_amplitude[1], x$ci_amplitude[2], x$effect_label_amplitude))
  cat(sprintf("  Displacement: %6.1f deg [%5.1f deg, %5.1f deg]\n",
              x$displacement, x$ci_displacement[1], x$ci_displacement[2]))

  cat("\nOCTANT CORRELATIONS\n")
  cat("-------------------\n")
  octant_names <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  for (i in 1:8) {
    cat(sprintf("  %s (%3d deg): %6.3f\n", octant_names[i], x$angles[i], x$correlations[i]))
  }

  cat("\n==============================================================\n")
  invisible(x)
}

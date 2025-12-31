#' Comprehensive SSM Analysis with Bootstrap CIs
#'
#' Performs a full SSM analysis including parameter estimation, bootstrap
#' confidence intervals, effect size interpretation, and post-hoc power.
#'
#' @param external_var Numeric vector of external variable scores (e.g.,
#'   a personality measure like LPFS).
#' @param octant_scores Matrix or data frame with 8 columns containing
#'   octant scores for each participant. Columns should be ordered
#'   PA, BC, DE, FG, HI, JK, LM, NO (or corresponding angles).
#' @param angles Numeric vector of octant angles in degrees. Default is
#'   `c(0, 45, 90, 135, 180, 225, 270, 315)`.
#' @param n_boot Number of bootstrap resamples. Default is 2000.
#' @param conf_level Confidence level for intervals. Default is 0.95.
#' @param seed Random seed for reproducibility. Default is NULL (no seed).
#'
#' @return An object of class `ssm_analysis` containing:
#' \describe{
#'   \item{elevation, amplitude, displacement}{Point estimates}
#'   \item{ci_elevation, ci_amplitude, ci_displacement}{Bootstrap CIs}
#'   \item{effect_label_amplitude, effect_label_elevation}{Effect size categories}
#'   \item{power_amplitude, power_elevation}{Post-hoc power estimates}
#'   \item{n, n_boot, conf_level}{Analysis metadata}
#'   \item{correlations, angles}{Octant correlations and angles}
#' }
#'
#' @details
#' This function computes correlations between the external variable and
#' each octant, then derives SSM parameters from these correlations.
#' Bootstrap resampling provides confidence intervals that account for
#' sampling variability in the correlations.
#'
#' **Interpretation:**
#' - Amplitude indicates the strength/distinctiveness of the circumplex profile
#' - Displacement indicates the angular location (interpersonal theme)
#' - Elevation indicates the average correlation across all octants
#'
#' @seealso [ssm_parameters()] for parameter calculation without bootstrap
#'
#' @examples
#' # Simulate example data
#' set.seed(42)
#' n <- 200
#' external <- rnorm(n)
#' octants <- matrix(rnorm(n * 8), ncol = 8)
#' # Add structure: higher external -> higher PA octant
#' octants[, 1] <- octants[, 1] + 0.3 * external
#'
#' result <- ssm_analyze(external, octants, n_boot = 500)
#' print(result)
#'
#' @export
ssm_analyze <- function(external_var, octant_scores,
                       angles = seq(0, 315, by = 45),
                       n_boot = 2000,
                       conf_level = 0.95,
                       seed = NULL) {

 # Input validation
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

 n <- length(external_var)

 # Set seed if provided
 if (!is.null(seed)) {
   set.seed(seed)
 }

 # Point estimates
 cors <- sapply(1:8, function(i) {
   stats::cor(external_var, octant_scores[, i], use = "complete.obs")
 })
 params <- ssm_parameters(cors, angles)

 # Bootstrap
 boot_results <- matrix(NA, nrow = n_boot, ncol = 3)
 colnames(boot_results) <- c("elevation", "amplitude", "displacement")

 for (b in 1:n_boot) {
   idx <- sample(n, replace = TRUE)
   boot_cors <- sapply(1:8, function(i) {
     stats::cor(external_var[idx], octant_scores[idx, i], use = "complete.obs")
   })
   boot_params <- ssm_parameters(boot_cors, angles)
   boot_results[b, ] <- c(
     boot_params$elevation,
     boot_params$amplitude,
     boot_params$displacement_deg
   )
 }

 # Calculate CIs
 alpha <- 1 - conf_level
 ci_elevation <- stats::quantile(boot_results[, "elevation"], c(alpha / 2, 1 - alpha / 2))
 ci_amplitude <- stats::quantile(boot_results[, "amplitude"], c(alpha / 2, 1 - alpha / 2))
 ci_displacement <- stats::quantile(boot_results[, "displacement"], c(alpha / 2, 1 - alpha / 2))

 # Power calculations (post-hoc)
 power_amplitude <- ssm_power_amplitude(params$amplitude, n)$power
 power_elevation <- ssm_power_elevation(abs(params$elevation), n)$power

 # Effect size labels
 label_amplitude <- ssm_effect_label(params$amplitude, "amplitude")
 label_elevation <- ssm_effect_label(abs(params$elevation), "elevation")

 result <- list(
   # Point estimates
   elevation = params$elevation,
   amplitude = params$amplitude,
   displacement = params$displacement_deg,

   # CIs
   ci_elevation = ci_elevation,
   ci_amplitude = ci_amplitude,
   ci_displacement = ci_displacement,

   # Effect sizes
   effect_label_amplitude = label_amplitude,
   effect_label_elevation = label_elevation,

   # Power
   power_amplitude = power_amplitude,
   power_elevation = power_elevation,

   # Meta
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
 cat("                    SSM ANALYSIS RESULTS                       \n")
 cat("==============================================================\n")
 cat(sprintf("Sample size: N = %d | Bootstrap samples: %d\n", x$n, x$n_boot))
 cat("--------------------------------------------------------------\n\n")

 cat("PARAMETER ESTIMATES\n")
 cat("-------------------\n")
 cat(sprintf("  Elevation:    %6.3f  [%5.3f, %5.3f]  (%s)\n",
             x$elevation, x$ci_elevation[1], x$ci_elevation[2], x$effect_label_elevation))
 cat(sprintf("  Amplitude:    %6.3f  [%5.3f, %5.3f]  (%s)\n",
             x$amplitude, x$ci_amplitude[1], x$ci_amplitude[2], x$effect_label_amplitude))
 cat(sprintf("  Displacement: %6.1f deg [%5.1f, %5.1f]\n",
             x$displacement, x$ci_displacement[1], x$ci_displacement[2]))

 cat("\nPOST-HOC POWER (alpha = .05)\n")
 cat("----------------------------\n")
 cat(sprintf("  Amplitude != 0:  %.1f%%\n", x$power_amplitude * 100))
 cat(sprintf("  Elevation != 0:  %.1f%%\n", x$power_elevation * 100))

 cat("\nOCTANT CORRELATIONS\n")
 cat("-------------------\n")
 octant_names <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
 for (i in 1:8) {
   cat(sprintf("  %s (%3d deg): %6.3f\n", octant_names[i], x$angles[i], x$correlations[i]))
 }

 cat("\n==============================================================\n")
 invisible(x)
}

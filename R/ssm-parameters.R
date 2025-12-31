#' Calculate SSM Parameters from Octant Scores
#'
#' Computes elevation, amplitude, and displacement from a vector of 8 octant
#' scores (typically correlations with an external variable). Based on
#' Gurtman (1992) and the circumplex Structural Summary Method.
#'
#' @param scores Numeric vector of length 8 containing octant scores or
#'   correlations. Must not contain NA values.
#' @param angles Numeric vector of octant angles in degrees. Default is
#'   `c(0, 45, 90, 135, 180, 225, 270, 315)` corresponding to standard
#'   circumplex octants (PA, BC, DE, FG, HI, JK, LM, NO).
#'
#' @return An object of class `ssm_params` containing:
#' \describe{
#'   \item{elevation}{Mean of the octant scores}
#'   \item{amplitude}{Distance from origin to the profile's center}
#'   \item{displacement_deg}{Angular location in degrees (0-360)}
#'   \item{displacement_rad}{Angular location in radians}
#'   \item{x_val}{X-coordinate (Love/Affiliation axis)}
#'   \item{y_val}{Y-coordinate (Dominance axis)}
#'   \item{scores}{Original input scores}
#'   \item{angles}{Angles used for calculation}
#' }
#'
#' @details
#' The SSM parameters are computed using trigonometric projection:
#' \deqn{X = \frac{2}{m} \sum_{i=1}^{m} r_i \cos(\theta_i)}
#' \deqn{Y = \frac{2}{m} \sum_{i=1}^{m} r_i \sin(\theta_i)}
#' \deqn{Amplitude = \sqrt{X^2 + Y^2}}
#' \deqn{Displacement = \arctan(Y/X)}
#' \deqn{Elevation = \bar{r}}
#'
#' where \eqn{m = 8} for octant models, \eqn{r_i} are the octant scores,
#' and \eqn{\theta_i} are the octant angles.
#'
#' @references
#' Gurtman, M. B. (1992). Construct validity of interpersonal personality
#' measures: The interpersonal circumplex as a nomological net.
#' *Journal of Personality and Social Psychology, 63*, 105-118.
#'
#' Gurtman, M. B., & Pincus, A. L. (2003). The circumplex model: Methods
#' and research applications. In J. A. Schinka & W. F. Velicer (Eds.),
#' *Handbook of psychology: Vol. 2. Research methods in psychology*
#' (pp. 407-428). Wiley.
#'
#' @examples
#' # Example: correlations of an external variable with 8 IIP octants
#' octant_cors <- c(0.4, 0.3, 0.1, -0.1, -0.2, -0.1, 0.1, 0.3)
#' ssm_parameters(octant_cors)
#'
#' # With custom angles
#' ssm_parameters(octant_cors, angles = seq(0, 315, by = 45))
#'
#' @export
ssm_parameters <- function(scores, angles = seq(0, 315, by = 45)) {

 # Input validation
 if (!is.numeric(scores)) {
   stop("scores must be a numeric vector")
 }
 if (length(scores) != 8) {
   stop("scores must be a vector of length 8 (one per octant)")
 }
 if (length(angles) != length(scores)) {
   stop("scores and angles must have the same length")
 }
 if (any(is.na(scores))) {
   stop("scores contains NA values; handle missingness before calling ssm_parameters()")
 }

 # Convert angles to radians
 angles_rad <- angles * pi / 180

 # Compute x and y coordinates (Gurtman, 1992, Appendix)
 # Factor of 2/m (m=8) bounds values to [-1, 1]
 x_val <- (2 / 8) * sum(scores * cos(angles_rad))
 y_val <- (2 / 8) * sum(scores * sin(angles_rad))

 # Calculate SSM parameters
 elevation <- mean(scores)
 amplitude <- sqrt(x_val^2 + y_val^2)
 displacement_rad <- atan2(y_val, x_val)
 displacement_deg <- (displacement_rad * 180 / pi) %% 360

 # Return structured object
 result <- list(
   elevation = elevation,
   amplitude = amplitude,
   displacement_deg = displacement_deg,
   displacement_rad = displacement_rad,
   x_val = x_val,
   y_val = y_val,
   scores = scores,
   angles = angles
 )
 class(result) <- "ssm_params"
 return(result)
}


#' @export
print.ssm_params <- function(x, ...) {
 cat("\nSSM Parameters\n")
 cat("==============\n")
 cat(sprintf("Elevation:    %6.3f\n", x$elevation))
 cat(sprintf("Amplitude:    %6.3f\n", x$amplitude))
 cat(sprintf("Displacement: %6.1f\n", x$displacement_deg))
 cat(sprintf("X (LOV):      %6.3f\n", x$x_val))
 cat(sprintf("Y (DOM):      %6.3f\n", x$y_val))
 invisible(x)
}

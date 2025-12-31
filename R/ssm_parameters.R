#' Calculate SSM Parameters from Octant Scores
#'
#' Computes elevation, amplitude, and displacement from a vector of 8 octant
#' scores (typically correlations with an external variable).
#'
#' @param scores Numeric vector of length 8 (octant scores/correlations)
#' @param angles Numeric vector of octant angles in degrees (default: 0, 45, ..., 315)
#'
#' @return An object of class \code{ssm_params} containing:
#' \describe{
#'   \item{elevation}{Mean of the octant scores}
#'   \item{amplitude}{Distance from origin to profile center}
#'   \item{displacement_deg}{Angular location in degrees (0-360)}
#'   \item{displacement_rad}{Angular location in radians}
#'   \item{x_val}{X coordinate (Dominance axis)}
#'   \item{y_val}{Y coordinate (Love/Affiliation axis)}
#'   \item{scores}{Original input scores}
#'   \item{angles}{Original input angles}
#' }
#'
#' @references
#' Gurtman, M. B. (1992). Construct validity of interpersonal personality
#' measures: The interpersonal circumplex as a nomological net.
#' \emph{Journal of Personality and Social Psychology, 63}, 105-118.
#'
#' @examples
#' # Example: correlations of external variable with 8 IPC octants
#' octant_cors <- c(0.4, 0.3, 0.1, -0.1, -0.2, -0.1, 0.1, 0.3)
#' ssm_parameters(octant_cors)
#'
#' @export
ssm_parameters <- function(scores, angles = seq(0, 315, by = 45)) {

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

  angles_rad <- angles * pi / 180
  x_val <- (2/8) * sum(scores * cos(angles_rad))
  y_val <- (2/8) * sum(scores * sin(angles_rad))

  elevation <- mean(scores)
  amplitude <- sqrt(x_val^2 + y_val^2)
  displacement_rad <- atan2(y_val, x_val)
  displacement_deg <- (displacement_rad * 180 / pi) %% 360

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
  cat(sprintf("Displacement: %6.1f degrees\n", x$displacement_deg))
  cat(sprintf("X (DOM):      %6.3f\n", x$x_val))
  cat(sprintf("Y (LOV):      %6.3f\n", x$y_val))
  invisible(x)
}

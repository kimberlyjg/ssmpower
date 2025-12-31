#' Launch the SSM Power Analysis Shiny App
#'
#' Opens an interactive Shiny web application for SSM power analysis
#' and sample size planning. The app provides a graphical interface
#' for all power analysis functions in this package.
#'
#' @param launch.browser Logical; should the app open in a browser?
#'   Default is TRUE.
#'
#' @return This function does not return a value; it launches a Shiny app.
#'
#' @details
#' The Shiny app includes:
#' \itemize{
#'   \item Power Calculator: Calculate power with real-time visualization
#'   \item Sample Size Calculator: Determine required N with preset effect sizes
#'   \item Power Tables: Generate customizable power tables with heatmaps
#'   \item Quick Reference: Summary of benchmarks, formulas, and references
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the app
#' run_app()
#' }
#'
#' @export
run_app <- function(launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. ",
         "Please install it with: install.packages('shiny')")
  }
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package 'bslib' is required to run the app. ",
         "Please install it with: install.packages('bslib')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to run the app. ",
         "Please install it with: install.packages('ggplot2')")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required to run the app. ",
         "Please install it with: install.packages('DT')")
  }
  
  app_dir <- system.file("shiny", package = "ssmpower")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. ",
         "Try re-installing the package.")
  }
  
  shiny::runApp(app_dir, launch.browser = launch.browser)
}

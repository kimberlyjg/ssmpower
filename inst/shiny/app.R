# =============================================================================
# SSM Power Analysis Toolkit - Shiny App
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# =============================================================================
# CORE SSM FUNCTIONS
# =============================================================================

ssm_parameters <- function(scores, angles = seq(0, 315, by = 45)) {
  if (!is.numeric(scores)) stop("scores must be a numeric vector")
  if (length(scores) != 8) stop("scores must be a vector of length 8")
  if (any(is.na(scores))) stop("scores contains NA values")
  
  angles_rad <- angles * pi / 180
  x_val <- (2/8) * sum(scores * cos(angles_rad))
  y_val <- (2/8) * sum(scores * sin(angles_rad))
  
  elevation <- mean(scores)
  amplitude <- sqrt(x_val^2 + y_val^2)
  displacement_rad <- atan2(y_val, x_val)
  displacement_deg <- (displacement_rad * 180 / pi) %% 360
  
  list(
    elevation = elevation,
    amplitude = amplitude,
    displacement_deg = displacement_deg,
    displacement_rad = displacement_rad,
    x_val = x_val,
    y_val = y_val,
    scores = scores,
    angles = angles
  )
}

ssm_power_amplitude <- function(effect, n, alpha = 0.05, k = 0.41, one_sided = TRUE) {
  se <- k / sqrt(n)
  z_effect <- effect / se
  
  if (one_sided) {
    z_crit <- qnorm(1 - alpha)
    power <- pnorm(z_effect - z_crit)
  } else {
    z_crit <- qnorm(1 - alpha/2)
    power <- pnorm(z_effect - z_crit) + pnorm(-z_effect - z_crit)
  }
  
  list(power = power, effect = effect, n = n, alpha = alpha, k = k, se = se,
       one_sided = one_sided, type = "amplitude", design = "single_sample")
}

ssm_power_elevation <- function(effect, n, alpha = 0.05, k = 0.60, one_sided = FALSE) {
  se <- k / sqrt(n)
  z_effect <- effect / se
  
  if (one_sided) {
    z_crit <- qnorm(1 - alpha)
    power <- pnorm(z_effect - z_crit)
  } else {
    z_crit <- qnorm(1 - alpha/2)
    power <- pnorm(z_effect - z_crit) + pnorm(-z_effect - z_crit)
  }
  
  list(power = power, effect = effect, n = n, alpha = alpha, k = k, se = se,
       one_sided = one_sided, type = "elevation", design = "single_sample")
}

ssm_power_amplitude_diff <- function(effect, n1, n2 = n1, alpha = 0.05, k = 0.41) {
  se_diff <- k * sqrt(1/n1 + 1/n2)
  z_effect <- effect / se_diff
  z_crit <- qnorm(1 - alpha/2)
  power <- pnorm(z_effect - z_crit) + pnorm(-z_effect - z_crit)
  
  list(power = power, effect = effect, n1 = as.integer(n1), n2 = as.integer(n2),
       alpha = alpha, k = k, se = se_diff, one_sided = FALSE,
       type = "amplitude_difference", design = "two_sample")
}

ssm_power_elevation_diff <- function(effect, n1, n2 = n1, alpha = 0.05, k = 0.60) {
  se_diff <- k * sqrt(1/n1 + 1/n2)
  z_effect <- effect / se_diff
  z_crit <- qnorm(1 - alpha/2)
  power <- pnorm(z_effect - z_crit) + pnorm(-z_effect - z_crit)
  
  list(power = power, effect = effect, n1 = as.integer(n1), n2 = as.integer(n2),
       alpha = alpha, k = k, se = se_diff, one_sided = FALSE,
       type = "elevation_difference", design = "two_sample")
}

ssm_sample_size_amplitude <- function(effect, power = 0.80, alpha = 0.05, k = 0.41, one_sided = TRUE) {
  z_beta <- qnorm(power)
  z_alpha <- if (one_sided) qnorm(1 - alpha) else qnorm(1 - alpha/2)
  
  n_exact <- (k * (z_alpha + z_beta) / effect)^2
  n_ceiling <- ceiling(n_exact)
  achieved_power <- ssm_power_amplitude(effect, n_ceiling, alpha, k, one_sided)$power
  
  list(n = n_ceiling, n_exact = n_exact, achieved_power = achieved_power,
       target_power = power, effect = effect, alpha = alpha, k = k,
       one_sided = one_sided, type = "amplitude", design = "single_sample")
}

ssm_sample_size_elevation <- function(effect, power = 0.80, alpha = 0.05, k = 0.60, one_sided = FALSE) {
  z_beta <- qnorm(power)
  z_alpha <- if (one_sided) qnorm(1 - alpha) else qnorm(1 - alpha/2)
  
  n_exact <- (k * (z_alpha + z_beta) / effect)^2
  n_ceiling <- ceiling(n_exact)
  achieved_power <- ssm_power_elevation(effect, n_ceiling, alpha, k, one_sided)$power
  
  list(n = n_ceiling, n_exact = n_exact, achieved_power = achieved_power,
       target_power = power, effect = effect, alpha = alpha, k = k,
       one_sided = one_sided, type = "elevation", design = "single_sample")
}

ssm_sample_size_amplitude_diff <- function(effect, power = 0.80, alpha = 0.05, k = 0.41, ratio = 1) {
  z_beta <- qnorm(power)
  z_alpha <- qnorm(1 - alpha/2)
  
  n1 <- (k * (z_alpha + z_beta) / effect)^2 * (1 + 1/ratio)
  n2 <- n1 * ratio
  
  n1_ceiling <- ceiling(n1)
  n2_ceiling <- ceiling(n2)
  achieved_power <- ssm_power_amplitude_diff(effect, n1_ceiling, n2_ceiling, alpha, k)$power
  
  list(n1 = n1_ceiling, n2 = n2_ceiling, n_total = n1_ceiling + n2_ceiling,
       achieved_power = achieved_power, target_power = power, effect = effect,
       alpha = alpha, k = k, ratio = ratio, type = "amplitude_difference", design = "two_sample")
}

ssm_sample_size_elevation_diff <- function(effect, power = 0.80, alpha = 0.05, k = 0.60, ratio = 1) {
  z_beta <- qnorm(power)
  z_alpha <- qnorm(1 - alpha/2)
  
  n1 <- (k * (z_alpha + z_beta) / effect)^2 * (1 + 1/ratio)
  n2 <- n1 * ratio
  
  n1_ceiling <- ceiling(n1)
  n2_ceiling <- ceiling(n2)
  achieved_power <- ssm_power_elevation_diff(effect, n1_ceiling, n2_ceiling, alpha, k)$power
  
  list(n1 = n1_ceiling, n2 = n2_ceiling, n_total = n1_ceiling + n2_ceiling,
       achieved_power = achieved_power, target_power = power, effect = effect,
       alpha = alpha, k = k, ratio = ratio, type = "elevation_difference", design = "two_sample")
}

ssm_ci_amplitude <- function(amplitude, n, conf_level = 0.95, k = 0.41) {
  se <- k / sqrt(n)
  z <- qnorm(1 - (1 - conf_level)/2)
  c(lower = max(0, amplitude - z * se), upper = amplitude + z * se)
}

ssm_ci_elevation <- function(elevation, n, conf_level = 0.95, k = 0.60) {
  se <- k / sqrt(n)
  z <- qnorm(1 - (1 - conf_level)/2)
  c(lower = elevation - z * se, upper = elevation + z * se)
}

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

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = "SSM Power Analysis Toolkit",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#18BC9C"
  ),
  
  nav_panel(
    title = "Power Calculator",
    icon = icon("bolt"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Parameters",
        width = 320,
        selectInput("power_type", "Parameter Type",
                    choices = c("Amplitude" = "amplitude", "Elevation" = "elevation")),
        selectInput("power_design", "Design",
                    choices = c("Single Sample" = "single", "Two Groups" = "two")),
        numericInput("power_effect", "Expected Effect Size", 
                     value = 0.16, min = 0.01, max = 1, step = 0.01),
        conditionalPanel(
          condition = "input.power_design == 'single'",
          numericInput("power_n", "Sample Size (N)", value = 100, min = 10, max = 10000, step = 1)
        ),
        conditionalPanel(
          condition = "input.power_design == 'two'",
          numericInput("power_n1", "Group 1 Size", value = 100, min = 10, max = 5000),
          numericInput("power_n2", "Group 2 Size", value = 100, min = 10, max = 5000)
        ),
        numericInput("power_alpha", "Alpha", value = 0.05, min = 0.001, max = 0.20, step = 0.01),
        conditionalPanel(
          condition = "input.power_design == 'single'",
          checkboxInput("power_onesided", "One-sided test", value = TRUE)
        ),
        hr(),
        p(class = "text-muted small",
          "Effect size benchmarks:", br(),
          strong("Amplitude:"), " Small=0.10, Med=0.16, Large=0.23", br(),
          strong("Elevation:"), " Small=0.02, Med=0.11, Large=0.27"
        )
      ),
      card(
        card_header("Power Analysis Results", class = "bg-primary text-white"),
        uiOutput("power_results")
      ),
      card(
        card_header("Power Curve"),
        plotOutput("power_curve", height = "350px")
      )
    )
  ),
  
  nav_panel(
    title = "Sample Size",
    icon = icon("users"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Parameters",
        width = 320,
        selectInput("ss_type", "Parameter Type",
                    choices = c("Amplitude" = "amplitude", "Elevation" = "elevation")),
        selectInput("ss_design", "Design",
                    choices = c("Single Sample" = "single", "Two Groups" = "two")),
        numericInput("ss_effect", "Expected Effect Size", 
                     value = 0.16, min = 0.01, max = 1, step = 0.01),
        sliderInput("ss_power", "Target Power", 
                    value = 0.80, min = 0.50, max = 0.99, step = 0.01),
        numericInput("ss_alpha", "Alpha", value = 0.05, min = 0.001, max = 0.20, step = 0.01),
        conditionalPanel(
          condition = "input.ss_design == 'single'",
          checkboxInput("ss_onesided", "One-sided test", value = TRUE)
        ),
        conditionalPanel(
          condition = "input.ss_design == 'two'",
          numericInput("ss_ratio", "Allocation Ratio", value = 1, min = 0.5, max = 4, step = 0.5)
        ),
        hr(),
        actionButton("ss_preset_small", "Small Effect", class = "btn-outline-secondary btn-sm"),
        actionButton("ss_preset_medium", "Medium Effect", class = "btn-outline-secondary btn-sm"),
        actionButton("ss_preset_large", "Large Effect", class = "btn-outline-secondary btn-sm")
      ),
      card(
        card_header("Sample Size Results", class = "bg-primary text-white"),
        uiOutput("ss_results")
      ),
      card(
        card_header("Sample Size by Effect Size"),
        plotOutput("ss_plot", height = "350px")
      )
    )
  ),
  
  nav_panel(
    title = "Power Table",
    icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Table Parameters",
        width = 300,
        selectInput("table_type", "Parameter Type",
                    choices = c("Amplitude" = "amplitude", "Elevation" = "elevation")),
        textInput("table_effects", "Effect Sizes (comma-separated)",
                  value = "0.10, 0.15, 0.20, 0.25"),
        textInput("table_ns", "Sample Sizes (comma-separated)",
                  value = "50, 100, 150, 200, 300"),
        numericInput("table_alpha", "Alpha", value = 0.05, min = 0.001, max = 0.20),
        actionButton("table_generate", "Generate Table", class = "btn-primary")
      ),
      card(
        card_header("Power Table"),
        DTOutput("power_table")
      ),
      card(
        card_header("Power Heatmap"),
        plotOutput("power_heatmap", height = "400px")
      )
    )
  ),
  
  nav_panel(
    title = "Quick Reference",
    icon = icon("book"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Effect Size Benchmarks", class = "bg-secondary text-white"),
        card_body(
          h5("Amplitude (k = 0.41)"),
          tags$table(class = "table table-striped",
            tags$thead(tags$tr(tags$th("Size"), tags$th("Value"), tags$th("Percentile"))),
            tags$tbody(
              tags$tr(tags$td("Small"), tags$td("0.10"), tags$td("25th")),
              tags$tr(tags$td("Medium"), tags$td("0.16"), tags$td("50th")),
              tags$tr(tags$td("Large"), tags$td("0.23"), tags$td("75th"))
            )
          ),
          hr(),
          h5("Elevation (k = 0.60)"),
          tags$table(class = "table table-striped",
            tags$thead(tags$tr(tags$th("Size"), tags$th("Value"), tags$th("Percentile"))),
            tags$tbody(
              tags$tr(tags$td("Small"), tags$td("0.02"), tags$td("25th")),
              tags$tr(tags$td("Medium"), tags$td("0.11"), tags$td("50th")),
              tags$tr(tags$td("Large"), tags$td("0.27"), tags$td("75th"))
            )
          )
        )
      ),
      card(
        card_header("Sample Sizes for 80% Power", class = "bg-secondary text-white"),
        card_body(uiOutput("quick_ref_table"))
      ),
      card(
        card_header("Formulas", class = "bg-info text-white"),
        card_body(
          h5("Power Calculation"),
          p(withMathJax("$$\\text{Power} = \\Phi\\left(\\frac{a \\sqrt{n}}{k} - z_{1-\\alpha}\\right)$$")),
          h5("Sample Size Calculation"),
          p(withMathJax("$$n = \\left[\\frac{k(z_{1-\\alpha} + z_{1-\\beta})}{a}\\right]^2$$")),
          h5("Standard Error"),
          p(withMathJax("$$SE = \\frac{k}{\\sqrt{n}}$$")),
          p(class = "text-muted mt-3", em("Where: a = expected effect, k = scaling constant, n = sample size"))
        )
      ),
      card(
        card_header("References", class = "bg-dark text-white"),
        card_body(
          tags$ul(
            tags$li("Cohen, J. (1988). ", em("Statistical power analysis for the behavioral sciences"), " (2nd ed.). Lawrence Erlbaum."),
            tags$li("Gurtman, M. B. (1992). Construct validity of interpersonal personality measures. ", em("JPSP, 63"), ", 105-118."),
            tags$li("Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation. ", em("Assessment, 24"), ", 3-23.")
          )
        )
      )
    )
  ),
  
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_header("About the SSM Power Analysis Toolkit"),
      card_body(
        h4("Purpose"),
        p("This toolkit provides power analysis and sample size planning for Structural Summary Method (SSM) analyses of circumplex data."),
        h4("Key Features"),
        tags$ul(
          tags$li("Calculate statistical power for amplitude and elevation tests"),
          tags$li("Determine required sample sizes for target power levels"),
          tags$li("Support for single-sample and two-group designs"),
          tags$li("Interactive visualizations of power curves"),
          tags$li("Power tables for study planning")
        ),
        h4("Scaling Constants"),
        p("The scaling constants (k) are empirically derived from bootstrap simulations:"),
        tags$ul(
          tags$li(strong("Amplitude:"), " k = 0.41"),
          tags$li(strong("Elevation:"), " k = 0.60")
        ),
        hr(),
        div(class = "alert alert-info",
          h5(icon("info-circle"), " Important Notes from Simulation Validation"),
          p("Monte Carlo validation (5,000 simulations per scenario) revealed:"),
          tags$ul(
            tags$li(strong("Power estimates are slightly conservative"), " for small and medium effects. Sample sizes from this toolkit will yield ", em("at least"), " the targeted power."),
            tags$li(strong("Confidence intervals may undercover:"), " Normal-approximation 95% CIs achieve approximately 90% coverage due to positive bias in amplitude estimation and slight underestimation of SE."),
            tags$li("For critical applications requiring precise coverage, consider using bootstrap CIs.")
          )
        ),
        hr(),
        h4("Citation"),
        p("If you use this toolkit, please cite the accompanying paper:"),
        div(class = "alert alert-success",
          p(class = "mb-0", strong("Gilbert, K. J. (2026)."), " Power analysis for the Structural Summary Method. ", em("Manuscript in preparation."))
        ),
        p("And the R package:"),
        tags$blockquote(class = "blockquote", "Gilbert, K. J. (2026). ", em("ssmpower: Power Analysis for the Structural Summary Method."), " R package version 1.0.0. https://github.com/kimberlyjg/ssmpower"),
        p(class = "text-muted", "Effect size benchmarks are from:"),
        p(class = "text-muted small", "Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation. ", em("Assessment, 24"), ", 3-23.")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  observeEvent(input$ss_preset_small, {
    val <- if (input$ss_type == "amplitude") 0.10 else 0.02
    updateNumericInput(session, "ss_effect", value = val)
  })
  
  observeEvent(input$ss_preset_medium, {
    val <- if (input$ss_type == "amplitude") 0.16 else 0.11
    updateNumericInput(session, "ss_effect", value = val)
  })
  
  observeEvent(input$ss_preset_large, {
    val <- if (input$ss_type == "amplitude") 0.23 else 0.27
    updateNumericInput(session, "ss_effect", value = val)
  })
  
  output$power_results <- renderUI({
    req(input$power_effect > 0)
    
    if (input$power_design == "single") {
      req(input$power_n >= 10)
      if (input$power_type == "amplitude") {
        result <- ssm_power_amplitude(input$power_effect, input$power_n, input$power_alpha, one_sided = input$power_onesided)
      } else {
        result <- ssm_power_elevation(input$power_effect, input$power_n, input$power_alpha, one_sided = input$power_onesided)
      }
      n_display <- paste("N =", result$n)
    } else {
      req(input$power_n1 >= 10, input$power_n2 >= 10)
      if (input$power_type == "amplitude") {
        result <- ssm_power_amplitude_diff(input$power_effect, input$power_n1, input$power_n2, input$power_alpha)
      } else {
        result <- ssm_power_elevation_diff(input$power_effect, input$power_n1, input$power_n2, input$power_alpha)
      }
      n_display <- paste0("n1 = ", result$n1, ", n2 = ", result$n2, " (Total = ", result$n1 + result$n2, ")")
    }
    
    power_pct <- round(result$power * 100, 1)
    power_class <- if (result$power >= 0.80) "text-success" else if (result$power >= 0.50) "text-warning" else "text-danger"
    effect_label <- ssm_effect_label(result$effect, input$power_type)
    test_type <- if (result$one_sided) "One-sided" else "Two-sided"
    
    tagList(
      div(class = "row",
        div(class = "col-md-6", h2(class = paste("mb-0", power_class), paste0(power_pct, "%")), p(class = "text-muted", "Statistical Power")),
        div(class = "col-md-6", h4(effect_label), p(class = "text-muted", "Effect Size Category"))
      ),
      hr(),
      tags$table(class = "table table-sm",
        tags$tbody(
          tags$tr(tags$td("Parameter:"), tags$td(strong(tools::toTitleCase(input$power_type)))),
          tags$tr(tags$td("Effect Size:"), tags$td(result$effect)),
          tags$tr(tags$td("Sample Size:"), tags$td(n_display)),
          tags$tr(tags$td("Alpha:"), tags$td(result$alpha)),
          tags$tr(tags$td("Test Type:"), tags$td(test_type)),
          tags$tr(tags$td("Standard Error:"), tags$td(round(result$se, 4)))
        )
      ),
      if (result$power < 0.80) {
        div(class = "alert alert-warning mt-3", icon("exclamation-triangle"), " Power is below 80%. Consider increasing sample size.")
      }
    )
  })
  
  output$power_curve <- renderPlot({
    req(input$power_effect > 0)
    ns <- seq(10, 500, by = 5)
    
    if (input$power_design == "single") {
      if (input$power_type == "amplitude") {
        powers <- sapply(ns, function(n) ssm_power_amplitude(input$power_effect, n, input$power_alpha, one_sided = input$power_onesided)$power)
      } else {
        powers <- sapply(ns, function(n) ssm_power_elevation(input$power_effect, n, input$power_alpha, one_sided = input$power_onesided)$power)
      }
      current_n <- input$power_n
    } else {
      if (input$power_type == "amplitude") {
        powers <- sapply(ns, function(n) ssm_power_amplitude_diff(input$power_effect, n, n, input$power_alpha)$power)
      } else {
        powers <- sapply(ns, function(n) ssm_power_elevation_diff(input$power_effect, n, n, input$power_alpha)$power)
      }
      current_n <- input$power_n1
    }
    
    df <- data.frame(n = ns, power = powers)
    current_power <- if (input$power_design == "single") {
      if (input$power_type == "amplitude") ssm_power_amplitude(input$power_effect, current_n, input$power_alpha, one_sided = input$power_onesided)$power
      else ssm_power_elevation(input$power_effect, current_n, input$power_alpha, one_sided = input$power_onesided)$power
    } else {
      if (input$power_type == "amplitude") ssm_power_amplitude_diff(input$power_effect, input$power_n1, input$power_n2, input$power_alpha)$power
      else ssm_power_elevation_diff(input$power_effect, input$power_n1, input$power_n2, input$power_alpha)$power
    }
    
    ggplot(df, aes(x = n, y = power)) +
      geom_line(color = "#2C3E50", linewidth = 1.2) +
      geom_hline(yintercept = 0.80, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
      annotate("point", x = current_n, y = current_power, color = "#18BC9C", size = 4) +
      annotate("text", x = 450, y = 0.82, label = "80% Power", color = "#E74C3C", size = 3.5) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(x = "Sample Size (N)", y = "Statistical Power", title = paste("Power Curve for", tools::toTitleCase(input$power_type)), subtitle = paste("Effect =", input$power_effect)) +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
  })
  
  output$ss_results <- renderUI({
    req(input$ss_effect > 0, input$ss_power > 0)
    
    if (input$ss_design == "single") {
      if (input$ss_type == "amplitude") {
        result <- ssm_sample_size_amplitude(input$ss_effect, input$ss_power, input$ss_alpha, one_sided = input$ss_onesided)
      } else {
        result <- ssm_sample_size_elevation(input$ss_effect, input$ss_power, input$ss_alpha, one_sided = input$ss_onesided)
      }
      n_display <- paste("N =", result$n)
      test_type <- if (result$one_sided) "One-sided" else "Two-sided"
    } else {
      if (input$ss_type == "amplitude") {
        result <- ssm_sample_size_amplitude_diff(input$ss_effect, input$ss_power, input$ss_alpha, ratio = input$ss_ratio)
      } else {
        result <- ssm_sample_size_elevation_diff(input$ss_effect, input$ss_power, input$ss_alpha, ratio = input$ss_ratio)
      }
      n_display <- paste0("n1 = ", result$n1, ", n2 = ", result$n2)
      test_type <- "Two-sided"
    }
    
    effect_label <- ssm_effect_label(result$effect, input$ss_type)
    
    tagList(
      div(class = "row",
        div(class = "col-md-6", h2(class = "mb-0 text-primary", if (input$ss_design == "single") result$n else result$n_total), p(class = "text-muted", if (input$ss_design == "single") "Required N" else "Total N Required")),
        div(class = "col-md-6", h4(paste0(round(result$achieved_power * 100, 1), "%")), p(class = "text-muted", "Achieved Power"))
      ),
      hr(),
      tags$table(class = "table table-sm",
        tags$tbody(
          tags$tr(tags$td("Parameter:"), tags$td(strong(tools::toTitleCase(input$ss_type)))),
          tags$tr(tags$td("Effect Size:"), tags$td(paste(result$effect, paste0("(", effect_label, ")")))),
          tags$tr(tags$td("Sample Size:"), tags$td(n_display)),
          tags$tr(tags$td("Target Power:"), tags$td(paste0(result$target_power * 100, "%"))),
          tags$tr(tags$td("Alpha:"), tags$td(result$alpha)),
          tags$tr(tags$td("Test Type:"), tags$td(test_type))
        )
      )
    )
  })
  
  output$ss_plot <- renderPlot({
    effects <- seq(0.05, 0.40, by = 0.01)
    
    if (input$ss_design == "single") {
      if (input$ss_type == "amplitude") {
        sample_sizes <- sapply(effects, function(e) ssm_sample_size_amplitude(e, input$ss_power, input$ss_alpha, one_sided = input$ss_onesided)$n)
      } else {
        sample_sizes <- sapply(effects, function(e) ssm_sample_size_elevation(e, input$ss_power, input$ss_alpha, one_sided = input$ss_onesided)$n)
      }
    } else {
      if (input$ss_type == "amplitude") {
        sample_sizes <- sapply(effects, function(e) ssm_sample_size_amplitude_diff(e, input$ss_power, input$ss_alpha, ratio = input$ss_ratio)$n_total)
      } else {
        sample_sizes <- sapply(effects, function(e) ssm_sample_size_elevation_diff(e, input$ss_power, input$ss_alpha, ratio = input$ss_ratio)$n_total)
      }
    }
    
    df <- data.frame(effect = effects, n = sample_sizes)
    benchmarks <- if (input$ss_type == "amplitude") data.frame(effect = c(0.10, 0.16, 0.23), label = c("Small", "Medium", "Large")) else data.frame(effect = c(0.02, 0.11, 0.27), label = c("Small", "Medium", "Large"))
    current_n_val <- df$n[which.min(abs(df$effect - input$ss_effect))]
    
    ggplot(df, aes(x = effect, y = n)) +
      geom_line(color = "#2C3E50", linewidth = 1.2) +
      geom_vline(data = benchmarks, aes(xintercept = effect), linetype = "dashed", color = "#95A5A6", alpha = 0.7) +
      annotate("point", x = input$ss_effect, y = current_n_val, color = "#18BC9C", size = 4) +
      scale_y_log10(breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000)) +
      coord_cartesian(ylim = c(10, max(sample_sizes, na.rm = TRUE) * 1.1)) +
      labs(x = "Expected Effect Size", y = "Required Sample Size (log scale)", title = paste("Sample Size for", round(input$ss_power * 100), "% Power"), subtitle = paste(tools::toTitleCase(input$ss_type), "-", if (input$ss_design == "single") "Single Sample" else "Two Groups")) +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
  })
  
  power_table_data <- eventReactive(input$table_generate, {
    effects <- as.numeric(trimws(strsplit(input$table_effects, ",")[[1]]))
    ns <- as.numeric(trimws(strsplit(input$table_ns, ",")[[1]]))
    effects <- effects[!is.na(effects) & effects > 0]
    ns <- ns[!is.na(ns) & ns > 0]
    req(length(effects) > 0, length(ns) > 0)
    power_func <- if (input$table_type == "amplitude") ssm_power_amplitude else ssm_power_elevation
    power_mat <- matrix(NA, nrow = length(effects), ncol = length(ns))
    for (i in seq_along(effects)) {
      for (j in seq_along(ns)) {
        power_mat[i, j] <- round(power_func(effects[i], ns[j], input$table_alpha)$power, 3)
      }
    }
    df <- data.frame(Effect = effects, power_mat)
    colnames(df)[-1] <- paste0("N=", ns)
    df
  }, ignoreNULL = FALSE)
  
  output$power_table <- renderDT({
    if (input$table_generate == 0) {
      effects <- c(0.10, 0.15, 0.20, 0.25)
      ns <- c(50, 100, 150, 200, 300)
      power_func <- ssm_power_amplitude
      power_mat <- matrix(NA, nrow = length(effects), ncol = length(ns))
      for (i in seq_along(effects)) {
        for (j in seq_along(ns)) {
          power_mat[i, j] <- round(power_func(effects[i], ns[j], 0.05)$power, 3)
        }
      }
      df <- data.frame(Effect = effects, power_mat)
      colnames(df)[-1] <- paste0("N=", ns)
    } else {
      df <- power_table_data()
    }
    datatable(df, options = list(dom = 't', pageLength = 20), rownames = FALSE) %>%
      formatStyle(columns = 2:ncol(df), backgroundColor = styleInterval(c(0.5, 0.8), c('#FADBD8', '#FCF3CF', '#D5F5E3')))
  })
  
  output$power_heatmap <- renderPlot({
    if (input$table_generate == 0) {
      effects <- c(0.10, 0.15, 0.20, 0.25)
      ns <- c(50, 100, 150, 200, 300)
      type <- "amplitude"
      alpha <- 0.05
    } else {
      effects <- as.numeric(trimws(strsplit(input$table_effects, ",")[[1]]))
      ns <- as.numeric(trimws(strsplit(input$table_ns, ",")[[1]]))
      effects <- effects[!is.na(effects) & effects > 0]
      ns <- ns[!is.na(ns) & ns > 0]
      type <- input$table_type
      alpha <- input$table_alpha
    }
    power_func <- if (type == "amplitude") ssm_power_amplitude else ssm_power_elevation
    grid <- expand.grid(effect = effects, n = ns)
    grid$power <- mapply(function(e, n) power_func(e, n, alpha)$power, grid$effect, grid$n)
    ggplot(grid, aes(x = factor(n), y = factor(effect), fill = power)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", power)), color = "black", size = 4) +
      scale_fill_gradient2(low = "#E74C3C", mid = "#F39C12", high = "#27AE60", midpoint = 0.8, limits = c(0, 1), name = "Power") +
      labs(x = "Sample Size", y = "Effect Size", title = paste("Power Heatmap -", tools::toTitleCase(type))) +
      theme_minimal(base_size = 14) + theme(panel.grid = element_blank())
  })
  
  output$quick_ref_table <- renderUI({
    power <- 0.80
    amp_small <- ssm_sample_size_amplitude(0.10, power)$n
    amp_med <- ssm_sample_size_amplitude(0.16, power)$n
    amp_large <- ssm_sample_size_amplitude(0.23, power)$n
    elev_small <- ssm_sample_size_elevation(0.02, power)$n
    elev_med <- ssm_sample_size_elevation(0.11, power)$n
    elev_large <- ssm_sample_size_elevation(0.27, power)$n
    amp_diff_small <- ssm_sample_size_amplitude_diff(0.10, power)$n1
    amp_diff_med <- ssm_sample_size_amplitude_diff(0.16, power)$n1
    amp_diff_large <- ssm_sample_size_amplitude_diff(0.23, power)$n1
    tagList(
      h5("Single Sample Tests"),
      tags$table(class = "table table-sm table-striped",
        tags$thead(tags$tr(tags$th("Parameter"), tags$th("Small"), tags$th("Medium"), tags$th("Large"))),
        tags$tbody(
          tags$tr(tags$td("Amplitude"), tags$td(amp_small), tags$td(amp_med), tags$td(amp_large)),
          tags$tr(tags$td("Elevation"), tags$td(elev_small), tags$td(elev_med), tags$td(elev_large))
        )
      ),
      hr(),
      h5("Two-Group Comparisons (per group)"),
      tags$table(class = "table table-sm table-striped",
        tags$thead(tags$tr(tags$th("Parameter"), tags$th("Small"), tags$th("Medium"), tags$th("Large"))),
        tags$tbody(tags$tr(tags$td("Amplitude"), tags$td(amp_diff_small), tags$td(amp_diff_med), tags$td(amp_diff_large)))
      )
    )
  })
}

shinyApp(ui = ui, server = server)

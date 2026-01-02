# About Tab
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
            tags$li(strong("Power estimates are slightly conservative"), 
                    " for small and medium effects. Sample sizes from this toolkit will yield ",
                    em("at least"), " the targeted power."),
            tags$li(strong("Confidence intervals may undercover:"), 
                    " Normal-approximation 95% CIs achieve approximately 90% coverage due to positive bias ",
                    "in amplitude estimation and slight underestimation of SE."),
            tags$li("For critical applications requiring precise coverage, consider using bootstrap CIs.")
          )
        ),
        
        hr(),
        
        h4("Citation"),
        p("If you use this toolkit, please cite the accompanying paper:"),
        div(class = "alert alert-success",
          p(class = "mb-0", 
            strong("Gilbert, K. J. (2026)."), 
            " Power analysis for the Structural Summary Method. ",
            em("Manuscript in preparation."))
        ),
        p("And the R package:"),
        tags$blockquote(class = "blockquote",
          "Gilbert, K. J. (2026). ",
          em("ssmpower: Power Analysis for the Structural Summary Method."),
          " R package version 1.0.0. https://github.com/kimberlyjg/ssmpower"
        ),
        p(class = "text-muted", "Effect size benchmarks are from:"),
        p(class = "text-muted small",
          "Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation. ",
          em("Assessment, 24"), ", 3-23."
        )
      )
    )
  )
)
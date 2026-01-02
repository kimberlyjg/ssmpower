# Launch the SSM Power Analysis Shiny App

Opens an interactive Shiny web application for SSM power analysis and
sample size planning. The app provides a graphical interface for all
power analysis functions in this package.

## Usage

``` r
run_app(launch.browser = TRUE)
```

## Arguments

- launch.browser:

  Logical; should the app open in a browser? Default is TRUE.

## Value

This function does not return a value; it launches a Shiny app.

## Details

The Shiny app includes:

- Power Calculator: Calculate power with real-time visualization

- Sample Size Calculator: Determine required N with preset effect sizes

- Power Tables: Generate customizable power tables with heatmaps

- Quick Reference: Summary of benchmarks, formulas, and references

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the app
run_app()
} # }
```

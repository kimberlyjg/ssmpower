# Effect Size Label

Interprets effect size based on Zimmermann and Wright (2017) benchmarks

## Usage

``` r
ssm_effect_label(effect, type = c("amplitude", "elevation"))
```

## Arguments

- effect:

  Effect size value

- type:

  Either "amplitude" or "elevation"

## Value

Character label describing effect size magnitude

## Details

Benchmarks from Zimmermann & Wright (2017) based on 433 SSM analyses:

- Amplitude: Small = 0.10, Medium = 0.16, Large = 0.23

- Elevation: Small = 0.02, Medium = 0.11, Large = 0.27

## Examples

``` r
ssm_effect_label(0.12, "amplitude")
#> [1] "Small"
ssm_effect_label(0.25, "elevation")
#> [1] "Medium-Large"
```

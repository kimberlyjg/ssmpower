# Calculate SSM Parameters from Octant Scores

Computes elevation, amplitude, and displacement from a vector of 8
octant scores (typically correlations with an external variable).

## Usage

``` r
ssm_parameters(scores, angles = seq(0, 315, by = 45))
```

## Arguments

- scores:

  Numeric vector of length 8 (octant scores/correlations)

- angles:

  Numeric vector of octant angles in degrees (default: 0, 45, ..., 315)

## Value

An object of class `ssm_params` containing:

- elevation:

  Mean of the octant scores

- amplitude:

  Distance from origin to profile center

- displacement_deg:

  Angular location in degrees (0-360)

- displacement_rad:

  Angular location in radians

- x_val:

  X coordinate (Dominance axis)

- y_val:

  Y coordinate (Love/Affiliation axis)

- scores:

  Original input scores

- angles:

  Original input angles

## References

Gurtman, M. B. (1992). Construct validity of interpersonal personality
measures: The interpersonal circumplex as a nomological net. *Journal of
Personality and Social Psychology, 63*, 105-118.

## Examples

``` r
# Example: correlations of external variable with 8 IPC octants
octant_cors <- c(0.4, 0.3, 0.1, -0.1, -0.2, -0.1, 0.1, 0.3)
ssm_parameters(octant_cors)
#> 
#> SSM Parameters
#> ==============
#> Elevation:     0.100
#> Amplitude:     0.291
#> Displacement:    0.0 degrees
#> X (DOM):       0.291
#> Y (LOV):      -0.000
```

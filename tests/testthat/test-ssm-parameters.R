# Tests for ssm_parameters()

test_that("ssm_parameters validates input correctly", {
  # Not numeric

  expect_error(ssm_parameters("a"), "must be a numeric vector")
  
  # Wrong length
  expect_error(ssm_parameters(c(1, 2, 3)), "must be a vector of length 8")
  
  # Contains NA
  expect_error(ssm_parameters(c(1, 2, 3, 4, NA, 6, 7, 8)), "contains NA values")
  
  # Mismatched angles
  expect_error(ssm_parameters(rep(0.5, 8), angles = c(0, 45, 90)), 
               "must have the same length")
})


test_that("ssm_parameters returns correct structure", {
  scores <- c(0.4, 0.3, 0.1, -0.1, -0.2, -0.1, 0.1, 0.3)
  result <- ssm_parameters(scores)
  
  expect_s3_class(result, "ssm_params")
  expect_named(result, c("elevation", "amplitude", "displacement_deg", 
                         "displacement_rad", "x_val", "y_val", "scores", "angles"))
})


test_that("ssm_parameters computes elevation correctly", {
  # Uniform scores: elevation = mean, amplitude = 0
  scores <- rep(0.5, 8)
  result <- ssm_parameters(scores)
  
  expect_equal(result$elevation, 0.5)
  expect_equal(result$amplitude, 0, tolerance = 1e-10)
})


test_that("ssm_parameters computes amplitude correctly for pure profiles", {
  # Profile peaking at 0 degrees (PA)
  angles_rad <- seq(0, 315, by = 45) * pi / 180
  scores <- cos(angles_rad)  # Peaks at 0
  result <- ssm_parameters(scores)
  
  # Amplitude should be 1 for perfect cosine
  expect_equal(result$amplitude, 1, tolerance = 0.01)
  
  # Displacement should be at 0 degrees

  expect_equal(result$displacement_deg, 0, tolerance = 1)
})


test_that("ssm_parameters computes displacement correctly", {
  # Profile peaking at 90 degrees (DE)
  angles_rad <- seq(0, 315, by = 45) * pi / 180
  scores <- cos(angles_rad - pi/2)  # Peaks at 90
  result <- ssm_parameters(scores)
  
  expect_equal(result$displacement_deg, 90, tolerance = 1)
  
  # Profile peaking at 180 degrees (HI)
  scores <- cos(angles_rad - pi)  # Peaks at 180
  result <- ssm_parameters(scores)
  
  expect_equal(result$displacement_deg, 180, tolerance = 1)
})


test_that("ssm_parameters x_val and y_val are bounded", {
  # Random profiles should have x and y in reasonable range
  set.seed(42)
  for (i in 1:10) {
    scores <- runif(8, -1, 1)
    result <- ssm_parameters(scores)
    
    expect_true(result$x_val >= -1 && result$x_val <= 1)
    expect_true(result$y_val >= -1 && result$y_val <= 1)
  }
})


test_that("print.ssm_params works", {
  scores <- c(0.4, 0.3, 0.1, -0.1, -0.2, -0.1, 0.1, 0.3)
  result <- ssm_parameters(scores)
  
  expect_output(print(result), "SSM Parameters")
  expect_output(print(result), "Elevation")
  expect_output(print(result), "Amplitude")
})

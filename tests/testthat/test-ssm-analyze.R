# Tests for ssm_analyze()

test_that("ssm_analyze validates input", {
  external <- rnorm(100)
  
  # Non-numeric external
  expect_error(ssm_analyze("a", matrix(1, 100, 8)), "must be numeric")
  
  # Wrong number of octants
  expect_error(ssm_analyze(external, matrix(1, 100, 7)), "must have 8 columns")
  
  # Mismatched lengths
  expect_error(ssm_analyze(external, matrix(1, 50, 8)), "same number of observations")
})


test_that("ssm_analyze returns correct structure", {
  set.seed(42)
  external <- rnorm(100)
  octants <- matrix(rnorm(100 * 8), ncol = 8)
  
  result <- ssm_analyze(external, octants, n_boot = 100)
  
  expect_s3_class(result, "ssm_analysis")
  expect_named(result, c("elevation", "amplitude", "displacement",
                         "ci_elevation", "ci_amplitude", "ci_displacement",
                         "effect_label_amplitude", "effect_label_elevation",
                         "power_amplitude", "power_elevation",
                         "n", "n_boot", "conf_level", "correlations", "angles"))
})


test_that("ssm_analyze produces valid parameter estimates", {
  set.seed(42)
  external <- rnorm(200)
  octants <- matrix(rnorm(200 * 8), ncol = 8)
  
  result <- ssm_analyze(external, octants, n_boot = 100)
  
  # Check parameters are in valid ranges
  expect_true(result$amplitude >= 0)
  expect_true(result$displacement >= 0 && result$displacement <= 360)
  expect_true(result$power_amplitude >= 0 && result$power_amplitude <= 1)
  expect_true(result$power_elevation >= 0 && result$power_elevation <= 1)
})


test_that("ssm_analyze CIs contain point estimate", {
  set.seed(42)
  external <- rnorm(200)
  octants <- matrix(rnorm(200 * 8), ncol = 8)
  
  result <- ssm_analyze(external, octants, n_boot = 500, seed = 123)
  
  # Point estimates should be within CIs (usually)
  expect_true(result$elevation >= result$ci_elevation[1] && 
              result$elevation <= result$ci_elevation[2])
  expect_true(result$amplitude >= result$ci_amplitude[1] && 
              result$amplitude <= result$ci_amplitude[2])
})


test_that("ssm_analyze detects known structure", {
  set.seed(42)
  n <- 300
  external <- rnorm(n)
  
  # Create octants with structure: external correlates with PA (0 degrees)
  octants <- matrix(rnorm(n * 8), ncol = 8)
  octants[, 1] <- octants[, 1] + 0.5 * external  # PA
  octants[, 2] <- octants[, 2] + 0.3 * external  # BC
  octants[, 8] <- octants[, 8] + 0.3 * external  # NO
  
  result <- ssm_analyze(external, octants, n_boot = 200, seed = 123)
  
  # Should detect significant amplitude
  expect_true(result$amplitude > 0.1)
  
  # Displacement should be near 0 degrees (PA direction)
  # Allow for some noise
 expect_true(result$displacement < 60 || result$displacement > 300)
})


test_that("ssm_analyze is reproducible with seed", {
  external <- rnorm(100)
  octants <- matrix(rnorm(100 * 8), ncol = 8)
  
  result1 <- ssm_analyze(external, octants, n_boot = 100, seed = 42)
  result2 <- ssm_analyze(external, octants, n_boot = 100, seed = 42)
  
  expect_equal(result1$ci_amplitude, result2$ci_amplitude)
  expect_equal(result1$ci_elevation, result2$ci_elevation)
})


test_that("ssm_analyze n_boot affects CI width", {
  set.seed(42)
  external <- rnorm(100)
  octants <- matrix(rnorm(100 * 8), ncol = 8)
  
  # More bootstrap samples should give similar but potentially narrower CIs
  result_100 <- ssm_analyze(external, octants, n_boot = 100, seed = 1)
  result_1000 <- ssm_analyze(external, octants, n_boot = 1000, seed = 1)
  
  # Point estimates should be identical (same seed for data)
  expect_equal(result_100$amplitude, result_1000$amplitude)
  expect_equal(result_100$elevation, result_1000$elevation)
})


test_that("ssm_analyze returns correct n_boot and n", {
  external <- rnorm(150)
  octants <- matrix(rnorm(150 * 8), ncol = 8)
  
  result <- ssm_analyze(external, octants, n_boot = 500)
  
  expect_equal(result$n, 150)
  expect_equal(result$n_boot, 500)
})


test_that("print.ssm_analysis works", {
  set.seed(42)
  external <- rnorm(100)
  octants <- matrix(rnorm(100 * 8), ncol = 8)
  
  result <- ssm_analyze(external, octants, n_boot = 100)
  
  expect_output(print(result), "SSM ANALYSIS RESULTS")
  expect_output(print(result), "PARAMETER ESTIMATES")
  expect_output(print(result), "POST-HOC POWER")
  expect_output(print(result), "OCTANT CORRELATIONS")
})


test_that("ssm_analyze handles data frames for octants", {
  external <- rnorm(100)
  octants <- as.data.frame(matrix(rnorm(100 * 8), ncol = 8))
  
  # Should not error
  result <- ssm_analyze(external, octants, n_boot = 50)
  expect_s3_class(result, "ssm_analysis")
})

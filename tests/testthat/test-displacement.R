# Tests for displacement power functions

test_that("ssm_power_displacement returns expected structure", {
  result <- ssm_power_displacement(delta_diff = 30, amplitude = 0.25, n = 100)

  expect_type(result, "list")
  expect_named(result, c("power", "delta_diff", "amplitude", "n", "se",
                         "alpha", "k_delta", "two_group", "type", "design"))
  expect_equal(result$type, "displacement")
  expect_true(result$power >= 0 && result$power <= 1)
})

test_that("ssm_power_displacement handles low amplitude", {
  # Very low amplitude should return NA
  result <- suppressWarnings(ssm_power_displacement(delta_diff = 30, amplitude = 0.03, n = 100))
  expect_true(is.na(result$power))

  # Moderately low amplitude should still calculate
  result <- suppressWarnings(ssm_power_displacement(delta_diff = 30, amplitude = 0.08, n = 100))
  expect_false(is.na(result$power))
})

test_that("ssm_power_displacement respects two_group parameter", {
  result_two <- ssm_power_displacement(delta_diff = 30, amplitude = 0.25,
                                        n = 100, two_group = TRUE)
  result_one <- ssm_power_displacement(delta_diff = 30, amplitude = 0.25,
                                        n = 100, two_group = FALSE)

  # Single-sample should have smaller SE (no sqrt(2) factor)
  expect_lt(result_one$se, result_two$se)
  expect_equal(result_two$design, "two_sample")
  expect_equal(result_one$design, "single_sample")
})

test_that("ssm_sample_size_displacement returns valid sample sizes", {
  result <- ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.25)

  expect_type(result, "list")
  expect_true(result$n > 0)
  expect_true(result$n == ceiling(result$n))  # Integer
  expect_true(result$achieved_power >= result$target_power - 0.01)  # Meets target
})

test_that("ssm_sample_size_displacement increases with smaller effect", {
  n_large_diff <- ssm_sample_size_displacement(delta_diff = 45, amplitude = 0.25)$n
  n_small_diff <- ssm_sample_size_displacement(delta_diff = 15, amplitude = 0.25)$n

  expect_gt(n_small_diff, n_large_diff)
})

test_that("ssm_sample_size_displacement increases with smaller amplitude", {
  n_high_amp <- ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.40)$n
  n_low_amp <- ssm_sample_size_displacement(delta_diff = 30, amplitude = 0.20)$n

  expect_gt(n_low_amp, n_high_amp)
})

test_that("K_DISPLACEMENT is 31", {
  expect_equal(K_DISPLACEMENT, 31)
})

test_that("ssm_displacement_precision_table returns valid data frame", {
  result <- ssm_displacement_precision_table()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(ncol(result) > 1)
})

test_that("displacement SE formula is correct", {
  result <- ssm_power_displacement(delta_diff = 30, amplitude = 0.25,
                                    n = 100, two_group = FALSE)

  expected_se <- 31 / (0.25 * sqrt(100))
  expect_equal(result$se, expected_se, tolerance = 0.01)
})

test_that("two-group SE formula is correct", {
  result <- ssm_power_displacement(delta_diff = 30, amplitude = 0.25,
                                    n = 100, two_group = TRUE)

  expected_se <- 31 * sqrt(2/100) / 0.25
  expect_equal(result$se, expected_se, tolerance = 0.01)
})

test_that("ssm_power_displacement_diff handles unequal n", {
  result <- ssm_power_displacement_diff(delta_diff = 30, amplitude = 0.25,
                                         n1 = 100, n2 = 200)

  expect_type(result, "list")
  expect_true(result$power > 0)

  expected_se <- 31 * sqrt(1/100 + 1/200) / 0.25
  expect_equal(result$se, expected_se, tolerance = 0.01)
})

test_that("ssm_sample_size_displacement_diff respects ratio", {
  result_equal <- ssm_sample_size_displacement_diff(delta_diff = 30,
                                                     amplitude = 0.25, ratio = 1)
  result_2to1 <- ssm_sample_size_displacement_diff(delta_diff = 30,
                                                    amplitude = 0.25, ratio = 2)

  expect_equal(result_equal$n1, result_equal$n2)
  # Allow for ceiling differences
  expect_true(abs(result_2to1$n2 - result_2to1$n1 * 2) <= 1)
})
# Tests for sample size functions

test_that("ssm_sample_size_amplitude validates input", {
  expect_error(ssm_sample_size_amplitude(-0.1), "must be a single positive number")
  expect_error(ssm_sample_size_amplitude(0.1, power = 1.5), "must be a single number between 0 and 1")
  expect_error(ssm_sample_size_amplitude(0.1, power = 0), "must be a single number between 0 and 1")
})


test_that("ssm_sample_size_amplitude returns correct structure", {
  result <- ssm_sample_size_amplitude(0.16, power = 0.80)
  
  expect_s3_class(result, "ssm_sample_size")
  expect_equal(result$effect, 0.16)
  expect_equal(result$target_power, 0.80)
  expect_true(result$n >= 1)
  expect_true(result$achieved_power >= result$target_power)
})


test_that("ssm_sample_size_amplitude achieves target power", {
  # The returned n should actually achieve at least the target power
  result <- ssm_sample_size_amplitude(0.16, power = 0.80)
  
  # Verify by calculating power with returned n
  power_check <- ssm_power_amplitude(0.16, result$n)$power
  expect_true(power_check >= 0.80)
})


test_that("ssm_sample_size matches known benchmarks", {
  # Medium amplitude effect should need ~41 for 80% power (one-sided)
  result <- ssm_sample_size_amplitude(0.16, power = 0.80)
  expect_equal(result$n, 41)
  
  # Small amplitude effect should need ~104
  result <- ssm_sample_size_amplitude(0.10, power = 0.80)
  expect_equal(result$n, 104)
  
  # Large amplitude effect should need ~20
  result <- ssm_sample_size_amplitude(0.23, power = 0.80)
  expect_equal(result$n, 20)
})


test_that("ssm_sample_size_amplitude decreases with larger effects", {
  n_small <- ssm_sample_size_amplitude(0.10)$n
  n_medium <- ssm_sample_size_amplitude(0.16)$n
  n_large <- ssm_sample_size_amplitude(0.23)$n
  
  expect_true(n_small > n_medium)
  expect_true(n_medium > n_large)
})


test_that("ssm_sample_size_amplitude increases with higher power", {
  n_80 <- ssm_sample_size_amplitude(0.16, power = 0.80)$n
  n_90 <- ssm_sample_size_amplitude(0.16, power = 0.90)$n
  n_95 <- ssm_sample_size_amplitude(0.16, power = 0.95)$n
  
  expect_true(n_80 < n_90)
  expect_true(n_90 < n_95)
})


test_that("ssm_sample_size_elevation requires larger n than amplitude", {
  # Same effect size should require larger n for elevation
  n_amp <- ssm_sample_size_amplitude(0.16)$n
  n_elev <- ssm_sample_size_elevation(0.16)$n
  
  expect_true(n_elev > n_amp)
})


test_that("ssm_sample_size_amplitude_diff works for two-group design", {
  result <- ssm_sample_size_amplitude_diff(0.16, power = 0.80)
  
  expect_s3_class(result, "ssm_sample_size")
  expect_equal(result$design, "two_sample")
  expect_true(result$n1 >= 1)
  expect_true(result$n2 >= 1)
  expect_equal(result$n_total, result$n1 + result$n2)
})


test_that("ssm_sample_size_amplitude_diff handles allocation ratio", {
  balanced <- ssm_sample_size_amplitude_diff(0.16, ratio = 1)
  unbalanced <- ssm_sample_size_amplitude_diff(0.16, ratio = 2)
  
  expect_equal(balanced$n1, balanced$n2)
  expect_true(unbalanced$n2 > unbalanced$n1)
  
  # Unbalanced requires more total N
  expect_true(unbalanced$n_total > balanced$n_total)
})


test_that("two-sample requires larger n than single-sample", {
  # Two-sided test for both
  single <- ssm_sample_size_amplitude(0.16, one_sided = FALSE)$n
  two_group <- ssm_sample_size_amplitude_diff(0.16)$n1
  
  expect_true(two_group > single)
})


test_that("print.ssm_sample_size works", {
  result <- ssm_sample_size_amplitude(0.16)
  expect_output(print(result), "SSM Sample Size")
  expect_output(print(result), "Required n")
  expect_output(print(result), "Achieved power")
})


test_that("inverse relationship: sample_size and power functions agree", {
  # If we calculate required n for 80% power, then calculate power with that n,
  # we should get ~80% power
  
  for (effect in c(0.10, 0.16, 0.23)) {
    ss_result <- ssm_sample_size_amplitude(effect, power = 0.80)
    power_result <- ssm_power_amplitude(effect, ss_result$n)
    
    expect_equal(power_result$power, ss_result$achieved_power)
    expect_true(power_result$power >= 0.80)
  }
})

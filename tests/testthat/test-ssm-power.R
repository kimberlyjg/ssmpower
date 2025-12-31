# Tests for power functions

test_that("ssm_power_amplitude validates input", {
  expect_error(ssm_power_amplitude(-0.1, 100), "must be a single positive number")
  expect_error(ssm_power_amplitude(0.1, -100), "must be a single positive number")
  expect_error(ssm_power_amplitude(0.1, 100, alpha = 1.5), "must be a single number between 0 and 1")
  expect_error(ssm_power_amplitude(c(0.1, 0.2), 100), "must be a single positive number")
})


test_that("ssm_power_amplitude returns correct structure", {
  result <- ssm_power_amplitude(0.16, 100)
  
  expect_s3_class(result, "ssm_power")
  expect_equal(result$effect, 0.16)
  expect_equal(result$n, 100)
  expect_equal(result$type, "amplitude")
  expect_equal(result$design, "single_sample")
  expect_true(result$power >= 0 && result$power <= 1)
})


test_that("ssm_power_amplitude increases with n", {
  power_50 <- ssm_power_amplitude(0.16, 50)$power
  power_100 <- ssm_power_amplitude(0.16, 100)$power
  power_200 <- ssm_power_amplitude(0.16, 200)$power
  
  expect_true(power_50 < power_100)
  expect_true(power_100 < power_200)
})


test_that("ssm_power_amplitude increases with effect size", {
  power_small <- ssm_power_amplitude(0.10, 100)$power
  power_medium <- ssm_power_amplitude(0.16, 100)$power
  power_large <- ssm_power_amplitude(0.23, 100)$power
  
  expect_true(power_small < power_medium)
  expect_true(power_medium < power_large)
})


test_that("ssm_power_amplitude approaches 1 with large n", {
  result <- ssm_power_amplitude(0.16, 10000)
  expect_true(result$power > 0.999)
})


test_that("ssm_power_elevation uses different k constant", {
  result <- ssm_power_elevation(0.16, 100)
  
  expect_equal(result$k, 0.60)
  expect_equal(result$type, "elevation")
  
  # Same effect size should have lower power for elevation (higher k)
  power_amp <- ssm_power_amplitude(0.16, 100)$power
  power_elev <- ssm_power_elevation(0.16, 100)$power
  
  expect_true(power_elev < power_amp)
})


test_that("ssm_power_amplitude_diff works for two-sample design", {
  result <- ssm_power_amplitude_diff(0.16, n1 = 100, n2 = 100)
  
  expect_s3_class(result, "ssm_power")
  expect_equal(result$design, "two_sample")
  expect_equal(result$n1, 100)
  expect_equal(result$n2, 100)
  expect_false(result$one_sided)  # Two-sided for group comparisons
})


test_that("ssm_power_amplitude_diff handles unbalanced designs", {
  balanced <- ssm_power_amplitude_diff(0.16, n1 = 100, n2 = 100)
  unbalanced <- ssm_power_amplitude_diff(0.16, n1 = 150, n2 = 50)
  
  # Balanced should have higher power for same total N
  expect_true(balanced$power > unbalanced$power)
})


test_that("one-sided vs two-sided tests differ appropriately", {
  one_sided <- ssm_power_amplitude(0.16, 100, one_sided = TRUE)$power
  two_sided <- ssm_power_amplitude(0.16, 100, one_sided = FALSE)$power
  
  # One-sided should have higher power
  expect_true(one_sided > two_sided)
})


test_that("print.ssm_power works for single and two-sample", {
  single <- ssm_power_amplitude(0.16, 100)
  two <- ssm_power_amplitude_diff(0.16, 100, 100)
  
  expect_output(print(single), "SSM Power Analysis")
  expect_output(print(single), "single_sample")
  expect_output(print(two), "two_sample")
  expect_output(print(two), "n1 = 100, n2 = 100")
})

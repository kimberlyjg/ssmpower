# Tests for utility functions

# CI functions

test_that("ssm_ci_amplitude returns correct structure", {
  ci <- ssm_ci_amplitude(0.25, n = 200)
  
  expect_named(ci, c("lower", "upper"))
  expect_true(ci["lower"] < ci["upper"])
  expect_equal(attr(ci, "conf_level"), 0.95)
})


test_that("ssm_ci_amplitude truncates at 0", {
  # With small amplitude and small n, lower bound should be 0
  ci <- ssm_ci_amplitude(0.05, n = 20)
  
  expect_equal(ci["lower"], 0)
})


test_that("ssm_ci_amplitude narrows with larger n", {
  ci_small <- ssm_ci_amplitude(0.25, n = 50)
  ci_large <- ssm_ci_amplitude(0.25, n = 500)
  
  width_small <- ci_small["upper"] - ci_small["lower"]
  width_large <- ci_large["upper"] - ci_large["lower"]
  
  expect_true(width_large < width_small)
})


test_that("ssm_ci_elevation allows negative bounds", {
  ci <- ssm_ci_elevation(0.05, n = 20)
  
  # Lower bound can be negative for elevation
  expect_true(ci["lower"] < 0)
})


test_that("ssm_ci_elevation changes with conf_level", {
  ci_95 <- ssm_ci_elevation(0.25, n = 100, conf_level = 0.95)
  ci_99 <- ssm_ci_elevation(0.25, n = 100, conf_level = 0.99)
  
  width_95 <- ci_95["upper"] - ci_95["lower"]
  width_99 <- ci_99["upper"] - ci_99["lower"]
  
  expect_true(width_99 > width_95)
})


# Effect label functions

test_that("ssm_effect_label returns correct amplitude categories", {
  expect_equal(ssm_effect_label(0.05, "amplitude"), "< Small")
  expect_equal(ssm_effect_label(0.10, "amplitude"), "Small")
  expect_equal(ssm_effect_label(0.16, "amplitude"), "Medium")
  expect_equal(ssm_effect_label(0.23, "amplitude"), "Large")
  expect_equal(ssm_effect_label(0.30, "amplitude"), "Large")
})


test_that("ssm_effect_label returns correct elevation categories", {
  expect_equal(ssm_effect_label(0.01, "elevation"), "< Small")
  expect_equal(ssm_effect_label(0.02, "elevation"), "Small")
  expect_equal(ssm_effect_label(0.11, "elevation"), "Medium")
  expect_equal(ssm_effect_label(0.27, "elevation"), "Large")
})


test_that("ssm_effect_label handles negative values", {
  # Should use absolute value
  expect_equal(ssm_effect_label(-0.16, "amplitude"), ssm_effect_label(0.16, "amplitude"))
  expect_equal(ssm_effect_label(-0.11, "elevation"), ssm_effect_label(0.11, "elevation"))
})


test_that("ssm_effect_label validates type argument", {
  expect_error(ssm_effect_label(0.16, "invalid"), "'arg' should be one of")
})


# Power table

test_that("ssm_power_table returns correct structure", {
  result <- ssm_power_table(effects = c(0.10, 0.20), ns = c(50, 100))
  
  expect_s3_class(result, "ssm_power_table")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)  # Effect + 2 n columns
})


test_that("ssm_power_table values are in valid range", {
  result <- ssm_power_table()
  
  # All power values should be between 0 and 1
  power_cols <- result[, -1]  # Remove Effect column
  expect_true(all(power_cols >= 0 & power_cols <= 1))
})


test_that("ssm_power_table works for elevation", {
  result <- ssm_power_table(type = "elevation")
  
  expect_equal(attr(result, "type"), "elevation")
})


test_that("print.ssm_power_table works", {
  result <- ssm_power_table()
  expect_output(print(result), "SSM Power Table")
})


# Sample size guide

test_that("ssm_sample_size_guide produces output", {
  expect_output(ssm_sample_size_guide(), "QUICK REFERENCE GUIDE")
  expect_output(ssm_sample_size_guide(), "SINGLE-SAMPLE")
  expect_output(ssm_sample_size_guide(), "TWO-GROUP")
})


test_that("ssm_sample_size_guide returns data frame invisibly", {
  result <- ssm_sample_size_guide()
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)  # 3 amplitude + 3 elevation
})


test_that("ssm_sample_size_guide respects power argument", {
  # Higher power should result in larger sample sizes
  result_80 <- ssm_sample_size_guide(power = 0.80)
  result_90 <- ssm_sample_size_guide(power = 0.90)
  
  expect_true(all(result_90$N_single > result_80$N_single))
})

# =============================================================================
# SIMULATION VALIDATION OF SSM POWER FUNCTIONS
# =============================================================================
# 
# Following ADEMP framework from:
#   Morris TP, White IR, Crowther MJ (2019). Using simulation studies to 
#   evaluate statistical methods. Statistics in Medicine, 38, 2074-2102.
#   https://doi.org/10.1002/sim.8086
#
# This script validates that ssmpower functions produce accurate power 
# estimates through Monte Carlo simulation.
#
# =============================================================================

library(ssmpower)

# =============================================================================
# ADEMP: AIMS
# =============================================================================
#
# Primary aim: Validate that the analytic power functions in ssmpower produce
# accurate power estimates by comparing predicted power to empirical rejection
# rates from Monte Carlo simulation.
#
# Specifically, we evaluate:
#   1. Whether the scaling constant k = 0.41 for amplitude is accurate
#      (i.e., SE_empirical ≈ 0.41 / sqrt(n))
#   2. Whether the scaling constant k = 0.60 for elevation is accurate
#   3. Whether predicted power matches empirical rejection rates
#   4. Whether the functions work for both single-sample and two-group designs
#
# Success criteria (following Morris et al. Table 6):
#   - Empirical rejection rates within 2 MCSE of predicted power
#   - SE ratio (empirical/expected) close to 1.0
#   - Coverage of CIs close to nominal level
#
# =============================================================================


# =============================================================================
# CONFIGURATION AND n_sim JUSTIFICATION (Morris et al. Section 5.3)
# =============================================================================
#
# The Monte Carlo SE for a proportion p estimated from n_sim repetitions is:
#   MCSE = sqrt(p * (1-p) / n_sim)
#
# For power = 0.80:
#   MCSE = sqrt(0.80 * 0.20 / n_sim) = sqrt(0.16 / n_sim)
#
# To achieve MCSE < 0.01 (1 percentage point):
#   n_sim > 0.16 / 0.01^2 = 1,600
#
# We use n_sim = 5,000, giving MCSE = 0.0057 at p = 0.80
# This provides 95% CI of approximately ±1.1 percentage points.
#
# For the SE ratio validation, the MCSE of empirical SE is (Morris Table 6):
#   MCSE(EmpSE) = EmpSE / sqrt(2 * (n_sim - 1))
#
# With n_sim = 5000 and EmpSE ≈ 0.06, MCSE ≈ 0.0006, which is negligible.
#
# =============================================================================

N_SIMS <- 5000       # Justified above
ALPHA <- 0.05        # Standard significance level  
SEED <- 20240115     # For reproducibility

set.seed(SEED)

cat("\n")
cat("================================================================\n")
cat("    MONTE CARLO VALIDATION OF SSM POWER FUNCTIONS               \n")
cat("    Following ADEMP Framework (Morris et al., 2019)             \n")
cat("================================================================\n\n")

cat("CONFIGURATION\n")
cat("-------------\n")
cat(sprintf("Simulations per scenario (n_sim): %d\n", N_SIMS))
cat(sprintf("Significance level (alpha): %.3f\n", ALPHA))
cat(sprintf("Random seed: %d\n", SEED))
cat(sprintf("\nMonte Carlo SE at different power levels:\n"))
for (p in c(0.50, 0.80, 0.90, 0.95)) {
  mcse <- sqrt(p * (1 - p) / N_SIMS)
  cat(sprintf("  Power = %.0f%%: MCSE = %.4f (%.2f%%)\n", p*100, mcse, mcse*100))
}
cat("\n")


# =============================================================================
# ADEMP: DATA-GENERATING MECHANISMS
# =============================================================================
#
# We simulate circumplex data where an external variable correlates with
# 8 octant scores following a cosine curve (the SSM model).
#
# Model for each octant i:
#   octant_i = (elevation + amplitude * cos(angle_i - displacement)) * external + noise
#
# where:
#   - external ~ N(0, 1)
#   - noise ~ N(0, noise_sd^2)
#   - angles = 0, 45, 90, 135, 180, 225, 270, 315 degrees
#
# Parameters held constant across scenarios:
#   - displacement = 45 degrees (arbitrary; doesn't affect amplitude/elevation tests)
#   - noise_sd = 1 (standard normal residuals)
#   - alpha = 0.05
#
# Parameters varied:
#   - Sample size: n ∈ {20, 41, 50, 104, 150, 200, 300}
#   - Effect size (amplitude): a ∈ {0.10, 0.16, 0.23} (small, medium, large benchmarks)
#   - Effect size (elevation): e ∈ {0.11, 0.27} (medium, large - small requires huge n)
#
# Design: Fully factorial for amplitude scenarios
#
# =============================================================================

#' Simulate circumplex data with known SSM parameters
#' 
#' @param n Sample size
#' @param amplitude True population amplitude  
#' @param displacement_deg True displacement in degrees (default 45)
#' @param elevation True elevation (default 0 for amplitude-only tests)
#' @param noise_sd Standard deviation of residual noise (default 1)
#' @return List with external variable and octant score matrix
simulate_ssm_data <- function(n, amplitude, displacement_deg = 45, 
                              elevation = 0, noise_sd = 1) {
  angles <- seq(0, 315, by = 45)
  angles_rad <- angles * pi / 180
  displacement_rad <- displacement_deg * pi / 180
  
  # External variable (standardized)
  external <- rnorm(n, mean = 0, sd = 1)
  
  # Generate octant scores with circumplex structure
  octants <- matrix(NA, nrow = n, ncol = 8)
  for (i in 1:8) {
    # True loading at this angle (Gurtman, 1992 model)
    true_loading <- elevation + amplitude * cos(angles_rad[i] - displacement_rad)
    # Octant score = loading * external + noise
    octants[, i] <- true_loading * external + rnorm(n, sd = noise_sd)
  }
  
  list(external = external, octants = octants)
}


# =============================================================================
# ADEMP: ESTIMANDS
# =============================================================================
#
# Our estimands are:
#   1. SSM amplitude - the "distinctiveness" of the circumplex profile
#   2. SSM elevation - the average correlation across octants
#
# These are computed from the correlations between the external variable
# and the 8 octant scores using the formulas in Gurtman (1992):
#
#   X = (2/8) * sum(r_i * cos(theta_i))
#   Y = (2/8) * sum(r_i * sin(theta_i))
#   amplitude = sqrt(X^2 + Y^2)
#   elevation = mean(r_i)
#
# We evaluate whether tests of H0: amplitude = 0 (or elevation = 0)
# reject at the rate predicted by our power functions.
#
# =============================================================================


# =============================================================================
# ADEMP: METHODS
# =============================================================================
#
# We evaluate the power functions in ssmpower:
#   - ssm_power_amplitude() for single-sample amplitude tests
#   - ssm_power_elevation() for single-sample elevation tests  
#   - ssm_power_amplitude_diff() for two-group amplitude comparisons
#
# The test procedure for each simulation:
#   1. Simulate data with known amplitude/elevation
#   2. Compute octant correlations r_i = cor(external, octant_i)
#   3. Calculate SSM parameters using ssm_parameters()
#   4. Compute z-statistic: z = estimate / SE, where SE = k / sqrt(n)
#   5. Reject if z > z_crit (one-sided for amplitude) or |z| > z_crit (two-sided)
#
# The key question: Does the scaling constant k produce accurate SEs?
#
# =============================================================================


# =============================================================================
# ADEMP: PERFORMANCE MEASURES (Morris et al. Table 6)
# =============================================================================
#
# Primary performance measures:
#
# 1. Empirical rejection rate (power)
#    - Estimate: mean(rejections)
#    - MCSE: sqrt(p_hat * (1 - p_hat) / n_sim)
#
# 2. Bias of power estimate
#    - Estimate: empirical_power - predicted_power  
#    - Target: should be close to 0
#
# 3. SE ratio (validates scaling constant)
#    - Estimate: empirical_SE / expected_SE where expected_SE = k / sqrt(n)
#    - MCSE: EmpSE / sqrt(2 * (n_sim - 1))
#    - Target: should be close to 1.0
#
# 4. Coverage (for CI validation)
#    - Estimate: proportion of CIs containing true value
#    - MCSE: sqrt(coverage * (1 - coverage) / n_sim)
#    - Target: should be close to nominal (e.g., 95%)
#
# Secondary performance measures:
#   - Bias-eliminated coverage (Morris et al. new measure)
#   - Mean estimated amplitude/elevation
#
# =============================================================================


# =============================================================================
# SIMULATION FUNCTIONS
# =============================================================================

#' Run full validation for amplitude power
#' 
#' Returns all performance measures from Morris et al. Table 6
#' 
#' @param n Sample size
#' @param true_amplitude True population amplitude
#' @param n_sims Number of simulation repetitions
#' @param k Scaling constant (default 0.41)
#' @param alpha Significance level (default 0.05)
#' @param one_sided Use one-sided test (default TRUE for amplitude)
#' @return Data frame with all performance measures and MCSEs
validate_amplitude <- function(n, true_amplitude, n_sims = N_SIMS,
                               k = 0.41, alpha = ALPHA, one_sided = TRUE) {
  
  # Get predicted power from our function
  predicted <- ssm_power_amplitude(true_amplitude, n, alpha, k, one_sided)$power
  
  # Storage for simulation results
  amplitudes <- numeric(n_sims)      # Point estimates
  model_ses <- numeric(n_sims)       # Model-based SEs
  rejections <- logical(n_sims)      # Test decisions
  ci_covers <- logical(n_sims)       # CI coverage
  z_stats <- numeric(n_sims)         # z-statistics (for zip plot)
  
  # Expected SE based on scaling constant
  expected_se <- k / sqrt(n)
  
  # Run simulations
  for (i in 1:n_sims) {
    # Generate data
    data <- simulate_ssm_data(n, amplitude = true_amplitude)
    
    # Compute correlations and SSM parameters
    cors <- sapply(1:8, function(j) cor(data$external, data$octants[, j]))
    params <- ssm_parameters(cors)
    
    # Store estimates
    amplitudes[i] <- params$amplitude
    model_ses[i] <- expected_se  # Using analytic SE
    
    # Compute test statistic
    z_stats[i] <- params$amplitude / expected_se
    
    # Test decision
    if (one_sided) {
      z_crit <- qnorm(1 - alpha)
      rejections[i] <- z_stats[i] > z_crit
    } else {
      z_crit <- qnorm(1 - alpha/2)
      rejections[i] <- abs(z_stats[i]) > z_crit
    }
    
    # CI coverage (using analytic SE)
    ci_lower <- max(0, params$amplitude - qnorm(0.975) * expected_se)
    ci_upper <- params$amplitude + qnorm(0.975) * expected_se
    ci_covers[i] <- (ci_lower <= true_amplitude) & (true_amplitude <= ci_upper)
  }
  
  # ===== COMPUTE PERFORMANCE MEASURES (Morris Table 6) =====
  
  # 1. Empirical power (rejection rate)
  empirical_power <- mean(rejections)
  mcse_power <- sqrt(empirical_power * (1 - empirical_power) / n_sims)
  
  # 2. Bias of amplitude estimator
  mean_amplitude <- mean(amplitudes)
  bias <- mean_amplitude - true_amplitude
  
  # 3. Empirical SE and its MCSE
  empirical_se <- sd(amplitudes)
  mcse_empse <- empirical_se / sqrt(2 * (n_sims - 1))
  
  # 4. SE ratio (key validation of scaling constant)
  se_ratio <- empirical_se / expected_se
  
  # 5. Coverage
  coverage <- mean(ci_covers)
  mcse_coverage <- sqrt(coverage * (1 - coverage) / n_sims)
  
  # 6. Bias-eliminated coverage (Morris et al. new measure)
  # Use mean of estimates instead of true value
  be_ci_covers <- sapply(1:n_sims, function(i) {
    ci_lower <- max(0, amplitudes[i] - qnorm(0.975) * expected_se)
    ci_upper <- amplitudes[i] + qnorm(0.975) * expected_se
    (ci_lower <= mean_amplitude) & (mean_amplitude <= ci_upper)
  })
  be_coverage <- mean(be_ci_covers)
  
  # Return comprehensive results
  data.frame(
    # Scenario
    parameter = "amplitude",
    design = "single_sample",
    n = n,
    true_effect = true_amplitude,
    
    # Power validation
    predicted_power = predicted,
    empirical_power = empirical_power,
    mcse_power = mcse_power,
    power_diff = empirical_power - predicted,
    within_2mcse = abs(empirical_power - predicted) < 2 * mcse_power,
    
    # Estimator properties
    mean_estimate = mean_amplitude,
    bias = bias,
    
    # SE validation (key for scaling constant)
    empirical_se = empirical_se,
    expected_se = expected_se,
    mcse_empse = mcse_empse,
    se_ratio = se_ratio,
    
    # Coverage
    coverage = coverage,
    mcse_coverage = mcse_coverage,
    be_coverage = be_coverage,
    
    # For zip plot
    z_stats = I(list(z_stats)),
    ci_covers = I(list(ci_covers))
  )
}


#' Run validation for elevation power
validate_elevation <- function(n, true_elevation, n_sims = N_SIMS,
                               k = 0.60, alpha = ALPHA) {
  
  predicted <- ssm_power_elevation(true_elevation, n, alpha, k, one_sided = FALSE)$power
  
  elevations <- numeric(n_sims)
  rejections <- logical(n_sims)
  ci_covers <- logical(n_sims)
  z_stats <- numeric(n_sims)
  
  expected_se <- k / sqrt(n)
  
  for (i in 1:n_sims) {
    # For elevation test: data where elevation is the signal (no amplitude)
    external <- rnorm(n)
    octants <- matrix(NA, nrow = n, ncol = 8)
    for (j in 1:8) {
      # All octants have same loading (pure elevation)
      octants[, j] <- true_elevation * external + rnorm(n)
    }
    
    cors <- sapply(1:8, function(j) cor(external, octants[, j]))
    params <- ssm_parameters(cors)
    
    elevations[i] <- params$elevation
    z_stats[i] <- params$elevation / expected_se
    
    # Two-sided test for elevation
    rejections[i] <- abs(z_stats[i]) > qnorm(1 - alpha/2)
    
    # CI coverage
    ci_lower <- params$elevation - qnorm(0.975) * expected_se
    ci_upper <- params$elevation + qnorm(0.975) * expected_se
    ci_covers[i] <- (ci_lower <= true_elevation) & (true_elevation <= ci_upper)
  }
  
  empirical_power <- mean(rejections)
  mcse_power <- sqrt(empirical_power * (1 - empirical_power) / n_sims)
  
  mean_elevation <- mean(elevations)
  empirical_se <- sd(elevations)
  mcse_empse <- empirical_se / sqrt(2 * (n_sims - 1))
  
  coverage <- mean(ci_covers)
  mcse_coverage <- sqrt(coverage * (1 - coverage) / n_sims)
  
  data.frame(
    parameter = "elevation",
    design = "single_sample",
    n = n,
    true_effect = true_elevation,
    predicted_power = predicted,
    empirical_power = empirical_power,
    mcse_power = mcse_power,
    power_diff = empirical_power - predicted,
    within_2mcse = abs(empirical_power - predicted) < 2 * mcse_power,
    mean_estimate = mean_elevation,
    bias = mean_elevation - true_elevation,
    empirical_se = empirical_se,
    expected_se = expected_se,
    mcse_empse = mcse_empse,
    se_ratio = empirical_se / expected_se,
    coverage = coverage,
    mcse_coverage = mcse_coverage,
    be_coverage = NA,  # Skip for brevity
    z_stats = I(list(z_stats)),
    ci_covers = I(list(ci_covers))
  )
}


#' Run validation for two-group amplitude difference
validate_amplitude_diff <- function(n1, n2, true_diff, n_sims = N_SIMS,
                                    k = 0.41, alpha = ALPHA) {
  
  predicted <- ssm_power_amplitude_diff(true_diff, n1, n2, alpha, k)$power
  
  diffs <- numeric(n_sims)
  rejections <- logical(n_sims)
  
  base_amplitude <- 0.20  # Arbitrary baseline
  expected_se <- k * sqrt(1/n1 + 1/n2)
  
  for (i in 1:n_sims) {
    # Group 1
    data1 <- simulate_ssm_data(n1, amplitude = base_amplitude)
    cors1 <- sapply(1:8, function(j) cor(data1$external, data1$octants[, j]))
    amp1 <- ssm_parameters(cors1)$amplitude
    
    # Group 2 (lower amplitude)
    data2 <- simulate_ssm_data(n2, amplitude = base_amplitude - true_diff)
    cors2 <- sapply(1:8, function(j) cor(data2$external, data2$octants[, j]))
    amp2 <- ssm_parameters(cors2)$amplitude
    
    diffs[i] <- amp1 - amp2
    
    z <- abs(amp1 - amp2) / expected_se
    rejections[i] <- z > qnorm(1 - alpha/2)
  }
  
  empirical_power <- mean(rejections)
  mcse_power <- sqrt(empirical_power * (1 - empirical_power) / n_sims)
  
  empirical_se <- sd(diffs)
  
  data.frame(
    parameter = "amplitude_diff",
    design = "two_sample",
    n = paste0(n1, "+", n2),
    true_effect = true_diff,
    predicted_power = predicted,
    empirical_power = empirical_power,
    mcse_power = mcse_power,
    power_diff = empirical_power - predicted,
    within_2mcse = abs(empirical_power - predicted) < 2 * mcse_power,
    mean_estimate = mean(diffs),
    bias = mean(diffs) - true_diff,
    empirical_se = empirical_se,
    expected_se = expected_se,
    mcse_empse = empirical_se / sqrt(2 * (n_sims - 1)),
    se_ratio = empirical_se / expected_se,
    coverage = NA,
    mcse_coverage = NA,
    be_coverage = NA,
    z_stats = I(list(numeric(0))),
    ci_covers = I(list(logical(0)))
  )
}


# =============================================================================
# RUN SIMULATIONS
# =============================================================================

cat("================================================================\n")
cat("                    RUNNING SIMULATIONS                         \n")
cat("================================================================\n\n")

# -----------------------------------------------------------------------------
# VALIDATION 1: Single-sample amplitude (k = 0.41)
# -----------------------------------------------------------------------------

cat("VALIDATION 1: Single-Sample Amplitude (k = 0.41)\n")
cat("------------------------------------------------\n")
cat("Testing whether SE = 0.41/sqrt(n) is accurate\n\n")

# Fully factorial design
amplitude_scenarios <- expand.grid(
  effect = c(0.10, 0.16, 0.23),  # Small, medium, large benchmarks
  n = c(20, 41, 50, 104, 150, 200)
)

amplitude_results <- do.call(rbind, lapply(1:nrow(amplitude_scenarios), function(i) {
  result <- validate_amplitude(
    n = amplitude_scenarios$n[i],
    true_amplitude = amplitude_scenarios$effect[i]
  )
  
  cat(sprintf("  n=%3d, a=%.2f: Power: Pred=%.3f Emp=%.3f (±%.3f) | SE ratio=%.2f | Cov=%.1f%%\n",
              result$n, result$true_effect, 
              result$predicted_power, result$empirical_power, 
              1.96 * result$mcse_power,
              result$se_ratio,
              result$coverage * 100))
  
  # Remove list columns for summary (keep for plotting)
  result[, !names(result) %in% c("z_stats", "ci_covers")]
}))

cat(sprintf("\n  SUMMARY:\n"))
cat(sprintf("    Scenarios within 2 MCSE: %d/%d (%.1f%%)\n",
            sum(amplitude_results$within_2mcse), nrow(amplitude_results),
            100 * mean(amplitude_results$within_2mcse)))
cat(sprintf("    Mean |power difference|: %.4f\n", mean(abs(amplitude_results$power_diff))))
cat(sprintf("    Mean SE ratio: %.3f (target: 1.0)\n", mean(amplitude_results$se_ratio)))
cat(sprintf("    Mean coverage: %.1f%% (target: 95%%)\n\n", mean(amplitude_results$coverage) * 100))


# -----------------------------------------------------------------------------
# VALIDATION 2: Critical target power check (the key test)
# -----------------------------------------------------------------------------

cat("VALIDATION 2: Target Power Check\n")
cat("---------------------------------\n")
cat("Critical test: n=41 should give 80% power for amplitude=0.16\n\n")

# More repetitions for precision on this key check
target_result <- validate_amplitude(
  n = 41, 
  true_amplitude = 0.16,
  n_sims = 10000
)

cat(sprintf("  Sample size:      n = 41\n"))
cat(sprintf("  Effect size:      amplitude = 0.16 (medium)\n"))
cat(sprintf("  Target power:     80%%\n\n"))
cat(sprintf("  Predicted power:  %.4f\n", target_result$predicted_power))
cat(sprintf("  Empirical power:  %.4f\n", target_result$empirical_power))
cat(sprintf("  MCSE:             %.4f\n", target_result$mcse_power))
cat(sprintf("  95%% CI:           [%.4f, %.4f]\n",
            target_result$empirical_power - 1.96 * target_result$mcse_power,
            target_result$empirical_power + 1.96 * target_result$mcse_power))
cat(sprintf("  Difference:       %+.4f\n", target_result$power_diff))
cat(sprintf("  Within 2 MCSE:    %s\n\n", 
            ifelse(abs(target_result$power_diff) < 2 * target_result$mcse_power, "YES ✓", "NO ✗")))


# -----------------------------------------------------------------------------
# VALIDATION 3: Single-sample elevation (k = 0.60)
# -----------------------------------------------------------------------------

cat("VALIDATION 3: Single-Sample Elevation (k = 0.60)\n")
cat("------------------------------------------------\n")

elevation_scenarios <- expand.grid(
  effect = c(0.11, 0.27),  # Medium, large (skip small - requires n > 7000)
  n = c(31, 100, 185, 300)
)

elevation_results <- do.call(rbind, lapply(1:nrow(elevation_scenarios), function(i) {
  result <- validate_elevation(
    n = elevation_scenarios$n[i],
    true_elevation = elevation_scenarios$effect[i]
  )
  
  cat(sprintf("  n=%3d, e=%.2f: Power: Pred=%.3f Emp=%.3f (±%.3f) | SE ratio=%.2f\n",
              result$n, result$true_effect,
              result$predicted_power, result$empirical_power,
              1.96 * result$mcse_power,
              result$se_ratio))
  
  result[, !names(result) %in% c("z_stats", "ci_covers")]
}))

cat(sprintf("\n  SUMMARY:\n"))
cat(sprintf("    Scenarios within 2 MCSE: %d/%d (%.1f%%)\n",
            sum(elevation_results$within_2mcse), nrow(elevation_results),
            100 * mean(elevation_results$within_2mcse)))
cat(sprintf("    Mean SE ratio: %.3f (target: 1.0)\n\n", mean(elevation_results$se_ratio)))


# -----------------------------------------------------------------------------
# VALIDATION 4: Two-group amplitude comparison
# -----------------------------------------------------------------------------

cat("VALIDATION 4: Two-Group Amplitude Comparison\n")
cat("---------------------------------------------\n")

twogroup_scenarios <- data.frame(
  n1 = c(82, 132, 200),
  n2 = c(82, 132, 200),
  effect = c(0.16, 0.10, 0.10)
)

twogroup_results <- do.call(rbind, lapply(1:nrow(twogroup_scenarios), function(i) {
  result <- validate_amplitude_diff(
    n1 = twogroup_scenarios$n1[i],
    n2 = twogroup_scenarios$n2[i],
    true_diff = twogroup_scenarios$effect[i]
  )
  
  cat(sprintf("  n=%s, d=%.2f: Power: Pred=%.3f Emp=%.3f (±%.3f) | SE ratio=%.2f\n",
              result$n, result$true_effect,
              result$predicted_power, result$empirical_power,
              1.96 * result$mcse_power,
              result$se_ratio))
  
  result[, !names(result) %in% c("z_stats", "ci_covers")]
}))

cat(sprintf("\n  SUMMARY:\n"))
cat(sprintf("    Scenarios within 2 MCSE: %d/%d\n\n",
            sum(twogroup_results$within_2mcse), nrow(twogroup_results)))


# =============================================================================
# RESULTS TABLE (Morris et al. Table 8 style)
# =============================================================================

cat("================================================================\n")
cat("      RESULTS TABLE (Morris et al., 2019, Table 8 style)        \n")
cat("================================================================\n\n")

all_results <- rbind(amplitude_results, elevation_results, twogroup_results)

# Print formatted table
cat(sprintf("%-12s %7s %6s %9s %9s %7s %7s %7s %7s\n",
            "Parameter", "n", "Effect", "Pred.Pwr", "Emp.Pwr", "MCSE", "Diff", "SE.Rat", "Cover"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (i in 1:nrow(all_results)) {
  cov_str <- if (is.na(all_results$coverage[i])) "  --" else sprintf("%5.1f%%", all_results$coverage[i] * 100)
  cat(sprintf("%-12s %7s %6.2f %9.3f %9.3f %7.4f %+7.4f %7.2f %s\n",
              all_results$parameter[i],
              all_results$n[i],
              all_results$true_effect[i],
              all_results$predicted_power[i],
              all_results$empirical_power[i],
              all_results$mcse_power[i],
              all_results$power_diff[i],
              all_results$se_ratio[i],
              cov_str))
}

cat("\nNote: MCSE = Monte Carlo Standard Error; SE.Rat = Empirical SE / Expected SE\n")
cat("      Cover = 95% CI coverage; Diff = Empirical - Predicted power\n")


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("                    VALIDATION SUMMARY                          \n")
cat("================================================================\n\n")

n_total <- nrow(all_results)
n_ok <- sum(all_results$within_2mcse)

cat("POWER VALIDATION\n")
cat("----------------\n")
cat(sprintf("Total scenarios tested:          %d\n", n_total))
cat(sprintf("Within 2 MCSE of predicted:      %d (%.1f%%)\n", n_ok, 100 * n_ok / n_total))
cat(sprintf("Mean |predicted - empirical|:    %.4f\n", mean(abs(all_results$power_diff))))
cat(sprintf("Max  |predicted - empirical|:    %.4f\n", max(abs(all_results$power_diff))))

cat("\nSCALING CONSTANT VALIDATION (SE ratio should be ~1.0)\n")
cat("-----------------------------------------------------\n")
cat(sprintf("Amplitude (k=0.41): Mean SE ratio = %.3f (SD = %.3f)\n",
            mean(amplitude_results$se_ratio), sd(amplitude_results$se_ratio)))
cat(sprintf("Elevation (k=0.60): Mean SE ratio = %.3f (SD = %.3f)\n",
            mean(elevation_results$se_ratio), sd(elevation_results$se_ratio)))

cat("\nCOVERAGE VALIDATION (should be ~95%%)\n")
cat("-------------------------------------\n")
amp_cov <- amplitude_results$coverage[!is.na(amplitude_results$coverage)]
cat(sprintf("Amplitude CIs: Mean coverage = %.1f%% (range: %.1f%% - %.1f%%)\n",
            mean(amp_cov) * 100, min(amp_cov) * 100, max(amp_cov) * 100))

cat("\n")
cat("INTERPRETATION GUIDE\n")
cat("--------------------\n")
cat("✓ Power validation passes if >95% of scenarios are within 2 MCSE\n")
cat("✓ Scaling constants validated if SE ratio is close to 1.0 (±0.05)\n")
cat("✓ CI procedure validated if coverage is close to 95% (±2%)\n")
cat("\n")

if (n_ok / n_total >= 0.95 && 
    abs(mean(amplitude_results$se_ratio) - 1) < 0.05 &&
    abs(mean(elevation_results$se_ratio) - 1) < 0.05) {
  cat("CONCLUSION: All validation checks PASSED ✓\n")
  cat("The power functions and scaling constants are empirically validated.\n")
} else {
  cat("CONCLUSION: Some validation checks may need attention.\n")
  cat("Review the detailed results above.\n")
}

cat("================================================================\n")


# =============================================================================
# GRAPHICAL OUTPUT (Morris et al. Section 5.1, 6.2)
# =============================================================================

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  cat("\nGenerating validation plots...\n")
  
  # ----- Plot 1: Predicted vs Empirical Power -----
  plot_data <- all_results
  plot_data$parameter <- factor(plot_data$parameter,
                                 levels = c("amplitude", "elevation", "amplitude_diff"),
                                 labels = c("Amplitude", "Elevation", "Amplitude (2-group)"))
  
  p1 <- ggplot(plot_data, aes(x = predicted_power, y = empirical_power)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = empirical_power - 1.96*mcse_power, 
                      ymax = empirical_power + 1.96*mcse_power),
                  width = 0.02, alpha = 0.5) +
    geom_point(aes(color = parameter), size = 2.5) +
    scale_color_brewer(palette = "Set1", name = "Parameter") +
    labs(
      title = "Validation: Predicted vs Empirical Power",
      subtitle = sprintf("n_sim = %d; error bars = 95%% CI based on MCSE", N_SIMS),
      x = "Predicted Power (from ssm_power_* functions)",
      y = "Empirical Power (Monte Carlo)"
    ) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("validation_01_power.png", p1, width = 7, height = 7, dpi = 150)
  
  
  # ----- Plot 2: SE Ratio Validation -----
  se_data <- rbind(
    data.frame(parameter = "Amplitude (k=0.41)", 
               n = amplitude_results$n, 
               se_ratio = amplitude_results$se_ratio),
    data.frame(parameter = "Elevation (k=0.60)", 
               n = elevation_results$n, 
               se_ratio = elevation_results$se_ratio)
  )
  
  p2 <- ggplot(se_data, aes(x = n, y = se_ratio, color = parameter)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = c(0.95, 1.05), linetype = "dotted", color = "gray70") +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(
      title = "Scaling Constant Validation",
      subtitle = "SE ratio = Empirical SE / Expected SE; should be ~1.0",
      x = "Sample Size (n)",
      y = "SE Ratio"
    ) +
    ylim(0.8, 1.2) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("validation_02_se_ratio.png", p2, width = 8, height = 5, dpi = 150)
  
  
  # ----- Plot 3: Coverage -----
  cov_data <- amplitude_results[!is.na(amplitude_results$coverage), ]
  
  p3 <- ggplot(cov_data, aes(x = factor(n), y = coverage * 100)) +
    geom_hline(yintercept = 95, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = c(93, 97), linetype = "dotted", color = "gray70") +
    geom_errorbar(aes(ymin = (coverage - 1.96*mcse_coverage) * 100,
                      ymax = (coverage + 1.96*mcse_coverage) * 100),
                  width = 0.2) +
    geom_point(aes(color = factor(true_effect)), size = 3) +
    scale_color_brewer(palette = "Set1", name = "True Amplitude") +
    labs(
      title = "Coverage Validation for Amplitude CIs",
      subtitle = "95% CIs should contain true value 95% of the time",
      x = "Sample Size (n)",
      y = "Coverage (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("validation_03_coverage.png", p3, width = 8, height = 5, dpi = 150)
  
  cat("Plots saved: validation_01_power.png, validation_02_se_ratio.png, validation_03_coverage.png\n")
}


# =============================================================================
# SAVE RESULTS
# =============================================================================

results_summary <- list(
  amplitude = amplitude_results,
  elevation = elevation_results,
  two_group = twogroup_results,
  target_check = target_result[, !names(target_result) %in% c("z_stats", "ci_covers")],
  config = list(
    n_sims = N_SIMS,
    alpha = ALPHA,
    seed = SEED
  ),
  summary = data.frame(
    n_scenarios = n_total,
    n_within_2mcse = n_ok,
    pct_within_2mcse = 100 * n_ok / n_total,
    mean_abs_power_diff = mean(abs(all_results$power_diff)),
    max_abs_power_diff = max(abs(all_results$power_diff)),
    amplitude_mean_se_ratio = mean(amplitude_results$se_ratio),
    amplitude_sd_se_ratio = sd(amplitude_results$se_ratio),
    elevation_mean_se_ratio = mean(elevation_results$se_ratio),
    elevation_sd_se_ratio = sd(elevation_results$se_ratio),
    amplitude_mean_coverage = mean(amplitude_results$coverage, na.rm = TRUE)
  )
)

saveRDS(results_summary, "simulation_validation_morris.rds")
write.csv(all_results, "simulation_validation_results.csv", row.names = FALSE)

cat("\nResults saved to:\n")
cat("  - simulation_validation_morris.rds (full results)\n")
cat("  - simulation_validation_results.csv (summary table)\n")
cat("\nSimulation study complete.\n")

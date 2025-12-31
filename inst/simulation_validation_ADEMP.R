# =============================================================================
# SIMULATION VALIDATION OF SSM POWER FUNCTIONS
# =============================================================================
# 
# Following ADEMP framework from:
#   Morris TP, White IR, Crowther MJ (2019). Using simulation studies to 
#   evaluate statistical methods. Statistics in Medicine, 38, 2074-2102.
#   https://doi.org/10.1002/sim.8086
#
# =============================================================================

library(ssmpower)

# =============================================================================
# ADEMP: AIMS
# =============================================================================
# 
# Primary aim: Validate that the power functions in ssmpower produce accurate
# power estimates by comparing predicted power to empirical rejection rates
# from Monte Carlo simulation.
#
# Specifically, we test whether:
#   1. The scaling constant k = 0.41 for amplitude is accurate
#   2. The scaling constant k = 0.60 for elevation is accurate
#   3. Power estimates are accurate across effect sizes and sample sizes
#   4. The functions work for both single-sample and two-group designs
#
# Success criterion: Empirical rejection rates should fall within 2 Monte Carlo
# SEs of predicted power for >95% of scenarios tested.
#
# =============================================================================

# =============================================================================
# CONFIGURATION AND n_sim JUSTIFICATION
# =============================================================================
#
# Following Morris et al. (2019) Section 5.3, we choose n_sim based on required
# Monte Carlo SE precision.
#
# For power/rejection rate, MCSE = sqrt(p(1-p)/n_sim)
# 
# At p = 0.80 (our target power):
#   MCSE = sqrt(0.80 * 0.20 / n_sim)
#
# We want MCSE < 0.01 (1 percentage point):
#   n_sim > 0.16 / 0.01^2 = 1600
#
# Using n_sim = 5000 gives MCSE = 0.0057 at p = 0.80, which provides
# 95% CI of ±1.1 percentage points. This is acceptable precision.
#
# For coverage at p = 0.95, MCSE = sqrt(0.95 * 0.05 / 5000) = 0.003
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
cat(sprintf("Expected MCSE at 80%% power: %.4f (%.2f%%)\n", 
            sqrt(0.80 * 0.20 / N_SIMS), 100 * sqrt(0.80 * 0.20 / N_SIMS)))
cat("\n")


# =============================================================================
# ADEMP: DATA-GENERATING MECHANISMS
# =============================================================================
#
# We simulate circumplex data where an external variable correlates with
# 8 octant scores following a cosine curve (the SSM model).
#
# Model: octant_i = (elevation + amplitude * cos(angle_i - displacement)) * external + noise
#
# Parameters held constant:
#   - displacement = 45 degrees (arbitrary; doesn't affect amplitude/elevation)
#   - noise_sd = 1 (standard normal residuals)
#   - external variable ~ N(0, 1)
#
# Parameters varied (fully factorial where feasible):
#   - Sample size: n ∈ {20, 41, 50, 104, 150, 200}
#   - Effect size (amplitude): a ∈ {0.10, 0.16, 0.23} (small, medium, large)
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
    # True loading at this angle
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
# Our estimand is the SSM amplitude (or elevation), computed from the 
# correlations between the external variable and the 8 octant scores.
#
# We evaluate whether tests of H0: amplitude = 0 (or elevation = 0)
# reject at the rate predicted by our power functions.
#
# =============================================================================


# =============================================================================
# ADEMP: METHODS
# =============================================================================
#
# We evaluate the power functions:
#   - ssm_power_amplitude() for single-sample amplitude tests
#   - ssm_power_elevation() for single-sample elevation tests  
#   - ssm_power_amplitude_diff() for two-group amplitude comparisons
#
# The test procedure for each simulation:
#   1. Simulate data with known amplitude/elevation
#   2. Compute octant correlations
#   3. Calculate SSM parameters using ssm_parameters()
#   4. Compute z-statistic: z = estimate / SE, where SE = k / sqrt(n)
#   5. Reject if z > z_crit (one-sided) or |z| > z_crit (two-sided)
#
# =============================================================================


# =============================================================================
# ADEMP: PERFORMANCE MEASURES
# =============================================================================
#
# Primary performance measure: Empirical rejection rate vs predicted power
#
# For each scenario, we compute:
#   - Predicted power from ssm_power_*() functions
#   - Empirical rejection rate from n_sim simulations
#   - Monte Carlo SE of empirical rate: sqrt(p_hat * (1 - p_hat) / n_sim)
#   - Difference: empirical - predicted
#   - Whether |difference| < 2 * MCSE (coverage check)
#
# Secondary performance measures:
#   - Mean estimated amplitude across simulations
#   - Empirical SE of amplitude estimates
#   - Comparison to expected SE = k / sqrt(n)
#
# =============================================================================


# =============================================================================
# SIMULATION FUNCTIONS
# =============================================================================

#' Run validation for amplitude power
#' 
#' @param n Sample size
#' @param true_amplitude True population amplitude
#' @param n_sims Number of simulation repetitions
#' @param k Scaling constant (default 0.41)
#' @param alpha Significance level (default 0.05)
#' @param one_sided Use one-sided test (default TRUE)
#' @return Data frame with validation results
validate_amplitude <- function(n, true_amplitude, n_sims = N_SIMS,
                               k = 0.41, alpha = ALPHA, one_sided = TRUE) {
  
  # Get predicted power
  predicted <- ssm_power_amplitude(true_amplitude, n, alpha, k, one_sided)$power
  
  # Storage for simulation results
  amplitudes <- numeric(n_sims)
  rejections <- logical(n_sims)
  
  # Run simulations
  for (i in 1:n_sims) {
    # Generate data
    data <- simulate_ssm_data(n, amplitude = true_amplitude)
    
    # Compute correlations and SSM parameters
    cors <- sapply(1:8, function(j) cor(data$external, data$octants[, j]))
    params <- ssm_parameters(cors)
    
    # Store amplitude estimate
    amplitudes[i] <- params$amplitude
    
    # Compute test statistic and rejection decision
    se <- k / sqrt(n)
    z <- params$amplitude / se
    
    if (one_sided) {
      z_crit <- qnorm(1 - alpha)
      rejections[i] <- z > z_crit
    } else {
      z_crit <- qnorm(1 - alpha/2)
      rejections[i] <- abs(z) > z_crit
    }
  }
  
  # Compute performance measures
  empirical_power <- mean(rejections)
  mcse <- sqrt(empirical_power * (1 - empirical_power) / n_sims)
  
  mean_amplitude <- mean(amplitudes)
  empirical_se <- sd(amplitudes)
  expected_se <- k / sqrt(n)
  
  data.frame(
    parameter = "amplitude",
    design = "single_sample",
    n = n,
    true_effect = true_amplitude,
    predicted_power = predicted,
    empirical_power = empirical_power,
    mcse = mcse,
    diff = empirical_power - predicted,
    within_2mcse = abs(empirical_power - predicted) < 2 * mcse,
    mean_estimate = mean_amplitude,
    empirical_se = empirical_se,
    expected_se = expected_se,
    se_ratio = empirical_se / expected_se
  )
}


#' Run validation for elevation power
validate_elevation <- function(n, true_elevation, n_sims = N_SIMS,
                               k = 0.60, alpha = ALPHA) {
  
  predicted <- ssm_power_elevation(true_elevation, n, alpha, k, one_sided = FALSE)$power
  
  elevations <- numeric(n_sims)
  rejections <- logical(n_sims)
  
  for (i in 1:n_sims) {
    # For elevation test, we need data where elevation is the signal
    external <- rnorm(n)
    octants <- matrix(NA, nrow = n, ncol = 8)
    for (j in 1:8) {
      # All octants have same loading (pure elevation, no amplitude)
      octants[, j] <- true_elevation * external + rnorm(n)
    }
    
    cors <- sapply(1:8, function(j) cor(external, octants[, j]))
    params <- ssm_parameters(cors)
    
    elevations[i] <- params$elevation
    
    se <- k / sqrt(n)
    z <- abs(params$elevation) / se
    rejections[i] <- z > qnorm(1 - alpha/2)
  }
  
  empirical_power <- mean(rejections)
  mcse <- sqrt(empirical_power * (1 - empirical_power) / n_sims)
  
  data.frame(
    parameter = "elevation",
    design = "single_sample",
    n = n,
    true_effect = true_elevation,
    predicted_power = predicted,
    empirical_power = empirical_power,
    mcse = mcse,
    diff = empirical_power - predicted,
    within_2mcse = abs(empirical_power - predicted) < 2 * mcse,
    mean_estimate = mean(elevations),
    empirical_se = sd(elevations),
    expected_se = k / sqrt(n),
    se_ratio = sd(elevations) / (k / sqrt(n))
  )
}


#' Run validation for two-group amplitude difference
validate_amplitude_diff <- function(n1, n2, true_diff, n_sims = N_SIMS,
                                    k = 0.41, alpha = ALPHA) {
  
  predicted <- ssm_power_amplitude_diff(true_diff, n1, n2, alpha, k)$power
  
  diffs <- numeric(n_sims)
  rejections <- logical(n_sims)
  
  base_amplitude <- 0.20  # Arbitrary baseline
  
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
    
    se_diff <- k * sqrt(1/n1 + 1/n2)
    z <- abs(amp1 - amp2) / se_diff
    rejections[i] <- z > qnorm(1 - alpha/2)
  }
  
  empirical_power <- mean(rejections)
  mcse <- sqrt(empirical_power * (1 - empirical_power) / n_sims)
  
  data.frame(
    parameter = "amplitude_diff",
    design = "two_sample",
    n = paste0(n1, "+", n2),
    true_effect = true_diff,
    predicted_power = predicted,
    empirical_power = empirical_power,
    mcse = mcse,
    diff = empirical_power - predicted,
    within_2mcse = abs(empirical_power - predicted) < 2 * mcse,
    mean_estimate = mean(diffs),
    empirical_se = sd(diffs),
    expected_se = k * sqrt(1/n1 + 1/n2),
    se_ratio = sd(diffs) / (k * sqrt(1/n1 + 1/n2))
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

amplitude_scenarios <- expand.grid(
  effect = c(0.10, 0.16, 0.23),  # Small, medium, large benchmarks
  n = c(20, 41, 50, 104, 150, 200)
)

amplitude_results <- do.call(rbind, lapply(1:nrow(amplitude_scenarios), function(i) {
  result <- validate_amplitude(
    n = amplitude_scenarios$n[i],
    true_amplitude = amplitude_scenarios$effect[i]
  )
  
  cat(sprintf("  n=%3d, a=%.2f: Pred=%.3f, Emp=%.3f (MCSE=%.3f), Diff=%+.3f %s\n",
              result$n, result$true_effect, 
              result$predicted_power, result$empirical_power, result$mcse,
              result$diff, ifelse(result$within_2mcse, "[OK]", "[!]")))
  result
}))

cat(sprintf("\n  SUMMARY: %d/%d scenarios within 2 MCSE (%.1f%%)\n",
            sum(amplitude_results$within_2mcse), nrow(amplitude_results),
            100 * mean(amplitude_results$within_2mcse)))
cat(sprintf("  Mean |difference|: %.4f\n", mean(abs(amplitude_results$diff))))
cat(sprintf("  Mean SE ratio (empirical/expected): %.3f\n\n", 
            mean(amplitude_results$se_ratio)))


# -----------------------------------------------------------------------------
# VALIDATION 2: Critical target power check
# -----------------------------------------------------------------------------

cat("VALIDATION 2: Target Power Check (n=41, a=0.16, target=80%%)\n")
cat("------------------------------------------------------------\n")

target_result <- validate_amplitude(n = 41, true_amplitude = 0.16, n_sims = 10000)

cat(sprintf("  Predicted power:  %.4f\n", target_result$predicted_power))
cat(sprintf("  Empirical power:  %.4f (MCSE = %.4f)\n", 
            target_result$empirical_power, target_result$mcse))
cat(sprintf("  95%% CI:           [%.4f, %.4f]\n",
            target_result$empirical_power - 1.96 * target_result$mcse,
            target_result$empirical_power + 1.96 * target_result$mcse))
cat(sprintf("  Difference:       %+.4f\n", target_result$diff))
cat(sprintf("  Within 2 MCSE:    %s\n\n", 
            ifelse(target_result$within_2mcse, "YES", "NO")))


# -----------------------------------------------------------------------------
# VALIDATION 3: Single-sample elevation (k = 0.60)
# -----------------------------------------------------------------------------

cat("VALIDATION 3: Single-Sample Elevation (k = 0.60)\n")
cat("------------------------------------------------\n")

elevation_scenarios <- expand.grid(
  effect = c(0.11, 0.27),  # Medium, large (skip small - requires huge n)
  n = c(31, 100, 185, 300)
)

elevation_results <- do.call(rbind, lapply(1:nrow(elevation_scenarios), function(i) {
  result <- validate_elevation(
    n = elevation_scenarios$n[i],
    true_elevation = elevation_scenarios$effect[i]
  )
  
  cat(sprintf("  n=%3d, e=%.2f: Pred=%.3f, Emp=%.3f (MCSE=%.3f), Diff=%+.3f %s\n",
              result$n, result$true_effect,
              result$predicted_power, result$empirical_power, result$mcse,
              result$diff, ifelse(result$within_2mcse, "[OK]", "[!]")))
  result
}))

cat(sprintf("\n  SUMMARY: %d/%d scenarios within 2 MCSE (%.1f%%)\n",
            sum(elevation_results$within_2mcse), nrow(elevation_results),
            100 * mean(elevation_results$within_2mcse)))
cat(sprintf("  Mean SE ratio (empirical/expected): %.3f\n\n",
            mean(elevation_results$se_ratio)))


# -----------------------------------------------------------------------------
# VALIDATION 4: Two-group amplitude comparison
# -----------------------------------------------------------------------------

cat("VALIDATION 4: Two-Group Amplitude Comparison\n")
cat("---------------------------------------------\n")

twogroup_scenarios <- data.frame(
  n1 = c(82, 82, 132, 200),
  n2 = c(82, 82, 132, 200),
  effect = c(0.16, 0.10, 0.10, 0.10)
)

twogroup_results <- do.call(rbind, lapply(1:nrow(twogroup_scenarios), function(i) {
  result <- validate_amplitude_diff(
    n1 = twogroup_scenarios$n1[i],
    n2 = twogroup_scenarios$n2[i],
    true_diff = twogroup_scenarios$effect[i]
  )
  
  cat(sprintf("  n=%s, d=%.2f: Pred=%.3f, Emp=%.3f (MCSE=%.3f), Diff=%+.3f %s\n",
              result$n, result$true_effect,
              result$predicted_power, result$empirical_power, result$mcse,
              result$diff, ifelse(result$within_2mcse, "[OK]", "[!]")))
  result
}))

cat(sprintf("\n  SUMMARY: %d/%d scenarios within 2 MCSE (%.1f%%)\n\n",
            sum(twogroup_results$within_2mcse), nrow(twogroup_results),
            100 * mean(twogroup_results$within_2mcse)))


# =============================================================================
# RESULTS TABLE (Morris et al. Table 8 style)
# =============================================================================

cat("================================================================\n")
cat("                    RESULTS TABLE                               \n")
cat("================================================================\n\n")

all_results <- rbind(
  amplitude_results[, c("parameter", "n", "true_effect", "predicted_power", 
                        "empirical_power", "mcse", "diff", "se_ratio")],
  elevation_results[, c("parameter", "n", "true_effect", "predicted_power",
                        "empirical_power", "mcse", "diff", "se_ratio")],
  twogroup_results[, c("parameter", "n", "true_effect", "predicted_power",
                       "empirical_power", "mcse", "diff", "se_ratio")]
)

# Format and print
cat(sprintf("%-15s %8s %8s %10s %10s %8s %8s %8s\n",
            "Parameter", "n", "Effect", "Predicted", "Empirical", "MCSE", "Diff", "SE.ratio"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in 1:nrow(all_results)) {
  cat(sprintf("%-15s %8s %8.2f %10.3f %10.3f %8.4f %+8.4f %8.2f\n",
              all_results$parameter[i],
              all_results$n[i],
              all_results$true_effect[i],
              all_results$predicted_power[i],
              all_results$empirical_power[i],
              all_results$mcse[i],
              all_results$diff[i],
              all_results$se_ratio[i]))
}


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("                    VALIDATION SUMMARY                          \n")
cat("================================================================\n\n")

n_total <- nrow(amplitude_results) + nrow(elevation_results) + nrow(twogroup_results)
n_ok <- sum(amplitude_results$within_2mcse) + sum(elevation_results$within_2mcse) + 
        sum(twogroup_results$within_2mcse)

cat(sprintf("Total scenarios tested:        %d\n", n_total))
cat(sprintf("Within 2 MCSE of predicted:    %d (%.1f%%)\n", n_ok, 100 * n_ok / n_total))
cat(sprintf("Mean |predicted - empirical|:  %.4f\n", mean(abs(all_results$diff)))  )
cat(sprintf("Max  |predicted - empirical|:  %.4f\n", max(abs(all_results$diff))))

cat("\nScaling constant validation (SE ratio should be ~1.0):\n")
cat(sprintf("  Amplitude (k=0.41): Mean SE ratio = %.3f\n", 
            mean(amplitude_results$se_ratio)))
cat(sprintf("  Elevation (k=0.60): Mean SE ratio = %.3f\n",
            mean(elevation_results$se_ratio)))

cat("\n")
cat("INTERPRETATION:\n")
cat("---------------\n")
cat("If >95% of scenarios are within 2 MCSE and SE ratios are close to 1.0,\n")
cat("the power functions and scaling constants are validated.\n")
cat("\n")
cat("References:\n")
cat("  Morris TP, White IR, Crowther MJ (2019). Using simulation studies\n")
cat("  to evaluate statistical methods. Statist Med, 38, 2074-2102.\n")
cat("================================================================\n")


# =============================================================================
# SAVE RESULTS
# =============================================================================

results_summary <- list(
  amplitude = amplitude_results,
  elevation = elevation_results,
  two_group = twogroup_results,
  target_check = target_result,
  config = list(
    n_sims = N_SIMS,
    alpha = ALPHA,
    seed = SEED
  ),
  summary = data.frame(
    n_scenarios = n_total,
    n_within_2mcse = n_ok,
    pct_within_2mcse = 100 * n_ok / n_total,
    mean_abs_diff = mean(abs(all_results$diff)),
    max_abs_diff = max(abs(all_results$diff)),
    amplitude_se_ratio = mean(amplitude_results$se_ratio),
    elevation_se_ratio = mean(elevation_results$se_ratio)
  )
)

saveRDS(results_summary, "simulation_validation_ADEMP.rds")
write.csv(all_results, "simulation_validation_results.csv", row.names = FALSE)

cat("\nResults saved to:\n")
cat("  - simulation_validation_ADEMP.rds (full results)\n")
cat("  - simulation_validation_results.csv (summary table)\n")


# =============================================================================
# OPTIONAL: GRAPHICAL EXPLORATION (Morris et al. Section 5.1)
# =============================================================================

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  # Plot: Predicted vs Empirical Power
  plot_data <- all_results
  plot_data$parameter <- factor(plot_data$parameter,
                                 levels = c("amplitude", "elevation", "amplitude_diff"),
                                 labels = c("Amplitude", "Elevation", "Amplitude (2-group)"))
  
  p1 <- ggplot(plot_data, aes(x = predicted_power, y = empirical_power)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = empirical_power - 1.96*mcse, 
                      ymax = empirical_power + 1.96*mcse),
                  width = 0.02, alpha = 0.5) +
    geom_point(aes(color = parameter), size = 2.5) +
    scale_color_brewer(palette = "Set1", name = "Parameter") +
    labs(
      title = "Validation: Predicted vs Empirical Power",
      subtitle = sprintf("n_sim = %d per scenario; error bars = 95%% CI", N_SIMS),
      x = "Predicted Power",
      y = "Empirical Power"
    ) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("validation_power_plot.png", p1, width = 7, height = 7, dpi = 150)
  cat("\nPlot saved to: validation_power_plot.png\n")
  
  # Plot: SE ratio validation
  p2 <- ggplot(plot_data, aes(x = as.numeric(as.character(n)), y = se_ratio)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = parameter), size = 2.5) +
    facet_wrap(~parameter, scales = "free_x") +
    scale_color_brewer(palette = "Set1", guide = "none") +
    labs(
      title = "Scaling Constant Validation",
      subtitle = "Ratio of empirical SE to expected SE (k/√n); should be ~1.0",
      x = "Sample Size",
      y = "SE Ratio (Empirical / Expected)"
    ) +
    theme_minimal(base_size = 11)
  
  ggsave("validation_se_ratio_plot.png", p2, width = 9, height = 4, dpi = 150)
  cat("Plot saved to: validation_se_ratio_plot.png\n")
}

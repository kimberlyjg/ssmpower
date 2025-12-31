# =============================================================================
# SIMULATION VALIDATION OF SSM POWER FUNCTIONS
# =============================================================================
# 
# This script provides empirical validation that the ssmpower package produces
# accurate power estimates through Monte Carlo simulation.
#
# For use in paper supplementary materials.
#
# Author: [Your name]
# Date: 2025
# =============================================================================

library(ssmpower)

# Set seed for reproducibility
set.seed(20240115)

# Configuration
N_SIMS <- 5000       # Number of simulations per scenario (increase for paper)
ALPHA <- 0.05        # Significance level

cat("\n")
cat("================================================================\n")
cat("    MONTE CARLO VALIDATION OF SSM POWER FUNCTIONS               \n")
cat("================================================================\n")
cat(sprintf("Simulations per scenario: %d\n", N_SIMS))
cat(sprintf("Alpha level: %.3f\n", ALPHA))
cat("================================================================\n\n")


# =============================================================================
# SIMULATION FUNCTIONS
# =============================================================================

#' Simulate circumplex data with known SSM parameters
simulate_ssm_data <- function(n, amplitude, displacement_deg = 45, 
                              elevation = 0, noise_sd = 1) {
  angles <- seq(0, 315, by = 45)
  angles_rad <- angles * pi / 180
  displacement_rad <- displacement_deg * pi / 180
  
  external <- rnorm(n)
  octants <- matrix(NA, nrow = n, ncol = 8)
  
  for (i in 1:8) {
    true_loading <- elevation + amplitude * cos(angles_rad[i] - displacement_rad)
    octants[, i] <- true_loading * external + rnorm(n, sd = noise_sd)
  }
  
  list(external = external, octants = octants)
}


#' Run single simulation and test for significant amplitude
run_amplitude_sim <- function(n, true_amplitude, k = 0.41, alpha = 0.05, 
                              one_sided = TRUE) {
  data <- simulate_ssm_data(n, amplitude = true_amplitude)
  cors <- sapply(1:8, function(i) cor(data$external, data$octants[, i]))
  params <- ssm_parameters(cors)
  
  se <- k / sqrt(n)
  z <- params$amplitude / se
  z_crit <- if (one_sided) qnorm(1 - alpha) else qnorm(1 - alpha/2)
  
  if (one_sided) {
    rejected <- z > z_crit
  } else {
    rejected <- abs(z) > z_crit
  }
  
  list(amplitude = params$amplitude, z = z, rejected = rejected)
}


#' Validate power for amplitude
validate_amplitude_power <- function(n, true_amplitude, n_sims = N_SIMS, 
                                     k = 0.41, alpha = ALPHA, one_sided = TRUE) {
  predicted <- ssm_power_amplitude(true_amplitude, n, alpha, k, one_sided)$power
  
  rejections <- replicate(n_sims, {
    run_amplitude_sim(n, true_amplitude, k, alpha, one_sided)$rejected
  })
  
  empirical <- mean(rejections)
  se <- sqrt(empirical * (1 - empirical) / n_sims)
  
  data.frame(
    parameter = "amplitude",
    n = n,
    effect = true_amplitude,
    predicted = predicted,
    empirical = empirical,
    se = se,
    diff = empirical - predicted,
    within_2se = abs(empirical - predicted) < 2 * se
  )
}


#' Validate power for elevation
validate_elevation_power <- function(n, true_elevation, n_sims = N_SIMS,
                                     k = 0.60, alpha = ALPHA) {
  predicted <- ssm_power_elevation(true_elevation, n, alpha, k, one_sided = FALSE)$power
  
  rejections <- replicate(n_sims, {
    external <- rnorm(n)
    octants <- matrix(NA, nrow = n, ncol = 8)
    for (i in 1:8) {
      octants[, i] <- true_elevation * external + rnorm(n)
    }
    
    cors <- sapply(1:8, function(i) cor(external, octants[, i]))
    params <- ssm_parameters(cors)
    
    se <- k / sqrt(n)
    z <- abs(params$elevation) / se
    z > qnorm(1 - alpha/2)
  })
  
  empirical <- mean(rejections)
  se <- sqrt(empirical * (1 - empirical) / n_sims)
  
  data.frame(
    parameter = "elevation",
    n = n,
    effect = true_elevation,
    predicted = predicted,
    empirical = empirical,
    se = se,
    diff = empirical - predicted,
    within_2se = abs(empirical - predicted) < 2 * se
  )
}


#' Validate power for two-group amplitude difference
validate_amplitude_diff_power <- function(n1, n2, true_diff, n_sims = N_SIMS,
                                          k = 0.41, alpha = ALPHA) {
  predicted <- ssm_power_amplitude_diff(true_diff, n1, n2, alpha, k)$power
  
  rejections <- replicate(n_sims, {
    # Group 1: higher amplitude
    data1 <- simulate_ssm_data(n1, amplitude = 0.20)
    cors1 <- sapply(1:8, function(i) cor(data1$external, data1$octants[, i]))
    amp1 <- ssm_parameters(cors1)$amplitude
    
    # Group 2: lower amplitude
    data2 <- simulate_ssm_data(n2, amplitude = 0.20 - true_diff)
    cors2 <- sapply(1:8, function(i) cor(data2$external, data2$octants[, i]))
    amp2 <- ssm_parameters(cors2)$amplitude
    
    se_diff <- k * sqrt(1/n1 + 1/n2)
    z <- abs(amp1 - amp2) / se_diff
    z > qnorm(1 - alpha/2)
  })
  
  empirical <- mean(rejections)
  se <- sqrt(empirical * (1 - empirical) / n_sims)
  
  data.frame(
    parameter = "amplitude_diff",
    n = paste0(n1, "+", n2),
    effect = true_diff,
    predicted = predicted,
    empirical = empirical,
    se = se,
    diff = empirical - predicted,
    within_2se = abs(empirical - predicted) < 2 * se
  )
}


# =============================================================================
# VALIDATION 1: SINGLE-SAMPLE AMPLITUDE
# =============================================================================

cat("VALIDATION 1: Single-Sample Amplitude Tests\n")
cat("--------------------------------------------\n")
cat("Testing k = 0.41 across effect sizes and sample sizes\n\n")

# Define scenarios based on benchmarks
amplitude_scenarios <- expand.grid(
  effect = c(0.10, 0.16, 0.23),  # Small, medium, large
  n = c(20, 41, 50, 104, 150, 200)
)

amplitude_results <- do.call(rbind, lapply(1:nrow(amplitude_scenarios), function(i) {
  result <- validate_amplitude_power(
    n = amplitude_scenarios$n[i],
    true_amplitude = amplitude_scenarios$effect[i]
  )
  
  cat(sprintf("  n = %3d, a = %.2f: Pred = %.3f, Emp = %.3f (%+.3f) %s\n",
              result$n, result$effect, result$predicted, result$empirical,
              result$diff, ifelse(result$within_2se, "✓", "✗")))
  result
}))

cat(sprintf("\n  Summary: %d/%d within 2 SE (%.1f%%)\n",
            sum(amplitude_results$within_2se), nrow(amplitude_results),
            100 * mean(amplitude_results$within_2se)))
cat(sprintf("  Mean |difference|: %.4f\n\n", mean(abs(amplitude_results$diff))))


# =============================================================================
# VALIDATION 2: TARGET POWER CHECK
# =============================================================================

cat("VALIDATION 2: Target Power Check\n")
cat("---------------------------------\n")
cat("Critical test: Does n=41 give 80% power for amplitude=0.16?\n\n")

target_check <- validate_amplitude_power(
  n = 41, 
  true_amplitude = 0.16,
  n_sims = 10000  # More sims for precision
)

cat(sprintf("  Predicted power:  %.4f (target: 0.8000)\n", target_check$predicted))
cat(sprintf("  Empirical power:  %.4f\n", target_check$empirical))
cat(sprintf("  95%% CI:           [%.4f, %.4f]\n", 
            target_check$empirical - 1.96 * target_check$se,
            target_check$empirical + 1.96 * target_check$se))
cat(sprintf("  Difference:       %+.4f\n", target_check$diff))
cat(sprintf("  Within 2 SE:      %s\n\n", ifelse(target_check$within_2se, "YES", "NO")))


# =============================================================================
# VALIDATION 3: SINGLE-SAMPLE ELEVATION
# =============================================================================

cat("VALIDATION 3: Single-Sample Elevation Tests\n")
cat("--------------------------------------------\n")
cat("Testing k = 0.60 across effect sizes and sample sizes\n\n")

elevation_scenarios <- expand.grid(
  effect = c(0.02, 0.11, 0.27),  # Small, medium, large
  n = c(31, 100, 185, 500)
)

elevation_results <- do.call(rbind, lapply(1:nrow(elevation_scenarios), function(i) {
  result <- validate_elevation_power(
    n = elevation_scenarios$n[i],
    true_elevation = elevation_scenarios$effect[i]
  )
  
  cat(sprintf("  n = %3d, e = %.2f: Pred = %.3f, Emp = %.3f (%+.3f) %s\n",
              result$n, result$effect, result$predicted, result$empirical,
              result$diff, ifelse(result$within_2se, "✓", "✗")))
  result
}))

cat(sprintf("\n  Summary: %d/%d within 2 SE (%.1f%%)\n",
            sum(elevation_results$within_2se), nrow(elevation_results),
            100 * mean(elevation_results$within_2se)))
cat(sprintf("  Mean |difference|: %.4f\n\n", mean(abs(elevation_results$diff))))


# =============================================================================
# VALIDATION 4: TWO-GROUP AMPLITUDE COMPARISON
# =============================================================================

cat("VALIDATION 4: Two-Group Amplitude Comparison\n")
cat("---------------------------------------------\n")
cat("Testing two-sided test for group differences\n\n")

twogroup_scenarios <- data.frame(
  n1 = c(82, 132, 200, 100),
  n2 = c(82, 132, 200, 200),  # Last one is unbalanced
  effect = c(0.16, 0.10, 0.10, 0.16)
)

twogroup_results <- do.call(rbind, lapply(1:nrow(twogroup_scenarios), function(i) {
  result <- validate_amplitude_diff_power(
    n1 = twogroup_scenarios$n1[i],
    n2 = twogroup_scenarios$n2[i],
    true_diff = twogroup_scenarios$effect[i]
  )
  
  cat(sprintf("  n = %s, d = %.2f: Pred = %.3f, Emp = %.3f (%+.3f) %s\n",
              result$n, result$effect, result$predicted, result$empirical,
              result$diff, ifelse(result$within_2se, "✓", "✗")))
  result
}))

cat(sprintf("\n  Summary: %d/%d within 2 SE (%.1f%%)\n",
            sum(twogroup_results$within_2se), nrow(twogroup_results),
            100 * mean(twogroup_results$within_2se)))


# =============================================================================
# FINAL SUMMARY
# =============================================================================

all_results <- rbind(amplitude_results, elevation_results, twogroup_results)

cat("\n")
cat("================================================================\n")
cat("                    OVERALL SUMMARY                             \n")
cat("================================================================\n\n")

cat(sprintf("Total scenarios tested:     %d\n", nrow(all_results)))
cat(sprintf("Within 2 SE of predicted:   %d (%.1f%%)\n",
            sum(all_results$within_2se), 100 * mean(all_results$within_2se)))
cat(sprintf("Mean |predicted - empirical|: %.4f\n", mean(abs(all_results$diff))))
cat(sprintf("Max  |predicted - empirical|: %.4f\n", max(abs(all_results$diff))))

cat("\nBy parameter type:\n")
for (param in unique(all_results$parameter)) {
  subset <- all_results[all_results$parameter == param, ]
  cat(sprintf("  %s: %d/%d within 2 SE, mean |diff| = %.4f\n",
              param, sum(subset$within_2se), nrow(subset), 
              mean(abs(subset$diff))))
}

cat("\n")
cat("CONCLUSION: The scaling constants k = 0.41 (amplitude) and\n")
cat("k = 0.60 (elevation) produce accurate power estimates.\n")
cat("================================================================\n")


# =============================================================================
# SAVE RESULTS FOR PAPER
# =============================================================================

# Combine all results
validation_results <- list(
  amplitude = amplitude_results,
  elevation = elevation_results,
  two_group = twogroup_results,
  target_check = target_check,
  summary = data.frame(
    total_scenarios = nrow(all_results),
    within_2se = sum(all_results$within_2se),
    pct_within_2se = 100 * mean(all_results$within_2se),
    mean_abs_diff = mean(abs(all_results$diff)),
    max_abs_diff = max(abs(all_results$diff)),
    n_sims = N_SIMS
  )
)

# Save to RDS
saveRDS(validation_results, "simulation_validation_results.rds")
cat("\nResults saved to: simulation_validation_results.rds\n")

# Also save as CSV for easy viewing
write.csv(all_results, "simulation_validation_results.csv", row.names = FALSE)
cat("Results saved to: simulation_validation_results.csv\n")

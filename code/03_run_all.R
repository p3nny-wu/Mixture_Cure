# 03_run_all.R — run simulations over a grid and save CSVs
set.seed(123)

# Fixed parameters
mu1 <- log(4)
sigma <- 1
event_rate <- 0.25  # adjust per manuscript
n_per_group <- 5000
n_simulations <- 100

# Varying parameters
censor_times <- seq(10, 30, by = 5)
mu2_values <- c(4, 6, 8, 10)

# Storage
results_df <- data.frame()

total_steps <- length(censor_times) * length(mu2_values)
step <- 0L

for (censor in censor_times) {
  for (mu2_value in mu2_values) {
    mu2 <- log(mu2_value)
    results <- simulate_scenario(mu1, mu2, sigma, censor, event_rate,
                                 n_per_group, n_simulations)

    temp_df <- data.frame(
      censor = rep(censor, n_simulations),
      mu2 = rep(mu2_value, n_simulations),
      cox_coefs = results$cox_coefs,
      cox_prob_A = results$cox_prob_A, cox_prob_B = results$cox_prob_B,
      logistic_coefs = results$logistic_coefs,
      logistic_prob_A = results$logistic_prob_A, logistic_prob_B = results$logistic_prob_B,
      lognor_prob_A = results$lognor_prob_A, lognor_prob_B = results$lognor_prob_B,
      mix_prob_A = results$mix_prob_A, mix_prob_B = results$mix_prob_B
    )

    results_df <- rbind(results_df, temp_df)

    step <- step + 1L
    message(sprintf("Progress: %d / %d", step, total_steps))
  }
}

# Derived quantities
results_df2 <- results_df |>
  dplyr::mutate(
    cox_prob_diff = cox_prob_A - cox_prob_B,
    logistic_prob_diff = logistic_prob_A - logistic_prob_B,
    mix_prob_diff = mix_prob_A - mix_prob_B,
    lognor_prob_diff = lognor_prob_A - lognor_prob_B
  )

# Save outputs
write.csv(results_df, file = "data/simulation_results_raw.csv", row.names = FALSE)
write.csv(results_df2, file = "data/simulation_results_summary.csv", row.names = FALSE)

# Record session info
writeLines(capture.output(sessionInfo()), "session-info/sessionInfo.txt")

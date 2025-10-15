# 02_simulate.R — core simulation & analysis
# Simulation function
# Parameters:
#  - mu1, mu2, sigma: log-normal parameters for event time among diagnosed
#  - censor: administrative censoring time (scalar)
#  - event_rate: proportion with event=1 at baseline assignment
#  - n_per_group: sample size per group
#  - n_simulations

simulate_scenario <- function(mu1, mu2, sigma, censor, event_rate,
                              n_per_group = 5000, n_simulations = 100) {
  cox_coefs <- numeric(n_simulations)
  logistic_coefs <- numeric(n_simulations)
  cox_prob_A <- numeric(n_simulations)
  cox_prob_B <- numeric(n_simulations)
  logistic_prob_A <- numeric(n_simulations)
  logistic_prob_B <- numeric(n_simulations)
  lognor_prob_A <- numeric(n_simulations)
  lognor_prob_B <- numeric(n_simulations)
  mix_prob_A <- numeric(n_simulations)
  mix_prob_B <- numeric(n_simulations)

  for (i in seq_len(n_simulations)) {
    set.seed(i)
    sim <- data.frame(
      grp = c(rep(0, n_per_group), rep(1, n_per_group)),
      event = c(rep(1, n_per_group * event_rate), rep(0, n_per_group * (1 - event_rate)),
                rep(1, n_per_group * event_rate), rep(0, n_per_group * (1 - event_rate)))
    )
    # natural loss to follow-up
    sim$censor <- runif(2 * n_per_group, min = 0, max = 30)
    sim$time_to_event <- NA_real_
    sim$time_to_event[sim$grp == 0 & sim$event == 1] <- rlnorm(sum(sim$grp == 0 & sim$event == 1),
                                                                meanlog = mu1, sdlog = sigma)
    sim$time_to_event[sim$grp == 1 & sim$event == 1] <- rlnorm(sum(sim$grp == 1 & sim$event == 1),
                                                                meanlog = mu2, sdlog = sigma)

    sim$event <- ifelse(sim$event == 1 & sim$time_to_event <= sim$censor, 1, 0)
    sim$time_to_event <- ifelse(is.na(sim$time_to_event) | sim$time_to_event > sim$censor,
                                sim$censor, sim$time_to_event)

    # administrative censoring
    sim$censor <- censor
    sim$diagnosis <- ifelse(sim$event == 1 & sim$time_to_event <= sim$censor, 1, 0)
    sim$time_to_diagnosis <- ifelse(is.na(sim$time_to_event) | sim$time_to_event > sim$censor,
                                    sim$censor, sim$time_to_event)

    # Cox PH
    cox_fit <- coxph(Surv(time_to_diagnosis, diagnosis) ~ grp, data = sim)
    cox_coefs[i] <- summary(cox_fit)$coefficients[1, 1]

    # Evaluate survival prob at the censoring time (more reproducible than "last" time)
    sf_A <- survfit(cox_fit, newdata = data.frame(grp = 0))
    sf_B <- survfit(cox_fit, newdata = data.frame(grp = 1))
    sA <- summary(sf_A, times = censor, extend = TRUE)$surv
    sB <- summary(sf_B, times = censor, extend = TRUE)$surv
    cox_prob_A[i] <- 1 - as.numeric(sA)
    cox_prob_B[i] <- 1 - as.numeric(sB)

    # Logistic regression
    logistic_fit <- glm(diagnosis ~ grp, data = sim, family = binomial())
    logistic_coefs[i] <- coef(summary(logistic_fit))[2, 1]
    probs <- predict(logistic_fit, newdata = data.frame(grp = c(0, 1)), type = "response")
    logistic_prob_A[i] <- probs[1]; logistic_prob_B[i] <- probs[2]

    # Log-normal AFT via flexsurv
    lognormal_fit <- flexsurvreg(Surv(time_to_diagnosis, diagnosis) ~ grp,
                                 data = sim, dist = "lognormal")
    survival_A <- tidyr::unnest(predict(lognormal_fit, newdata = data.frame(grp = 0),
                                        type = "survival"), .pred)
    survival_B <- tidyr::unnest(predict(lognormal_fit, newdata = data.frame(grp = 1),
                                        type = "survival"), .pred)
    surv_prob_A <- survival_A[order(survival_A$.time, decreasing = TRUE), ][1, 2]
    surv_prob_B <- survival_B[order(survival_B$.time, decreasing = TRUE), ][1, 2]
    lognor_prob_A[i] <- as.numeric(1 - surv_prob_A)
    lognor_prob_B[i] <- as.numeric(1 - surv_prob_B)

    # Mixture cure model (cuRe)
    mix_fit <- fit.cure.model(Surv(time_to_diagnosis, diagnosis) ~ grp,
                              data = sim,
                              formula.surv = list(~ grp, ~ grp),
                              dist = "lognormal", link = "logit")
    cr <- predict(mix_fit, newdata = data.frame(grp = c(0, 1)), type = "curerate")
    mix1_prob_A[i] <- as.numeric(1 - cr[[1]][1])
    mix1_prob_B[i] <- as.numeric(1 - cr[[2]][1])

    # Stratified mixture cure (flexsurvcure)
    mix_fit_A <- flexsurvcure(Surv(time_to_diagnosis, diagnosis) ~ 1,
                              data = sim[sim$grp == 0, ], dist = "lnorm",
                              mixture = TRUE, link = "logistic")
    mix_fit_B <- flexsurvcure(Surv(time_to_diagnosis, diagnosis) ~ 1,
                              data = sim[sim$grp == 1, ], dist = "lnorm",
                              mixture = TRUE, link = "logistic")
    mix_prob_A[i] <- as.numeric(1 - mix_fit_A$res[1, 1])
    mix_prob_B[i] <- as.numeric(1 - mix_fit_B$res[1, 1])
  }

  list(
    cox_coefs = cox_coefs,
    cox_prob_A = cox_prob_A, cox_prob_B = cox_prob_B,
    logistic_coefs = logistic_coefs,
    logistic_prob_A = logistic_prob_A, logistic_prob_B = logistic_prob_B,
    lognor_prob_A = lognor_prob_A, lognor_prob_B = lognor_prob_B,
    mix_prob_A = mix_prob_A, mix_prob_B = mix_prob_B
  )
}

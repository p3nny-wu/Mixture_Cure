# 04_figures.R — produce manuscript figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7")

results_df2 <- read.csv("data/simulation_results_summary.csv")

# Long forms for plotting
results_df2_long1 <- results_df2 |>
  tidyr::pivot_longer(
    cols = c(cox_prob_A, logistic_prob_A, mix_prob_A, lognor_prob_A,
             cox_prob_B, logistic_prob_B, mix_prob_B, lognor_prob_B),
    names_to = "model_type", values_to = "prob"
  ) |>
  tidyr::separate(model_type, into = c("model", "group"), sep = "_prob_") |>
  dplyr::mutate(model = factor(model, levels = c("logistic", "cox", "lognor", "mix2")))


# ---- Figure 1: Distribution of predicted probabilities by group ----
results_df2_long11 <- results_df2_long1
results_df2_long11$mu2 <- factor(results_df2_long11$mu2)
levels(results_df2_long11$mu2) <- paste0("lambda[2] == '", levels(results_df2_long11$mu2), "'")
results_df2_long11$model <- factor(results_df2_long11$model,
                                   levels = c("logistic","cox","lognor","mix2"),
                                   labels = c("Logistic","Cox-PH","Lognormal","Mixture Cure"))
results_df2_long11$group <- factor(results_df2_long11$group, levels = c("A","B"),
                                   labels = c("'Group 1'","'Group 2'"))

prob_plot <- ggplot(results_df2_long11, aes(x = as.factor(censor), y = prob)) +
  facet_grid(mu2 ~ group, scales = "free_y", space = "free_y", labeller = label_parsed) +
  geom_violin(aes(fill = model), position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "#999999", linewidth = 0.2) +
  labs(x = "Censor Time", y = "Predicted Probability", fill = "Model") +
  theme_pubr(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(color = "grey95", size = 0.25),
        panel.grid.minor = element_line(color = "grey95", size = 0.25)) +
  ggtitle("Distribution of Predicted Probability of the Event") +
  scale_fill_manual(values = palette, labels = c("Logistic","Cox-PH","Lognormal","Mixture Cure")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05),
                     limits = function(x) c(min(x), max(x))) +
  theme(legend.box.spacing = unit(0.5, "lines"))
ggsave("figs/prob_plot.png", plot = prob_plot, width = 8, height = 12, dpi = 600)

# ---- Figure 2: Kaplan-Meier curves by mu2 ----
set.seed(123)
mu1 <- log(4)
mu2_vals <- c(4, 6, 8, 10)
sigma <- 1
censor <- 30
event_rate <- 0.25
n_per_group <- 5000

all_data <- data.frame()

for (mu2_val in mu2_vals) {
  mu2 <- log(mu2_val)
  sim <- data.frame(
    grp = c(rep(0, n_per_group), rep(1, n_per_group)),
    event = c(rep(1, n_per_group * event_rate), rep(0, n_per_group * (1 - event_rate)),
              rep(1, n_per_group * event_rate), rep(0, n_per_group * (1 - event_rate)))
  )
  sim$censor <- runif(2 * n_per_group, min = 0, max = 30)
  sim$time_to_event <- NA_real_
  sim$time_to_event[sim$grp == 0 & sim$event == 1] <- rlnorm(sum(sim$grp == 0 & sim$event == 1),
                                                              meanlog = mu1, sdlog = sigma)
  sim$time_to_event[sim$grp == 1 & sim$event == 1] <- rlnorm(sum(sim$grp == 1 & sim$event == 1),
                                                              meanlog = mu2, sdlog = sigma)
  sim$event <- ifelse(sim$event == 1 & sim$time_to_event <= sim$censor, 1, 0)
  sim$time_to_event <- pmin(sim$time_to_event, sim$censor, na.rm = TRUE)
  sim$mu2 <- mu2_val
  all_data <- rbind(all_data, sim)
}
all_data$mu2 <- factor(all_data$mu2)
levels(all_data$mu2) <- paste0("lambda[2] == '", levels(all_data$mu2), "'")
all_data$grp <- factor(all_data$grp, levels = c(0, 1), labels = c("Group 1", "Group 2"))

km_plot <- ggplot(all_data, aes(time = time_to_event, status = event, color = grp, group = grp)) +
  geom_km() + geom_kmticks(alpha = 0.5, size = 0.2) +
  facet_wrap(~mu2, ncol = 1, labeller = label_parsed) +
  theme_pubr(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(color = "grey95", size = 0.25),
        panel.grid.minor = element_line(color = "grey95", size = 0.25)) +
  ggtitle("Kaplan–Meier Curve for Diagnosis") +
  labs(x = "Time", y = "No-Diagnosis Probability", color = "Group") +
  scale_color_manual(values = c("steelblue","orange"))

ggsave("figs/km_plot.png", plot = km_plot, width = 8, height = 12, dpi = 600)
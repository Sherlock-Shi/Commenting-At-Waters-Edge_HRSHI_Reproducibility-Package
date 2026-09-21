#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(lmtest)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript diagnostic.R <monthly_outcomes.csv> <output_dir>")
}
input_file <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(args[[2]], winslash = "/", mustWork = TRUE)

monthly <- read.csv(input_file, stringsAsFactors = FALSE)
required_columns <- c(
  "outcome", "outcome_label", "predictor", "predictor_label",
  "sample_month", "predictor_value", "case_value", "n_cases"
)
if (!identical(names(monthly), required_columns)) {
  stop("Diagnostic monthly input has an unexpected schema.")
}
stopifnot(length(unique(monthly$outcome)) == 9)
monthly$post_breakpoint <- as.integer(monthly$sample_month >= "2023-09")

model_results <- list()
diagnostic_results <- list()
residual_results <- list()
acf_results <- list()

for (outcome in unique(monthly$outcome)) {
  data <- monthly[monthly$outcome == outcome, ]
  data <- data[order(data$sample_month), ]
  stopifnot(nrow(data) == 34)
  stopifnot(length(unique(data$predictor)) == 1)

  model <- lm(
    case_value ~ predictor_value + post_breakpoint,
    data = data
  )
  coefficients <- summary(model)$coefficients
  residual_values <- residuals(model)
  outcome_label <- data$outcome_label[1]
  predictor <- data$predictor[1]

  model_results[[outcome]] <- data.frame(
    outcome = outcome,
    outcome_label = outcome_label,
    predictor = predictor,
    n_months = nobs(model),
    predictor_estimate = coefficients["predictor_value", "Estimate"],
    predictor_std_error = coefficients["predictor_value", "Std. Error"],
    predictor_p_value = coefficients["predictor_value", "Pr(>|t|)"],
    breakpoint_estimate = coefficients["post_breakpoint", "Estimate"],
    breakpoint_std_error = coefficients["post_breakpoint", "Std. Error"],
    breakpoint_p_value = coefficients["post_breakpoint", "Pr(>|t|)"],
    r_squared = summary(model)$r.squared
  )

  dw <- dwtest(model)
  bg <- bgtest(model, order = 1)
  bp <- bptest(model)
  diagnostic_results[[outcome]] <- data.frame(
    outcome = outcome,
    outcome_label = outcome_label,
    predictor = predictor,
    lag1_residual_acf = acf(residual_values, plot = FALSE, lag.max = 1)$acf[2],
    durbin_watson_p = dw$p.value,
    breusch_godfrey_lag1_p = bg$p.value,
    breusch_pagan_p = bp$p.value
  )

  residual_results[[outcome]] <- data.frame(
    outcome = outcome,
    outcome_label = outcome_label,
    predictor = predictor,
    sample_month = data$sample_month,
    fitted = fitted(model),
    residual = residual_values,
    sqrt_abs_standardized_residual = sqrt(abs(rstandard(model)))
  )

  outcome_acf <- acf(residual_values, plot = FALSE, lag.max = 10)
  acf_results[[outcome]] <- data.frame(
    outcome = outcome,
    outcome_label = outcome_label,
    predictor = predictor,
    lag = as.integer(outcome_acf$lag),
    acf = as.numeric(outcome_acf$acf),
    bound = 1.96 / sqrt(length(residual_values))
  )
}

model_results <- do.call(rbind, model_results)
diagnostic_results <- do.call(rbind, diagnostic_results)
residual_results <- do.call(rbind, residual_results)
acf_results <- do.call(rbind, acf_results)

write.csv(model_results, file.path(output_dir, "classical_ols_results.csv"), row.names = FALSE)
write.csv(diagnostic_results, file.path(output_dir, "residual_diagnostic_tests.csv"), row.names = FALSE)
write.csv(residual_results, file.path(output_dir, "classical_ols_residuals.csv"), row.names = FALSE)

bootstrap_counts <- data.frame(
  replications = c(5000L, 10000L, 20000L, 50000L),
  replication_role = c(
    "precision_reference",
    "planned_initial",
    "threshold_overlap_extension",
    "exceptional_borderline_extension"
  )
)
decision_thresholds <- data.frame(
  decision_threshold = c(0.05 / 4, 0.05 / 3, 0.025, 0.05),
  threshold_role = c(
    "holm_first_of_four",
    "holm_second_of_four",
    "holm_third_of_four",
    "unadjusted_or_holm_fourth"
  )
)
bootstrap_mcse <- merge(bootstrap_counts, decision_thresholds)
bootstrap_mcse$monte_carlo_std_error <- sqrt(
  bootstrap_mcse$decision_threshold *
    (1 - bootstrap_mcse$decision_threshold) /
    bootstrap_mcse$replications
)
bootstrap_mcse$monte_carlo_margin_95 <- qnorm(0.975) *
  bootstrap_mcse$monte_carlo_std_error
write.csv(
  bootstrap_mcse,
  file.path(output_dir, "bootstrap_replication_mcse.csv"),
  row.names = FALSE
)

diagnostic_theme <- theme_minimal(base_family = "serif", base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(margin = margin(b = 8)),
    plot.title.position = "plot",
    plot.margin = margin(t = 12, r = 12, b = 12, l = 12)
  )

residual_fitted <- ggplot(residual_results, aes(fitted, residual)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(shape = 21, fill = "white", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linetype = "dashed", colour = "grey35") +
  facet_wrap(~ outcome_label, scales = "free", ncol = 3) +
  labs(
    title = "Classical OLS Residuals Versus Fitted Values",
    subtitle = "Outcome-specific confirmatory predictor and fixed September 2023 breakpoint",
    x = "Fitted monthly outcome",
    y = "Residual"
  ) + diagnostic_theme

residual_time <- ggplot(
  residual_results,
  aes(as.Date(paste0(sample_month, "-01")), residual)
) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = as.Date("2023-09-01"), linetype = "dashed", colour = "grey45") +
  geom_line(colour = "grey35") +
  geom_point(size = 1.2) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 3) +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y-%m") +
  labs(
    title = "Classical OLS Residuals Across Calendar Months",
    subtitle = "Dashed line marks the fixed 2023-09 breakpoint",
    x = "Calendar month",
    y = "Residual"
  ) + diagnostic_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 7.5),
    plot.margin = margin(t = 12, r = 24, b = 12, l = 24)
  )

residual_acf <- ggplot(acf_results, aes(lag, acf)) +
  geom_hline(yintercept = 0) +
  geom_hline(aes(yintercept = bound), linetype = "dashed", colour = "grey45") +
  geom_hline(aes(yintercept = -bound), linetype = "dashed", colour = "grey45") +
  geom_segment(aes(xend = lag, y = 0, yend = acf)) +
  geom_point(shape = 21, fill = "white", size = 1.4) +
  facet_wrap(~ outcome_label, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  labs(
    title = "Autocorrelation Functions of Classical OLS Residuals",
    x = "Lag in months",
    y = "Residual autocorrelation"
  ) + diagnostic_theme

scale_location <- ggplot(
  residual_results,
  aes(fitted, sqrt_abs_standardized_residual)
) +
  geom_point(shape = 21, fill = "white", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linetype = "dashed", colour = "grey35") +
  facet_wrap(~ outcome_label, scales = "free_x", ncol = 3) +
  labs(
    title = "Classical OLS Scale-Location Diagnostics",
    x = "Fitted monthly outcome",
    y = expression(sqrt("|Standardized residual|"))
  ) + diagnostic_theme

ggsave(file.path(output_dir, "residuals_vs_fitted.png"), residual_fitted, width = 10, height = 9, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "residuals_over_time.png"), residual_time, width = 11, height = 9, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "residual_acf.png"), residual_acf, width = 10, height = 9, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "scale_location.png"), scale_location, width = 10, height = 9, dpi = 300, bg = "white")

cat("Fitted", nrow(model_results), "classical monthly OLS models with the fixed 2023-09 breakpoint.\n")
cat("Calculated Monte Carlo precision for four candidate bootstrap counts.\n")
cat("Outputs:", output_dir, "\n")

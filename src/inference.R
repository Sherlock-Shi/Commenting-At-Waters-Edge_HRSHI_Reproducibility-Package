#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(nanoparquet)
  library(sandwich)
  library(jsonlite)
})

METHOD_ID <- "P4-METHOD-2026-08-04-v3"
BREAKPOINT_MONTH <- "2023-09"
STUDY_MONTHS <- format(
  seq(as.Date("2022-03-01"), as.Date("2024-12-01"), by = "month"),
  "%Y-%m"
)
INITIAL_BOOTSTRAP_REPLICATIONS <- 10000L
EXTENDED_BOOTSTRAP_REPLICATIONS <- 20000L
EXCEPTIONAL_BOOTSTRAP_REPLICATIONS <- 50000L
BOOTSTRAP_SEED <- 20260804L
HAC_LAG <- 1L
ALPHA <- 0.05
MONTHLY_CPS_DISPLAY_DERIVATIVE_ID <-
  "P4-MONTHLY-CPS-DISPLAY-INTERVAL-2026-08-17-v1"
MONTHLY_CPS_INTERVAL_LEVEL <- 0.95
MONTHLY_CPS_INTERVAL_QUANTILE_TYPE <- 6L
MONTHLY_CPS_INTERVAL_REPLICATIONS <- INITIAL_BOOTSTRAP_REPLICATIONS
# Paths and arguments ---------------------------------------------------------

script_path <- function() {
  value <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(value)) {
    return(NA_character_)
  }
  normalizePath(sub("^--file=", "", value[[1]]), winslash = "/", mustWork = TRUE)
}

find_repo_root <- function() {
  normalizePath(
    dirname(dirname(script_path())),
    winslash = "/",
    mustWork = TRUE
  )
}

default_paths <- function(repo_root = find_repo_root()) {
  list(
    repo_root = repo_root,
    case_input = file.path(
      repo_root, "data", "input", "comment_annotations.parquet"
    ),
    diagnostic_reference = file.path(
      repo_root, "data", "output", "monthly", "monthly_outcomes.csv"
    ),
    pbi_input = file.path(
      repo_root, "data", "input", "party_brand_index_timeseries.csv"
    ),
    stance_input = file.path(
      repo_root, "data", "input", "monthly_stance.csv"
    ),
    output_dir = file.path(
      repo_root, "data", "output", "inference"
    )
  )
}

require_files <- function(paths) {
  labels <- c("case_input", "diagnostic_reference", "pbi_input", "stance_input")
  missing <- labels[!vapply(paths[labels], file.exists, logical(1))]
  if (length(missing)) {
    stop("Missing required input path(s): ", paste(missing, collapse = ", "))
  }
}


# Input requirements ----------------------------------------------------------

required_case_columns <- c(
  "comment_id",
  "submission_id",
  "sample_month",
  "battery1_eligible",
  "downstream_eligible",
  "affiliation_recognizable_ev",
  "gate_pass",
  "conditional_psi",
  "observed_unconditional_psi",
  "ingroup_self_positioning_scaled",
  "emotional_intensity_scaled",
  "outgroup_construction_scaled",
  "coalition_direction_centered"
)

require_binary_flag <- function(values, name, allow_missing = FALSE) {
  if (!allow_missing && anyNA(values)) {
    stop(name, " contains missing values.")
  }
  observed <- unique(values[!is.na(values)])
  if (length(observed) && any(!observed %in% c(FALSE, TRUE, 0, 1))) {
    stop(name, " must contain only binary logical values.")
  }
}

require_numeric_domain <- function(values, name, lower, upper) {
  numeric <- suppressWarnings(as.numeric(values))
  observed <- numeric[!is.na(numeric)]
  if (
    length(observed) &&
      (any(!is.finite(observed)) || any(observed < lower) || any(observed > upper))
  ) {
    stop(name, " contains a nonfinite or out-of-domain value.")
  }
}

read_case_input <- function(path) {
  cases <- nanoparquet::read_parquet(path, col_select = required_case_columns)
  missing <- setdiff(required_case_columns, names(cases))
  if (length(missing)) {
    stop("Case input omits required columns: ", paste(missing, collapse = ", "))
  }
  cases <- cases[required_case_columns]
  if (nrow(cases) == 0L) {
    stop("Case input is empty.")
  }
  if (anyNA(cases$comment_id) || anyDuplicated(as.character(cases$comment_id))) {
    stop("Case input must contain one nonmissing row per comment_id.")
  }
  if (anyNA(cases$submission_id) || any(!nzchar(as.character(cases$submission_id)))) {
    stop("Case input contains a missing submission_id.")
  }
  cases$submission_id <- as.character(cases$submission_id)
  cases$sample_month <- as.character(cases$sample_month)
  if (!setequal(unique(cases$sample_month), STUDY_MONTHS)) {
    stop("Case input does not cover the exact 34-month study window.")
  }
  if (any(table(cases$sample_month) < 1L)) {
    stop("Case input contains an empty study month.")
  }
  require_binary_flag(
    cases$battery1_eligible,
    "battery1_eligible"
  )
  require_binary_flag(
    cases$downstream_eligible,
    "downstream_eligible"
  )
  require_binary_flag(cases$gate_pass, "gate_pass", allow_missing = TRUE)
  missing_gate <- is.na(cases$gate_pass)
  if (any(
    missing_gate &
      (as.logical(cases$battery1_eligible) |
        as.logical(cases$downstream_eligible))
  )) {
    stop("A case with a missing affiliation gate is marked analytically eligible.")
  }
  for (name in c(
    "affiliation_recognizable_ev",
    "conditional_psi",
    "observed_unconditional_psi",
    "ingroup_self_positioning_scaled",
    "emotional_intensity_scaled",
    "outgroup_construction_scaled"
  )) {
    require_numeric_domain(cases[[name]], name, 0, 1)
  }
  require_numeric_domain(
    cases$coalition_direction_centered,
    "coalition_direction_centered",
    -1,
    1
  )
  cases
}

read_monthly_predictors <- function(pbi_path, stance_path) {
  pbi <- read.csv(pbi_path, stringsAsFactors = FALSE, check.names = FALSE)
  stance <- read.csv(stance_path, stringsAsFactors = FALSE, check.names = FALSE)
  pbi_required <- c("year_month", "party_brand_index")
  stance_required <- c("year_month", "party", "dimension", "mean")
  if (length(setdiff(pbi_required, names(pbi)))) {
    stop("Party Brand Index input has an incompatible schema.")
  }
  if (length(setdiff(stance_required, names(stance)))) {
    stop("Monthly stance input has an incompatible schema.")
  }
  q1 <- stance[stance$dimension == "q1", stance_required, drop = FALSE]
  if (nrow(q1) != 2L * length(STUDY_MONTHS)) {
    stop("Monthly stance input does not contain one Q1 value per party and month.")
  }
  republican <- q1[q1$party == "republican", c("year_month", "mean")]
  democratic <- q1[q1$party == "democrat", c("year_month", "mean")]
  if (
    anyDuplicated(republican$year_month) ||
      anyDuplicated(democratic$year_month) ||
      !setequal(republican$year_month, STUDY_MONTHS) ||
      !setequal(democratic$year_month, STUDY_MONTHS)
  ) {
    stop("Monthly stance input must contain one Q1 value per party and month.")
  }
  names(republican)[2] <- "q1_republican"
  names(democratic)[2] <- "q1_democratic"
  predictors <- merge(
    pbi[c("year_month", "party_brand_index")],
    republican,
    by = "year_month",
    all = FALSE,
    sort = FALSE
  )
  predictors <- merge(
    predictors,
    democratic,
    by = "year_month",
    all = FALSE,
    sort = FALSE
  )
  predictors <- predictors[match(STUDY_MONTHS, predictors$year_month), ]
  if (
    nrow(predictors) != length(STUDY_MONTHS) ||
      anyNA(predictors) ||
      !identical(as.character(predictors$year_month), STUDY_MONTHS)
  ) {
    stop("Monthly predictors do not align exactly to the study calendar.")
  }
  data.frame(
    sample_month = STUDY_MONTHS,
    party_brand_index_10 = predictors$party_brand_index / 10,
    q1_stance_republican_10 = predictors$q1_republican / 10,
    q1_stance_democratic_10 = predictors$q1_democratic / 10,
    post_breakpoint = as.integer(STUDY_MONTHS >= BREAKPOINT_MONTH),
    stringsAsFactors = FALSE
  )
}


# Submission aggregation ------------------------------------------------------

outcome_names <- c(
  "affiliation_recognizability",
  "conditional_psi",
  "observed_unconditional_psi",
  "ingroup_self_positioning",
  "emotional_intensity",
  "outgroup_construction",
  "republican_skew",
  "democratic_skew"
)

numeric_or_na <- function(x) {
  suppressWarnings(as.numeric(x))
}

logical_true <- function(x) {
  !is.na(x) & as.logical(x)
}

prepare_case_metrics <- function(cases) {
  b1_eligible <- logical_true(cases$battery1_eligible)
  downstream_eligible <- logical_true(cases$downstream_eligible)
  gate_passed <- logical_true(cases$gate_pass)
  affiliation <- numeric_or_na(cases$affiliation_recognizable_ev)
  conditional <- numeric_or_na(cases$conditional_psi)
  unconditional <- numeric_or_na(cases$observed_unconditional_psi)
  ingroup <- numeric_or_na(cases$ingroup_self_positioning_scaled)
  emotional <- numeric_or_na(cases$emotional_intensity_scaled)
  outgroup <- numeric_or_na(cases$outgroup_construction_scaled)
  direction <- numeric_or_na(cases$coalition_direction_centered)
  downstream <- downstream_eligible & gate_passed
  h4_complete <- downstream &
    is.finite(ingroup) &
    is.finite(emotional) &
    is.finite(outgroup)
  data.frame(
    submission_id = cases$submission_id,
    sample_month = cases$sample_month,
    affiliation_recognizability = affiliation,
    affiliation_recognizability_eligible = b1_eligible & is.finite(affiliation),
    conditional_psi = conditional,
    conditional_psi_eligible = downstream & is.finite(conditional),
    observed_unconditional_psi = unconditional,
    observed_unconditional_psi_eligible = b1_eligible & is.finite(unconditional),
    ingroup_self_positioning = ingroup,
    ingroup_self_positioning_eligible = h4_complete,
    emotional_intensity = emotional,
    emotional_intensity_eligible = h4_complete,
    outgroup_construction = outgroup,
    outgroup_construction_eligible = h4_complete,
    republican_skew = pmax(direction, 0),
    republican_skew_eligible = downstream & is.finite(direction),
    democratic_skew = pmax(-direction, 0),
    democratic_skew_eligible = downstream & is.finite(direction),
    h6_direction = direction,
    h6_outcome = conditional,
    h6_eligible = downstream & is.finite(direction) & is.finite(conditional),
    stringsAsFactors = FALSE
  )
}

rowsum_aligned <- function(values, groups, target_groups) {
  result <- rowsum(values, groups, reorder = FALSE)
  as.numeric(result[match(target_groups, rownames(result)), 1])
}

prepare_submission_statistics <- function(cases) {
  metrics <- prepare_case_metrics(cases)
  stats <- unique(metrics[c("submission_id", "sample_month")])
  stats <- stats[order(stats$submission_id, stats$sample_month), ]
  rownames(stats) <- NULL
  metric_groups <- paste(
    metrics$submission_id,
    metrics$sample_month,
    sep = "\u001f"
  )
  statistic_groups <- paste(
    stats$submission_id,
    stats$sample_month,
    sep = "\u001f"
  )
  for (outcome in outcome_names) {
    eligible <- metrics[[paste0(outcome, "_eligible")]]
    values <- ifelse(eligible, metrics[[outcome]], 0)
    stats[[paste0(outcome, "_sum")]] <- rowsum_aligned(
      values, metric_groups, statistic_groups
    )
    stats[[paste0(outcome, "_n")]] <- rowsum_aligned(
      as.numeric(eligible), metric_groups, statistic_groups
    )
  }
  direction_cases <- metrics$h6_eligible
  x <- ifelse(direction_cases, metrics$h6_direction, 0)
  y <- ifelse(direction_cases, metrics$h6_outcome, 0)
  stats$h6_n <- rowsum_aligned(as.numeric(direction_cases), metric_groups, statistic_groups)
  stats$h6_sum_x <- rowsum_aligned(x, metric_groups, statistic_groups)
  stats$h6_sum_y <- rowsum_aligned(y, metric_groups, statistic_groups)
  stats$h6_sum_x2 <- rowsum_aligned(x^2, metric_groups, statistic_groups)
  stats$h6_sum_xy <- rowsum_aligned(x * y, metric_groups, statistic_groups)
  if (!setequal(unique(stats$sample_month), STUDY_MONTHS)) {
    stop("Submission statistics do not cover the exact study calendar.")
  }
  stats
}

monthly_direction_slope <- function(n, sum_x, sum_y, sum_x2, sum_xy) {
  denominator <- sum_x2 - sum_x^2 / n
  if (!is.finite(n) || n < 2 || !is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }
  (sum_xy - sum_x * sum_y / n) / denominator
}

aggregate_monthly <- function(stats, weights = rep(1, nrow(stats))) {
  if (length(weights) != nrow(stats) || any(!is.finite(weights)) || any(weights < 0)) {
    stop("Submission weights are invalid.")
  }
  monthly <- data.frame(sample_month = STUDY_MONTHS, stringsAsFactors = FALSE)
  for (outcome in outcome_names) {
    monthly[[outcome]] <- NA_real_
  }
  monthly$direction_slope <- NA_real_
  for (month_index in seq_along(STUDY_MONTHS)) {
    selected <- stats$sample_month == STUDY_MONTHS[[month_index]]
    w <- weights[selected]
    for (outcome in outcome_names) {
      numerator <- sum(w * stats[[paste0(outcome, "_sum")]][selected])
      denominator <- sum(w * stats[[paste0(outcome, "_n")]][selected])
      monthly[[outcome]][month_index] <- if (denominator > 0) {
        numerator / denominator
      } else {
        NA_real_
      }
    }
    monthly$direction_slope[month_index] <- monthly_direction_slope(
      sum(w * stats$h6_n[selected]),
      sum(w * stats$h6_sum_x[selected]),
      sum(w * stats$h6_sum_y[selected]),
      sum(w * stats$h6_sum_x2[selected]),
      sum(w * stats$h6_sum_xy[selected])
    )
  }
  monthly
}


# Shared bootstrap ------------------------------------------------------------

generate_bootstrap_counts <- function(
  n_replications,
  submission_ids,
  seed = BOOTSTRAP_SEED
) {
  submission_ids <- as.character(submission_ids)
  n_submissions <- length(submission_ids)
  if (n_replications < 1L || n_submissions < 1L) {
    stop("Bootstrap dimensions must be positive.")
  }
  if (anyNA(submission_ids) || anyDuplicated(submission_ids)) {
    stop("Bootstrap submission identifiers must be unique and nonmissing.")
  }
  set.seed(seed)
  counts <- matrix(
    0L,
    nrow = as.integer(n_replications),
    ncol = as.integer(n_submissions)
  )
  colnames(counts) <- submission_ids
  for (replication in seq_len(n_replications)) {
    counts[replication, ] <- tabulate(
      sample.int(n_submissions, n_submissions, replace = TRUE),
      nbins = n_submissions
    )
  }
  counts
}

aggregate_bootstrap_monthly <- function(stats, bootstrap_counts) {
  submission_columns <- match(stats$submission_id, colnames(bootstrap_counts))
  if (anyNA(submission_columns)) {
    stop("Bootstrap counts omit a submission in the statistics table.")
  }
  b <- nrow(bootstrap_counts)
  t <- length(STUDY_MONTHS)
  output <- setNames(
    lapply(c(outcome_names, "direction_slope"), function(x) {
      matrix(NA_real_, nrow = b, ncol = t)
    }),
    c(outcome_names, "direction_slope")
  )
  metric_columns <- c(
    as.vector(rbind(
      paste0(outcome_names, "_sum"),
      paste0(outcome_names, "_n")
    )),
    "h6_n", "h6_sum_x", "h6_sum_y", "h6_sum_x2", "h6_sum_xy"
  )
  for (month_index in seq_along(STUDY_MONTHS)) {
    selected <- which(stats$sample_month == STUDY_MONTHS[[month_index]])
    totals <- bootstrap_counts[
      , submission_columns[selected], drop = FALSE
    ] %*%
      as.matrix(stats[selected, metric_columns, drop = FALSE])
    for (outcome in outcome_names) {
      numerator <- totals[, paste0(outcome, "_sum")]
      denominator <- totals[, paste0(outcome, "_n")]
      output[[outcome]][, month_index] <- ifelse(
        denominator > 0,
        numerator / denominator,
        NA_real_
      )
    }
    h6_n <- totals[, "h6_n"]
    h6_denominator <- totals[, "h6_sum_x2"] -
      totals[, "h6_sum_x"]^2 / h6_n
    output$direction_slope[, month_index] <- ifelse(
      h6_n >= 2 & is.finite(h6_denominator) & h6_denominator > 0,
      (
        totals[, "h6_sum_xy"] -
          totals[, "h6_sum_x"] * totals[, "h6_sum_y"] / h6_n
      ) / h6_denominator,
      NA_real_
    )
  }
  output
}

summarize_monthly_conditional_psi_intervals <- function(
  monthly,
  bootstrap_monthly
) {
  values <- bootstrap_monthly$conditional_psi
  if (
    is.null(values) ||
      !is.matrix(values) ||
      nrow(values) != MONTHLY_CPS_INTERVAL_REPLICATIONS ||
      ncol(values) != length(STUDY_MONTHS)
  ) {
    stop(
      "Monthly Conditional PSI intervals require the fixed initial ",
      MONTHLY_CPS_INTERVAL_REPLICATIONS,
      "-replication shared bootstrap matrix."
    )
  }
  finite_replications <- colSums(is.finite(values))
  if (any(finite_replications != MONTHLY_CPS_INTERVAL_REPLICATIONS)) {
    stop(
      "Every study month must have all ",
      MONTHLY_CPS_INTERVAL_REPLICATIONS,
      " finite Conditional PSI bootstrap replications."
    )
  }
  if (
    length(monthly$conditional_psi) != length(STUDY_MONTHS) ||
      any(!is.finite(monthly$conditional_psi))
  ) {
    stop("Observed monthly Conditional PSI values are incomplete or nonfinite.")
  }
  bounds <- vapply(
    seq_along(STUDY_MONTHS),
    function(month_index) {
      as.numeric(quantile(
        values[, month_index],
        c(0.025, 0.975),
        names = FALSE,
        type = MONTHLY_CPS_INTERVAL_QUANTILE_TYPE,
        na.rm = FALSE
      ))
    },
    numeric(2)
  )
  if (
    any(!is.finite(bounds)) ||
      any(bounds[1, ] > bounds[2, ]) ||
      any(bounds < 0 | bounds > 1)
  ) {
    stop("Monthly Conditional PSI interval bounds are invalid.")
  }
  data.frame(
    method_id = METHOD_ID,
    derivative_id = MONTHLY_CPS_DISPLAY_DERIVATIVE_ID,
    role = "descriptive_figure_support_not_confirmatory_inference",
    sample_month = STUDY_MONTHS,
    conditional_psi_mean = monthly$conditional_psi,
    ci_lower = bounds[1, ],
    ci_upper = bounds[2, ],
    confidence_level = MONTHLY_CPS_INTERVAL_LEVEL,
    interval_method = "pointwise_percentile_bootstrap",
    quantile_type = MONTHLY_CPS_INTERVAL_QUANTILE_TYPE,
    bootstrap_stage = "initial",
    bootstrap_seed = BOOTSTRAP_SEED,
    bootstrap_replications = MONTHLY_CPS_INTERVAL_REPLICATIONS,
    bootstrap_valid_replications = finite_replications,
    resampling_unit = "submission_id",
    stringsAsFactors = FALSE
  )
}


# OLS and HAC(1) --------------------------------------------------------------

require_full_rank <- function(x, name) {
  rank <- qr(x)$rank
  if (rank != ncol(x)) {
    stop(name, " is rank deficient: rank ", rank, " of ", ncol(x), ".")
  }
}

require_positive_variance <- function(value, name) {
  if (!is.finite(value) || value <= 0) {
    stop(name, " is not finite and strictly positive.")
  }
}

require_positive_definite <- function(matrix, name, tolerance = 1e-12) {
  matrix <- (matrix + t(matrix)) / 2
  eigenvalues <- eigen(matrix, symmetric = TRUE, only.values = TRUE)$values
  scale <- max(1, max(abs(eigenvalues)))
  if (any(!is.finite(eigenvalues)) || min(eigenvalues) <= tolerance * scale) {
    stop(name, " is not numerically positive definite.")
  }
}

hac_covariance <- function(x, residuals, lag = HAC_LAG) {
  x <- as.matrix(x)
  residuals <- as.numeric(residuals)
  n <- nrow(x)
  if (length(residuals) != n || n <= lag) {
    stop("HAC inputs are incompatible.")
  }
  require_full_rank(x, "Monthly OLS design")
  bread <- solve(crossprod(x))
  scores <- x * residuals
  meat <- crossprod(scores)
  if (lag > 0L) {
    for (lag_index in seq_len(lag)) {
      weight <- 1 - lag_index / (lag + 1)
      gamma <- crossprod(
        scores[(lag_index + 1L):n, , drop = FALSE],
        scores[1L:(n - lag_index), , drop = FALSE]
      )
      meat <- meat + weight * (gamma + t(gamma))
    }
  }
  covariance <- bread %*% meat %*% bread
  (covariance + t(covariance)) / 2
}

fit_monthly_ols_hac <- function(outcome, predictor, post_breakpoint) {
  outcome <- as.numeric(outcome)
  x <- cbind(
    intercept = 1,
    predictor = as.numeric(predictor),
    post_breakpoint = as.numeric(post_breakpoint)
  )
  if (any(!is.finite(outcome)) || any(!is.finite(x))) {
    stop("Monthly regression contains nonfinite values.")
  }
  coefficients <- as.numeric(solve(crossprod(x), crossprod(x, outcome)))
  names(coefficients) <- colnames(x)
  residuals <- as.numeric(outcome - x %*% coefficients)
  covariance <- hac_covariance(x, residuals, lag = HAC_LAG)
  require_positive_variance(
    covariance["predictor", "predictor"],
    "HAC(1) predictor variance"
  )
  standard_errors <- sqrt(diag(covariance))
  names(standard_errors) <- colnames(x)
  list(
    coefficients = coefficients,
    covariance = covariance,
    standard_errors = standard_errors,
    residuals = residuals,
    design = x
  )
}

bootstrap_scalar_inference <- function(
  observed_outcome,
  bootstrap_outcomes,
  predictor,
  post_breakpoint,
  expected_direction = "positive"
) {
  observed <- fit_monthly_ols_hac(
    observed_outcome,
    predictor,
    post_breakpoint
  )
  beta <- observed$coefficients[["predictor"]]
  se <- observed$standard_errors[["predictor"]]
  observed_statistic <- beta / se
  require_positive_variance(se^2, "Observed HAC(1) predictor variance")
  b <- nrow(bootstrap_outcomes)
  bootstrap_beta <- rep(NA_real_, b)
  bootstrap_se <- rep(NA_real_, b)
  bootstrap_statistic <- rep(NA_real_, b)
  for (replication in seq_len(b)) {
    fit <- tryCatch(
      fit_monthly_ols_hac(
        bootstrap_outcomes[replication, ],
        predictor,
        post_breakpoint
      ),
      error = function(error) {
        stop(
          "Scalar bootstrap replication ", replication,
          " is nonestimable: ", conditionMessage(error)
        )
      }
    )
    bootstrap_beta[[replication]] <- fit$coefficients[["predictor"]]
    bootstrap_se[[replication]] <- fit$standard_errors[["predictor"]]
    bootstrap_statistic[[replication]] <- (
      bootstrap_beta[[replication]] - beta
    ) / bootstrap_se[[replication]]
  }
  if (any(!is.finite(bootstrap_statistic))) {
    stop("A scalar bootstrap replication is nonestimable.")
  }
  exceedances <- sum(abs(bootstrap_statistic) >= abs(observed_statistic))
  p_value <- (exceedances + 1) / (b + 1)
  quantiles <- as.numeric(
    quantile(bootstrap_statistic, c(0.025, 0.975), names = FALSE, type = 6)
  )
  confidence_interval <- c(
    lower = beta - quantiles[[2]] * se,
    upper = beta - quantiles[[1]] * se
  )
  monte_carlo_interval <- binom.test(exceedances, b)$conf.int
  direction_pass <- switch(
    expected_direction,
    positive = beta > 0,
    negative = beta < 0,
    nondirectional = TRUE,
    stop("Unknown expected direction.")
  )
  list(
    estimate = beta,
    hac_standard_error = se,
    observed_statistic = observed_statistic,
    bootstrap_p_value = p_value,
    confidence_interval = confidence_interval,
    exceedances = exceedances,
    replications = b,
    monte_carlo_standard_error = sqrt(p_value * (1 - p_value) / b),
    monte_carlo_interval = monte_carlo_interval,
    direction_pass = direction_pass,
    supported_unadjusted = direction_pass && p_value <= ALPHA,
    bootstrap_estimates = bootstrap_beta,
    bootstrap_standard_errors = bootstrap_se,
    bootstrap_statistics = bootstrap_statistic
  )
}


# Joint component system ------------------------------------------------------

component_names <- c(
  "ingroup_self_positioning",
  "emotional_intensity",
  "outgroup_construction"
)

h4_member_names <- c("H4a", "H4b", "H4c")

h4_equality_matrix <- rbind(
  c(1, -1, 0),
  c(0, 1, -1)
)

h4_pairwise_matrix <- rbind(
  H4a_vs_H4b = c(1, -1, 0),
  H4a_vs_H4c = c(1, 0, -1),
  H4b_vs_H4c = c(0, 1, -1)
)

hac_cross_meat <- function(x, residual_left, residual_right, lag = HAC_LAG) {
  left_scores <- x * as.numeric(residual_left)
  right_scores <- x * as.numeric(residual_right)
  n <- nrow(x)
  meat <- crossprod(left_scores, right_scores)
  if (lag > 0L) {
    for (lag_index in seq_len(lag)) {
      weight <- 1 - lag_index / (lag + 1)
      meat <- meat + weight * (
        crossprod(
          left_scores[(lag_index + 1L):n, , drop = FALSE],
          right_scores[1L:(n - lag_index), , drop = FALSE]
        ) +
          crossprod(
            left_scores[1L:(n - lag_index), , drop = FALSE],
            right_scores[(lag_index + 1L):n, , drop = FALSE]
          )
      )
    }
  }
  meat
}

fit_component_system <- function(outcomes, predictor, post_breakpoint) {
  outcomes <- as.matrix(outcomes)
  if (ncol(outcomes) != length(component_names)) {
    stop("H4 requires exactly three component outcomes.")
  }
  colnames(outcomes) <- component_names
  x <- cbind(
    intercept = 1,
    predictor = as.numeric(predictor),
    post_breakpoint = as.numeric(post_breakpoint)
  )
  if (any(!is.finite(outcomes)) || any(!is.finite(x))) {
    stop("H4 component system contains nonfinite values.")
  }
  require_full_rank(x, "H4 monthly component-system design")
  bread <- solve(crossprod(x))
  coefficients <- bread %*% crossprod(x, outcomes)
  residuals <- outcomes - x %*% coefficients
  slope_covariance <- matrix(
    NA_real_,
    nrow = length(component_names),
    ncol = length(component_names),
    dimnames = list(component_names, component_names)
  )
  for (left in seq_along(component_names)) {
    for (right in seq_along(component_names)) {
      covariance_block <- bread %*%
        hac_cross_meat(
          x,
          residuals[, left],
          residuals[, right],
          lag = HAC_LAG
        ) %*%
        bread
      slope_covariance[left, right] <- covariance_block["predictor", "predictor"]
    }
  }
  slope_covariance <- (slope_covariance + t(slope_covariance)) / 2
  for (component in component_names) {
    require_positive_variance(
      slope_covariance[component, component],
      paste("H4 HAC(1) slope variance for", component)
    )
  }
  slope_estimates <- coefficients["predictor", ]
  list(
    coefficients = coefficients,
    residuals = residuals,
    slope_estimates = slope_estimates,
    slope_covariance = slope_covariance,
    slope_standard_errors = sqrt(diag(slope_covariance))
  )
}

bootstrap_tail_summary <- function(observed_statistic, bootstrap_statistics) {
  if (any(!is.finite(bootstrap_statistics))) {
    stop("Bootstrap statistics contain a nonfinite value.")
  }
  b <- length(bootstrap_statistics)
  exceedances <- sum(abs(bootstrap_statistics) >= abs(observed_statistic))
  p_value <- (exceedances + 1) / (b + 1)
  list(
    p_value = p_value,
    exceedances = exceedances,
    replications = b,
    monte_carlo_standard_error = sqrt(p_value * (1 - p_value) / b),
    monte_carlo_interval = binom.test(exceedances, b)$conf.int
  )
}

studentized_interval <- function(estimate, standard_error, bootstrap_statistics) {
  quantiles <- as.numeric(
    quantile(bootstrap_statistics, c(0.025, 0.975), names = FALSE, type = 6)
  )
  c(
    lower = estimate - quantiles[[2]] * standard_error,
    upper = estimate - quantiles[[1]] * standard_error
  )
}

run_h4 <- function(monthly, bootstrap_monthly, predictors) {
  observed_outcomes <- as.matrix(monthly[component_names])
  observed <- fit_component_system(
    observed_outcomes,
    predictors$party_brand_index_10,
    predictors$post_breakpoint
  )
  b <- nrow(bootstrap_monthly[[component_names[[1]]]])
  bootstrap_slopes <- matrix(
    NA_real_, b, length(component_names),
    dimnames = list(NULL, component_names)
  )
  bootstrap_covariances <- array(
    NA_real_,
    dim = c(length(component_names), length(component_names), b)
  )
  for (replication in seq_len(b)) {
    replication_outcomes <- do.call(
      cbind,
      lapply(component_names, function(outcome) {
        bootstrap_monthly[[outcome]][replication, ]
      })
    )
    fitted <- tryCatch(
      fit_component_system(
        replication_outcomes,
        predictors$party_brand_index_10,
        predictors$post_breakpoint
      ),
      error = function(error) {
        stop(
          "H4 bootstrap replication ", replication,
          " is nonestimable: ", conditionMessage(error)
        )
      }
    )
    bootstrap_slopes[replication, ] <- fitted$slope_estimates
    bootstrap_covariances[, , replication] <- fitted$slope_covariance
  }
  component_rows <- vector("list", length(component_names))
  raw_family_p <- numeric(length(component_names) + 1L)
  names(raw_family_p) <- c(h4_member_names, "H4_component_heterogeneity")
  for (component_index in seq_along(component_names)) {
    estimate <- observed$slope_estimates[[component_index]]
    standard_error <- observed$slope_standard_errors[[component_index]]
    observed_statistic <- estimate / standard_error
    bootstrap_statistics <- (
      bootstrap_slopes[, component_index] - estimate
    ) / sqrt(bootstrap_covariances[component_index, component_index, ])
    summary <- bootstrap_tail_summary(observed_statistic, bootstrap_statistics)
    interval <- studentized_interval(
      estimate, standard_error, bootstrap_statistics
    )
    raw_family_p[[h4_member_names[[component_index]]]] <- summary$p_value
    component_rows[[component_index]] <- data.frame(
      method_id = METHOD_ID,
      hypothesis = h4_member_names[[component_index]],
      outcome = component_names[[component_index]],
      predictor = "party_brand_index_10",
      expected_direction = "positive",
      test_sidedness = "two_sided",
      estimate = estimate,
      hac_standard_error = standard_error,
      observed_statistic = observed_statistic,
      bootstrap_p_value = summary$p_value,
      ci_lower = interval[["lower"]],
      ci_upper = interval[["upper"]],
      bootstrap_replications = summary$replications,
      bootstrap_exceedances = summary$exceedances,
      monte_carlo_standard_error = summary$monte_carlo_standard_error,
      monte_carlo_ci_lower = summary$monte_carlo_interval[[1]],
      monte_carlo_ci_upper = summary$monte_carlo_interval[[2]],
      direction_pass = estimate > 0,
      stringsAsFactors = FALSE
    )
  }
  observed_difference <- as.numeric(
    h4_equality_matrix %*% observed$slope_estimates
  )
  observed_difference_covariance <- h4_equality_matrix %*%
    observed$slope_covariance %*%
    t(h4_equality_matrix)
  require_positive_definite(
    observed_difference_covariance,
    "Observed H4 equality restriction covariance"
  )
  observed_wald <- as.numeric(
    t(observed_difference) %*%
      solve(observed_difference_covariance, observed_difference)
  )
  bootstrap_wald <- rep(NA_real_, b)
  for (replication in seq_len(b)) {
    centered_difference <- as.numeric(
      h4_equality_matrix %*%
        (bootstrap_slopes[replication, ] - observed$slope_estimates)
    )
    replication_covariance <- h4_equality_matrix %*%
      bootstrap_covariances[, , replication] %*%
      t(h4_equality_matrix)
    require_positive_definite(
      replication_covariance,
      paste("H4 bootstrap equality restriction covariance", replication)
    )
    bootstrap_wald[[replication]] <- as.numeric(
      t(centered_difference) %*%
        solve(replication_covariance, centered_difference)
    )
  }
  equality_exceedances <- sum(bootstrap_wald >= observed_wald)
  equality_p <- (equality_exceedances + 1) / (b + 1)
  equality_mc_interval <- binom.test(equality_exceedances, b)$conf.int
  raw_family_p[["H4_component_heterogeneity"]] <- equality_p
  adjusted_family_p <- p.adjust(raw_family_p, method = "holm")
  for (component_index in seq_along(component_rows)) {
    component_rows[[component_index]]$holm_adjusted_p_value <-
      adjusted_family_p[[h4_member_names[[component_index]]]]
    component_rows[[component_index]]$verdict <- if (
      component_rows[[component_index]]$direction_pass &&
        component_rows[[component_index]]$holm_adjusted_p_value <= ALPHA
    ) "SUPPORTED" else "NOT_SUPPORTED"
  }
  equality_row <- data.frame(
    method_id = METHOD_ID,
    hypothesis = "H4_component_heterogeneity",
    outcome = "component_slope_equality",
    predictor = "party_brand_index_10",
    expected_direction = "nondirectional",
    test_sidedness = "two_sided_joint",
    estimate = NA_real_,
    hac_standard_error = NA_real_,
    observed_statistic = observed_wald,
    bootstrap_p_value = equality_p,
    holm_adjusted_p_value = adjusted_family_p[["H4_component_heterogeneity"]],
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    bootstrap_replications = b,
    bootstrap_exceedances = equality_exceedances,
    monte_carlo_standard_error = sqrt(equality_p * (1 - equality_p) / b),
    monte_carlo_ci_lower = equality_mc_interval[[1]],
    monte_carlo_ci_upper = equality_mc_interval[[2]],
    direction_pass = TRUE,
    verdict = if (
      adjusted_family_p[["H4_component_heterogeneity"]] <= ALPHA
    ) "HETEROGENEOUS" else "NOT_HETEROGENEOUS",
    stringsAsFactors = FALSE
  )
  pairwise_rows <- list()
  if (equality_row$verdict == "HETEROGENEOUS") {
    pairwise_raw_p <- numeric(nrow(h4_pairwise_matrix))
    pairwise_rows <- vector("list", nrow(h4_pairwise_matrix))
    for (contrast_index in seq_len(nrow(h4_pairwise_matrix))) {
      contrast <- h4_pairwise_matrix[contrast_index, ]
      estimate <- as.numeric(contrast %*% observed$slope_estimates)
      standard_error <- sqrt(as.numeric(
        contrast %*% observed$slope_covariance %*% contrast
      ))
      require_positive_variance(
        standard_error^2,
        paste("Observed H4 pairwise variance", rownames(h4_pairwise_matrix)[[contrast_index]])
      )
      observed_statistic <- estimate / standard_error
      bootstrap_statistics <- rep(NA_real_, b)
      for (replication in seq_len(b)) {
        replication_variance <- as.numeric(
          contrast %*% bootstrap_covariances[, , replication] %*% contrast
        )
        require_positive_variance(
          replication_variance,
          paste(
            "H4 bootstrap pairwise variance",
            rownames(h4_pairwise_matrix)[[contrast_index]],
            replication
          )
        )
        bootstrap_statistics[[replication]] <- as.numeric(
          contrast %*% (
            bootstrap_slopes[replication, ] - observed$slope_estimates
          )
        ) / sqrt(replication_variance)
      }
      summary <- bootstrap_tail_summary(observed_statistic, bootstrap_statistics)
      interval <- studentized_interval(
        estimate, standard_error, bootstrap_statistics
      )
      pairwise_raw_p[[contrast_index]] <- summary$p_value
      pairwise_rows[[contrast_index]] <- data.frame(
        method_id = METHOD_ID,
        contrast = rownames(h4_pairwise_matrix)[[contrast_index]],
        expected_direction = "nondirectional",
        test_sidedness = "two_sided",
        estimate = estimate,
        hac_standard_error = standard_error,
        observed_statistic = observed_statistic,
        bootstrap_p_value = summary$p_value,
        ci_lower = interval[["lower"]],
        ci_upper = interval[["upper"]],
        bootstrap_replications = summary$replications,
        bootstrap_exceedances = summary$exceedances,
        monte_carlo_standard_error = summary$monte_carlo_standard_error,
        monte_carlo_ci_lower = summary$monte_carlo_interval[[1]],
        monte_carlo_ci_upper = summary$monte_carlo_interval[[2]],
        stringsAsFactors = FALSE
      )
    }
    adjusted_pairwise_p <- p.adjust(pairwise_raw_p, method = "holm")
    for (contrast_index in seq_along(pairwise_rows)) {
      pairwise_rows[[contrast_index]]$holm_adjusted_p_value <-
        adjusted_pairwise_p[[contrast_index]]
      pairwise_rows[[contrast_index]]$verdict <- if (
        adjusted_pairwise_p[[contrast_index]] <= ALPHA
      ) "DIFFERENT" else "NOT_DIFFERENT"
    }
  }
  component_verdict <- data.frame(
    method_id = METHOD_ID,
    hypothesis = "H4",
    decision_rule = paste(
      "H4a, H4b, and H4c estimates must be positive and all three",
      "four-member-family Holm-adjusted p values must be <= .05"
    ),
    verdict = if (all(vapply(
      component_rows,
      function(row) row$verdict[[1]] == "SUPPORTED",
      logical(1)
    ))) "SUPPORTED" else "NOT_SUPPORTED",
    stringsAsFactors = FALSE
  )
  pairwise_gate <- data.frame(
    method_id = METHOD_ID,
    hypothesis = "H4_pairwise_localization",
    gate = "Holm-adjusted H4 component-heterogeneity test <= .05",
    status = if (
      equality_row$verdict == "HETEROGENEOUS"
    ) "EXECUTED" else "NOT_RUN_GATE_CLOSED",
    stringsAsFactors = FALSE
  )
  list(
    family = do.call(rbind, c(component_rows, list(equality_row))),
    pairwise = if (length(pairwise_rows)) do.call(rbind, pairwise_rows) else NULL,
    component_verdict = component_verdict,
    pairwise_gate = pairwise_gate,
    observed_fit = observed,
    bootstrap_slopes = bootstrap_slopes,
    bootstrap_covariances = bootstrap_covariances,
    bootstrap_equality_statistics = bootstrap_wald
  )
}


# H1 -------------------------------------------------------------------------

run_h1 <- function(monthly, bootstrap_monthly, predictors) {
  result <- bootstrap_scalar_inference(
    observed_outcome = monthly$affiliation_recognizability,
    bootstrap_outcomes = bootstrap_monthly$affiliation_recognizability,
    predictor = predictors$party_brand_index_10,
    post_breakpoint = predictors$post_breakpoint,
    expected_direction = "positive"
  )
  data.frame(
    method_id = METHOD_ID,
    hypothesis = "H1",
    outcome = "affiliation_recognizability",
    predictor = "party_brand_index_10",
    expected_direction = "positive",
    test_sidedness = "two_sided",
    estimate = result$estimate,
    hac_standard_error = result$hac_standard_error,
    observed_statistic = result$observed_statistic,
    bootstrap_p_value = result$bootstrap_p_value,
    ci_lower = result$confidence_interval[["lower"]],
    ci_upper = result$confidence_interval[["upper"]],
    bootstrap_replications = result$replications,
    bootstrap_exceedances = result$exceedances,
    monte_carlo_standard_error = result$monte_carlo_standard_error,
    monte_carlo_ci_lower = result$monte_carlo_interval[[1]],
    monte_carlo_ci_upper = result$monte_carlo_interval[[2]],
    direction_pass = result$direction_pass,
    verdict = if (result$supported_unadjusted) "SUPPORTED" else "NOT_SUPPORTED",
    stringsAsFactors = FALSE
  )
}


# H2 -------------------------------------------------------------------------

run_h2 <- function(monthly, bootstrap_monthly, predictors) {
  result <- bootstrap_scalar_inference(
    observed_outcome = monthly$conditional_psi,
    bootstrap_outcomes = bootstrap_monthly$conditional_psi,
    predictor = predictors$party_brand_index_10,
    post_breakpoint = predictors$post_breakpoint,
    expected_direction = "positive"
  )
  data.frame(
    method_id = METHOD_ID,
    hypothesis = "H2",
    outcome = "conditional_psi",
    predictor = "party_brand_index_10",
    expected_direction = "positive",
    test_sidedness = "two_sided",
    estimate = result$estimate,
    hac_standard_error = result$hac_standard_error,
    observed_statistic = result$observed_statistic,
    bootstrap_p_value = result$bootstrap_p_value,
    ci_lower = result$confidence_interval[["lower"]],
    ci_upper = result$confidence_interval[["upper"]],
    bootstrap_replications = result$replications,
    bootstrap_exceedances = result$exceedances,
    monte_carlo_standard_error = result$monte_carlo_standard_error,
    monte_carlo_ci_lower = result$monte_carlo_interval[[1]],
    monte_carlo_ci_upper = result$monte_carlo_interval[[2]],
    direction_pass = result$direction_pass,
    verdict = if (result$supported_unadjusted) "SUPPORTED" else "NOT_SUPPORTED",
    stringsAsFactors = FALSE
  )
}


# H3 -------------------------------------------------------------------------

run_h3 <- function(monthly, bootstrap_monthly, predictors) {
  result <- bootstrap_scalar_inference(
    observed_outcome = monthly$observed_unconditional_psi,
    bootstrap_outcomes = bootstrap_monthly$observed_unconditional_psi,
    predictor = predictors$party_brand_index_10,
    post_breakpoint = predictors$post_breakpoint,
    expected_direction = "positive"
  )
  data.frame(
    method_id = METHOD_ID,
    hypothesis = "H3",
    outcome = "observed_unconditional_psi",
    predictor = "party_brand_index_10",
    expected_direction = "positive",
    test_sidedness = "two_sided",
    estimate = result$estimate,
    hac_standard_error = result$hac_standard_error,
    observed_statistic = result$observed_statistic,
    bootstrap_p_value = result$bootstrap_p_value,
    ci_lower = result$confidence_interval[["lower"]],
    ci_upper = result$confidence_interval[["upper"]],
    bootstrap_replications = result$replications,
    bootstrap_exceedances = result$exceedances,
    monte_carlo_standard_error = result$monte_carlo_standard_error,
    monte_carlo_ci_lower = result$monte_carlo_interval[[1]],
    monte_carlo_ci_upper = result$monte_carlo_interval[[2]],
    direction_pass = result$direction_pass,
    verdict = if (result$supported_unadjusted) "SUPPORTED" else "NOT_SUPPORTED",
    stringsAsFactors = FALSE
  )
}


# H5 -------------------------------------------------------------------------

run_h5 <- function(monthly, bootstrap_monthly, predictors) {
  specifications <- list(
    list(
      hypothesis = "H5a",
      outcome = "republican_skew",
      predictor = "q1_stance_republican_10"
    ),
    list(
      hypothesis = "H5b",
      outcome = "democratic_skew",
      predictor = "q1_stance_democratic_10"
    )
  )
  rows <- vector("list", length(specifications))
  component_support <- logical(length(specifications))
  for (index in seq_along(specifications)) {
    specification <- specifications[[index]]
    result <- bootstrap_scalar_inference(
      observed_outcome = monthly[[specification$outcome]],
      bootstrap_outcomes = bootstrap_monthly[[specification$outcome]],
      predictor = predictors[[specification$predictor]],
      post_breakpoint = predictors$post_breakpoint,
      expected_direction = "positive"
    )
    component_support[[index]] <- result$supported_unadjusted
    rows[[index]] <- data.frame(
      method_id = METHOD_ID,
      hypothesis = specification$hypothesis,
      outcome = specification$outcome,
      predictor = specification$predictor,
      expected_direction = "positive",
      test_sidedness = "two_sided",
      estimate = result$estimate,
      hac_standard_error = result$hac_standard_error,
      observed_statistic = result$observed_statistic,
      bootstrap_p_value = result$bootstrap_p_value,
      ci_lower = result$confidence_interval[["lower"]],
      ci_upper = result$confidence_interval[["upper"]],
      bootstrap_replications = result$replications,
      bootstrap_exceedances = result$exceedances,
      monte_carlo_standard_error = result$monte_carlo_standard_error,
      monte_carlo_ci_lower = result$monte_carlo_interval[[1]],
      monte_carlo_ci_upper = result$monte_carlo_interval[[2]],
      direction_pass = result$direction_pass,
      component_verdict = if (
        result$supported_unadjusted
      ) "PASSES_H5_COMPONENT" else "FAILS_H5_COMPONENT",
      stringsAsFactors = FALSE
    )
  }
  list(
    components = do.call(rbind, rows),
    verdict = data.frame(
      method_id = METHOD_ID,
      hypothesis = "H5",
      decision_rule = paste(
        "both party-matched estimates positive and both two-sided",
        "unadjusted bootstrap p values <= .05"
      ),
      verdict = if (all(component_support)) "SUPPORTED" else "NOT_SUPPORTED",
      stringsAsFactors = FALSE
    )
  )
}


# H6 -------------------------------------------------------------------------

run_h6 <- function(monthly, bootstrap_monthly, predictors) {
  result <- bootstrap_scalar_inference(
    observed_outcome = monthly$direction_slope,
    bootstrap_outcomes = bootstrap_monthly$direction_slope,
    predictor = predictors$party_brand_index_10,
    post_breakpoint = predictors$post_breakpoint,
    expected_direction = "nondirectional"
  )
  data.frame(
    method_id = METHOD_ID,
    hypothesis = "H6",
    outcome = "monthly_conditional_psi_direction_slope",
    predictor = "party_brand_index_10",
    expected_direction = "nondirectional",
    test_sidedness = "two_sided",
    estimate = result$estimate,
    hac_standard_error = result$hac_standard_error,
    observed_statistic = result$observed_statistic,
    bootstrap_p_value = result$bootstrap_p_value,
    ci_lower = result$confidence_interval[["lower"]],
    ci_upper = result$confidence_interval[["upper"]],
    bootstrap_replications = result$replications,
    bootstrap_exceedances = result$exceedances,
    monte_carlo_standard_error = result$monte_carlo_standard_error,
    monte_carlo_ci_lower = result$monte_carlo_interval[[1]],
    monte_carlo_ci_upper = result$monte_carlo_interval[[2]],
    direction_pass = TRUE,
    verdict = if (result$supported_unadjusted) "SUPPORTED" else "NOT_SUPPORTED",
    stringsAsFactors = FALSE
  )
}


# Bounded self-test -----------------------------------------------------------

synthetic_inputs <- function() {
  months <- STUDY_MONTHS
  predictor <- seq(-1.5, 1.5, length.out = length(months))
  post <- as.integer(months >= BREAKPOINT_MONTH)
  rows <- list()
  row_index <- 1L
  for (month_index in seq_along(months)) {
    for (submission_index in 1:12) {
      for (comment_index in 1:3) {
        direction <- -0.9 + 1.8 * (
          (submission_index - 1) * 3 + comment_index - 1
        ) / (12 * 3 - 1)
        affiliation <- plogis(
          -0.2 + 0.18 * predictor[[month_index]] +
            0.05 * post[[month_index]] +
            0.02 * submission_index +
            0.01 * comment_index
        )
        component_a <- plogis(
          -0.5 + 0.14 * predictor[[month_index]] + 0.2 * direction +
            0.02 * sin(month_index + submission_index)
        )
        component_b <- plogis(
          -0.3 + 0.09 * predictor[[month_index]] - 0.1 * direction +
            0.02 * cos(0.7 * month_index + submission_index)
        )
        component_c <- plogis(
          -0.4 + 0.05 * predictor[[month_index]] + 0.1 * direction +
            0.02 * sin(0.4 * month_index + 0.6 * submission_index)
        )
        conditional <- mean(c(component_a, component_b, component_c))
        rows[[row_index]] <- data.frame(
          comment_id = paste(month_index, submission_index, comment_index, sep = "_"),
          submission_id = if (submission_index == 1L && month_index <= 2L) {
            "cross_month_submission_1"
          } else {
            paste("month", month_index, "submission", submission_index, sep = "_")
          },
          sample_month = months[[month_index]],
          battery1_eligible = TRUE,
          downstream_eligible = TRUE,
          affiliation_recognizable_ev = affiliation,
          gate_pass = TRUE,
          conditional_psi = conditional,
          observed_unconditional_psi = conditional,
          ingroup_self_positioning_scaled = component_a,
          emotional_intensity_scaled = component_b,
          outgroup_construction_scaled = component_c,
          coalition_direction_centered = direction,
          stringsAsFactors = FALSE
        )
        row_index <- row_index + 1L
      }
    }
  }
  cases <- do.call(rbind, rows)
  predictors <- data.frame(
    sample_month = months,
    party_brand_index_10 = predictor,
    q1_stance_republican_10 = predictor + 0.25,
    q1_stance_democratic_10 = predictor - 0.25,
    post_breakpoint = post,
    stringsAsFactors = FALSE
  )
  list(cases = cases, predictors = predictors)
}

validate_hac_against_sandwich <- function(monthly, predictors) {
  fitted <- fit_monthly_ols_hac(
    monthly$affiliation_recognizability,
    predictors$party_brand_index_10,
    predictors$post_breakpoint
  )
  reference_model <- lm(
    monthly$affiliation_recognizability ~
      predictors$party_brand_index_10 +
      predictors$post_breakpoint
  )
  reference_covariance <- sandwich::NeweyWest(
    reference_model,
    lag = HAC_LAG,
    prewhite = FALSE,
    adjust = FALSE
  )
  if (
    max(abs(unname(coef(reference_model)) - fitted$coefficients)) > 1e-12 ||
      max(abs(unname(reference_covariance) - fitted$covariance)) > 1e-12
  ) {
    stop("OLS or HAC(1) parity with sandwich::NeweyWest failed.")
  }
  invisible(TRUE)
}

run_self_test_monthly_cps_interval <- function() {
  synthetic <- synthetic_inputs()
  synthetic$cases$submission_id <- rep(
    rep(paste0("display_interval_submission_", 1:12), each = 3),
    times = length(STUDY_MONTHS)
  )
  stats <- prepare_submission_statistics(synthetic$cases)
  monthly <- aggregate_monthly(stats)
  counts <- generate_bootstrap_counts(
    n_replications = MONTHLY_CPS_INTERVAL_REPLICATIONS,
    submission_ids = sort(unique(stats$submission_id)),
    seed = BOOTSTRAP_SEED
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  result <- summarize_monthly_conditional_psi_intervals(
    monthly,
    bootstrap_monthly
  )
  expected_columns <- c(
    "method_id", "derivative_id", "role", "sample_month",
    "conditional_psi_mean", "ci_lower", "ci_upper", "confidence_level",
    "interval_method", "quantile_type", "bootstrap_stage", "bootstrap_seed",
    "bootstrap_replications", "bootstrap_valid_replications", "resampling_unit"
  )
  first_month_bounds <- as.numeric(quantile(
    bootstrap_monthly$conditional_psi[, 1],
    c(0.025, 0.975),
    names = FALSE,
    type = MONTHLY_CPS_INTERVAL_QUANTILE_TYPE,
    na.rm = FALSE
  ))
  if (
    nrow(result) != length(STUDY_MONTHS) ||
      !identical(names(result), expected_columns) ||
      !identical(result$sample_month, STUDY_MONTHS) ||
      max(abs(result$conditional_psi_mean - monthly$conditional_psi)) > 1e-12 ||
      max(abs(c(result$ci_lower[[1]], result$ci_upper[[1]]) - first_month_bounds)) > 1e-12 ||
      any(result$bootstrap_valid_replications != MONTHLY_CPS_INTERVAL_REPLICATIONS) ||
      any(result$bootstrap_stage != "initial")
  ) {
    stop("Monthly Conditional PSI interval self-test failed.")
  }
  cat(
    "PASS: monthly Conditional PSI pointwise shared-bootstrap interval self-test.\n"
  )
  invisible(result)
}

run_self_test_h1 <- function() {
  synthetic <- synthetic_inputs()
  stats <- prepare_submission_statistics(synthetic$cases)
  monthly <- aggregate_monthly(stats)
  counts <- generate_bootstrap_counts(
    n_replications = 199L,
    submission_ids = sort(unique(stats$submission_id)),
    seed = 8128L
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  validate_hac_against_sandwich(monthly, synthetic$predictors)
  result <- run_h1(monthly, bootstrap_monthly, synthetic$predictors)
  required <- c(
    "estimate", "hac_standard_error", "bootstrap_p_value",
    "ci_lower", "ci_upper", "verdict"
  )
  if (
    nrow(result) != 1L ||
      any(!required %in% names(result)) ||
      any(!is.finite(unlist(result[c(
        "estimate", "hac_standard_error", "bootstrap_p_value",
        "ci_lower", "ci_upper"
      )])))
  ) {
    stop("H1 bounded self-test produced an invalid result.")
  }
  cat("PASS: H1 aggregation, OLS, HAC(1), and shared-bootstrap self-test.\n")
  invisible(result)
}

run_self_test_h2 <- function() {
  synthetic <- synthetic_inputs()
  synthetic$cases$conditional_psi[[1]] <- NA_real_
  synthetic$cases$downstream_eligible[[4]] <- FALSE
  synthetic$cases$gate_pass[[7]] <- FALSE
  stats <- prepare_submission_statistics(synthetic$cases)
  monthly <- aggregate_monthly(stats)
  eligible <- with(
    synthetic$cases,
    downstream_eligible &
      gate_pass &
      is.finite(conditional_psi)
  )
  direct_monthly <- tapply(
    synthetic$cases$conditional_psi[eligible],
    synthetic$cases$sample_month[eligible],
    mean
  )
  direct_monthly <- as.numeric(direct_monthly[STUDY_MONTHS])
  if (max(abs(monthly$conditional_psi - direct_monthly)) > 1e-12) {
    stop("H2 eligible-comment monthly aggregation self-test failed.")
  }
  counts <- generate_bootstrap_counts(
    n_replications = 199L,
    submission_ids = sort(unique(stats$submission_id)),
    seed = 8128L
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  result <- run_h2(monthly, bootstrap_monthly, synthetic$predictors)
  required <- c(
    "estimate", "hac_standard_error", "bootstrap_p_value",
    "ci_lower", "ci_upper", "verdict"
  )
  if (
    nrow(result) != 1L ||
      any(!required %in% names(result)) ||
      any(!is.finite(unlist(result[c(
        "estimate", "hac_standard_error", "bootstrap_p_value",
        "ci_lower", "ci_upper"
      )])))
  ) {
    stop("H2 bounded self-test produced an invalid result.")
  }
  cat("PASS: H2 OLS, HAC(1), and shared-bootstrap self-test.\n")
  invisible(result)
}

run_self_test_h3 <- function() {
  synthetic <- synthetic_inputs()
  synthetic$cases$gate_pass[[1]] <- FALSE
  synthetic$cases$downstream_eligible[[1]] <- FALSE
  synthetic$cases$conditional_psi[[1]] <- NA_real_
  synthetic$cases$observed_unconditional_psi[[1]] <- 0
  synthetic$cases$battery1_eligible[[4]] <- FALSE
  synthetic$cases$observed_unconditional_psi[[4]] <- NA_real_
  stats <- prepare_submission_statistics(synthetic$cases)
  monthly <- aggregate_monthly(stats)
  eligible <- with(
    synthetic$cases,
    battery1_eligible & is.finite(observed_unconditional_psi)
  )
  if (!eligible[[1]] || eligible[[4]]) {
    stop("H3 gate-failed-zero eligibility self-test failed.")
  }
  direct_monthly <- tapply(
    synthetic$cases$observed_unconditional_psi[eligible],
    synthetic$cases$sample_month[eligible],
    mean
  )
  direct_monthly <- as.numeric(direct_monthly[STUDY_MONTHS])
  if (max(abs(monthly$observed_unconditional_psi - direct_monthly)) > 1e-12) {
    stop("H3 eligible-comment monthly aggregation self-test failed.")
  }
  counts <- generate_bootstrap_counts(
    n_replications = 199L,
    submission_ids = sort(unique(stats$submission_id)),
    seed = 8128L
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  result <- run_h3(monthly, bootstrap_monthly, synthetic$predictors)
  required <- c(
    "estimate", "hac_standard_error", "bootstrap_p_value",
    "ci_lower", "ci_upper", "verdict"
  )
  if (
    nrow(result) != 1L ||
      any(!required %in% names(result)) ||
      any(!is.finite(unlist(result[c(
        "estimate", "hac_standard_error", "bootstrap_p_value",
        "ci_lower", "ci_upper"
      )])))
  ) {
    stop("H3 bounded self-test produced an invalid result.")
  }
  cat("PASS: H3 OLS, HAC(1), and shared-bootstrap self-test.\n")
  invisible(result)
}

run_self_test_h4 <- function() {
  synthetic <- synthetic_inputs()
  stats <- prepare_submission_statistics(synthetic$cases)
  monthly <- aggregate_monthly(stats)
  counts <- generate_bootstrap_counts(
    n_replications = 199L,
    submission_ids = sort(unique(stats$submission_id)),
    seed = 8128L
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  result <- run_h4(monthly, bootstrap_monthly, synthetic$predictors)
  if (
    nrow(result$family) != 4L ||
      !identical(
        as.character(result$family$hypothesis),
        c(h4_member_names, "H4_component_heterogeneity")
      ) ||
      any(!is.finite(result$family$bootstrap_p_value)) ||
      any(!is.finite(result$family$holm_adjusted_p_value))
  ) {
    stop("H4 family self-test produced an invalid result.")
  }
  separate_variances <- vapply(
    component_names,
    function(outcome) {
      fit_monthly_ols_hac(
        monthly[[outcome]],
        synthetic$predictors$party_brand_index_10,
        synthetic$predictors$post_breakpoint
      )$covariance["predictor", "predictor"]
    },
    numeric(1)
  )
  multivariate_model <- lm(
    as.matrix(monthly[component_names]) ~
      synthetic$predictors$party_brand_index_10 +
      synthetic$predictors$post_breakpoint
  )
  multivariate_covariance <- sandwich::NeweyWest(
    multivariate_model,
    lag = HAC_LAG,
    prewhite = FALSE,
    adjust = FALSE
  )
  predictor_indices <- seq(2L, nrow(multivariate_covariance), by = 3L)
  reference_slope_covariance <- multivariate_covariance[
    predictor_indices, predictor_indices, drop = FALSE
  ]
  if (
    max(abs(diag(result$observed_fit$slope_covariance) - separate_variances)) >
      1e-12 ||
      max(abs(
        result$observed_fit$slope_covariance - reference_slope_covariance
      )) > 1e-12 ||
      max(abs(
        result$observed_fit$slope_covariance -
          t(result$observed_fit$slope_covariance)
      )) > 1e-12
  ) {
    stop("H4 joint HAC covariance self-test failed.")
  }
  if (is.null(result$pairwise) || nrow(result$pairwise) != 3L) {
    stop("H4 gated pairwise localization self-test failed.")
  }
  cap_fixture <- list(
    H4 = list(
      family = data.frame(
        bootstrap_p_value = 0.05,
        monte_carlo_ci_lower = 0.04,
        monte_carlo_ci_upper = 0.06,
        verdict = "HETEROGENEOUS"
      ),
      pairwise = data.frame(
        bootstrap_p_value = 0.50,
        monte_carlo_ci_lower = 0.45,
        monte_carlo_ci_upper = 0.55,
        verdict = "NOT_DIFFERENT"
      ),
      component_verdict = data.frame(verdict = "NOT_SUPPORTED"),
      pairwise_gate = data.frame(status = "EXECUTED")
    )
  )
  cap_result <- apply_monte_carlo_cap_status(cap_fixture, "H4")
  if (
    !is.null(cap_result$H4$pairwise) ||
      cap_result$H4$pairwise_gate$status[[1]] !=
        "INDETERMINATE_MONTE_CARLO_AT_CAP"
  ) {
    stop("H4 Monte Carlo cap must suppress gated pairwise localization.")
  }
  pairwise_cap_fixture <- cap_fixture
  pairwise_cap_fixture$H4$family$bootstrap_p_value <- 0.50
  pairwise_cap_fixture$H4$family$monte_carlo_ci_lower <- 0.45
  pairwise_cap_fixture$H4$family$monte_carlo_ci_upper <- 0.55
  pairwise_cap_fixture$H4$pairwise$bootstrap_p_value <- 0.05
  pairwise_cap_fixture$H4$pairwise$monte_carlo_ci_lower <- 0.04
  pairwise_cap_fixture$H4$pairwise$monte_carlo_ci_upper <- 0.06
  pairwise_cap_result <- apply_monte_carlo_cap_status(
    pairwise_cap_fixture,
    "H4"
  )
  if (
    !is.null(pairwise_cap_result$H4$pairwise) ||
      pairwise_cap_result$H4$pairwise_gate$status[[1]] !=
        "INDETERMINATE_MONTE_CARLO_AT_CAP" ||
      pairwise_cap_result$H4$family$verdict[[1]] != "HETEROGENEOUS"
  ) {
    stop("H4 pairwise-only Monte Carlo cap handling self-test failed.")
  }
  cat("PASS: H4 joint HAC(1), four-member Holm, and gated-pairwise self-test.\n")
  invisible(result)
}

run_self_test_h5 <- function() {
  synthetic <- synthetic_inputs()
  month_index <- match(synthetic$cases$sample_month, STUDY_MONTHS)
  monthly_predictor <- synthetic$predictors$party_brand_index_10[month_index]
  comment_index <- as.integer(sub(".*_", "", synthetic$cases$comment_id))
  original_direction <- c(-0.5, 0, 0.5)[comment_index]
  submission_index <- as.integer(sub(".*_submission_", "", synthetic$cases$submission_id))
  nonlinear_variation <- 0.02 * sin(0.7 * month_index + submission_index)
  synthetic$cases$coalition_direction_centered <- ifelse(
    original_direction >= 0,
    original_direction + 0.08 * monthly_predictor + nonlinear_variation,
    original_direction - 0.08 * monthly_predictor - nonlinear_variation
  )
  stats <- prepare_submission_statistics(synthetic$cases)
  monthly <- aggregate_monthly(stats)
  downstream <- with(
    synthetic$cases,
    downstream_eligible &
      gate_pass &
      is.finite(coalition_direction_centered)
  )
  direct_republican <- tapply(
    pmax(synthetic$cases$coalition_direction_centered[downstream], 0),
    synthetic$cases$sample_month[downstream],
    mean
  )
  direct_democratic <- tapply(
    pmax(-synthetic$cases$coalition_direction_centered[downstream], 0),
    synthetic$cases$sample_month[downstream],
    mean
  )
  if (
    max(abs(
      monthly$republican_skew - as.numeric(direct_republican[STUDY_MONTHS])
    )) > 1e-12 ||
      max(abs(
        monthly$democratic_skew - as.numeric(direct_democratic[STUDY_MONTHS])
      )) > 1e-12
  ) {
    stop("H5 party-skew monthly aggregation self-test failed.")
  }
  counts <- generate_bootstrap_counts(
    n_replications = 199L,
    submission_ids = sort(unique(stats$submission_id)),
    seed = 8128L
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  result <- run_h5(monthly, bootstrap_monthly, synthetic$predictors)
  if (
    nrow(result$components) != 2L ||
      !identical(
        as.character(result$components$predictor),
        c("q1_stance_republican_10", "q1_stance_democratic_10")
      ) ||
      any(!is.finite(result$components$bootstrap_p_value)) ||
      nrow(result$verdict) != 1L
  ) {
    stop("H5 bounded self-test produced an invalid result.")
  }
  failure_predictors <- synthetic$predictors
  failure_predictors$q1_stance_democratic_10 <-
    -failure_predictors$q1_stance_democratic_10
  failure_result <- run_h5(monthly, bootstrap_monthly, failure_predictors)
  democratic_row <- failure_result$components$hypothesis ==
    "H5b"
  if (
    failure_result$components$direction_pass[democratic_row] ||
      failure_result$verdict$verdict != "NOT_SUPPORTED"
  ) {
    stop("H5 intersection-union failure-path self-test failed.")
  }
  cat("PASS: H5 party-matched intersection-union self-test.\n")
  invisible(result)
}

run_self_test_h6 <- function() {
  synthetic <- synthetic_inputs()
  month_index <- match(synthetic$cases$sample_month, STUDY_MONTHS)
  predictor <- synthetic$predictors$party_brand_index_10[month_index]
  direction <- synthetic$cases$coalition_direction_centered
  submission_index <- as.integer(sub(".*_submission_", "", synthetic$cases$submission_id))
  comment_index <- as.integer(sub(".*_", "", synthetic$cases$comment_id))
  synthetic$cases$conditional_psi <-
    0.45 +
    (0.10 + 0.04 * predictor) * direction +
    0.01 * sin(0.6 * month_index + submission_index + comment_index)
  synthetic$cases$downstream_eligible[[1]] <- FALSE
  synthetic$cases$gate_pass[[4]] <- FALSE
  synthetic$cases$coalition_direction_centered[[7]] <- NA_real_
  synthetic$cases$conditional_psi[[10]] <- NA_real_
  stats <- prepare_submission_statistics(synthetic$cases)
  monthly <- aggregate_monthly(stats)
  eligible <- with(
    synthetic$cases,
    downstream_eligible &
      gate_pass &
      is.finite(coalition_direction_centered) &
      is.finite(conditional_psi)
  )
  direct_slopes <- vapply(
    STUDY_MONTHS,
    function(month) {
      selected <- eligible & synthetic$cases$sample_month == month
      unname(coef(lm(
        synthetic$cases$conditional_psi[selected] ~
          synthetic$cases$coalition_direction_centered[selected]
      ))[[2]])
    },
    numeric(1)
  )
  if (max(abs(monthly$direction_slope - direct_slopes)) > 1e-12) {
    stop("H6 within-month direction-slope reconstruction self-test failed.")
  }
  counts <- generate_bootstrap_counts(
    n_replications = 199L,
    submission_ids = sort(unique(stats$submission_id)),
    seed = 8128L
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  checked_replications <- c(1L, 50L, 199L)
  for (replication in checked_replications) {
    case_weights <- counts[
      replication,
      match(synthetic$cases$submission_id, colnames(counts))
    ]
    direct_bootstrap_slopes <- vapply(
      STUDY_MONTHS,
      function(month) {
        selected <- eligible &
          synthetic$cases$sample_month == month &
          case_weights > 0
        unname(coef(lm(
          synthetic$cases$conditional_psi[selected] ~
            synthetic$cases$coalition_direction_centered[selected],
          weights = case_weights[selected]
        ))[[2]])
      },
      numeric(1)
    )
    if (max(abs(
      bootstrap_monthly$direction_slope[replication, ] -
        direct_bootstrap_slopes
    )) > 1e-12) {
      stop("H6 bootstrap Stage-1 reconstruction self-test failed.")
    }
  }
  result <- run_h6(monthly, bootstrap_monthly, synthetic$predictors)
  stage2_reference <- lm(
    monthly$direction_slope ~
      synthetic$predictors$party_brand_index_10 +
      synthetic$predictors$post_breakpoint
  )
  stage2_reference_covariance <- sandwich::NeweyWest(
    stage2_reference,
    lag = HAC_LAG,
    prewhite = FALSE,
    adjust = FALSE
  )
  if (
    abs(
      result$estimate -
        unname(coef(stage2_reference)[[2]])
    ) > 1e-12 ||
      abs(
        result$hac_standard_error -
          sqrt(stage2_reference_covariance[2, 2])
      ) > 1e-12
  ) {
    stop("H6 Stage-2 OLS or HAC(1) parity self-test failed.")
  }
  if (
    nrow(result) != 1L ||
      result$expected_direction != "nondirectional" ||
      result$test_sidedness != "two_sided" ||
      any(!is.finite(unlist(result[c(
        "estimate", "hac_standard_error", "bootstrap_p_value",
        "ci_lower", "ci_upper"
      )])))
  ) {
    stop("H6 bounded self-test produced an invalid result.")
  }
  cat("PASS: H6 two-stage slope reconstruction and inference self-test.\n")
  invisible(result)
}


# Integrated inference --------------------------------------------------------

bind_rows_fill <- function(frames) {
  frames <- frames[!vapply(frames, is.null, logical(1))]
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  normalized <- lapply(frames, function(frame) {
    missing <- setdiff(columns, names(frame))
    for (column in missing) {
      frame[[column]] <- NA
    }
    frame[columns]
  })
  do.call(rbind, normalized)
}

fit_inference_models <- function(
  monthly,
  bootstrap_monthly,
  predictors,
  selected = c("H1", "H2", "H3", "H4", "H5", "H6"),
  existing = list()
) {
  results <- existing
  if ("H1" %in% selected) results$H1 <- run_h1(monthly, bootstrap_monthly, predictors)
  if ("H2" %in% selected) results$H2 <- run_h2(monthly, bootstrap_monthly, predictors)
  if ("H3" %in% selected) results$H3 <- run_h3(monthly, bootstrap_monthly, predictors)
  if ("H4" %in% selected) results$H4 <- run_h4(monthly, bootstrap_monthly, predictors)
  if ("H5" %in% selected) results$H5 <- run_h5(monthly, bootstrap_monthly, predictors)
  if ("H6" %in% selected) results$H6 <- run_h6(monthly, bootstrap_monthly, predictors)
  results
}

interval_overlaps <- function(lower, upper, threshold) {
  is.finite(lower) && is.finite(upper) && lower <= threshold && upper >= threshold
}

scalar_requires_extension <- function(row, threshold = ALPHA) {
  interval_overlaps(
    row$monte_carlo_ci_lower[[1]],
    row$monte_carlo_ci_upper[[1]],
    threshold
  )
}

holm_requires_extension <- function(rows) {
  if (is.null(rows) || !nrow(rows)) return(FALSE)
  ordering <- order(rows$bootstrap_p_value)
  ordered <- rows[ordering, , drop = FALSE]
  family_size <- nrow(ordered)
  thresholds <- ALPHA / (family_size - seq_len(family_size) + 1L)
  threshold_overlap <- vapply(
    seq_len(family_size),
    function(index) {
      interval_overlaps(
        ordered$monte_carlo_ci_lower[[index]],
        ordered$monte_carlo_ci_upper[[index]],
        thresholds[[index]]
      )
    },
    logical(1)
  )
  ordering_uncertain <- FALSE
  if (family_size > 1L) {
    ordering_uncertain <- any(
      ordered$monte_carlo_ci_upper[-family_size] >=
        ordered$monte_carlo_ci_lower[-1L]
    )
  }
  any(threshold_overlap) || ordering_uncertain
}

models_requiring_extension <- function(results) {
  flags <- c(
    H1 = scalar_requires_extension(results$H1),
    H2 = scalar_requires_extension(results$H2),
    H3 = scalar_requires_extension(results$H3),
    H4 = holm_requires_extension(results$H4$family) ||
      holm_requires_extension(results$H4$pairwise),
    H5 = any(vapply(
      seq_len(nrow(results$H5$components)),
      function(index) scalar_requires_extension(results$H5$components[index, ]),
      logical(1)
    )),
    H6 = scalar_requires_extension(results$H6)
  )
  names(flags)[flags]
}

compact_h4_result <- function(result) {
  result[c("family", "pairwise", "component_verdict", "pairwise_gate")]
}

apply_monte_carlo_cap_status <- function(results, unresolved_models) {
  if (!length(unresolved_models)) return(results)
  cap_label <- "INDETERMINATE_MONTE_CARLO_AT_CAP"
  for (hypothesis in intersect(c("H1", "H2", "H3", "H6"), unresolved_models)) {
    results[[hypothesis]]$verdict <- cap_label
  }
  if ("H5" %in% unresolved_models) {
    uncertain_components <- vapply(
      seq_len(nrow(results$H5$components)),
      function(index) scalar_requires_extension(results$H5$components[index, ]),
      logical(1)
    )
    results$H5$components$component_verdict[uncertain_components] <- cap_label
    results$H5$verdict$verdict <- cap_label
  }
  if ("H4" %in% unresolved_models) {
    family_uncertain <- holm_requires_extension(results$H4$family)
    pairwise_uncertain <- holm_requires_extension(results$H4$pairwise)
    if (family_uncertain) {
      results$H4$family$verdict <- cap_label
      results$H4$component_verdict$verdict <- cap_label
    }
    if (family_uncertain || pairwise_uncertain) {
      results$H4$pairwise_gate$status <- cap_label
      results$H4["pairwise"] <- list(NULL)
    }
  }
  results
}

run_bootstrap_stage <- function(
  stats,
  monthly,
  predictors,
  replications,
  selected,
  existing,
  capture_monthly_cps_interval = FALSE
) {
  counts <- generate_bootstrap_counts(
    n_replications = replications,
    submission_ids = sort(unique(stats$submission_id)),
    seed = BOOTSTRAP_SEED
  )
  bootstrap_monthly <- aggregate_bootstrap_monthly(stats, counts)
  monthly_cps_interval <- if (capture_monthly_cps_interval) {
    summarize_monthly_conditional_psi_intervals(monthly, bootstrap_monthly)
  } else {
    NULL
  }
  results <- fit_inference_models(
    monthly,
    bootstrap_monthly,
    predictors,
    selected = selected,
    existing = existing
  )
  if (!is.null(results$H4)) {
    results$H4 <- compact_h4_result(results$H4)
  }
  rm(counts, bootstrap_monthly)
  invisible(gc())
  list(
    results = results,
    monthly_cps_interval = monthly_cps_interval
  )
}

validate_diagnostic_monthly_parity <- function(monthly, path) {
  reference <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  mapping <- c(
    affiliation_recognizability = "affiliation_recognizable_ev_mean",
    conditional_psi = "conditional_psi_mean",
    observed_unconditional_psi = "observed_unconditional_psi_mean",
    ingroup_self_positioning = "ingroup_self_positioning_mean",
    emotional_intensity = "emotional_intensity_mean",
    outgroup_construction = "outgroup_construction_mean",
    republican_skew = "rep_skew_mean",
    democratic_skew = "dem_skew_mean",
    direction_slope = "direction_slope"
  )
  differences <- vapply(
    names(mapping),
    function(outcome) {
      selected <- reference[
        reference$outcome == mapping[[outcome]],
        c("sample_month", "case_value"),
        drop = FALSE
      ]
      selected <- selected[match(STUDY_MONTHS, selected$sample_month), ]
      if (nrow(selected) != length(STUDY_MONTHS) || anyNA(selected)) {
        stop("Diagnostic monthly reference is incomplete for ", outcome, ".")
      }
      max(abs(monthly[[outcome]] - selected$case_value))
    },
    numeric(1)
  )
  if (max(differences) > 1e-12) {
    stop("Case-level aggregation does not reproduce the diagnostic monthly data.")
  }
  invisible(differences)
}

write_inference_outputs <- function(
  paths,
  monthly,
  predictors,
  monthly_cps_interval,
  results,
  stage_log,
  n_cases,
  n_submissions
) {
  if (dir.exists(paths$output_dir) && length(list.files(paths$output_dir))) {
    stop("Inference output directory already exists and is not empty: ", paths$output_dir)
  }
  dir.create(paths$output_dir, recursive = TRUE, showWarnings = FALSE)
  monthly_output <- merge(
    predictors,
    monthly,
    by = "sample_month",
    all = TRUE,
    sort = FALSE
  )
  monthly_output <- monthly_output[
    match(STUDY_MONTHS, monthly_output$sample_month),
  ]
  scalar_results <- bind_rows_fill(list(
    results$H1,
    results$H2,
    results$H3,
    results$H4$family,
    results$H5$components,
    results$H6
  ))
  hypothesis_verdicts <- data.frame(
    method_id = METHOD_ID,
    hypothesis = c(
      "H1", "H2", "H3", "H4a", "H4b", "H4c",
      "H4", "H4_component_heterogeneity", "H5", "H6"
    ),
    verdict = c(
      results$H1$verdict,
      results$H2$verdict,
      results$H3$verdict,
      results$H4$family$verdict[match("H4a", results$H4$family$hypothesis)],
      results$H4$family$verdict[match("H4b", results$H4$family$hypothesis)],
      results$H4$family$verdict[match("H4c", results$H4$family$hypothesis)],
      results$H4$component_verdict$verdict,
      results$H4$family$verdict[
        match("H4_component_heterogeneity", results$H4$family$hypothesis)
      ],
      results$H5$verdict$verdict,
      results$H6$verdict
    ),
    stringsAsFactors = FALSE
  )
  write.csv(
    monthly_output,
    file.path(paths$output_dir, "monthly_analysis_data.csv"),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    monthly_cps_interval,
    file.path(paths$output_dir, "monthly_conditional_psi_intervals.csv"),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    scalar_results,
    file.path(paths$output_dir, "confirmatory_inference.csv"),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    hypothesis_verdicts,
    file.path(paths$output_dir, "hypothesis_verdicts.csv"),
    row.names = FALSE,
    na = ""
  )
  if (!is.null(results$H4$pairwise)) {
    write.csv(
      results$H4$pairwise,
      file.path(paths$output_dir, "h4_pairwise_localization.csv"),
      row.names = FALSE,
      na = ""
    )
  }
  write.csv(
    results$H5$verdict,
    file.path(paths$output_dir, "h5_conjunctive_verdict.csv"),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    results$H4$component_verdict,
    file.path(paths$output_dir, "h4_component_verdict.csv"),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    results$H4$pairwise_gate,
    file.path(paths$output_dir, "h4_pairwise_gate.csv"),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    stage_log,
    file.path(paths$output_dir, "bootstrap_stage_decisions.csv"),
    row.names = FALSE,
    na = ""
  )
  summary <- list(
    status = "COMPLETE",
    method_id = METHOD_ID,
    executed_at = format(Sys.time(), tz = "Europe/London", usetz = TRUE),
    bootstrap_seed = BOOTSTRAP_SEED,
    initial_replications = INITIAL_BOOTSTRAP_REPLICATIONS,
    extended_replications = EXTENDED_BOOTSTRAP_REPLICATIONS,
    exceptional_replications = EXCEPTIONAL_BOOTSTRAP_REPLICATIONS,
    hac_lag = HAC_LAG,
    hac_kernel = "Bartlett",
    hac_prewhite = FALSE,
    hac_finite_sample_adjustment = FALSE,
    regression = "monthly_outcome ~ predictor + post_breakpoint",
    test_sidedness = "two_sided",
    bootstrap_unit = "submission_id",
    bootstrap_statistic = "centered HAC(1)-studentized",
    monthly_conditional_psi_display_interval = list(
      derivative_id = MONTHLY_CPS_DISPLAY_DERIVATIVE_ID,
      role = "descriptive_figure_support_not_confirmatory_inference",
      interval_method = "pointwise_percentile_bootstrap",
      confidence_level = MONTHLY_CPS_INTERVAL_LEVEL,
      quantile_type = MONTHLY_CPS_INTERVAL_QUANTILE_TYPE,
      bootstrap_stage = "initial",
      bootstrap_seed = BOOTSTRAP_SEED,
      bootstrap_replications = MONTHLY_CPS_INTERVAL_REPLICATIONS,
      simultaneous = FALSE,
      changes_confirmatory_results_or_verdicts = FALSE
    ),
    output_mode = "direct",
    study_months = STUDY_MONTHS,
    case_count = n_cases,
    submission_count = n_submissions,
    inputs = unname(vapply(
      paths[c(
        "case_input", "diagnostic_reference", "pbi_input", "stance_input"
      )],
      function(path) {
        normalized_root <- paste0(
          normalizePath(paths$repo_root, winslash = "/", mustWork = TRUE),
          "/"
        )
        normalized_path <- normalizePath(
          path,
          winslash = "/",
          mustWork = TRUE
        )
        if (!startsWith(normalized_path, normalized_root)) {
          stop("Run-summary input is outside the repository root.")
        }
        substring(normalized_path, nchar(normalized_root) + 1L)
      },
      character(1)
    )),
    outputs = sort(list.files(paths$output_dir)),
    stage_decisions = stage_log
  )
  jsonlite::write_json(
    summary,
    file.path(paths$output_dir, "run_summary.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
}

run_production_inference <- function() {
  paths <- default_paths()
  require_files(paths)
  cases <- read_case_input(paths$case_input)
  predictors <- read_monthly_predictors(paths$pbi_input, paths$stance_input)
  stats <- prepare_submission_statistics(cases)
  monthly <- aggregate_monthly(stats)
  validate_diagnostic_monthly_parity(monthly, paths$diagnostic_reference)
  stage_log <- data.frame(
    stage = "initial",
    replications = INITIAL_BOOTSTRAP_REPLICATIONS,
    selected_models = "H1|H2|H3|H4|H5|H6",
    models_still_uncertain = NA_character_,
    stringsAsFactors = FALSE
  )
  initial_stage <- run_bootstrap_stage(
    stats,
    monthly,
    predictors,
    INITIAL_BOOTSTRAP_REPLICATIONS,
    c("H1", "H2", "H3", "H4", "H5", "H6"),
    list(),
    capture_monthly_cps_interval = TRUE
  )
  results <- initial_stage$results
  monthly_cps_interval <- initial_stage$monthly_cps_interval
  selected <- models_requiring_extension(results)
  stage_log$models_still_uncertain[[1]] <- paste(selected, collapse = "|")
  if (length(selected)) {
    extended_stage <- run_bootstrap_stage(
      stats,
      monthly,
      predictors,
      EXTENDED_BOOTSTRAP_REPLICATIONS,
      selected,
      results
    )
    results <- extended_stage$results
    next_selected <- models_requiring_extension(results)
    stage_log <- rbind(
      stage_log,
      data.frame(
        stage = "extended",
        replications = EXTENDED_BOOTSTRAP_REPLICATIONS,
        selected_models = paste(selected, collapse = "|"),
        models_still_uncertain = paste(next_selected, collapse = "|"),
        stringsAsFactors = FALSE
      )
    )
    selected <- next_selected
  }
  if (length(selected)) {
    exceptional_stage <- run_bootstrap_stage(
      stats,
      monthly,
      predictors,
      EXCEPTIONAL_BOOTSTRAP_REPLICATIONS,
      selected,
      results
    )
    results <- exceptional_stage$results
    stage_log <- rbind(
      stage_log,
      data.frame(
        stage = "exceptional",
        replications = EXCEPTIONAL_BOOTSTRAP_REPLICATIONS,
        selected_models = paste(selected, collapse = "|"),
        models_still_uncertain = paste(
          models_requiring_extension(results), collapse = "|"
        ),
        stringsAsFactors = FALSE
      )
    )
    unresolved_at_cap <- models_requiring_extension(results)
    results <- apply_monte_carlo_cap_status(results, unresolved_at_cap)
  }
  write_inference_outputs(
    paths,
    monthly,
    predictors,
    monthly_cps_interval,
    results,
    stage_log,
    n_cases = nrow(cases),
    n_submissions = length(unique(cases$submission_id))
  )
  cat("PASS: inference outputs written to ", paths$output_dir, "\n", sep = "")
  invisible(results)
}


# Entrypoint ------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--run")) {
    run_production_inference()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-all")) {
    run_self_test_monthly_cps_interval()
    run_self_test_h1()
    run_self_test_h2()
    run_self_test_h3()
    run_self_test_h4()
    run_self_test_h5()
    run_self_test_h6()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-monthly-cps-interval")) {
    run_self_test_monthly_cps_interval()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-h1")) {
    run_self_test_h1()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-h2")) {
    run_self_test_h2()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-h3")) {
    run_self_test_h3()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-h4")) {
    run_self_test_h4()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-h5")) {
    run_self_test_h5()
    return(invisible(NULL))
  }
  if (identical(args, "--self-test-h6")) {
    run_self_test_h6()
    return(invisible(NULL))
  }
  stop("Use --run, or a --self-test option for a bounded implementation check.")
}

if (sys.nframe() == 0L) {
  main()
}

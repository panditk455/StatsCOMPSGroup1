# simulation_functions.R
# All core simulation functions — source this from any run script.
# No data loading, no model fitting. Parameters are hardcoded from fit_inc / fit_err.

library(dplyr)
library(tidyr)
library(purrr)


# Turning log-odds into probabilities here.
inv_logit  <- function(x) 1 / (1 + exp(-x))

# Nudging probs away from exact 0 and 1 here.
# Doing this keeps rbinom() safe.
clamp_prob <- function(p, lo = 1e-6, hi = 1 - 1e-6) pmax(lo, pmin(hi, p))

# Using lookup labels here for the integer-coded values.
# It stays faster this way.
DIFF_LEVELS  <- c("VEasy", "Easy", "Norm", "Diff", "VDiff")
QUAL1_LEVELS <- c("A", "B", "C")
QUAL2_LEVELS <- c("A", "B", "C", "D")


# Hardcoded model parameters from fit_inc and fit_err on the real data.
# These never need to be recomputed — paste them here once and source everywhere.

DEFAULT_BETA_INC <- list(
  intercept =  0.1495,
  diff      = c(-17.2028, -5.0423, -1.1706, 0.0000, 0.9113),
  skill     = c(0, 0, 0),
  quality1  = c(0.0000,   0.5377,  1.6007),
  quality2  = c(0.0000,   0.2857,  0.2219, 0.6841),
  hawthorne = 0.5
)
DEFAULT_SD_EXAM_INC <- 1.1677
DEFAULT_SD_CSET_INC <- 0.8605

DEFAULT_BETA_ERR <- list(
  intercept = -2.2259,
  diff      = c(-3.0829, -3.0829, -0.6228, 0.0000, 0.5032),
  skill     = c(0, 0, 0),
  quality1  = c(0.0000,  0.1779,  1.4058),
  quality2  = c(0.0000,  0.6122,  0.5528, 0.7438),
  hawthorne = 0.0
)
DEFAULT_SD_EXAM_ERR <- 0.9655
DEFAULT_SD_CSET_ERR <- 0.0


one_simulation <- function(
  # Scenario inputs
  per_diff        = rep(0.2, 5),       # [VEasy, Easy, Norm, Diff, VDiff] proportions
  per_exam_skill  = rep(1/3, 3),       # [Low, Medium, High] examiner skill proportions
  per_quality1    = c(1/3, 1/3, 1/3), # Q1 quality [A, B, C] proportions
  per_quality2    = rep(0.25, 4),      # Q2 quality [A, B, C, D] proportions
  Hawthorne       = 0,                 # 0 = no effect; higher = more inconclusive shift
  n_examiners     = 49,
  nKQ             = 50,
  nQQ             = 50,
  mated_prop      = 0.5,
  seed            = NULL,
  # Partial assignment: from real data each examiner sees ~29.4 csets per type
  mean_assign     = 29.4,
  sd_assign       = 13.2,
  min_assign      = 9,
  # Inconclusive model parameters (from fit_inc on real data)
  beta_inc        = DEFAULT_BETA_INC,
  sd_exam_inc     = DEFAULT_SD_EXAM_INC,
  sd_cset_inc     = DEFAULT_SD_CSET_INC,
  # Error model parameters (from fit_err on real data)
  beta_err        = DEFAULT_BETA_ERR,
  sd_exam_err     = DEFAULT_SD_EXAM_ERR,
  sd_cset_err     = DEFAULT_SD_CSET_ERR
) {
  if (!is.null(seed)) set.seed(seed)

  # Checking the proportions sum to 1 here.
  stopifnot(
    length(per_diff) == 5, length(per_exam_skill) == 3,
    length(per_quality1) == length(beta_inc$quality1),
    length(per_quality2) == length(beta_inc$quality2),
    abs(sum(per_diff) - 1) < 1e-8, abs(sum(per_exam_skill) - 1) < 1e-8,
    abs(sum(per_quality1) - 1) < 1e-8, abs(sum(per_quality2) - 1) < 1e-8
  )

  n_csets <- nKQ + nQQ

  # Drawing examiner effects here.
  # Each examiner keeps the same tendency across all items.
  examiners <- tibble(
    AnonID   = paste0("E", sprintf("%03d", seq_len(n_examiners))),
    SkillIdx = sample(1:3, n_examiners, replace = TRUE, prob = per_exam_skill),
    u_inc    = rnorm(n_examiners, 0, sd_exam_inc),
    u_err    = rnorm(n_examiners, 0, sd_exam_err)
  )

  # Drawing cset effects and assigning features here.
  # This is where the difficulty mix gets set.
  csets <- tibble(
    Cset       = paste0("C", sprintf("%04d", seq_len(n_csets))),
    Comparison = c(rep("KQ", nKQ), rep("QQ", nQQ)),
    Mating     = sample(c("Mated", "Nonmated"), n_csets, replace = TRUE,
                        prob = c(mated_prop, 1 - mated_prop)),
    DiffIdx    = sample(1:5, n_csets, replace = TRUE, prob = per_diff),
    Qual1Idx   = sample(seq_along(per_quality1), n_csets, replace = TRUE,
                        prob = per_quality1),
    Qual2Idx   = sample(seq_along(per_quality2), n_csets, replace = TRUE,
                        prob = per_quality2),
    Difficulty = DIFF_LEVELS[DiffIdx],
    Quality1   = QUAL1_LEVELS[Qual1Idx],
    Quality2   = QUAL2_LEVELS[Qual2Idx],
    v_inc      = rnorm(n_csets, 0, sd_cset_inc),
    v_err      = rnorm(n_csets, 0, sd_cset_err)
  )

  # Partial assignment: each examiner sees a random subset of KQ and QQ csets.
  # Matches real data structure (mean=29.4, sd=13.2, min=9, max=nKQ/nQQ per type).
  kq_csets <- filter(csets, Comparison == "KQ")
  qq_csets <- filter(csets, Comparison == "QQ")

  n_kq_assigned <- pmin(pmax(round(rnorm(n_examiners, mean_assign, sd_assign)),
                             min_assign), nKQ)
  n_qq_assigned <- pmin(pmax(round(rnorm(n_examiners, mean_assign, sd_assign)),
                             min_assign), nQQ)

  sim_data <- map_dfr(seq_len(n_examiners), function(i) {
    bind_rows(
      slice_sample(kq_csets, n = n_kq_assigned[i]),
      slice_sample(qq_csets, n = n_qq_assigned[i])
    ) %>%
      mutate(
        AnonID   = examiners$AnonID[i],
        SkillIdx = examiners$SkillIdx[i],
        u_inc    = examiners$u_inc[i],
        u_err    = examiners$u_err[i]
      )
  })

  # Generating inconclusive calls here.
  # Build the probability, then sample the outcome.
  sim_data <- sim_data %>%
    mutate(
      lp_inc =
        beta_inc$intercept          +
        beta_inc$diff[DiffIdx]      +
        beta_inc$skill[SkillIdx]    +
        beta_inc$quality1[Qual1Idx] +
        beta_inc$quality2[Qual2Idx] +
        beta_inc$hawthorne * Hawthorne +
        u_inc + v_inc,
      p_inc        = clamp_prob(inv_logit(lp_inc)),
      Inconclusive = rbinom(n(), 1, p_inc)
    )

  # Generating errors only for conclusive rows here.
  # Using indexing so the NA rows do not break things.
  sim_data <- sim_data %>%
    mutate(lp_err = NA_real_, p_err = NA_real_, Error = NA_integer_)

  conclusive_idx <- which(sim_data$Inconclusive == 0)

  sim_data$lp_err[conclusive_idx] <-
    beta_err$intercept +
    beta_err$diff[    sim_data$DiffIdx[ conclusive_idx]] +
    beta_err$skill[   sim_data$SkillIdx[conclusive_idx]] +
    beta_err$quality1[sim_data$Qual1Idx[conclusive_idx]] +
    beta_err$quality2[sim_data$Qual2Idx[conclusive_idx]] +
    beta_err$hawthorne * Hawthorne +
    sim_data$u_err[conclusive_idx] +
    sim_data$v_err[conclusive_idx]

  sim_data$p_err[conclusive_idx] <-
    clamp_prob(inv_logit(sim_data$lp_err[conclusive_idx]))

  sim_data$Error[conclusive_idx] <-
    rbinom(length(conclusive_idx), 1, sim_data$p_err[conclusive_idx])

  # Rebuilding decision labels here.
  # This keeps the coding aligned with the real data.
  sim_data <- sim_data %>%
    mutate(
      Decision_D = case_when(
        Inconclusive == 1                 ~ "Insuff",
        Error == 0 & Mating == "Mated"    ~ "ID",      # correct identification
        Error == 0 & Mating == "Nonmated" ~ "Excl",    # correct exclusion
        Error == 1 & Mating == "Mated"    ~ "Excl",    # false exclusion (FN)
        Error == 1 & Mating == "Nonmated" ~ "ID",      # false ID (FP)
        TRUE                              ~ NA_character_
      )
    )
  sim_data
}


bootstrap_statistic <- function(df, decision_col = "Decision_D") {

  dec    <- df[[decision_col]]
  n_tot  <- nrow(df)
  n_inc  <- sum(dec == "Insuff", na.rm = TRUE)
  n_conc <- sum(dec != "Insuff" & !is.na(dec))
  conc   <- df[!is.na(dec) & dec != "Insuff", ]
  n_err  <- sum(conc$Error == 1, na.rm = TRUE)

  # Getting the overall rates here.
  overall <- list(
    inc_rate = n_inc / n_tot,
    err_rate = if (n_conc > 0) n_err / n_conc else NA_real_,
    n_total  = n_tot, n_inc = n_inc, n_conc = n_conc, n_err = n_err
  )

  # Breaking results out by examiner here.
  by_examiner <- df %>%
    mutate(.dec = .data[[decision_col]]) %>%
    group_by(AnonID) %>%
    summarise(
      n_total  = n(),
      n_inc    = sum(.dec == "Insuff", na.rm = TRUE),
      n_conc   = sum(.dec != "Insuff" & !is.na(.dec)),
      n_err    = sum(Error == 1 & .dec != "Insuff", na.rm = TRUE),
      inc_rate = n_inc / n_total,
      err_rate = if_else(n_conc > 0, n_err / n_conc, NA_real_),
      .groups  = "drop"
    )

  # Breaking results out by cset here.
  by_cset <- df %>%
    mutate(.dec = .data[[decision_col]]) %>%
    group_by(Cset, Difficulty, Mating) %>%
    summarise(
      n_total  = n(),
      n_inc    = sum(.dec == "Insuff",  na.rm = TRUE),
      n_conc   = sum(.dec != "Insuff" & !is.na(.dec)),
      n_err    = sum(Error == 1 & .dec != "Insuff", na.rm = TRUE),
      inc_rate = n_inc / n_total,
      err_rate = if_else(n_conc > 0, n_err / n_conc, NA_real_),
      .groups  = "drop"
    )

  list(overall = overall, by_examiner = by_examiner, by_cset = by_cset)
}


true_parameters <- function(
    per_diff       = rep(0.2, 5),
    per_exam_skill = rep(1/3, 3),
    per_quality1   = c(1/3, 1/3, 1/3),
    per_quality2   = rep(0.25, 4),
    Hawthorne      = 0,
    mated_prop     = 0.5,
    beta_inc       = DEFAULT_BETA_INC,
    sd_exam_inc    = DEFAULT_SD_EXAM_INC,
    sd_cset_inc    = DEFAULT_SD_CSET_INC,
    beta_err       = DEFAULT_BETA_ERR,
    sd_exam_err    = DEFAULT_SD_EXAM_ERR,
    sd_cset_err    = DEFAULT_SD_CSET_ERR,
    n_mc           = 50000   # more draws = more accurate, but slower
) {
  # Sampling covariates fresh on every draw here.
  # Doing this avoids the old averaging shortcut.
  diff_id  <- sample(seq_along(per_diff),       n_mc, replace = TRUE, prob = per_diff)
  skill_id <- sample(seq_along(per_exam_skill), n_mc, replace = TRUE, prob = per_exam_skill)
  q1_id    <- sample(seq_along(per_quality1),   n_mc, replace = TRUE, prob = per_quality1)
  q2_id    <- sample(seq_along(per_quality2),   n_mc, replace = TRUE, prob = per_quality2)

  # Drawing random effects separately here.
  u_inc <- rnorm(n_mc, 0, sd_exam_inc)
  v_inc <- rnorm(n_mc, 0, sd_cset_inc)
  u_err <- rnorm(n_mc, 0, sd_exam_err)
  v_err <- rnorm(n_mc, 0, sd_cset_err)

  # Building the full linear predictors here.
  eta_inc <- beta_inc$intercept + beta_inc$diff[diff_id] +
             beta_inc$skill[skill_id] + beta_inc$quality1[q1_id] +
             beta_inc$quality2[q2_id] + beta_inc$hawthorne * Hawthorne +
             u_inc + v_inc

  eta_err <- beta_err$intercept + beta_err$diff[diff_id] +
             beta_err$skill[skill_id] + beta_err$quality1[q1_id] +
             beta_err$quality2[q2_id] + beta_err$hawthorne * Hawthorne +
             u_err + v_err

  p_inc <- inv_logit(eta_inc)
  p_err <- inv_logit(eta_err)

  # Computing the true error rate conditional on being conclusive.
  # Only non-inconclusive responses can contribute errors.
  list(
    true_inc_rate = mean(p_inc),
    true_err_rate = sum(p_err * (1 - p_inc)) / sum(1 - p_inc),
    mated_prop    = mated_prop
  )
}


Sim_boot_cov <- function(
    per_diff        = rep(0.2, 5),
    per_exam_skill  = rep(1/3, 3),
    per_quality1    = c(1/3, 1/3, 1/3),
    per_quality2    = rep(0.25, 4),
    Hawthorne       = 0,
    n_examiners     = 49,
    nKQ             = 50,
    nQQ             = 50,
    mated_prop      = 0.5,
    beta_inc        = DEFAULT_BETA_INC,
    sd_exam_inc     = DEFAULT_SD_EXAM_INC,
    sd_cset_inc     = DEFAULT_SD_CSET_INC,
    beta_err        = DEFAULT_BETA_ERR,
    sd_exam_err     = DEFAULT_SD_EXAM_ERR,
    sd_cset_err     = DEFAULT_SD_CSET_ERR,
    M                = 200,   # outer repetitions (datasets generated)
    B                = 300,   # bootstrap draws per repetition
    bootstrap_method = c("row", "examiner", "cset"),
    conf_level       = 0.95,
    decision_col     = "Decision_D",
    seed             = 42,
    keep_last        = FALSE,  # if TRUE: return last repetition's bootstrap table
    return_data      = FALSE   # if TRUE: return last repetition's simulated dataset
) {
  bootstrap_method <- match.arg(bootstrap_method)
  alpha <- 1 - conf_level

  # Keeping args separate here because true_parameters() takes fewer inputs.
  true_par_args <- list(per_diff = per_diff, per_exam_skill = per_exam_skill,
                        per_quality1 = per_quality1, per_quality2 = per_quality2,
                        Hawthorne = Hawthorne, mated_prop = mated_prop,
                        beta_inc = beta_inc, sd_exam_inc = sd_exam_inc,
                        sd_cset_inc = sd_cset_inc, beta_err = beta_err,
                        sd_exam_err = sd_exam_err, sd_cset_err = sd_cset_err)

  sim_args <- c(true_par_args, list(n_examiners = n_examiners, nKQ = nKQ, nQQ = nQQ))

  # Getting the ground-truth rates here.
  message("Computing true parameters via Monte Carlo...")
  true_par <- do.call(true_parameters, true_par_args)
  message(sprintf("  true_inc_rate = %.4f  |  true_err_rate = %.4f",
                  true_par$true_inc_rate, true_par$true_err_rate))
  message(sprintf("Running %d reps x %d boots [method = %s]...",
                  M, B, bootstrap_method))

  results   <- vector("list", M)
  last_boot <- NULL; last_sim <- NULL

  for (m in seq_len(M)) {
    if (m %% 25 == 0) message(sprintf("  rep %d / %d", m, M))
    set.seed(seed + m)

    # Generating one fake dataset here.
    sim_df   <- do.call(one_simulation, sim_args)
    obs_stat <- bootstrap_statistic(sim_df, decision_col)$overall

    # Running the bootstrap loop.
    boot_stats <- vector("list", B)
    for (b in seq_len(B)) {
      boot_df <- switch(bootstrap_method,

        # Resampling rows here — ignores both clustering levels.
        "row" = sim_df[sample(nrow(sim_df), nrow(sim_df), replace = TRUE), ],

        # Resampling whole examiners here — renaming duplicates keeps them separate.
        "examiner" = {
          exams   <- unique(sim_df$AnonID)
          sampled <- sample(exams, length(exams), replace = TRUE)
          lapply(seq_along(sampled), function(i)
            sim_df %>% filter(AnonID == sampled[i]) %>%
              mutate(AnonID = paste0(AnonID, "_b", i))) %>% bind_rows()
        },

        # Resampling whole csets here — renaming duplicates keeps them separate.
        "cset" = {
          cs      <- unique(sim_df$Cset)
          sampled <- sample(cs, length(cs), replace = TRUE)
          lapply(seq_along(sampled), function(i)
            sim_df %>% filter(Cset == sampled[i]) %>%
              mutate(Cset = paste0(Cset, "_b", i))) %>% bind_rows()
        }
      )

      s <- bootstrap_statistic(boot_df, decision_col)$overall
      boot_stats[[b]] <- tibble(inc_rate = s$inc_rate, err_rate = s$err_rate)
    }

    boot_tbl <- bind_rows(boot_stats)

    # Building percentile CIs here.
    ci <- boot_tbl %>%
      summarise(
        inc_lo = quantile(inc_rate, alpha / 2,     na.rm = TRUE),
        inc_hi = quantile(inc_rate, 1 - alpha / 2, na.rm = TRUE),
        err_lo = quantile(err_rate, alpha / 2,     na.rm = TRUE),
        err_hi = quantile(err_rate, 1 - alpha / 2, na.rm = TRUE)
      )

    # Checking whether the CI covers the true value here.
    results[[m]] <- tibble(
      rep          = m,
      obs_inc_rate = obs_stat$inc_rate, obs_err_rate = obs_stat$err_rate,
      inc_lo = ci$inc_lo, inc_hi = ci$inc_hi,
      err_lo = ci$err_lo, err_hi = ci$err_hi,
      inc_covered = (true_par$true_inc_rate >= ci$inc_lo &
                       true_par$true_inc_rate <= ci$inc_hi),
      err_covered = (true_par$true_err_rate >= ci$err_lo &
                       true_par$true_err_rate <= ci$err_hi)
    )

    if (keep_last   && m == M) last_boot <- boot_tbl
    if (return_data && m == M) last_sim  <- sim_df
  }

  rep_tbl <- bind_rows(results)

  # Summarizing coverage across all reps here.
  coverage_summary <- tibble(
    statistic        = c("inc_rate", "err_rate"),
    true_value       = c(true_par$true_inc_rate, true_par$true_err_rate),
    coverage_rate    = c(mean(rep_tbl$inc_covered, na.rm = TRUE),
                         mean(rep_tbl$err_covered, na.rm = TRUE)),
    mean_ci_width    = c(mean(rep_tbl$inc_hi - rep_tbl$inc_lo, na.rm = TRUE),
                         mean(rep_tbl$err_hi - rep_tbl$err_lo, na.rm = TRUE)),
    nominal_coverage = conf_level, M = M, B = B,
    bootstrap_method = bootstrap_method
  )

  message("\n Coverage Summary"); print(coverage_summary)

  out <- list(coverage_summary = coverage_summary, repetitions = rep_tbl,
              true_parameters = true_par, bootstrap_method = bootstrap_method,
              M = M, B = B, seed = seed)
  if (keep_last)   out$last_boot_replicates <- last_boot
  if (return_data) out$simulated_data       <- last_sim
  out
}

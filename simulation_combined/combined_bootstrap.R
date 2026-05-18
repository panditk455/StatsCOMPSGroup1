# combined_bootstrap.R
# Implements the two-way combined bootstrap (Cameron, Gelbach & Miller 2011).
#
# Formula:
#   theta*_combined = theta*_examiner + theta*_cset - theta*_row
#
# This corrects for both clustering levels simultaneously.
# Run instructions at bottom of file.

Sim_boot_cov_all <- function(
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
    M               = 200,
    B               = 300,
    conf_level      = 0.95,
    decision_col    = "Decision_D",
    seed            = 42
) {
  alpha <- 1 - conf_level

  true_par_args <- list(
    per_diff = per_diff, per_exam_skill = per_exam_skill,
    per_quality1 = per_quality1, per_quality2 = per_quality2,
    Hawthorne = Hawthorne, mated_prop = mated_prop,
    beta_inc = beta_inc, sd_exam_inc = sd_exam_inc,
    sd_cset_inc = sd_cset_inc, beta_err = beta_err,
    sd_exam_err = sd_exam_err, sd_cset_err = sd_cset_err
  )
  sim_args <- c(true_par_args,
                list(n_examiners = n_examiners, nKQ = nKQ, nQQ = nQQ))

  message("Computing true parameters via Monte Carlo...")
  true_par <- do.call(true_parameters, true_par_args)
  message(sprintf("  true_inc_rate = %.4f  |  true_err_rate = %.4f",
                  true_par$true_inc_rate, true_par$true_err_rate))
  message(sprintf("Running %d reps x %d boots [4 methods: row/examiner/cset/combined]...",
                  M, B))

  # Helper: build percentile CI from a bootstrap table
  build_ci <- function(tbl) {
    tbl %>% summarise(
      inc_lo = quantile(inc_rate, alpha / 2,     na.rm = TRUE),
      inc_hi = quantile(inc_rate, 1 - alpha / 2, na.rm = TRUE),
      err_lo = quantile(err_rate, alpha / 2,     na.rm = TRUE),
      err_hi = quantile(err_rate, 1 - alpha / 2, na.rm = TRUE)
    )
  }

  results <- vector("list", M)

  for (m in seq_len(M)) {
    if (m %% 25 == 0) message(sprintf("  rep %d / %d", m, M))
    set.seed(seed + m)

    # One dataset per rep — shared across all four methods
    sim_df   <- do.call(one_simulation, sim_args)
    obs_stat <- bootstrap_statistic(sim_df, decision_col)$overall

    exams <- unique(sim_df$AnonID)
    cs    <- unique(sim_df$Cset)

    boot_row  <- vector("list", B)
    boot_exam <- vector("list", B)
    boot_cset <- vector("list", B)

    for (b in seq_len(B)) {

      # Row bootstrap
      s <- bootstrap_statistic(
        sim_df[sample(nrow(sim_df), nrow(sim_df), replace = TRUE), ],
        decision_col)$overall
      boot_row[[b]] <- tibble(inc_rate = s$inc_rate, err_rate = s$err_rate)

      # Examiner-cluster bootstrap
      samp_e <- sample(exams, length(exams), replace = TRUE)
      s <- bootstrap_statistic(
        lapply(seq_along(samp_e), function(i)
          sim_df %>% filter(AnonID == samp_e[i]) %>%
            mutate(AnonID = paste0(AnonID, "_b", i))) %>% bind_rows(),
        decision_col)$overall
      boot_exam[[b]] <- tibble(inc_rate = s$inc_rate, err_rate = s$err_rate)

      # Cset-cluster bootstrap
      samp_c <- sample(cs, length(cs), replace = TRUE)
      s <- bootstrap_statistic(
        lapply(seq_along(samp_c), function(i)
          sim_df %>% filter(Cset == samp_c[i]) %>%
            mutate(Cset = paste0(Cset, "_b", i))) %>% bind_rows(),
        decision_col)$overall
      boot_cset[[b]] <- tibble(inc_rate = s$inc_rate, err_rate = s$err_rate)
    }

    tbl_row  <- bind_rows(boot_row)
    tbl_exam <- bind_rows(boot_exam)
    tbl_cset <- bind_rows(boot_cset)

    # Combined bootstrap (Cameron et al. 2011):
    # theta*_combined = theta*_exam + theta*_cset - theta*_row
    tbl_comb <- tibble(
      inc_rate = tbl_exam$inc_rate + tbl_cset$inc_rate - tbl_row$inc_rate,
      err_rate = tbl_exam$err_rate + tbl_cset$err_rate - tbl_row$err_rate
    )

    ci_row  <- build_ci(tbl_row)
    ci_exam <- build_ci(tbl_exam)
    ci_cset <- build_ci(tbl_cset)
    ci_comb <- build_ci(tbl_comb)

    results[[m]] <- tibble(
      rep          = m,
      obs_inc_rate = obs_stat$inc_rate,
      obs_err_rate = obs_stat$err_rate,
      # Row
      row_inc_lo  = ci_row$inc_lo,  row_inc_hi  = ci_row$inc_hi,
      row_err_lo  = ci_row$err_lo,  row_err_hi  = ci_row$err_hi,
      row_inc_cov = (true_par$true_inc_rate >= ci_row$inc_lo &
                       true_par$true_inc_rate <= ci_row$inc_hi),
      row_err_cov = (true_par$true_err_rate >= ci_row$err_lo &
                       true_par$true_err_rate <= ci_row$err_hi),
      # Examiner
      exam_inc_lo  = ci_exam$inc_lo, exam_inc_hi  = ci_exam$inc_hi,
      exam_err_lo  = ci_exam$err_lo, exam_err_hi  = ci_exam$err_hi,
      exam_inc_cov = (true_par$true_inc_rate >= ci_exam$inc_lo &
                        true_par$true_inc_rate <= ci_exam$inc_hi),
      exam_err_cov = (true_par$true_err_rate >= ci_exam$err_lo &
                        true_par$true_err_rate <= ci_exam$err_hi),
      # Cset
      cset_inc_lo  = ci_cset$inc_lo, cset_inc_hi  = ci_cset$inc_hi,
      cset_err_lo  = ci_cset$err_lo, cset_err_hi  = ci_cset$err_hi,
      cset_inc_cov = (true_par$true_inc_rate >= ci_cset$inc_lo &
                        true_par$true_inc_rate <= ci_cset$inc_hi),
      cset_err_cov = (true_par$true_err_rate >= ci_cset$err_lo &
                        true_par$true_err_rate <= ci_cset$err_hi),
      # Combined
      comb_inc_lo  = ci_comb$inc_lo, comb_inc_hi  = ci_comb$inc_hi,
      comb_err_lo  = ci_comb$err_lo, comb_err_hi  = ci_comb$err_hi,
      comb_inc_cov = (true_par$true_inc_rate >= ci_comb$inc_lo &
                        true_par$true_inc_rate <= ci_comb$inc_hi),
      comb_err_cov = (true_par$true_err_rate >= ci_comb$err_lo &
                        true_par$true_err_rate <= ci_comb$err_hi)
    )
  }

  rep_tbl <- bind_rows(results)

  method_map <- list(
    row      = "row",
    examiner = "exam",
    cset     = "cset",
    combined = "comb"
  )

  coverage_summary <- purrr::map_dfr(names(method_map), function(meth) {
    px <- method_map[[meth]]
    tibble(
      statistic        = c("inc_rate", "err_rate"),
      true_value       = c(true_par$true_inc_rate, true_par$true_err_rate),
      coverage_rate    = c(mean(rep_tbl[[paste0(px, "_inc_cov")]], na.rm = TRUE),
                           mean(rep_tbl[[paste0(px, "_err_cov")]], na.rm = TRUE)),
      mean_ci_width    = c(
        mean(rep_tbl[[paste0(px, "_inc_hi")]] -
               rep_tbl[[paste0(px, "_inc_lo")]], na.rm = TRUE),
        mean(rep_tbl[[paste0(px, "_err_hi")]] -
               rep_tbl[[paste0(px, "_err_lo")]], na.rm = TRUE)),
      nominal_coverage = conf_level,
      M = M, B = B,
      Method = meth
    )
  })

  message("\n=== Coverage Summary (all 4 methods) ===")
  print(coverage_summary, n = Inf)

  list(
    coverage_summary = coverage_summary,
    repetitions      = rep_tbl,
    true_parameters  = true_par,
    M = M, B = B, seed = seed
  )
}

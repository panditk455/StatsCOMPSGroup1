
# simulate_bootstrap_designs_fixedcounts.R
# ------------------------------------------------------------
# Implements your scenario table EXACTLY:
# - Always 50 KQ sets + 50 QQ sets
# - Always 15 Mated + 30 Nonmated (remaining sets get filled by sampling
#   to keep the same mated proportion)
# - Difficulty: Easy/Medium/Hard
# - Skill: Low/Medium/High
# - Hawthorne: 0, 0.15, 0.25, 0.4  (fraction of Insuff flipped to definitive)
# - Evidence quality: A/B/C/D sampled per set
#
# Produces:
# - Simulated response-level data (set x examiner)
# - Bootstrapped overall error rate (ALL calls in denominator)
#   for Undiscovered and Discovered under three resampling schemes:
#   row / examiner / cset
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

# ---------------------------
# Utilities
# ---------------------------
logit <- function(p) log(p / (1 - p))
invlogit <- function(x) 1 / (1 + exp(-x))
clip01 <- function(x, lo = 1e-6, hi = 1 - 1e-6) pmin(hi, pmax(lo, x))

# ---------------------------
# Scenario grid from your table (3*3*4 = 36)
# ---------------------------
make_scenarios <- function() {
  expand.grid(
    nKQ = 50,
    nQQ = 50,
    nMated = 15,
    nNonmated = 30,
    Difficulty = c("Easy", "Medium", "Hard"),
    Skill = c("Low", "Medium", "High"),
    Hawthorne = c(0, 0.15, 0.25, 0.40),
    stringsAsFactors = FALSE
  ) %>%
    arrange(Skill, Difficulty, Hawthorne) %>%
    mutate(ScenarioID = row_number())
}

# ---------------------------
# Parameters you can tune
# ---------------------------
default_params <- list(
  # Baselines
  p_insuff_base = 0.25,       # baseline inconclusive prob
  p_idside_mated = 0.75,      # P(ID-side | definitive, mated)
  p_idside_nonmated = 0.20,   # P(ID-side | definitive, nonmated)
  p_strong_id = 0.65,         # P(ID | ID-side)
  p_strong_ex = 0.65,         # P(Excl | Excl-side)
  
  # How evidence quality shifts (ordinal A>B>C>D)
  quality_effect_def = 0.35,  # worse quality => more insuff
  quality_effect_str = 0.25,  # better quality => more strong (vs lean)
  
  # Difficulty effects
  diff_effect_insuff = c(Easy = -0.35, Medium = 0.00, Hard = 0.35),
  diff_effect_str    = c(Easy =  0.25, Medium = 0.00, Hard = -0.25),
  diff_effect_side   = c(Easy =  0.10, Medium = 0.00, Hard = -0.10),
  
  # Skill effects
  skill_effect_insuff = c(Low = 0.35, Medium = 0.00, High = -0.35),
  skill_effect_str    = c(Low = -0.25, Medium = 0.00, High = 0.25),
  skill_effect_side   = c(Low = -0.10, Medium = 0.00, High = 0.10),
  
  # Model differences (KQ vs QQ)
  model_effect_insuff = c(KQ = 0.05, QQ = 0.00),
  model_effect_side   = c(KQ = 0.00, QQ = 0.00),
  model_effect_str    = c(KQ = 0.00, QQ = 0.00),
  
  # Random effects (optional)
  sd_examiner = 0.35,
  sd_cset = 0.25
)

# ---------------------------
# Evidence quality
# ---------------------------
sample_quality <- function(n, probs = c(A = 0.25, B = 0.30, C = 0.25, D = 0.20)) {
  sample(names(probs), size = n, replace = TRUE, prob = probs)
}
quality_to_score <- function(q) {
  recode(q, A = 4, B = 3, C = 2, D = 1) |> as.numeric()
}

# ---------------------------
# Create sets for a scenario (always 100 sets)
# ---------------------------
make_sets <- function(nKQ, nQQ, nMated, nNonmated, difficulty, seed = 1) {
  set.seed(seed)
  sets <- tibble(
    Cset = paste0("S", seq_len(nKQ + nQQ)),
    ModelType = c(rep("KQ", nKQ), rep("QQ", nQQ))
  )
  
  total <- nrow(sets)
  
  # Assign truth with fixed counts for the first (nMated + nNonmated),
  # then fill remaining sets using the same mated proportion.
  base_truth <- c(rep("Mated", nMated), rep("Nonmated", nNonmated))
  if (length(base_truth) < total) {
    pM <- nMated / (nMated + nNonmated)
    base_truth <- c(
      base_truth,
      sample(c("Mated","Nonmated"), total - length(base_truth),
             replace = TRUE, prob = c(pM, 1 - pM))
    )
  }
  sets$Mating <- sample(base_truth, total, replace = FALSE)
  
  sets$Difficulty <- difficulty
  sets$EvidenceQuality <- sample_quality(total)
  
  sets %>%
    mutate(
      Mating = factor(Mating, levels = c("Nonmated","Mated")),
      ModelType = factor(ModelType, levels = c("KQ","QQ")),
      Difficulty = factor(Difficulty, levels = c("Easy","Medium","Hard")),
      EvidenceQuality = factor(EvidenceQuality, levels = c("D","C","B","A"), ordered = TRUE)
    )
}

# ---------------------------
# Create examiners (skill fixed per scenario)
# ---------------------------
make_examiners <- function(n_examiners = 60, skill = "Medium", seed = 1, sd_examiner = 0.35) {
  set.seed(seed)
  tibble(
    AnonID = paste0("E", seq_len(n_examiners)),
    Skill = skill,
    re_exam = rnorm(n_examiners, 0, sd_examiner)
  ) %>%
    mutate(Skill = factor(Skill, levels = c("Low","Medium","High")))
}

# ---------------------------
# Expand to responses: each set seen by ~Poisson(mean_repeats) examiners
# ---------------------------
assign_examiners_to_sets <- function(sets, examiners, mean_repeats = 3, seed = 1) {
  set.seed(seed)
  reps <- map_dfr(seq_len(nrow(sets)), function(i) {
    k <- max(1, rpois(1, mean_repeats))
    chosen <- sample(examiners$AnonID, size = min(k, nrow(examiners)), replace = FALSE)
    tibble(Cset = sets$Cset[i], AnonID = chosen)
  })
  reps %>%
    left_join(sets, by = "Cset") %>%
    left_join(examiners, by = "AnonID")
}

# ---------------------------
# Simulate decisions (Undiscovered)
# Categories: ID, LeanID, Insuff, LeanExcl, Excl
# ---------------------------
simulate_decisions <- function(dat, params = default_params, seed = 1) {
  set.seed(seed)
  
  # Random effect per set
  re_cset <- rnorm(n_distinct(dat$Cset), 0, params$sd_cset)
  names(re_cset) <- unique(dat$Cset)
  
  q_score <- quality_to_score(as.character(dat$EvidenceQuality)) # 1..4
  q_center <- q_score - mean(q_score)
  
  diff <- as.character(dat$Difficulty)
  skill <- as.character(dat$Skill)
  model <- as.character(dat$ModelType)
  
  # 1) Insuff probability
  eta_insuff <- logit(params$p_insuff_base) +
    params$diff_effect_insuff[diff] +
    params$skill_effect_insuff[skill] +
    params$model_effect_insuff[model] +
    params$quality_effect_def * (-q_center) +
    dat$re_exam + re_cset[dat$Cset]
  
  p_insuff <- invlogit(eta_insuff) |> clip01(lo = 0.01, hi = 0.90)
  is_insuff <- rbinom(nrow(dat), 1, p_insuff) == 1
  
  # 2) Side among definitive
  p_id_base <- ifelse(dat$Mating == "Mated", params$p_idside_mated, params$p_idside_nonmated)
  
  eta_side <- logit(p_id_base) +
    params$diff_effect_side[diff] +
    params$skill_effect_side[skill] +
    params$model_effect_side[model] +
    0.10 * q_center +
    0.50 * dat$re_exam +
    0.30 * re_cset[dat$Cset]
  
  p_idside <- invlogit(eta_side) |> clip01(0.01, 0.99)
  
  # 3) Strength given side
  eta_str_id <- logit(params$p_strong_id) +
    params$diff_effect_str[diff] +
    params$skill_effect_str[skill] +
    params$model_effect_str[model] +
    params$quality_effect_str * q_center +
    0.30 * dat$re_exam +
    0.20 * re_cset[dat$Cset]
  
  p_str_id <- invlogit(eta_str_id) |> clip01(0.01, 0.99)
  
  eta_str_ex <- logit(params$p_strong_ex) +
    params$diff_effect_str[diff] +
    params$skill_effect_str[skill] +
    params$model_effect_str[model] +
    params$quality_effect_str * q_center +
    0.30 * dat$re_exam +
    0.20 * re_cset[dat$Cset]
  
  p_str_ex <- invlogit(eta_str_ex) |> clip01(0.01, 0.99)
  
  # Draw decisions
  Decision <- character(nrow(dat))
  Decision[is_insuff] <- "Insuff"
  
  idx_def <- which(!is_insuff)
  if (length(idx_def) > 0) {
    id_side <- rbinom(length(idx_def), 1, p_idside[idx_def]) == 1
    
    idx_id <- idx_def[id_side]
    if (length(idx_id) > 0) {
      strong <- rbinom(length(idx_id), 1, p_str_id[idx_id]) == 1
      Decision[idx_id] <- ifelse(strong, "ID", "LeanID")
    }
    
    idx_ex <- idx_def[!id_side]
    if (length(idx_ex) > 0) {
      strong <- rbinom(length(idx_ex), 1, p_str_ex[idx_ex]) == 1
      Decision[idx_ex] <- ifelse(strong, "Excl", "LeanExcl")
    }
  }
  
  dat %>%
    mutate(Decision_U = factor(Decision, levels = c("ID","LeanID","Insuff","LeanExcl","Excl")))
}

# ---------------------------
# Hawthorne: flip fraction of Insuff to definitive (redistribute by definitive mix)
# ---------------------------
apply_hawthorne <- function(dat, haw = 0.15, seed = 1) {
  set.seed(seed)
  dat2 <- dat
  
  inc_idx <- which(dat2$Decision_U == "Insuff")
  dat2$Decision_D <- dat2$Decision_U
  
  if (length(inc_idx) == 0 || haw <= 0) return(dat2)
  
  n_flip <- floor(haw * length(inc_idx))
  if (n_flip <= 0) return(dat2)
  
  flip_idx <- sample(inc_idx, n_flip, replace = FALSE)
  
  def <- dat2$Decision_U[dat2$Decision_U %in% c("ID","LeanID","Excl","LeanExcl")]
  if (length(def) == 0) return(dat2)
  
  probs <- table(def)
  probs <- as.numeric(probs) / sum(probs)
  names(probs) <- names(table(def))
  
  dat2$Decision_D[flip_idx] <- sample(names(probs), size = length(flip_idx),
                                      replace = TRUE, prob = probs)
  dat2$Decision_D <- factor(dat2$Decision_D, levels = levels(dat2$Decision_U))
  dat2
}

# ---------------------------
# Overall error (ALL calls in denominator)
# ---------------------------
overall_error <- function(dat, decision_col = "Decision_U") {
  dec <- dat[[decision_col]]
  mean(
    (dat$Mating == "Nonmated" & dec %in% c("ID","LeanID")) |
      (dat$Mating == "Mated"  & dec %in% c("Excl","LeanExcl")),
    na.rm = TRUE
  )
}

# ---------------------------
# Bootstrap (row / cluster)
# ---------------------------
boot_rows <- function(df, stat_fun, B = 1000, seed = 1) {
  set.seed(seed)
  n <- nrow(df)
  out <- numeric(B)
  for (b in 1:B) {
    idx <- sample.int(n, n, replace = TRUE)
    out[b] <- stat_fun(df[idx, , drop = FALSE])
  }
  out
}

boot_cluster <- function(df, cluster_var, stat_fun, B = 1000, seed = 1) {
  set.seed(seed)
  clusters <- unique(df[[cluster_var]])
  idx_by <- split(seq_len(nrow(df)), df[[cluster_var]])
  out <- numeric(B)
  for (b in 1:B) {
    samp <- sample(clusters, length(clusters), replace = TRUE)
    idx <- unlist(idx_by[samp], use.names = FALSE)
    out[b] <- stat_fun(df[idx, , drop = FALSE])
  }
  out
}

summ_ci <- function(x) {
  c(mean = mean(x), sd = sd(x),
    lo = quantile(x, 0.025),
    hi = quantile(x, 0.975))
}

# ---------------------------
# Run one scenario
# ---------------------------
run_scenario <- function(scenario,
                         n_examiners = 60,
                         mean_repeats = 3,
                         params = default_params,
                         Bboot = 2000,
                         seed = 1) {
  
  sets <- make_sets(
    nKQ = scenario$nKQ,
    nQQ = scenario$nQQ,
    nMated = scenario$nMated,
    nNonmated = scenario$nNonmated,
    difficulty = scenario$Difficulty,
    seed = seed + 10
  )
  
  examiners <- make_examiners(
    n_examiners = n_examiners,
    skill = scenario$Skill,
    seed = seed + 20,
    sd_examiner = params$sd_examiner
  )
  
  base <- assign_examiners_to_sets(sets, examiners, mean_repeats = mean_repeats, seed = seed + 30)
  
  dat_u <- simulate_decisions(base, params = params, seed = seed + 40)
  dat <- apply_hawthorne(dat_u, haw = scenario$Hawthorne, seed = seed + 50)
  
  statU <- function(d) overall_error(d, "Decision_U")
  statD <- function(d) overall_error(d, "Decision_D")
  
  # Bootstrap U
  valsU_row  <- boot_rows(dat, statU, B = Bboot, seed = seed + 100)
  valsU_exam <- boot_cluster(dat, "AnonID", statU, B = Bboot, seed = seed + 200)
  valsU_cset <- boot_cluster(dat, "Cset",   statU, B = Bboot, seed = seed + 300)
  
  # Bootstrap D
  valsD_row  <- boot_rows(dat, statD, B = Bboot, seed = seed + 400)
  valsD_exam <- boot_cluster(dat, "AnonID", statD, B = Bboot, seed = seed + 500)
  valsD_cset <- boot_cluster(dat, "Cset",   statD, B = Bboot, seed = seed + 600)
  
  summary <- bind_rows(
    tibble(method = "row",      condition = "Undiscovered", as.list(summ_ci(valsU_row))),
    tibble(method = "examiner", condition = "Undiscovered", as.list(summ_ci(valsU_exam))),
    tibble(method = "cset",     condition = "Undiscovered", as.list(summ_ci(valsU_cset))),
    tibble(method = "row",      condition = "Discovered",   as.list(summ_ci(valsD_row))),
    tibble(method = "examiner", condition = "Discovered",   as.list(summ_ci(valsD_exam))),
    tibble(method = "cset",     condition = "Discovered",   as.list(summ_ci(valsD_cset)))
  ) %>%
    mutate(
      ScenarioID = scenario$ScenarioID,
      Difficulty = scenario$Difficulty,
      Skill = scenario$Skill,
      Hawthorne = scenario$Hawthorne
    )
  
  list(scenario = scenario, data = dat, summary = summary)
}

# ---------------------------
# Run all scenarios and save outputs
# ---------------------------
main <- function(out_dir = "sim_outputs", Bboot = 1000, seed = 123) {
  scenarios <- make_scenarios()
  
  results <- vector("list", nrow(scenarios))
  for (i in seq_len(nrow(scenarios))) {
    cat("Running scenario", i, "of", nrow(scenarios), "\n")
    results[[i]] <- run_scenario(
      scenario = scenarios[i, ],
      n_examiners = 60,
      mean_repeats = 3,
      Bboot = Bboot,
      seed = seed + 1000*i
    )
  }
  
  summary_all <- bind_rows(lapply(results, `[[`, "summary"))
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(results, file.path(out_dir, "scenario_results.rds"))
  write.csv(summary_all, file.path(out_dir, "bootstrap_summary_all.csv"), row.names = FALSE)
  
  summary_all
}

# ---------------------------
# Visualization helper
# ---------------------------
plot_summary <- function(summary_all) {
  # Coverage not computed here; we plot mean and CI
  ggplot(summary_all, aes(x = Hawthorne, y = mean, color = method)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.02) +
    facet_grid(Skill ~ Difficulty + condition) +
    scale_x_continuous(breaks = c(0, 0.15, 0.25, 0.4)) +
    labs(
      title = "Overall Error Rate by Hawthorne, Difficulty, Skill (Toy Simulation)",
      x = "Hawthorne (fraction of Insuff flipped)",
      y = "Overall error rate (all calls in denominator)",
      color = "Bootstrap method"
    ) +
    theme_minimal()
}

# ---------------------------
# Run if desired:
# ---------------------------
# summary_all <- main(out_dir = "sim_outputs", Bboot = 1000, seed = 123)
# p <- plot_summary(summary_all)
# print(p)
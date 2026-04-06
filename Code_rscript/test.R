# simulation_run.R
library(dplyr)
library(tidyr)
library(purrr)

source("simulationedited.RMD")

#per_diff = proportions for [VEasy, Easy, Norm, Diff, VDiff]
diff_mixes <- list(
  Easy   = c(0.35, 0.35, 0.20, 0.07, 0.03),
  Medium = c(0.10, 0.20, 0.40, 0.20, 0.10),
  Hard   = c(0.03, 0.07, 0.20, 0.35, 0.35)
)

#per_exam_skill = proportions for [Low, Medium, High]
skill_mixes <- list(
  Low    = c(0.60, 0.30, 0.10),
  Medium = c(0.25, 0.50, 0.25),
  High   = c(0.10, 0.30, 0.60)
)

hawthorne_levels <- c(0, 0.15, 0.25, 0.40, 0.45, 0.50)

scenario_grid <- expand_grid(
  diff_label  = names(diff_mixes),
  skill_label = names(skill_mixes),
  Hawthorne   = hawthorne_levels
)

methods <- c("row", "examiner", "cset")

cat("Total scenarios:", nrow(scenario_grid),
    "| Methods:", length(methods),
    "| Total runs:", nrow(scenario_grid) * length(methods), "\n")
print(scenario_grid)

M_sim <- 200   # increase to 500 for final paper
B_sim <- 300   # increase to 500 for final paper

all_results <- list()

for (i in seq_len(nrow(scenario_grid))) {
  
  sc <- scenario_grid[i, ]
  
  for (method in methods) {
    
    key <- paste0(
      sc$diff_label,  "_",
      sc$skill_label, "_hw",
      sc$Hawthorne,   "_",
      method
    )
    
    message("\n--- ", key, " [", i, "/", nrow(scenario_grid), "] ---")
    
    all_results[[key]] <- tryCatch(
      Sim_boot_cov(
        per_diff         = diff_mixes[[sc$diff_label]],
        per_exam_skill   = skill_mixes[[sc$skill_label]],
        per_quality1     = c(1/3, 1/3, 1/3),
        per_quality2     = rep(0.25, 4),
        Hawthorne        = sc$Hawthorne,
        n_examiners      = 49,
        nKQ              = 50,
        nQQ              = 50,
        mated_prop       = 0.5,
        M                = M_sim,
        B                = B_sim,
        bootstrap_method = method,
        fit_each_time    = FIT_MODELS,
        seed             = 42
      ),
      error = function(e) {
        message("  ERROR: ", e$message)
        NULL
      }
    )
    saveRDS(all_results, "all_results_checkpoint.rds")
  }
}

#Assemble results table

results_table <- imap_dfr(all_results, function(result, key) {
  
  # Skip failed runs
  if (is.null(result)) {
    message("Skipping NULL result for key: ", key)
    return(NULL)
  }
  
  # Parse key back into labels
  # Key format: {diff}_{skill}_hw{hawthorne}_{method}
  # e.g. "Easy_Low_hw0_examiner"
  parts     <- strsplit(key, "_")[[1]]
  method    <- parts[length(parts)]
  hw_part   <- parts[length(parts) - 1]           # e.g. "hw0" or "hw0.15"
  hawthorne <- as.numeric(sub("hw", "", hw_part))
  # Remaining parts before hw are diff_label and skill_label
  # (both can be single words so this is unambiguous)
  remaining  <- parts[seq_len(length(parts) - 2)]
  diff_label  <- remaining[1]
  skill_label <- remaining[2]
  
  result$coverage_summary %>%
    mutate(
      Difficulty = diff_label,
      Skill      = skill_label,
      Hawthorne  = hawthorne,
      Method     = method,
      .before    = 1
    )
  
}) %>%
  select(Difficulty, Skill, Hawthorne, Method, statistic,
         true_value, coverage_rate, mean_ci_width, nominal_coverage) %>%
  arrange(Difficulty, Skill, Hawthorne, Method, statistic)

cat("\n=== Error rate coverage across all scenarios ===\n")
results_table %>%
  filter(statistic == "err_rate") %>%
  print(n = Inf)

cat("\n=== Inconclusive rate coverage across all scenarios ===\n")
results_table %>%
  filter(statistic == "inc_rate") %>%
  print(n = Inf)

saveRDS(results_table, "results_table.rds")
saveRDS(all_results,   "all_results.rds")

cat("\nSaved:\n  results_table.rds  — coverage summary table\n")
cat("  all_results.rds    — full output list (variance, repetitions, etc.)\n")

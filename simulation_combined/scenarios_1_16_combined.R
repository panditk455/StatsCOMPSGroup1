# scenarios_1_16_combined.R  — Scenarios 1–16, all four methods including combined

source("simulation_functions.R")
source("combined_bootstrap.R")

diff_mixes <- list(
  Easy          = c(0.35, 0.35, 0.20, 0.07, 0.03),
  Medium        = c(0.10, 0.20, 0.40, 0.20, 0.10),
  Difficult     = c(0.03, 0.07, 0.15, 0.55, 0.20),
  VeryDifficult = c(0.02, 0.05, 0.13, 0.25, 0.55)
)

quality_mixes <- list(
  A = list(q1 = c(0.80, 0.15, 0.05), q2 = c(0.75, 0.15, 0.07, 0.03)),
  B = list(q1 = c(0.15, 0.70, 0.15), q2 = c(0.10, 0.70, 0.15, 0.05)),
  C = list(q1 = c(0.05, 0.25, 0.70), q2 = c(0.07, 0.15, 0.68, 0.10)),
  D = list(q1 = c(0.05, 0.20, 0.75), q2 = c(0.05, 0.10, 0.15, 0.70))
)

skill_mix <- c(0.25, 0.50, 0.25)

all_scenarios <- list(
  s1  = list("Easy", 0.0, "A", "A"),
  s2  = list("Easy", 0.0, "B", "B"),
  s3  = list("Easy", 0.0, "C", "C"),
  s4  = list("Easy", 0.0, "C", "D"),
  s5  = list("Easy", 0.2, "A", "A"),
  s6  = list("Easy", 0.2, "B", "B"),
  s7  = list("Easy", 0.2, "C", "C"),
  s8  = list("Easy", 0.2, "C", "D"),
  s9  = list("Easy", 0.5, "A", "A"),
  s10 = list("Easy", 0.5, "B", "B"),
  s11 = list("Easy", 0.5, "C", "C"),
  s12 = list("Easy", 0.5, "C", "D"),
  s13 = list("Medium", 0.0, "A", "A"),
  s14 = list("Medium", 0.0, "B", "B"),
  s15 = list("Medium", 0.0, "C", "C"),
  s16 = list("Medium", 0.0, "C", "D")
)

M_sim <- 200
B_sim <- 300

args  <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage:  Rscript scenarios_1_16_combined.R <1..16>")

sc_id <- paste0("s", args[1])
if (!sc_id %in% names(all_scenarios))
  stop("Scenario '", sc_id, "' not found. Valid options: 1 to 16.")

sc        <- all_scenarios[[sc_id]]
diff_lbl  <- sc[[1]];  haw       <- sc[[2]]
qual1_lbl <- sc[[3]];  qual2_lbl <- sc[[4]]

per_diff     <- diff_mixes[[diff_lbl]]
per_quality1 <- quality_mixes[[qual1_lbl]]$q1
per_quality2 <- quality_mixes[[qual2_lbl]]$q2

message(sprintf("\n=== %s | %s | Haw=%.1f | Q1=%s Q2=%s ===",
                sc_id, diff_lbl, haw, qual1_lbl, qual2_lbl))

result <- Sim_boot_cov_all(
  per_diff = per_diff, per_exam_skill = skill_mix,
  per_quality1 = per_quality1, per_quality2 = per_quality2,
  Hawthorne = haw, n_examiners = 49, nKQ = 50, nQQ = 50,
  mated_prop = 0.5, M = M_sim, B = B_sim, seed = 42
)

results_table <- result$coverage_summary |>
  mutate(Scenario  = sc_id, Difficulty = diff_lbl,
         Hawthorne = haw,   Quality1   = qual1_lbl, Quality2 = qual2_lbl) |>
  select(Scenario, Difficulty, Hawthorne, Quality1, Quality2, Method,
         statistic, true_value, coverage_rate, mean_ci_width, nominal_coverage) |>
  arrange(Method, statistic)

cat("\n---- Coverage summary ----\n"); print(results_table, n = Inf)

out_file <- paste0("result_combined_", sc_id, ".rds")
saveRDS(list(coverage_summary = result$coverage_summary,
             repetitions      = result$repetitions,
             table            = results_table), out_file)
cat(sprintf("Saved to %s\n", out_file))

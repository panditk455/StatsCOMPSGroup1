library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

dir.create("visualization_combined", showWarnings = FALSE)

results_table <- read.csv("combined_situation1_48.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Quality = paste0(Quality1, Quality2),
    Method  = recode(Method,
                     "row"      = "Standard (Naive)",
                     "examiner" = "Examiner cluster",
                     "cset"     = "Item cluster",
                     "combined" = "Combined"),
    Method  = factor(Method,
                     levels = c("Standard (Naive)", "Examiner cluster",
                                "Item cluster", "Combined")),
    Difficulty = factor(Difficulty,
                        levels = c("Easy", "Medium", "Difficult", "VeryDifficult"))
  )

METHOD_COLOURS <- c("Standard (Naive)" = "#e41a1c",
                    "Examiner cluster" = "#377eb8",
                    "Item cluster"     = "#4daf4a",
                    "Combined"         = "#984ea3")

cat("Rows loaded:", nrow(results_table), "\n")
cat("Methods:", paste(levels(results_table$Method), collapse = ", "), "\n")

#  Naive only 
p7a <- results_table %>%
  filter(statistic == "err_rate", Method == "Standard (Naive)") %>%
  ggplot(aes(x = mean_ci_width, y = coverage_rate, shape = Difficulty)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  annotate("text", x = -Inf, y = 0.956, label = "95% target",
           hjust = -0.1, size = 3.2, colour = "grey40") +
  geom_point(size = 4, colour = "#e41a1c", alpha = 0.85) +
  scale_y_continuous(limits = c(0.3, 1.0), labels = percent) +
  labs(title    = "Standard bootstrap falls well below 95%",
       subtitle = "Each point is one scenario.",
       x = "CI width", y = "Coverage", shape = "Difficulty") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

ggsave("visualization_combined/fig7a_naive_only.png", p7a,
       width = 8, height = 5, dpi = 150)

# All 4 methods - build-up slide 2 
p7b <- results_table %>%
  filter(statistic == "err_rate") %>%
  ggplot(aes(x = mean_ci_width, y = coverage_rate,
             colour = Method, shape = Difficulty)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  annotate("text", x = -Inf, y = 0.956, label = "95% target",
           hjust = -0.1, size = 3.2, colour = "grey40") +
  geom_point(size = 4, alpha = 0.85) +
  scale_colour_manual(values = METHOD_COLOURS) +
  scale_y_continuous(limits = c(0.3, 1.0), labels = percent) +
  labs(title    = "Clustered methods stay near the 95% target",
       subtitle = "Upper-left = best: narrow CI and correct coverage.",
       x = "CI width", y = "Coverage",
       colour = "Method", shape = "Difficulty") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

ggsave("visualization_combined/fig7b_all_methods.png", p7b,
       width = 9, height = 5, dpi = 150)

# Coverage by quality, Hawthorne = 0, error rate 
p1 <- results_table %>%
  filter(Hawthorne == 0, statistic == "err_rate") %>%
  ggplot(aes(x = Quality, y = coverage_rate, colour = Method, group = Method)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~Difficulty, ncol = 2) +
  scale_y_continuous(limits = c(0.3, 1.0), labels = percent) +
  scale_colour_manual(values = METHOD_COLOURS) +
  labs(title    = "Error-rate coverage by bootstrap method (Hawthorne = 0)",
       subtitle = "Dashed line = 95% target. Values below = CIs too narrow.",
       x = "Quality combination", y = "Coverage", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("visualization_combined/fig1_coverage_by_quality_haw0.png", p1,
       width = 10, height = 6, dpi = 150)

# Coverage vs Hawthorne, error rate 
p2 <- results_table %>%
  filter(statistic == "err_rate") %>%
  ggplot(aes(x = factor(Hawthorne), y = coverage_rate,
             colour = Method, group = Method)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_grid(Quality ~ Difficulty) +
  scale_y_continuous(limits = c(0.3, 1.0), labels = percent) +
  scale_colour_manual(values = METHOD_COLOURS) +
  labs(title    = "How Hawthorne effect affects error-rate coverage",
       subtitle = "Each panel = one Difficulty x Quality combination",
       x = "Hawthorne delta", y = "Coverage", colour = "Method") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("visualization_combined/fig2_coverage_vs_hawthorne.png", p2,
       width = 12, height = 8, dpi = 150)

# CI width vs Hawthorne, error rate 
p3 <- results_table %>%
  filter(statistic == "err_rate") %>%
  mutate(Scenario = paste0(Difficulty, " / ", Quality)) %>%
  ggplot(aes(x = factor(Hawthorne), y = mean_ci_width,
             colour = Method, group = Method)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~Scenario, ncol = 4) +
  scale_colour_manual(values = METHOD_COLOURS) +
  labs(title    = "Mean CI width for error rate",
       subtitle = "Narrower = more precise. Best: coverage near 95% AND narrow CI.",
       x = "Hawthorne delta", y = "Mean CI width", colour = "Method") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("visualization_combined/fig3_ci_width_vs_hawthorne.png", p3,
       width = 14, height = 10, dpi = 150)

# Heatmap - error rate, all scenarios 
p4 <- results_table %>%
  filter(statistic == "err_rate") %>%
  mutate(Scenario = paste0(Difficulty, " / ", Quality)) %>%
  ggplot(aes(x = Method, y = Scenario, fill = coverage_rate)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(coverage_rate, accuracy = 1)),
            size = 2.8, fontface = "bold") +
  facet_wrap(~Hawthorne, ncol = 3,
             labeller = labeller(Hawthorne = function(x) paste0("Hawthorne = ", x))) +
  scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850",
                       midpoint = 0.95, limits = c(0.3, 1.0),
                       labels = percent, name = "Coverage") +
  labs(title    = "Bootstrap coverage (error rate) — all 48 scenarios",
       subtitle = "Green = ~95%. Red = undercoverage.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "right")

ggsave("visualization_combined/fig4_heatmap_err_rate.png", p4,
       width = 14, height = 8, dpi = 150)

# Heatmap — inconclusive rate, all scenarios
p5 <- results_table %>%
  filter(statistic == "inc_rate") %>%
  mutate(Scenario = paste0(Difficulty, " / ", Quality)) %>%
  ggplot(aes(x = Method, y = Scenario, fill = coverage_rate)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(coverage_rate, accuracy = 1)),
            size = 2.8, fontface = "bold") +
  facet_wrap(~Hawthorne, ncol = 3,
             labeller = labeller(Hawthorne = function(x) paste0("Hawthorne = ", x))) +
  scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850",
                       midpoint = 0.95, limits = c(0.3, 1.0),
                       labels = percent, name = "Coverage") +
  labs(title    = "Bootstrap coverage (inconclusive rate) — all 48 scenarios",
       subtitle = "Green = ~95%. Red = undercoverage.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "right")

ggsave("visualization_combined/fig5_heatmap_inc_rate.png", p5,
       width = 14, height = 8, dpi = 150)

# Diverging bar - deviation from 0.95 
p6_data <- results_table %>%
  filter(statistic == "err_rate") %>%
  group_by(Method, Difficulty, Hawthorne) %>%
  summarise(deviation = mean(coverage_rate - 0.95), .groups = "drop") %>%
  mutate(
    direction = if_else(deviation >= 0, "At or above 95%", "Below 95%"),
    Hawthorne = paste0("Hawthorne = ", Hawthorne)
  )

p6 <- ggplot(p6_data, aes(x = Difficulty, y = deviation, fill = direction)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, linewidth = 0.8, colour = "grey20") +
  facet_grid(Method ~ Hawthorne) +
  scale_fill_manual(values = c("At or above 95%" = "#1a9850",
                               "Below 95%"       = "#d73027")) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
  labs(title    = "Coverage deviation from 95% nominal (error rate)",
       subtitle = "Bars below zero = undercoverage.",
       x = "Difficulty", y = "Coverage − 95%", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x        = element_text(angle = 30, hjust = 1))

ggsave("visualization_combined/fig6_diverging_bar.png", p6,
       width = 12, height = 10, dpi = 150)


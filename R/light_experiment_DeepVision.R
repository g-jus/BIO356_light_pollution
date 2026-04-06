# ============================================================================
## 1. Setup - loading packages.
# ============================================================================

library(tidyverse)
library(here)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)

# ============================================================================
## 2. Loading and cleaning data.
# ============================================================================

## Load data.
dark_data <- read.csv2(here("data/length_trawl_data/First_night_trawl_length_DV.csv"))
light_data <- read.csv2(here("data/length_trawl_data/Second_night_trawl_length_DV.csv"))

## Add treatment labels.
dark_data$Treatment <- "Without artificial light"
light_data$Treatment <- "With artificial light"

## Combine df and clean.
combined_df <- bind_rows(dark_data, light_data) |>
  mutate(
    Species_short = case_when(
      grepl("Benthosema", Species) ~ "B. glaciale",
      grepl("Maurolicus", Species) ~ "M. muelleri",
      TRUE ~ Species
    )
  ) |>
  filter(!is.na(Length), !is.na(Depth))

# ============================================================================
## 3. Data summary / exploratory analysis.
# ============================================================================

## Summary statistics by species and treatment.
summary_stats <- combined_df |>
  group_by(Species_short, Treatment) |>
  summarise(
    Count = n(),
    Mean_Length = mean(Length, na.rm = TRUE),
    SD_Length = sd(Length, na.rm = TRUE),
    Min_Length = min(Length, na.rm = TRUE),
    Max_Length = max(Length, na.rm = TRUE),
    Mean_Depth = mean(Depth, na.rm = TRUE),
    SD_Depth = sd(Depth, na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================================
## 4. Length analysis?
# ============================================================================

## ANCOVA: Length ~ Treatment + Species + Depth (as covariate).
length_model <- lm(Length ~ Treatment + Species_short + Depth +
                    Treatment:Species_short,
                    data = combined_df)


# ============================================================================
## 5. Statistical test: T-test.
# ============================================================================

## T-tests for length differences by treatment within each species.
for (sp in unique(combined_df$Species_short)) {
  sp_data <- combined_df |> filter(Species_short == sp)
  dark <- sp_data |> filter(Treatment == "Without artificial light") |> pull(Length)
  light <- sp_data |> filter(Treatment == "With artificial light") |> pull(Length)

  t_result <- t.test(dark, light)

  cat(sprintf("\n%s:\n", sp))
  cat(sprintf("  Dark Mean: %.2f mm (SD: %.2f)\n", mean(dark), sd(dark)))
  cat(sprintf("  Light Mean: %.2f mm (SD: %.2f)\n", mean(light), sd(light)))
  cat(sprintf("  t-test p-value: %.4f\n", t_result$p.value))
}

## Simple t-test of both species.
t.test(Length ~ Treatment,
       data = combined_df |> filter(Species_short == "B. glaciale"))

t.test(Length ~ Treatment,
       data = combined_df |> filter(Species_short == "M. muelleri"))

## Small for loop
for (sp in c("B. glaciale", "M. muelleri")) {
  cat(sprintf("\n%s:\n", sp))

  result <- t.test(Length ~ Treatment,
                   data = combined_df |> filter(Species_short == sp))

  print(result)
}
# ============================================================================
## 6. Visualization.
# ============================================================================

## Plot 1: Boxplot of length by treatment and species.
p1 <- ggplot(combined_df, aes(x = Treatment, y = Length, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  facet_wrap(~Species_short) +
  labs(
    title = "Length Comparison: With vs Without Artificial Light",
    x = "Treatment",
    y = "Length (mm)",
    fill = "Treatment"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#ggsave(here("figures/01_length_boxplot.png"), p1, width = 10, height = 6)

## Plot 2: Length distribution by treatment and species.
p2 <- ggplot(combined_df, aes(x = Length, fill = Treatment)) +
  geom_histogram(position = "dodge", bins = 5, alpha = 0.7) +
  facet_wrap(~Species_short) +
  labs(
    title = "Length Distribution by Treatment and Species",
    x = "Length (mm)",
    y = "Count",
    fill = "Treatment"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#ggsave(here("figures/02_length_distribution.png"), p2, width = 10, height = 6)

## Plot 3: Length vs Depth by treatment.
p3 <- ggplot(combined_df, aes(x = Depth, y = Length, color = Treatment)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  facet_wrap(~Species_short) +
  labs(
    title = "Length vs Depth by Treatment",
    x = "Depth (m)",
    y = "Length (mm)",
    color = "Treatment"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#ggsave(here("figures/03_length_vs_depth.png"), p3, width = 10, height = 6)

## Plot 4: Count comparison bar plot.
p4 <- ggplot(summary_stats, aes(x = Species_short, y = Count, fill = Treatment)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Abundance: Number of Individuals Captured",
    x = "Species",
    y = "Count",
    fill = "Treatment"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#ggsave(here("figures/04_abundance_comparison.png"), p4, width = 8, height = 6)

# ============================================================================
## 1. Setup - loading packages.
# ============================================================================

library(tidyverse)
library(gt)
library(here)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)

# ============================================================================
## 2. Loading and cleaning data.
# ============================================================================

## Load data from 50 manual samples of length.
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

## Load data for all samples, indentified by AI system in DeepVision.
art_light <- read.csv(here("data/length_trawl_data/artificial_light.csv"))

# ============================================================================
## 3. Data summary / exploratory analysis.
# ============================================================================

## Summary count from second file.
summary_count <- art_light |>
  group_by(left_label, treatment) |>
  summarise(
    Count = n(),
    .groups = "drop"
  )

## Summary statistics by species and treatment.
summary_length <- combined_df |>
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

t1 <- summary_stats |>
  gt() |>
  fmt_number(
    columns = c(Mean_Length, Mean_Depth),
    decimals = 2
  ) |>
  fmt_number(
    columns = c(SD_Length, SD_Depth),
    decimals = 3
  )

# gtsave(filename = here("figures/table_summary.png"), data = t1)

# ============================================================================
## 4. Statistical test: Changes in abundance between treatments (fisher).
# ============================================================================

# Chi-squared test for independence.
abundance_matrix <- xtabs(Count ~ left_label + treatment, data = summary_count)
chisq.test(abundance_matrix)


# ============================================================================
## 5. Statistical test: T-test for length of capture in treatments.
# ============================================================================

## T-tests for length differences by treatment within each species.
for (sp in unique(combined_df$Species_short)) {
  cat(sprintf("\n%s:\n", sp))

  result <- t.test(Length ~ Treatment,
                   data = combined_df |> filter(Species_short == sp))

  print(result)
}

# ============================================================================
## 7. Fitting a model - exploratory.
# ============================================================================

## Fitting a negative binomial model to the data.
glm_nb <- MASS::glm.nb(Count ~ treatment,
                 data = summary_count)

summary(glm_nb)

## Simulating residuals.
sim <- DHARMa::simulateResiduals(glm_nb, plot = FALSE)

# Check for overdispersion
DHARMa::testDispersion(sim)
DHARMa::plotSimulatedResiduals(sim)

## Negative binomial fits the data with little overdispersion.
## But I do not know how to analyse residuals with only four points?
## Is the model reliable with only four observations?

# ============================================================================
## 8. Visualization.
# ============================================================================

## Plot 1: Count comparison bar plot.
p1 <- ggplot(summary_count, aes(x = left_label, y = Count, fill = treatment)) +
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

# ggsave(here("figures/01_abundance_comparison.png"), p1, width = 8, height = 6)


## Plot 2: Boxplot of length by treatment and species.
p2 <- ggplot(combined_df, aes(x = Treatment, y = Length, fill = Treatment)) +
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

# ggsave(here("figures/02_length_boxplot.png"), p2, width = 10, height = 6)

## Plot 3: Length distribution by treatment and species.
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

# ggsave(here("figures/03_length_distribution.png"), p3, width = 10, height = 6)

## Plot 4: Length vs Depth by treatment.
p4 <- ggplot(combined_df, aes(x = Length, y = Depth, color = Treatment)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  facet_wrap(~Species_short) +
  labs(
    title = "Length vs Depth by Treatment",
    x = "Length (mm)",
    y = "Depth (m)",
    color = "Treatment"
  ) +
  scale_y_reverse() +
  theme_bw() +
  theme(legend.position = "bottom")

# ggsave(here("figures/03_length_vs_depth.png"), p4, width = 10, height = 6)



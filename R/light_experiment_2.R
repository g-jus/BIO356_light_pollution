library(ggplot2)
library(dplyr)
library(readxl)
library(here)
library(tidyr)
library(MASS)
library(lme4)

# Load data with correct decimal separator (comma)
dark_data <- read.csv2(here("data/length_trawl_data/First_night_trawl_length_DV.csv"))
light_data <- read.csv2(here("data/length_trawl_data/Second_night_trawl_length_DV.csv"))

# Add treatment labels
dark_data$Treatment <- "Without artificial light"
light_data$Treatment <- "With artificial light"

# Combine datasets
combined_df <- bind_rows(dark_data, light_data)

# Data cleaning and preparation
combined_df <- combined_df |>
  mutate(
    Length = as.numeric(Length),  # Convert to numeric
    Depth = as.numeric(Depth),
    Species = trimws(Species)  # Remove any whitespace
  ) |>
  filter(!is.na(Length), !is.na(Depth))  # Remove NAs

# Simplify species names for easier plotting
combined_df <- combined_df |>
  mutate(
    Species_short = case_when(
      grepl("Benthosema", Species) ~ "B. glaciale",
      grepl("Maurolicus", Species) ~ "M. muelleri",
      TRUE ~ Species
    )
  )

# ============================================================================
# 1. EXPLORATORY ANALYSIS
# ============================================================================

# Summary statistics by species and treatment
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

print("Summary Statistics by Species and Treatment:")
print(summary_stats)

# ============================================================================
# 2. ABUNDANCE ANALYSIS (Count data)
# ============================================================================

# Count of individuals by species and treatment
abundance_data <- combined_df |>
  group_by(Treatment, Species_short) |>
  summarise(Count = n(), .groups = "drop")

print("\nAbundance Summary:")
print(abundance_data)

# Test for effect of light on abundance using Poisson GLM
glm_abundance <- glm(Count ~ Treatment * Species_short, 
                      family = poisson(link = "log"),
                      data = abundance_data)

print("\nPoisson GLM for Abundance:")
print(summary(glm_abundance))

# ============================================================================
# 3. LENGTH ANALYSIS
# ============================================================================

# Prepare data for length analysis
length_analysis <- combined_df |>
  select(Species_short, Treatment, Length, Depth)

# ANCOVA: Length ~ Treatment + Species + Depth (as covariate)
ancova_model <- lm(Length ~ Treatment + Species_short + Depth + 
                     Treatment:Species_short, 
                   data = length_analysis)

print("\nANCOVA Results (Length ~ Treatment + Species + Depth):")
print(summary(ancova_model))

# ============================================================================
# 4. VISUALIZATIONS
# ============================================================================

# Plot 1: Length distribution by treatment and species
p1 <- ggplot(combined_df, aes(x = Length, fill = Treatment)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7) +
  facet_wrap(~Species_short) +
  labs(
    title = "Length Distribution by Treatment and Species",
    x = "Length (mm)",
    y = "Count",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: Boxplot of length by treatment and species
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
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p2)

# Plot 3: Length vs Depth by treatment
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
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

# Plot 4: Count comparison bar plot
abundance_plot_data <- combined_df |>
  group_by(Species_short, Treatment) |>
  summarise(Count = n(), .groups = "drop")

p4 <- ggplot(abundance_plot_data, aes(x = Species_short, y = Count, fill = Treatment)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Abundance: Number of Individuals Captured",
    x = "Species",
    y = "Count",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p4)

# ============================================================================
# 5. STATISTICAL TESTS (pairwise comparisons)
# ============================================================================

# T-tests for length differences by treatment within each species
print("\n=== T-tests for Length Differences ===")

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

# ============================================================================
# Save plots
# ============================================================================

ggsave(here("outputs/01_length_distribution.png"), p1, width = 10, height = 6)
ggsave(here("outputs/02_length_boxplot.png"), p2, width = 10, height = 6)
ggsave(here("outputs/03_length_vs_depth.png"), p3, width = 10, height = 6)
ggsave(here("outputs/04_abundance_comparison.png"), p4, width = 8, height = 6)

print("\nPlots saved to outputs/ folder")
library(ggplot2)
library(dplyr)
library(readxl)
library(here)

pearlside_data <- read.table(here("data/Stox_SurveyEstimatePearlside_2026/output/baseline/LengthDistribution/LengthDistributionData.txt"),
                        header = TRUE,
                        sep = "\t")
benthozema_data <- read.table(here("data/Stox_SurveyEstimateLanternFish_2026/output/baseline/LengthDistribution/LengthDistributionData.txt"),
                         header = TRUE,
                         sep = "\t")

dark_data <- read_csv2(here("data/length_trawl_data/First_night_trawl_length_DV.csv"))
light_data <- read_csv2(here("data/length_trawl_data/Second_night_trawl_length_DV.csv"))

dark_data$Treatment <- "Without artifical light"
light_data$Treatment <- "With artifical light"

combined_df <- bind_rows(dark_data, light_data)

length_counts <- combined_df |>
  group_by(Treatment, Species) |>
  summarise(count = n(), .groups = "drop")

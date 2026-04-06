library(tidyverse)
library(here)

pearlside_data <- read.table(here("data/Stox_SurveyEstimatePearlside_2026/output/baseline/LengthDistribution/LengthDistributionData.txt"),
                        header = TRUE,
                        sep = "\t")
benthozema_data <- read.table(here("data/Stox_SurveyEstimateLanternFish_2026/output/baseline/LengthDistribution/LengthDistributionData.txt"),
                         header = TRUE,
                         sep = "\t")



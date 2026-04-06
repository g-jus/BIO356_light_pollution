# ============================================================================
## 1. Setup - loading packages.
# ============================================================================

library(tidyverse)
library(here)

# ============================================================================
## 2. Loading and cleaning data.
# ============================================================================

pearlside_data <- read.table(here("data/Stox_SurveyEstimatePearlside_2026/output/baseline/LengthDistribution/LengthDistributionData.txt"),
                        header = TRUE,
                        sep = "\t")
benthozema_data <- read.table(here("data/Stox_SurveyEstimateLanternFish_2026/output/baseline/LengthDistribution/LengthDistributionData.txt"),
                         header = TRUE,
                         sep = "\t")



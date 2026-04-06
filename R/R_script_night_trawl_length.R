
library(readxl)
library(tidyverse)

setwd("C:/Users/Julia/OneDrive/Työpöytä/bergen/BIO356/Excel_356")

night1 <- read_excel("First_night_trawl_length_DV.xlsx")
night2 <- read_excel("Second_night_trawl_length_DV.xlsx")


night1$treatment <- "without light"
night2$treatment <- "light"

night1$night <- "night without light"
night2$night <- "night with light"

# yhdistä datasetit
data_all <- bind_rows(night1, night2)

#

data_all <- data_all %>%
  mutate(length_bin = round(Length / 2) * 2)

length_counts <- data_all %>%
  group_by(night, treatment, Species, length_bin) %>%
  summarise(count = n(), .groups = "drop")

#kuvaaja

ggplot(length_counts,
       aes(x = length_bin,
           y = count,
           color = Species)) +
  
  geom_line(size = 0.9) +
  
  facet_wrap(~night) +
  
  labs(x = "length (mm)",
       y = "quantity",
       color = "species") +
  
  theme_minimal(base_size = 14)


#kokeillaan yhdessä
ggplot(length_counts,
       aes(x = length_bin,
           y = Species,
           fill = Species)) +
  
  geom_boxplot(alpha = 0.6) +
  
  geom_jitter(width = 0.2,
              alpha = 0.3,
              size = 1) +
  
  facet_grid(~treatment) +
  
  labs(x = "Length (mm)",
       y = "Species") +
  
  theme_minimal()

#in dark, without light
ggplot(night1,
       aes(x = Length,
           y = Species,
           fill = Species)) +
  
  geom_boxplot(alpha = 0.6) +
  
  geom_jitter(width = 0.2,
              alpha = 0.3,
              size = 1) +
  
  facet_grid(~treatment) +
  
  labs(x = "Length(mm)",
       y = "Species") +
  
  theme_minimal()


#in light, artificial light
ggplot(night2,
       aes(x = Length,
           y = Species,
           fill = Species)) +
  
  geom_boxplot(alpha = 0.6) +
  
  geom_jitter(width = 0.2,
              alpha = 0.3,
              size = 1) +
  
  facet_grid(~treatment) +
  
  labs(x = "Length (mm)",
       y = "Species") +
  
  theme_minimal()

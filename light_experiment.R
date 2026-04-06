# Load necessary libraries
library(ggplot2)
library(dplyr)

# Data loading with proper decimal handling
data <- read.csv("data/light_pollution_data.csv", dec = ",")  # specify your data file path

# Exploratory analysis
summary(data)
str(data)

# Abundance comparisons
abundance_comparison <- data %>%
  group_by(species) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Length comparisons
length_comparison <- data %>%
  group_by(species) %>%
  summarise(avg_length = mean(length, na.rm = TRUE), 
            sd_length = sd(length, na.rm = TRUE)) %>%
  arrange(desc(avg_length))

# Visualizations
ggplot(data, aes(x = species, y = length)) +
  geom_boxplot() +
  labs(title = "Length Comparison by Species",
       x = "Species",
       y = "Length") +
  theme_minimal()

# Abundance bar plot
ggplot(abundance_comparison, aes(x = reorder(species, -count), y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Species Abundance",
       x = "Species",
       y = "Count") +
  theme_minimal() +
  coord_flip()
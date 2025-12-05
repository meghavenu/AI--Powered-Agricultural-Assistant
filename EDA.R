# ------------------------------
# Libraries
# ------------------------------
library(tidyverse)

# ------------------------------
# Load preprocessed data
# ------------------------------
merged <- read.csv("output/merged_data.csv")

# ------------------------------
# Basic EDA
# ------------------------------

# 1. Summary statistics
summary(merged)

# 2. Data structure
str(merged)

# 3. Check unique states and crops
unique(merged$state)
unique(merged$crop_name)

# 4. Top crops by average yield
top_crops <- merged %>%
  group_by(crop_name) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE)) %>%
  arrange(desc(avg_yield))
print(top_crops)

# 5. Top states by average yield
top_states <- merged %>%
  group_by(state) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE)) %>%
  arrange(desc(avg_yield))
print(top_states)

# 6. Missing values check
colSums(is.na(merged))

# 7. Correlation overview
num_vars <- merged %>% select(yield, area, production, annual_rainfall, fertilizer, pesticide)
cor(num_vars, use = "complete.obs")

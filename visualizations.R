# ------------------------------
# Libraries
# ------------------------------
library(tidyverse)
library(plotly)

# ------------------------------
# Load preprocessed data
# ------------------------------
merged <- read.csv("output/merged_data.csv")

# ------------------------------
# 1. Yield Trend for a Crop (example: Rice)
# ------------------------------
yield_trend <- merged %>%
  filter(crop_name == "Rice") %>%
  group_by(year) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE))

plot_ly(yield_trend, x = ~year, y = ~avg_yield, type = "scatter", mode = "lines+markers",
        line = list(color = "forestgreen")) %>%
  layout(title = "Average Yield Trend for Rice")

# ------------------------------
# 2. Rainfall vs Yield
# ------------------------------
plot_ly(merged, x = ~annual_rainfall, y = ~yield, type = "scatter", mode = "markers",
        marker = list(color = "darkgreen")) %>%
  layout(title = "Rainfall vs Yield")

# ------------------------------
# 3. Regression: Rainfall vs Yield
# ------------------------------
fit <- lm(yield ~ annual_rainfall, data = merged)
merged$pred <- predict(fit, merged)

plot_ly(merged, x = ~annual_rainfall, y = ~yield, type = "scatter", mode = "markers",
        marker = list(color = "green")) %>%
  add_lines(x = ~annual_rainfall, y = ~pred, name = "Fit", line = list(color = "red")) %>%
  layout(title = "Regression: Rainfall vs Yield")

# ------------------------------
# 4. State-wise Average Yield
# ------------------------------
state_avg <- merged %>%
  group_by(state) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE))

plot_ly(state_avg, x = ~state, y = ~avg_yield, type = "bar",
        marker = list(color = "seagreen")) %>%
  layout(title = "State-wise Average Yield", xaxis = list(title = ""), yaxis = list(title = "Yield"))

# ------------------------------
# 5. Yield Distribution (Boxplot)
# ------------------------------
plot_ly(merged, x = ~state, y = ~yield, type = "box",
        marker = list(color = "darkolivegreen")) %>%
  layout(title = "Yield Distribution across States")

# ------------------------------
#correlation heatmap
# Select only numeric columns that exist
num_vars <- merged %>% 
  select(any_of(c("yield", "area", "production", "annual_rainfall", "fertilizer", "pesticide")))

# Compute correlation
cor_mat <- round(cor(num_vars, use = "complete.obs"), 2)

# Plot heatmap
plot_ly(z = cor_mat, type = "heatmap", colors = "Greens") %>%
  layout(title = "Correlation Heatmap")


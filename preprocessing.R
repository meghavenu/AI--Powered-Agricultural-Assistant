# ------------------------------
# Libraries
# ------------------------------
library(tidyverse)

# ------------------------------
# Load datasets
# ------------------------------
crop <- read.csv("data/crop_yield.csv")
rain <- read.csv("data/India_rainfall_act_dep_1901_2016_1.csv")

# ------------------------------
# Detect rainfall and state columns dynamically
# ------------------------------
rainfall_col <- colnames(rain)[grepl("JUN.*SEPT", colnames(rain), ignore.case = TRUE)][1]
state_col <- colnames(rain)[!colnames(rain) %in% c("YEAR", rainfall_col)][1]

# ------------------------------
# Rename columns
# ------------------------------
crop <- crop %>%
  rename(
    crop_name = Crop,
    year = Crop_Year,
    state = State,
    yield = Yield,
    annual_rainfall_crop = Annual_Rainfall
  )

rain <- rain %>%
  rename(
    year = YEAR,
    state = !!sym(state_col),
    rainfall_jun_sep = !!sym(rainfall_col)
  ) %>%
  select(state, year, rainfall_jun_sep)

# ------------------------------
# Convert join keys to character
# ------------------------------
crop$state <- as.character(crop$state)
rain$state <- as.character(rain$state)

# ------------------------------
# Merge datasets
# ------------------------------
merged <- crop %>%
  left_join(rain, by = c("state", "year")) %>%
  mutate(
    annual_rainfall = ifelse(!is.na(rainfall_jun_sep), rainfall_jun_sep, annual_rainfall_crop)
  ) %>%
  drop_na(yield, annual_rainfall)

# ------------------------------
# Save cleaned data
# ------------------------------
write.csv(merged, "output/merged_data.csv", row.names = FALSE)

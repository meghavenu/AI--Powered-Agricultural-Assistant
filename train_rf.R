=# ------------------------------
# train_rf_model.R
# ------------------------------
library(tidyverse)
library(randomForest)
library(caret)


set.seed(42)

# ------------------------------
# 1. Load data
# ------------------------------
merged <- read.csv("output/merged_data.csv", stringsAsFactors = FALSE)

# ------------------------------
# 2. Numeric columns
# ------------------------------
num_cols <- c("annual_rainfall", "area", "fertilizer", "pesticide", "production")
existing_num_cols <- num_cols[num_cols %in% names(merged)]
existing_num_cols <- existing_num_cols[sapply(merged[existing_num_cols], function(x) sum(!is.na(x)) > 0)]

# Ensure yield and year exist
if(!"yield" %in% names(merged)) merged$yield <- NA_real_
if(!"year" %in% names(merged)) merged$year <- NA_integer_

# ------------------------------
# 3. Convert types
# ------------------------------
merged <- merged %>%
  mutate(
    across(all_of(existing_num_cols), as.numeric),
    yield = as.numeric(yield),
    year = as.integer(year),
    crop_name = as.character(crop_name),
    state = as.character(state)
  )

# ------------------------------
# 4. Keep rows with target yield
# ------------------------------
df <- merged %>% filter(!is.na(yield) & !is.na(crop_name) & !is.na(state))

# ------------------------------
# 5. Median imputation
# ------------------------------
medians <- map_dbl(existing_num_cols, ~ median(df[[.x]], na.rm = TRUE))
df <- df %>% mutate(across(all_of(existing_num_cols), ~ ifelse(is.na(.), medians[cur_column()], .)))

# ------------------------------
# 6. Limit categorical levels
# ------------------------------
limit_levels <- function(x, max_levels = 50){
  tbl <- sort(table(x), decreasing = TRUE)
  top_levels <- names(tbl)[1:min(length(tbl), max_levels)]
  x[!x %in% top_levels] <- "Other"
  factor(x)
}

df <- df %>%
  mutate(
    crop_name = limit_levels(crop_name, 50),
    state = limit_levels(state, 50)
  )

meta <- list(
  crop_levels = levels(df$crop_name),
  state_levels = levels(df$state),
  medians = medians
)

# ------------------------------
# 7. Prepare model dataframe
# ------------------------------
model_df <- df %>% select(yield, all_of(existing_num_cols), crop_name, state, year)

if(nrow(model_df) < 2) stop("Not enough data points to train the model.")
cat("Rows available for modeling:", nrow(model_df), "\n")

# ------------------------------
# 8. Train/test split
# ------------------------------
train_index <- createDataPartition(model_df$yield, p = 0.8, list = FALSE)
train <- model_df[train_index, ]
test  <- model_df[-train_index, ]

# ------------------------------
# 9. Train Random Forest
# ------------------------------
rf_formula <- as.formula(paste("yield ~", paste(c(existing_num_cols, "crop_name", "state", "year"), collapse=" + ")))

rf_model <- randomForest(
  formula = rf_formula,
  data = train,
  ntree = 500,
  importance = TRUE
)

# ------------------------------
# 10. Evaluate
# ------------------------------
pred_test <- predict(rf_model, newdata = test)
rmse <- sqrt(mean((pred_test - test$yield)^2, na.rm = TRUE))
r2   <- 1 - sum((pred_test - test$yield)^2)/sum((test$yield - mean(test$yield))^2)
cat("Test RMSE:", round(rmse,2), "\n")
cat("Test R2  :", round(r2,3), "\n")

# ------------------------------
# 11. Save model and metadata
# ------------------------------
if(!dir.exists("models")) dir.create("models")
saveRDS(rf_model, "models/yield_rf.rds")
saveRDS(meta, "models/meta.rds")
cat("Saved models/yield_rf.rds and models/meta.rds\n")


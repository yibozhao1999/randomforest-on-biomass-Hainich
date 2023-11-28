library(terra)
library(dplyr)
library(purrr)

# Define base paths
base_path <- "path"
output_path <- "path"

# Define the file names and their respective variable names
file_names <- c("NDVI", "PSRI", "RDVI", "GNDVI", "WDRVI", "MSAVI2", "VDVI", "RVI", "EVI", "STVI")
file_paths <- paste0(base_path, file_names, ".tif")

# Load raster and polygon layers
polygons <- "path/file.shp"
v <- vect(polygons)

# Function to load rasters
load_rasters <- function(paths) {
  map(paths, ~rast(.x))
}

# Function to extract and calculate weighted mean
calculate_mean <- function(rasters, v) {
  map(rasters, ~{
    extracted <- terra::extract(.x, v, weight = TRUE, touches = TRUE)
    results <- extracted %>%
      group_by(ID) %>%
      summarize(weighted_mean = sum(get(names(.x)) * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE), .groups = 'drop')
    colnames(results) <- c("ID", paste0("weighted_mean_", tolower(names(.x))))
    results
  })
}

# Function to calculate median and rename columns
calculate_median <- function(rasters, v) {
  map(rasters, ~{
    median_result <- terra::extract(.x, v, fun = median, na.rm = TRUE)
    colnames(median_result) <- c("ID", paste0("median_", tolower(names(.x))))
    median_result
  })
}

# Function to calculate stand deviation and rename columns
calculate_sd <- function(rasters, v) {
  map(rasters, ~{
    sd_result <- terra::extract(.x, v, fun = sd, na.rm = TRUE)
    colnames(sd_result) <- c("ID", paste0("sd_", tolower(names(.x))))
    sd_result
  })
}

# Load rasters
rasters <- load_rasters(file_paths)

# Calculate results
results_weighted_mean <- calculate_mean(rasters, v)
results_median <- calculate_median(rasters, v)
results_sd <-calculate_sd(rasters, v)


# Combine the results

# Create a new list that groups corresponding data frames together
list_of_dfs <- list(results_median, results_weighted_mean, results_sd)

combined_results <- pmap(list_of_dfs, function(df1, df2, df3) {
  joined_df <- full_join(df1, df2, by = "ID") %>%
    full_join(df3, by = "ID")
  
})

# Write to CSV
write.csv(combined_results, file = paste0(output_path, "weighted_mean_medain_sd_results.csv"), row.names = FALSE)

##################### modelling ###########################################
# Load necessary libraries
library(randomForest)
library(caret)
library(ggplot2)

# Set a seed for reproducibility
set.seed(47)

biomass_model <- read.csv(file.choose())
# head(biomass_model)

# Ensure the 'BA' column is numeric
biomass_model$BA <- as.numeric(biomass_model$BA)

# Split data into training and testing sets
train_indices <- createDataPartition(biomass_model$BA, p = 0.7, list = FALSE)
train_data <- biomass_model[train_indices, ]
test_data <- biomass_model[-train_indices, ]

# Initialize a dataframe to store the results
result <- data.frame(ntree = integer(), mtry = integer(), RMSE = numeric())

# Grid search for parameter tuning using trainingdata
for (ntree in seq(500, 2500, by = 500)) {
  for (mtry in 6:25) {
    rf_model <- randomForest(BA ~., data = train_data, ntree = ntree, mtry = mtry, importance = TRUE)
    predictions <- predict(rf_model, train_data)  
    RMSE <- sqrt(mean((train_data$BA - predictions)^2))
    result <- rbind(result, data.frame(ntree = ntree, mtry = mtry, RMSE = RMSE))
  }
}


# Identify the best parameters from grid search
best_params <- result[which.min(result$RMSE), ]
best_mtry <- best_params$mtry
best_ntree <- best_params$ntree

# Plotting the grid search results
ggplot(result, aes(x = mtry, y = RMSE, group = factor(ntree), color = factor(ntree))) +
  geom_line() +
  geom_point() +
  geom_point(data = best_params, aes(x = mtry, y = RMSE), color = 'black', size = 4, shape = 19) +
  labs(
    x = "'mtry' value",
    y = "RMSE",
    color = "'ntree' value"
  ) +
  theme_minimal() 


# Print the best parameters
print(paste("Best mtry:", best_mtry, " Best ntree:", best_ntree))

# Set up repeated cross-validation
set.seed(47)
repeat_cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  search = "grid"
)

# Train the final model with the best parameters using training data
biomass <- train(
  BA ~ ., 
  data = train_data, 
  method = 'rf',
  ntree = best_ntree,
  tuneGrid = expand.grid(.mtry = best_mtry),
  trControl = repeat_cv,
  metric = 'RMSE'
)

# Summary of the final model
print(biomass$finalModel)

# Variable importance
var_imp <- varImp(biomass, scale = FALSE)$importance

#calculate Variable importance in %
var_imp$Overall <- (var_imp$Overall / sum(var_imp$Overall)) * 100
print(var_imp)

# Predictions on test data using trained model
y_hats <- predict(biomass, newdata = test_data)

# Model performance on test data
results <- postResample(pred = y_hats, obs = test_data$BA)
cat('Root Mean Square Error on testing data: ', round(results['RMSE'], 2), '\n')
cat('Mean Absolute Error on testing data: ', round(results['MAE'], 2), '\n')
cat('R-squared on testing data: ', round(results['Rsquared'], 2), '\n')

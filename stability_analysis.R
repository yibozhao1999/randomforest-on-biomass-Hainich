library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

num_iterations <- 100
combined_results <- data.frame()

set.seed(27) # Global seed for reproducibility

# Loop for each dataset
for (dataset_num in c("15", "25", "50", "75", "100")) {
  # Read each of datasets
  file_name <- paste0("path/to/file", dataset_num, ".csv")
  biomass_model <- read.csv(file_name)
  
  set.seed(27) # Seed for consistent data partitioning
  train_indices <- createDataPartition(biomass_model$BA, p = 0.7, list = FALSE)
  train_data <- biomass_model[train_indices, ]
  test_data <- biomass_model[-train_indices, ]
  
  # Select best mtry
  set.seed(123) # Seed for consistent mtry selection
  mtry <- tuneRF(train_data[-1], train_data$BA, ntreeTry=500, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
  best_mtry <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  # Select best ntree
  result <- data.frame()
  for (ntree in seq(500, 2500, by = 500)) {
    set.seed(123) # Seed for consistent random forest modeling
    rf_model <- randomForest(BA ~ ., data = train_data, ntree = ntree, mtry = best_mtry, importance = TRUE)
    predictions <- predict(rf_model, test_data)
    RMSE <- sqrt(mean((test_data$BA - predictions)^2))
    result <- rbind(result, data.frame(ntree = ntree, mtry = best_mtry, RMSE = RMSE))
  }
  
  best_params <- result[which.min(result$RMSE), ]
  best_ntree <- best_params$ntree
  
  # Stability analysis across different seeds
  r_squared_values <- numeric(num_iterations)
  for(i in 1:num_iterations) {
    set.seed(i) # Individual seed for each iteration to evaluate stability
    model <- randomForest(BA ~ ., data = train_data, ntree = best_ntree, mtry = best_mtry)
    predictions <- predict(model, newdata = test_data)
    results <- postResample(predictions, test_data$BA)
    r_squared_values[i] <- results['Rsquared']
  }
  
  # Creating dataframe for results
  r_squared_df <- data.frame(R_Squared = r_squared_values, Dataset = paste0(dataset_num, "*", dataset_num))
  combined_results <- rbind(combined_results, r_squared_df)
}

combined_results$Dataset <- factor(combined_results$Dataset, 
                                   levels = c("15*15", "25*25", "50*50", "75*75", "100*100"))

# Calculate mean R-squared for each dataset
combined_results <- combined_results %>% 
  group_by(Dataset) %>% 
  mutate(Mean_R_Squared = mean(R_Squared))

# Creating a boxplot for all datasets
boxplot_graph <- ggplot(combined_results, aes(x = Dataset, y = R_Squared, fill = Dataset)) +
  geom_boxplot(fill = "white") + 
  geom_jitter(width = 0.1, color = "grey", size = 1) +
  geom_hline(aes(yintercept = Mean_R_Squared), linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(title = "Basal Area", x = "Plot Size", y = "R-squared")

# Display the boxplot
print(boxplot_graph)

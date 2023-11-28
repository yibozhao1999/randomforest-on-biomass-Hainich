library(randomForest)
library(caret)
library(ggplot2)

# Set a seed for reproducibility
set.seed(47)

biomass_model <- read.csv(file.choose())

# Split data into training and testing sets
train_indices <- createDataPartition(biomass_model$BA, p = 0.7, list = FALSE)
train_data <- biomass_model[train_indices, ]
test_data <- biomass_model[-train_indices, ]

#select the best mtry with minimum oob in Randomforest
set.seed(47)
mtry <- tuneRF(train_data[-1], train_data$BA, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Initialize a dataframe to store the results
result <- data.frame(ntree = integer(), mtry = integer(), RMSE = numeric())

#gridsearch to select ntree 
set.seed(47)
for (ntree in seq(500, 2500, by = 500)) {
  rf_model <- randomForest(BA ~., data = train_data, ntree = ntree, mtry = best.m, importance = TRUE)
  predictions <- predict(rf_model, train_data) # #### or use test data 
  RMSE <- sqrt(mean((train_data$BA - predictions)^2))
  result <- rbind(result, data.frame(ntree = ntree, mtry = best.m, RMSE = RMSE))
  }


# Identify the best parameters from grid search
best_params <- result[which.min(result$RMSE), ]
best_mtry <- best_params$mtry
best_ntree <- best_params$ntree

# Print the best parameters
print(paste("Best mtry:", best_mtry, " Best ntree:", best_ntree))

####### train the random forest model again ############
# Set up repeated cross-validation
set.seed(47)
repeat_cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  search = "grid"
)

# Train the final model with the best parameters
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
var_imp$Overall <- (var_imp$Overall / sum(var_imp$Overall)) * 100
print(var_imp)

# Predictions on test data
y_hats <- predict(biomass, newdata = test_data)

# Model performance on test data
results <- postResample(pred = y_hats, obs = test_data$BA)
cat('Root Mean Square Error on testing data: ', round(results['RMSE'], 2), '\n')
cat('Mean Absolute Error on testing data: ', round(results['MAE'], 2), '\n')
cat('R-squared on testing data: ', round(results['Rsquared'], 2), '\n')

# Creating the plot
myplot <-
  ggplot(data = test_data, aes(x = BA, y = y_hats)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  ggtitle("15*15 m") +
  xlab("Observed BA m²/ha") +
  ylab("Predicted BA m²/ha") +
  annotate("text", x = min(test_data$BA), y = max(y_hats), hjust = 0, vjust = 1,
           label = paste("R² =", R2, "\nMAE =", MAE, "\nRMSE =", RMSE, "\nRMSE% =", RMSE_percent, "%"),
           size = 3)

# Adding linear regression line to the plot
myplot <- myplot + geom_smooth(method = "lm", se = FALSE, 
                               linetype = "dashed", color = "red", linewidth = 0.6)

# Deleting background and grid lines and displaying the plot
final_plot <- myplot + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Display the final plot
print(final_plot)

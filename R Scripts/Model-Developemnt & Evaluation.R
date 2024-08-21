# 1. Load Libraries and Dataset

#Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(gbm)
library(corrplot)
library(ggplot2)

# Load and preprocess data
data <- read_csv("C:/Users/Ahmad Afzal/Desktop/Refined_Scotland_WRFdata.csv")

data.frame(data)
# Ensure the column names are correct
colnames(data) <- c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "DATETIME", "WIND_SPEED", "Hour", "Day_Night", "Date")

# Convert DATETIME to POSIXct format and other columns as necessary
data$DATETIME <- as.POSIXct(data$DATETIME, format = "%d.%m.%Y.%H.%M")
data$Day_Night <- as.factor(data$Day_Night)
data$Hour <- as.numeric(data$Hour)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Display the structure of the dataset to verify changes
str(data)


#-------------------------------------------------------

# 2. Handle NA Values in datset

# 1. Define the critical columns, including RAINC
critical_columns <- c("TSK", "PSFC", "SMOIS", "WIND_SPEED", "RAINC")

# 2. Remove rows with any NA values in the critical columns
data <- data %>%
  filter(!if_any(all_of(critical_columns), is.na))

# 3. Verify that there are no NA values left
na_summary <- colSums(is.na(data))
print("Remaining NA values per column:")
print(na_summary)

# Proceed to the next steps only if NA values are handled
if (all(na_summary == 0)) {
  cat("All NA values have been handled.\n")
} else {
  stop("NA values still exist in the dataset. Please check the handling process.")
}

# View the cleaned data
print(head(data))

D#------------------------------------------------------



# 3.Data Preparation


# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$TSK, p = .7, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Check for and handle any missing values by filling NA with the mean of the previous two values
fill_na_with_mean <- function(x) {
  na_index <- which(is.na(x))
  for (i in na_index) {
    if (i > 2) {
      x[i] <- mean(c(x[i - 1], x[i - 2]), na.rm = TRUE)
    }
  }
  return(x)
}

# Check for zero-variance columns in the training data
zero_variance_cols <- nearZeroVar(trainData, saveMetrics = TRUE)
zero_variance_cols <- rownames(zero_variance_cols[zero_variance_cols$zeroVar == TRUE, ])

# Remove zero-variance columns from the training and testing data
trainData <- trainData[, !colnames(trainData) %in% zero_variance_cols]
testData <- testData[, !colnames(testData) %in% zero_variance_cols]

# Now, define the numeric columns again after removing zero-variance columns
numeric_cols <- names(trainData)[sapply(trainData, is.numeric)]

# Standardize the numeric columns
preProc <- preProcess(trainData[, numeric_cols], method = c("center", "scale"))
trainData[, numeric_cols] <- predict(preProc, trainData[, numeric_cols])
testData[, numeric_cols] <- predict(preProc, testData[, numeric_cols])

# Verify the preprocessed data
str(trainData)
str(testData)




#-------------------------------------------------------


# 4. Statistical Analysis


# 4.1. Descriptive Statistics and Correlation Analysis

# Summary statistics
summary(data)

# Correlation matrix and plot
cor_matrix <- cor(data[, numeric_cols], use = "complete.obs")
corrplot(cor_matrix, method = "circle")

# Specific correlation between TSK, PSFC, and other variables
cor(data$TSK, data$PSFC, use = "complete.obs")





# 4.2 T-Test for Daytime vs. Nighttime Analysis


# T-test for PSFC (surface pressure) between Daytime and Nighttime
daytime_data <- filter(data, Day_Night == "Daytime")
nighttime_data <- filter(data, Day_Night == "Nighttime")
t_test_result <- t.test(daytime_data$PSFC, nighttime_data$PSFC)
print(t_test_result)





#-----------------------------------------------------


# 5. Machine Learning Models


# 5.1 Linear Regression


# Train a Linear Regression model to predict TSK
lm_model <- lm(TSK ~ U10 + V10 + Q2 + RAINC + RAINNC + PSFC + SMOIS, data = trainData)





# Evaluate the model
lm_preds <- predict(lm_model, testData)




# Re-run the prediction and evaluation
lm_preds_clean <- predict(lm_model, testData)
lm_rmse_clean <- sqrt(mean((lm_preds - testData$TSK)^2))
cat("Linear Regression RMSE :", lm_rmse_clean, "\n")



# 5.2 Random Forest

# Train a Random Forest model
rf_model <- randomForest(TSK ~ U10 + V10 + Q2 + RAINC + RAINNC + PSFC + SMOIS, data = trainData, ntree = 100)

# Generate predictions on the test data

rf_preds <- predict(rf_model, testData)


# Remove NA values from predictions and actual values
valid_index <- complete.cases(testData$TSK, rf_preds)
rf_rmse <- sqrt(mean((rf_preds[valid_index] - testData$TSK[valid_index])^2))

# Print the RMSE
cat("Random Forest RMSE:", rf_rmse, "\n")






# 5.3 Support Vector Machine (SVM)


# Train an SVM model
svm_model <- svm(TSK ~ U10 + V10 + Q2 + RAINC + RAINNC + PSFC + SMOIS, data = trainData)

# Evaluate the SVM model
svm_preds <- predict(svm_model, testData)
svm_rmse <- sqrt(mean((svm_preds - testData$TSK)^2))
cat("SVM RMSE:", svm_rmse, "\n")





# 5.4 Gradient Boosting Machine (GBM)

# Train a Gradient Boosting Machine model
gbm_model <- gbm(TSK ~ U10 + V10 + Q2 + RAINC + RAINNC + PSFC + SMOIS, data = trainData, distribution = "gaussian", n.trees = 100, interaction.depth = 3)

# Evaluate the GBM model
gbm_preds <- predict(gbm_model, testData, n.trees = 100)
gbm_rmse <- sqrt(mean((gbm_preds - testData$TSK)^2))
cat("GBM RMSE:", gbm_rmse, "\n")





#-------------------------------------------------

# 6. Save the trained models


save(lm_model, file = "C:/Users/Ahmad Afzal/Desktop/lm_model.RData")
save(rf_model, file = "C:/Users/Ahmad Afzal/Desktop/rf_model.RData")
save(svm_model, file = "C:/Users/Ahmad Afzal/Desktop/svm_model.RData")
save(gbm_model, file = "C:/Users/Ahmad Afzal/Desktop/gbm_model.RData")




#--------------------------------------------------------------------------


# 7. Model Comparison and Selection

# Compare the RMSE of all models
cat("Linear Regression RMSE:", lm_rmse_clean, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")
cat("SVM RMSE:", svm_rmse, "\n")
cat("GBM RMSE:", gbm_rmse, "\n")

# Select the best model based on RMSE
best_model <- which.min(c(lm_rmse_clean, rf_rmse, svm_rmse, gbm_rmse))
model_names <- c("Linear Regression", "Random Forest", "SVM", "GBM")
cat("Best model based on RMSE is:", model_names[best_model], "\n")



#--------------------------------------------------

# 8. Final Model Evaluation on Test Data

# Depending on the selected best model, we can evaluate it further

# Random Forest 
final_model <- rf_model
final_preds <- predict(final_model, testData)
final_rmse <- sqrt(mean((final_preds - testData$TSK)^2))
cat("Final Model RMSE on Test Data:", final_rmse, "\n")

# Analyze residuals
residuals <- testData$TSK - final_preds
plot(residuals, main = "Residuals of Final Model", ylab = "Residuals")



# Libraries
library(readxl)
library(xgboost)
library(randomForest)

# ------------------------------
# 1. Load and Prepare the Data
# ------------------------------

# Read the dataset (ensure the file is in your working directory)
data <- read_excel("dataset.xlsx")

# Convert the date column to Date type (adjust the format if needed)
data$date <- as.Date(data$date)

# Define the target variable name as it appears in your dataset
target_col <- "Average_Price_Eggs_Grade_A_Large_Cost_per_Dozen_in_US_City_Average"

# Define predictor names by excluding the date and target columns
predictor_names <- setdiff(names(data), c("date", target_col))

# Create the matrix of predictors and ensure column names are set
X <- as.matrix(data[, predictor_names])
colnames(X) <- predictor_names

# Extract the target variable
y <- data[[target_col]]

# Create a DMatrix for xgboost training
dtrain <- xgb.DMatrix(data = X, label = y)

# ------------------------------
# 2. Train the xgboost Model
# ------------------------------

# Define xgboost parameters for regression
params <- list(
  objective = "reg:squarederror",  # Regression objective
  eval_metric = "rmse"             # Root Mean Squared Error metric
)

# Train the xgboost model (set seed for reproducibility)
set.seed(123)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)

# ------------------------------
# 3. Print Training RMSE
# ------------------------------

# Predict on the training data
pred_train <- predict(xgb_model, dtrain)
# Compute the RMSE on the training set
rmse_train <- sqrt(mean((pred_train - y)^2))
cat("Training RMSE:", rmse_train, "\n")

# ------------------------------
# 4. Define Function to Forecast a Predictor Using Random Forest
# ------------------------------

# This function creates lag features using embed() and trains a Random Forest model.
forecast_predictor <- function(series, lags = 3, ntree = 100) {
  n <- length(series)
  if(n <= lags) stop("Not enough data in series")
  
  # Create a data frame using embed() where the first column is the target.
  df <- as.data.frame(embed(series, lags + 1))
  colnames(df) <- c("target", paste0("lag", 1:lags))
  
  # Train a Random Forest model on the lagged data.
  rf_model <- randomForest(target ~ ., data = df, ntree = ntree)
  
  # Prepare new input using the last 'lags' values (reversed to match embed() order).
  new_input <- as.data.frame(t(rev(tail(series, lags))))
  colnames(new_input) <- paste0("lag", 1:lags)
  
  # Forecast and return the next value.
  predicted_value <- predict(rf_model, new_input)
  return(predicted_value)
}

# ------------------------------
# 5. Forecast All Predictors for February 2025
# ------------------------------

# Use sapply to forecast each predictor; the names are preserved in the output.
predicted_predictors <- sapply(predictor_names, function(pred) {
  forecast_predictor(data[[pred]], lags = 3)
})

# Convert the forecasted predictors into a data frame and ensure column names match.
new_data <- as.data.frame(as.list(predicted_predictors))
colnames(new_data) <- predictor_names

# ------------------------------
# 6. Predict Egg Price for February 2025
# ------------------------------

# Convert the new data into a matrix and assign column names.
new_matrix <- as.matrix(new_data)
colnames(new_matrix) <- predictor_names

# Create a DMatrix for prediction.
dnew <- xgb.DMatrix(data = new_matrix)

# Predict the egg price for Feb 2025 using the trained xgboost model.
predicted_eggprice <- predict(xgb_model, dnew)

# Print the forecasted egg price
cat("Forecasted Egg Price for Feb 2025:", predicted_eggprice, "\n")

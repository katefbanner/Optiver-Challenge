# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

# Step 1: Data Preparation
# Load the data
data <- read.csv('final_dataset.csv')  # Assuming the file is in CSV format

# Extract relevant columns
data <- data[, c('date', 'Average_Price_Eggs_Grade_A_Large_Cost_per_Dozen_in_US_City_Average')]

# Convert date column to Date type
data$date <- as.Date(data$date)

# Convert to time series object
ts_data <- ts(data$Average_Price_Eggs_Grade_A_Large_Cost_per_Dozen_in_US_City_Average, start=c(2006, 1), frequency=12)

# Step 2: Exploratory Data Analysis
# Plot the time series
autoplot(ts_data) + ggtitle('Price of a Dozen Eggs Over Time') + xlab('Date') + ylab('Price (USD)')

# Check for stationarity
adf_test <- adf.test(ts_data)
print(adf_test)

# Step 3: Model Identification
# Plot ACF and PACF
acf(ts_data, lag.max=20)
pacf(ts_data, lag.max=20)

# Step 4: Model Fitting
# Fit SARIMA model
# Example parameters (you may need to tune these based on ACF/PACF and stationarity)
sarima_model <- Arima(ts_data, order=c(1, 1, 1), seasonal=list(order=c(1, 1, 1), period=12)
                      
# Summary of the model
summary(sarima_model)
                      
# Step 5: Forecasting
# Forecast the next few months
forecast <- forecast(sarima_model, h=3)  # Adjust h as needed
print(forecast)
                      
# Plot the forecast
autoplot(forecast) + ggtitle('Price of a Dozen Eggs Forecast') + xlab('Date') + ylab('Price (USD)')
                      
# Step 6: Model Evaluation
# Evaluate the model (e.g., using train-test split and metrics like MAE, MSE, etc.)
# This step is crucial for validating the model's predictive powe
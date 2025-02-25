# Libraries
library(readxl)
library(forecast)

# ------------------------------
# 1. Load and Prepare the Data
# ------------------------------

# Read the dataset (ensure the file is in your working directory)
data <- read_excel("dataset_sarima.xlsx")

# Convert the date column to Date type
# Adjust the format string to match your dataset's date format
data$date <- as.Date(data$date, format = "%d.%m.%Y")

# The target variable is in the second column
price_col <- "Average_Price_Eggs_Grade_A_Large_Cost_per_Dozen_in_US_City_Average"

# Extract the egg price vector; assume that Feb 2025 has NA for the egg price.
egg_price <- data[[price_col]]
# Remove NA values (assumes Feb 2025 is missing the price)
egg_price <- egg_price[!is.na(egg_price)]

# Create a monthly time series starting from January 2006.
# Since your data goes from 01.01.2006 to (missing) Feb 2025, the last observed month is Jan 2025.
egg_price_ts <- ts(egg_price, start = c(2006, 1), frequency = 12)

# ------------------------------
# 2. Fit a SARIMA Model
# ------------------------------

# Use auto.arima to select the best SARIMA model automatically.
sarima_model <- auto.arima(egg_price_ts, seasonal = TRUE)

# Print model summary
summary(sarima_model)

# ------------------------------
# 3. Forecast the Egg Price for February 2025
# ------------------------------

# Forecast 1 step ahead (from Jan 2025 to Feb 2025)
forecast_result <- forecast(sarima_model, h = 1)

# Print and plot the forecast results
print(forecast_result)
plot(forecast_result)

## Packages
# Install necessary packages
install.packages("readr", "dplyr", "ggplot2", "tseries", "forecast")

# Load necessary packages
library(readr) ## Use to read read CSV file
library(dplyr) ## Use to manipulate data in a data frame
library(ggplot2) ## Use to create plots from data frames
library(tseries) ## Use to analyze time series data
library(forecast) ## Use to give forecasts from time series models


# Set working directory to data location and confirm working directory
setwd("SET DIRECTORY HERE")
getwd()

# Access csv file provided in readme for time vs. CFU data set
data <- read.csv("1_Year_CFU.csv", header = TRUE)
head(data)

# Determine metadata of data set
names(data)
str(data)
summary(data$CFU)
nrow(data)

# Clean data by removing rows where there is no CFU entry
data <- data[!is.na(data$CFU), ]

# Re-summarize data after cleaning
summary(data$CFU)
nrow(data)

## Time Series Analysis
# Create the time series object to prepare entries for time series analysis
if (nrow(data) > 0) {
  ts_data <- ts(data$CFU, frequency = 7)
  print(ts_data)
} else {
  cat("The dataset is empty after cleaning, or there is no numeric data in the data set.\n")
}

# Plot
# Decompose time series data to find categorical patterns in data
if (nrow(data) > 0) {
  decomposed <- stl(ts_data, s.window = "periodic")
  plot(decomposed)
  par(mar = c(4, 4, 2, 2))
}

# Use stationary (ADF) test to determine ARIMA model parameters
adf_test <- adf.test(ts_data, alternative = "stationary")
print(adf_test)

# Fit data to an ARIMA model
model <- auto.arima(ts_data)
summary(model)

# Forecast future data points and determine forecast accuracy
future_data <- forecast(model, h = 28)
accuracy(future_data)

# Extract ARIMA model parameters
arima_params <- paste("ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2], ")", sep="")

# Create a data frame to draw confidence intervals for future_data
future_df <- data.frame(
  Time = as.numeric(time(future_data$mean)),
  Forecast = as.numeric(future_data$mean),
  Lo80 = future_data$lower[, "80%"],
  Hi80 = future_data$upper[, "80%"],
  Lo95 = future_data$lower[, "95%"],
  Hi95 = future_data$upper[, "95%"]
)

# Combine historical and forecasted data
combined_df <- data.frame(
  Time = c(time(ts_data), as.numeric(time(future_data$mean))),
  Count = c(as.numeric(ts_data), as.numeric(future_data$mean)),
  Type = c(rep("Historical", length(ts_data)), rep("Forecast", length(future_data$mean)))
)

# Add confidence intervals to the forecasted data
combined_forecast_df <- future_df %>% 
  mutate(Type = "Forecast")

# Create a historical-forecasted time series plot
combined_plot <- ggplot() +
  geom_line(data = combined_df, aes(x = Time, y = Count, color = Type), size = 1) +
  geom_ribbon(data = combined_forecast_df, aes(x = Time, ymin = Lo95, ymax = Hi95), fill = "blue", alpha = 0.1, color = NA) +
  geom_ribbon(data = combined_forecast_df, aes(x = Time, ymin = Lo80, ymax = Hi80), fill = "blue", alpha = 0.2, color = NA) +
  labs(title = paste("Historical and Forecasted CFU using", arima_params, "Model"),
       x = "Time (Weeks)",
       y = "Count (CFU)") +
  scale_color_manual(values = c("Historical" = "black", "Forecast" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(combined_plot)
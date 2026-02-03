# Time Series Forecasting with Exponential Smoothing Models
# For multiple locations, considering cyclical effects

# Load required libraries
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)
library(purrr)
library(readr)

# Assume your data is loaded as 'data'
# If reading from CSV: data <- read_csv("your_file.csv")

# Data preprocessing
data <- dfx %>%
  arrange(location_id, year) %>%
  filter(!is.na(val))

# Function to fit ES model and forecast for a single location
forecast_location <- function(location_data, h = 29) {  # h = 2050 - 2021 = 29 years
  
  # Create time series object
  # Assuming annual data, set frequency = 1 for no seasonality
  # If you suspect longer cycles, you could experiment with higher frequencies
  ts_data <- ts(location_data$val, 
                start = min(location_data$year), 
                frequency = 1)
  
  # Try different ES models and select the best one
  models <- list()
  
  # Simple Exponential Smoothing (SES)
  models$ses <- tryCatch(ses(ts_data, h = h), error = function(e) NULL)
  
  # Holt's method (with trend)
  models$holt <- tryCatch(holt(ts_data, h = h), error = function(e) NULL)
  
  # Damped Holt's method
  models$holt_damped <- tryCatch(holt(ts_data, damped = TRUE, h = h), error = function(e) NULL)
  
  # Exponential smoothing state space model (automatic selection)
  models$ets <- tryCatch(ets(ts_data) %>% forecast(h = h), error = function(e) NULL)
  
  # For cyclical effects, we can try ETS with different seasonal patterns
  # Even though frequency = 1, ETS can still capture some patterns
  
  # Remove NULL models
  models <- models[!sapply(models, is.null)]
  
  if (length(models) == 0) {
    return(NULL)
  }
  
  # Select best model based on AIC (for ETS) or in-sample accuracy
  best_model <- NULL
  best_aic <- Inf
  
  for (i in seq_along(models)) {
    current_aic <- tryCatch({
      if ("ets" %in% class(models[[i]]$model)) {
        models[[i]]$model$aic
      } else {
        # For other models, use in-sample accuracy as proxy
        accuracy(models[[i]])[1, "MAPE"]  # Use MAPE as selection criterion
      }
    }, error = function(e) Inf)
    
    if (current_aic < best_aic) {
      best_aic <- current_aic
      best_model <- models[[i]]
    }
  }
  
  return(best_model)
}

# Function to detect and handle potential cyclical patterns
detect_cycles <- function(ts_data, max_period = 10) {
  # Simple approach: test different frequencies for cyclical patterns
  n <- length(ts_data)
  if (n < 20) return(1)  # Too short for cycle detection
  
  best_aic <- Inf
  best_freq <- 1
  
  for (freq in 2:min(max_period, floor(n/4))) {
    tryCatch({
      ts_temp <- ts(ts_data, frequency = freq)
      model_temp <- ets(ts_temp)
      if (model_temp$aic < best_aic) {
        best_aic <- model_temp$aic
        best_freq <- freq
      }
    }, error = function(e) NULL)
  }
  
  return(best_freq)
}

# Enhanced function with cycle detection
forecast_location_with_cycles <- function(location_data, h = 29) {
  
  # Detect optimal frequency for potential cycles
  optimal_freq <- detect_cycles(location_data$val)
  
  # Create time series with detected frequency
  ts_data <- ts(location_data$val, 
                start = min(location_data$year), 
                frequency = optimal_freq)
  
  # Fit ETS model with automatic model selection
  tryCatch({
    ets_model <- ets(ts_data)
    forecast_result <- forecast(ets_model, h = h)
    
    # Add frequency information to the result
    forecast_result$frequency_used <- optimal_freq
    
    return(forecast_result)
  }, error = function(e) {
    # Fallback to simple exponential smoothing
    ts_simple <- ts(location_data$val, start = min(location_data$year), frequency = 1)
    return(ses(ts_simple, h = h))
  })
}

# Apply forecasting to all locations
forecast_results <- data %>%
  group_by(location_id, location_name) %>%
  nest() %>%
  mutate(
    forecast = map(data, forecast_location_with_cycles),
    # Extract forecast values
    forecast_values = map(forecast, function(x) {
      if (is.null(x)) return(tibble(year = integer(), forecast = numeric(), 
                                    lower_80 = numeric(), upper_80 = numeric(),
                                    lower_95 = numeric(), upper_95 = numeric()))
      tibble(
        year = 2022:2050,
        forecast = as.numeric(x$mean),
        lower_80 = as.numeric(x$lower[, "80%"]),
        upper_80 = as.numeric(x$upper[, "80%"]),
        lower_95 = as.numeric(x$lower[, "95%"]),
        upper_95 = as.numeric(x$upper[, "95%"])
      )
    })
  )

# Extract forecasts into a clean format
forecast_summary <- forecast_results %>%
  select(location_id, location_name, forecast_values) %>%
  unnest(forecast_values)

# Combine historical and forecast data
complete_data <- data %>%
  select(location_id, location_name, year, actual = val) %>%
  full_join(
    forecast_summary %>% select(location_id, location_name, year, forecast, lower_80, upper_80, lower_95, upper_95),
    by = c("location_id", "location_name", "year")
  ) %>%
  arrange(location_id, year)

write.csv(complete_data,"2050.csv",row.names = F)


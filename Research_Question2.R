

# Load required libraries
library(tidyverse)  
library(lubridate)  
library(forecast)   # 

#  Load the dataset
file_path <- "C:/Users/Ankit/OneDrive/Desktop/UIUC SEM/Fall 2024/Data Stats Info/Project/final.csv"
data <- read.csv(file_path)

# Convert the Date column to a proper date format
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Create Lagged Variables (e.g., GDP, REAINTRATREARAT10Y, etc.)
create_lags <- function(df, cols, lags) {
  for (col in cols) {
    for (lag in lags) {
      lagged_col_name <- paste0(col, "_lag", lag)
      df[[lagged_col_name]] <- lag(df[[col]], lag)
    }
  }
  return(df)
}

# Create lagged variables for the economic indicators
lagged_cols <- c("GDP", "REAINTRATREARAT10Y", "CPALTT01USM657N", "UNEMPLOY")
data <- create_lags(data, lagged_cols, c(1, 3, 6))  # Lag 1, Lag 3, and Lag 6

#  Define sectors and economic indicators
sectors <- c("Energy", "Financials", "HealthSector", "IT", "RealEstate")
indicators <- c("GDP", "REAINTRATREARAT10Y", "CPALTT01USM657N", "UNEMPLOY")

#  Cross-correlation analysis for each sector and economic indicator
for (sector in sectors) {
  for (indicator in indicators) {
    ccf(data[[indicator]], data[[sector]], lag.max = 6, 
        main = paste("Cross-Correlation:", indicator, "vs", sector))
  }
}

# Fit regression models for each sector using lagged GDP variables
fit_model <- function(sector, data) {
  formula <- as.formula(paste(sector, "~", 
                              paste(c("GDP_lag1", "GDP_lag3", "GDP_lag6",
                                      "REAINTRATREARAT10Y_lag1", "REAINTRATREARAT10Y_lag3", "REAINTRATREARAT10Y_lag6",
                                      "CPALTT01USM657N_lag1", "CPALTT01USM657N_lag3", "CPALTT01USM657N_lag6",
                                      "UNEMPLOY_lag1", "UNEMPLOY_lag3", "UNEMPLOY_lag6"), 
                                    collapse = " + ")))
  model <- lm(formula, data = data)
  return(model)
}

# Fit models for all sectors
models <- list()
for (sector in sectors) {
  models[[sector]] <- fit_model(sector, data)
  # Display summary for each model
  print(paste("Model Summary for", sector))
  print(summary(models[[sector]]))
}

# Calculate MSE (Mean Squared Error) for each sector model
calculate_mse <- function(model, sector, data) {
  predictions <- predict(model, data)
  actual <- data[[sector]]
  mse <- mean((actual - predictions)^2, na.rm = TRUE)
  return(mse)
}

# Compute MSE for all sector models
mse_results <- data.frame(Sector = sectors, MSE = NA)
for (i in 1:length(sectors)) {
  sector <- sectors[i]
  mse_results$MSE[i] <- calculate_mse(models[[sector]], sector, data)
}

# Print MSE results to evaluate model performance
print("MSE for all sector models:")
print(mse_results)

#  Find the best lag for each economic indicator (GDP, REAINTRATREARAT10Y, CPALTT01USM657N, UNEMPLOY)
find_best_lag <- function(model, indicator) {
  # Extract model summary
  model_summary <- summary(model)
  
  # Extract coefficients and p-values for lagged variables for the specified indicator
  coefficients <- model_summary$coefficients
  lag_vars <- grep(paste0(indicator, "_lag"), rownames(coefficients), value = TRUE)
  
  # Filter coefficients and p-values for the selected indicator's lagged variables
  lagged_indicators <- coefficients[lag_vars, ]
  
  # Select the lag with the smallest p-value
  best_lag <- rownames(lagged_indicators)[which.min(lagged_indicators[, "Pr(>|t|)"])]
  return(best_lag)
}

#  Analyze the best lag for each indicator (GDP, REAINTRATREARAT10Y, CPALTT01USM657N, UNEMPLOY) for each sector
best_lags_all_indicators <- data.frame(Sector = sectors, 
                                       GDP_Best_Lag = NA, 
                                       REAINTRATREARAT10Y_Best_Lag = NA,
                                       CPALTT01USM657N_Best_Lag = NA, 
                                       UNEMPLOY_Best_Lag = NA)

for (i in 1:length(sectors)) {
  sector <- sectors[i]
  best_lags_all_indicators$GDP_Best_Lag[i] <- find_best_lag(models[[sector]], "GDP")
  best_lags_all_indicators$REAINTRATREARAT10Y_Best_Lag[i] <- find_best_lag(models[[sector]], "REAINTRATREARAT10Y")
  best_lags_all_indicators$CPALTT01USM657N_Best_Lag[i] <- find_best_lag(models[[sector]], "CPALTT01USM657N")
  best_lags_all_indicators$UNEMPLOY_Best_Lag[i] <- find_best_lag(models[[sector]], "UNEMPLOY")
}

# Print the best lag for each economic indicator for each sector
print("Best lag for each sector based on each economic indicator:")
print(best_lags_all_indicators)

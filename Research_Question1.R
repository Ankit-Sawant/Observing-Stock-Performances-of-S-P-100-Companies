

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(broom)

# Read the data
data <- read.csv("C:/Users/Ankit/OneDrive/Desktop/UIUC SEM/Fall 2024/Data Stats Info/Project/final.csv")
head(data)
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Function to calculate Mean Squared Error (MSE)
calculate_mse <- function(model) {
  # Calculate the predicted values
  predicted <- predict(model)
  
  # Calculate the actual values
  actual <- model$model[[1]]
  
  # Calculate Mean Squared Error
  mse <- mean((actual - predicted)^2)
  
  return(mse)
}

# Sectors to analyze
sectors <- c("Energy", "Financials", "HealthSector", "IT", "RealEstate")

# Linear Regression Analysis
# Loop through each sector for simple linear regression
linear_models <- list()
simple_linear_mse <- list()

for (sector in sectors) {
  model <- lm(data[[sector]] ~ CPALTT01USM657N, data=data)
  linear_models[[sector]] <- summary(model)
  
  # Calculate and store MSE for simple linear regression
  simple_linear_mse[[sector]] <- calculate_mse(model)
  
  print(paste("Linear Regression Summary for", sector))
  print(linear_models[[sector]])
}

# Print Simple Linear Regression MSE
cat("\nSimple Linear Regression MSE:\n")
for (sector in sectors) {
  cat(paste0(sector, " MSE: ", round(simple_linear_mse[[sector]], 4), "\n"))
}

# Scatterplot for each sector with regression line
for (sector in sectors) {
  p <- ggplot(data, aes_string(x = "CPALTT01USM657N", y = sector)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(title = paste("CPI vs", sector, "Performance"),
         x = "CPI (Inflation)",
         y = paste(sector, "Performance")) +
    theme_minimal()
  
  print(p)
}

# High - Low Performance Classification
for (sector in sectors) {
  class_column <- paste0(sector, "_class")
  data[[class_column]] <- ifelse(data[[sector]] > median(data[[sector]], na.rm = TRUE), "high", "low")
}

print(colnames(data))

for (sector in sectors) {
  class_column <- paste0(sector, "_class")
  
  p <- ggplot(data, aes_string(x = class_column, y = "CPALTT01USM657N", fill = class_column)) +
    geom_boxplot() +
    labs(title = paste("CPI Distribution for High and Low Performance:", sector),
         x = "Performance Classification",
         y = "CPI (Inflation)") +
    theme_minimal() +
    scale_fill_manual(values = c("low" = "skyblue", "high" = "orange"))
  
  print(p)
}

# Multivariate Regression Analysis
multivariate_models <- list()
multivariate_mse <- list()

for (sector in sectors) {
  formula <- as.formula(paste0(sector, " ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP"))
  model <- lm(formula, data=data)
  multivariate_models[[sector]] <- summary(model)
  
  # Calculate and store MSE for multivariate regression
  multivariate_mse[[sector]] <- calculate_mse(model)
  
  print(paste("Multivariate Regression Summary for", sector))
  print(multivariate_models[[sector]])
}

# Print Multivariate Regression MSE
cat("\nMultivariate Regression MSE:\n")
for (sector in sectors) {
  cat(paste0(sector, " MSE: ", round(multivariate_mse[[sector]], 4), "\n"))
}

# Visualize MSE Comparison
simple_mse_df <- data.frame(
  Sector = names(simple_linear_mse),
  MSE = unlist(simple_linear_mse),
  Type = "Simple Linear"
)

multivariate_mse_df <- data.frame(
  Sector = names(multivariate_mse),
  MSE = unlist(multivariate_mse),
  Type = "Multivariate"
)

mse_comparison <- rbind(simple_mse_df, multivariate_mse_df)

# MSE Comparison Plot
mse_plot <- ggplot(mse_comparison, aes(x = Sector, y = MSE, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Squared Error Comparison",
       x = "Sector",
       y = "Mean Squared Error") +
  theme_minimal() +
  scale_fill_manual(values = c("Simple Linear" = "blue", "Multivariate" = "red"))

print(mse_plot)

# Reshape data to long format for sector performance plot
long_data <- data %>%
  pivot_longer(cols = all_of(sectors), names_to = "Sector", values_to = "Performance")

# Sector Performance Over Time Plot
p <- ggplot(long_data, aes(x = Date, y = Performance, color = Sector)) +
  geom_line() +
  labs(title = "Sector Performance Over Time",
       x = "Date",
       y = "Performance") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange"))

print(p)

# Calculate correlation matrix and create heatmap
numeric_data <- data %>%
  select(all_of(c(sectors, "CPALTT01USM657N", "REAINTRATREARAT10Y", "UNEMPLOY", "GDP")))

cor_matrix <- cor(numeric_data, use="complete.obs")
cor_data <- melt(cor_matrix)

correlation_heatmap <- ggplot(cor_data, aes(Var1, Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1, 1)) +
  labs(title="Correlation Matrix", x="", y="") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

print(correlation_heatmap)

# Coefficients visualization
coefficients_data <- data.frame()

for (sector in sectors) {
  model <- lm(as.formula(paste0(sector, " ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP")), data=data)
  tidy_model <- tidy(model)
  tidy_model$Sector <- sector
  coefficients_data <- rbind(coefficients_data, tidy_model)
}

coefficients_plot <- ggplot(coefficients_data, aes(x=term, y=estimate, fill=Sector)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Coefficients from Multivariate Regression Models",
       x="Predictor",
       y="Coefficient Estimate") +
  theme_minimal() +
  scale_fill_manual(values=c("red", "blue", "green", "purple", "orange"))

print(coefficients_plot)
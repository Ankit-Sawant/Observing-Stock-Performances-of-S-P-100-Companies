library(dplyr)
library(ggplot2)
library(broom)

# Read the data
data <- read.csv("C:/Users/Ankit/OneDrive/Desktop/UIUC SEM/Fall 2024/Data Stats Info/Project/final.csv")

# Convert Date to Date type
data$Date <- as.Date(data$Date)

# Filter data from 2019 to July 2020
data <- data %>%
  filter(Date >= as.Date("2019-12-31") & Date <= as.Date("2020-07-31"))

# Define sectors
sectors <- c("Energy", "Financials", "HealthSector", "IT", "RealEstate")

# Volatility Analysis
volatility <- data.frame(Sector = sectors, Volatility = numeric(length(sectors)))

for (i in 1:length(sectors)) {
  sector_returns <- diff(log(data[[sectors[i]]]))
  volatility$Volatility[i] <- sd(sector_returns, na.rm = TRUE)
}

# Plot volatility
ggplot(volatility, aes(x = Sector, y = Volatility)) +
  geom_bar(stat = "identity", fill = "pink", width = 0.5) +
  labs(title = "Volatility by Sector (Covid) ", y = "Standard Deviation of Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Magnitude Analysis
magnitude <- data.frame(Sector = sectors, Magnitude = numeric(length(sectors)))

for (i in 1:length(sectors)) {
  sector_changes <- diff(data[[sectors[i]]])
  magnitude$Magnitude[i] <- mean(abs(sector_changes), na.rm = TRUE)
}

# Plot magnitude
ggplot(magnitude, aes(x = Sector, y = Magnitude)) +
  geom_bar(stat = "identity", fill = "purple", width = 0.5) +
  labs(title = "Average Monthly Price Change (Covid)",
       y = "Average Absolute Monthly Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Linear regression models
model_energy <- lm(Energy ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_financials <- lm(Financials ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_health <- lm(HealthSector ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_tech <- lm(IT ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_realestate <- lm(RealEstate ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)

# Print model summaries
print(summary(model_energy))
print(summary(model_financials))
print(summary(model_health))
print(summary(model_tech))
print(summary(model_realestate))

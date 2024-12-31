library(dplyr)
library(ggplot2)

install.packages("broom")
library(broom)

data <- read.csv("C:/Users/Ankit/OneDrive/Desktop/UIUC SEM/Fall 2024/Data Stats Info/Project/final.csv")
data$Date <- as.Date(data$Date)

#Define sectors 
sectors <- c("Energy", "Financials", "HealthSector", "IT", "RealEstate")

# Volatility Analysis
volatility = data.frame(Sector = sectors, Volatility = numeric(length(sectors)))

for (i in 1:length(sectors)) {
  sector_returns = diff(log(data[[sectors[i]]]))
  volatility$Volatility[i] = sd(sector_returns, na.rm = TRUE)
}

# Plot volatility
ggplot(volatility, aes(x = Sector, y = Volatility)) +
  geom_bar(stat = "identity", fill = "skyblue", width=0.5) +
  labs(title = "Volatility by Sector", y = "Standard Deviation of Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Magnitude 
magnitude = data.frame(Sector = sectors, Magnitude = numeric(length(sectors)))
for (i in 1:length(sectors)) {
  sector_changes = diff(data[[sectors[i]]])
  magnitude$Magnitude[i] = mean(abs(sector_changes), na.rm = TRUE)
}

ggplot(magnitude, aes(x = Sector, y = Magnitude)) +
  geom_bar(stat = "identity", fill = "lightgreen", width = 0.5) +
  labs(title = "Average Monthly Price Change by Sector", 
       y = "Average Absolute Monthly Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Multivariate Regression Analysis
colnames(data)

# Models
model_energy <- lm(Energy ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_financials <- lm(Financials ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_health <- lm(HealthSector ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_tech<- lm(IT ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)
model_realestate <- lm(RealEstate ~ CPALTT01USM657N + REAINTRATREARAT10Y + UNEMPLOY + GDP, data = data)

#Summaries
print(summary(model_energy))
print(summary(model_financials))
print(summary(model_health))
print(summary(model_tech))
print(summary(model_realestate))

#Plots
par(mfrow = c(2, 3))

# Plot for Energy model: residuals vs predictors
plot(data$CPALTT01USM657N, residuals(model_energy), main = "Energy vs CPI Residuals", xlab = "CPI", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$REAINTRATREARAT10Y, residuals(model_energy), main = "Energy vs Interest Rate Residuals", xlab = "Interest Rate", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$UNEMPLOY, residuals(model_energy), main = "Energy vs Unemployment Residuals", xlab = "Unemployment", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$GDP, residuals(model_energy), main = "Energy vs GDP Residuals", xlab = "GDP", ylab = "Residuals")
abline(h = 0, col = "red")

# Plot for Financials model: residuals vs predictors
plot(data$CPALTT01USM657N, residuals(model_financials), main = "Financials vs CPI Residuals", xlab = "CPI", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(data$REAINTRATREARAT10Y, residuals(model_financials), main = "Financials vs Interest Rate Residuals", xlab = "Interest Rate", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(data$UNEMPLOY, residuals(model_financials), main = "Financials vs Unemployment Residuals", xlab = "Unemployment", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(data$GDP, residuals(model_financials), main = "Financials vs GDP Residuals", xlab = "GDP", ylab = "Residuals")
abline(h = 0, col = "blue")

# Plot for HealthSector model: residuals vs predictors
plot(data$CPALTT01USM657N, residuals(model_health), main = "HealthSector vs CPI Residuals", xlab = "CPI", ylab = "Residuals")
abline(h = 0, col = "green")

plot(data$REAINTRATREARAT10Y, residuals(model_health), main = "HealthSector vs Interest Rate Residuals", xlab = "Interest Rate", ylab = "Residuals")
abline(h = 0, col = "green")

plot(data$UNEMPLOY, residuals(model_health), main = "HealthSector vs Unemployment Residuals", xlab = "Unemployment", ylab = "Residuals")
abline(h = 0, col = "green")

plot(data$GDP, residuals(model_health), main = "HealthSector vs GDP Residuals", xlab = "GDP", ylab = "Residuals")
abline(h = 0, col = "green")

# Plot for IT model: residuals vs predictors
plot(data$CPALTT01USM657N, residuals(model_tech), main = "IT vs CPI Residuals", xlab = "CPI", ylab = "Residuals")
abline(h = 0, col = "purple")

plot(data$REAINTRATREARAT10Y, residuals(model_tech), main = "IT vs Interest Rate Residuals", xlab = "Interest Rate", ylab = "Residuals")
abline(h = 0, col = "purple")

plot(data$UNEMPLOY, residuals(model_tech), main = "IT vs Unemployment Residuals", xlab = "Unemployment", ylab = "Residuals")
abline(h = 0, col = "purple")

plot(data$GDP, residuals(model_tech), main = "IT vs GDP Residuals", xlab = "GDP", ylab = "Residuals")
abline(h = 0, col = "purple")

# Plot for RealEstate model: residuals vs predictors
plot(data$CPALTT01USM657N, residuals(model_realestate), main = "RealEstate vs CPI Residuals", xlab = "CPI", ylab = "Residuals")
abline(h = 0, col = "orange")

plot(data$REAINTRATREARAT10Y, residuals(model_realestate), main = "RealEstate vs Interest Rate Residuals", xlab = "Interest Rate", ylab = "Residuals")
abline(h = 0, col = "orange")

plot(data$UNEMPLOY, residuals(model_realestate), main = "RealEstate vs Unemployment Residuals", xlab = "Unemployment", ylab = "Residuals")
abline(h = 0, col = "orange")

plot(data$GDP, residuals(model_realestate), main = "RealEstate vs GDP Residuals", xlab = "GDP", ylab = "Residuals")
abline(h = 0, col = "orange")

par(mfrow = c(1, 1))



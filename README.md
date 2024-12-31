# Observing Stock Performances of S&P 100 Companies, Sector Reactions, and Relations to Economic Indicators
# Overview
This project explores the relationship between key economic indicators and stock market performance across five critical sectors of the S&P 100 index: Energy, Real Estate, Healthcare, Information Technology (IT), and Financials. The analysis addresses the impact of inflation, lag times between economic changes and stock performance, and the volatility and magnitude of sector-specific reactions during major economic events.

# Research Questions
1.Effect of Inflation: How does inflation (measured by CPI) impact stock performance in selected sectors?
2.Lag Times: What are the delays between changes in economic indicators and sectoral stock performance over the last decade?
3.Volatility and Magnitude: How do different sectors react in terms of volatility and magnitude during significant economic shifts?

# Data Sources
1.S&P 100 Sector Data: Extracted from S&P Global Indices.
2.Economic Indicators: Sourced from Kaggle's "Financial Indicators of US Recession" dataset and other reliable sources (October 2014 to February 2023).

# Methodology
1.Data Preparation
a.Cleaning: Addressed missing values and ensured consistency in data formats.
b.Aggregation: Resampled daily data to monthly frequency.
c.Merging: Combined sector performance with economic indicators for time-series analysis.
2.Analytical Techniques
1.Regression Modeling: Multivariate regression to assess combined effects of indicators.
2.Cross-Correlation Analysis: Evaluated lag relationships between indicators and stock performance.
3.Volatility and Magnitude Analysis: Calculated standard deviations and absolute changes in stock returns.
# Key Findings
Research Question 1: Effect of Inflation
The Financials sector exhibited high sensitivity to inflation.
The Energy sector showed minimal responsiveness to inflation.
Research Question 2: Lag Times
GDP Lag (1 month): Strongest predictor across sectors.
Interest rates significantly affected Energy and Real Estate sectors.
Unemployment heavily influenced IT and Healthcare sectors.
Research Question 3: Volatility and Magnitude
During the early months of COVID-19 (Dec 2019 - Jul 2020):
Real Estate was the most stable sector.
IT and Healthcare sectors remained relatively stable despite significant economic disruption.
# Results Visualization
Key visualizations and model summaries are provided in the project for:
Relationships between CPI and sector performance.
Volatility and magnitude across time.
Regression results for economic shifts (e.g., early COVID-19 period).


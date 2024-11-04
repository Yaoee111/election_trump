#### Preamble ####
# Purpose: Test real data for Donald Trump.
# Author: Yiyi Yao
# Date: 01 November 2024
# Contact: ee.yao@mail.utoronto.ca
# License: MIT




# Load necessary libraries
library(tidyverse)
library(broom)

# Load the real data file
# Replace the path with the actual location if necessary
real_data <- read_csv("path/to/your/trump.csv")

# Ensure the date column is in Date format
real_data <- real_data %>%
  mutate(end_date = as.Date(end_date, format = "%Y-%m-%d"))

# Fit a linear regression model to test for a trend over time
# Here, we model Trump's support (pct) over time (end_date)
model_real <- lm(pct ~ end_date, data = real_data)

# Summarize model results to examine the trend
model_summary_real <- summary(model_real)
print(model_summary_real)

# Display model coefficients and check for a significant trend
tidy(model_real)

# Plot the trend in the real data with model fit
ggplot(real_data, aes(x = end_date, y = pct)) +
  geom_line(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Testing Support Rate Trend in Real Data",
       x = "Date",
       y = "Support Rate (%)") +
  theme_minimal()

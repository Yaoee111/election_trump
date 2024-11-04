#### Preamble ####
# Purpose: Test simulate data for Donald Trump.
# Author: Yiyi Yao
# Date: 01 November 2024
# Contact: ee.yao@mail.utoronto.ca
# License: MIT



# Load necessary libraries
library(tidyverse)
library(broom)

# Fit a linear regression model to the simulated data
# We want to see if the support rate increases with time (date)
model <- lm(support_rate ~ date, data = simulated_data)

# Summarize model results
model_summary <- summary(model)
model_summary

# Display model coefficients and check for a significant trend
tidy(model)

# Plot the model fit over the simulated data
ggplot(simulated_data, aes(x = date, y = support_rate)) +
  geom_line(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Testing Simulated Support Rate Trend",
       x = "Date",
       y = "Support Rate (%)") +
  theme_minimal()

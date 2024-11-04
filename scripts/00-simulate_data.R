#### Preamble ####
# Purpose: Simulate data for Donald Trump.
# Author: Yiyi Yao
# Date: 01 November 2024
# Contact: ee.yao@mail.utoronto.ca
# License: MIT
# Pre-requisites: none


#### Workspace setup ####
library(tidyverse)


#### Simulate Data ####
set.seed(123)  # For reproducibility
n_days <- 365  # Number of days to simulate (1 year)
initial_support <- 45  # Starting support rate in percentage
daily_trend <- 0.02  # Average daily increase or decrease in support percentage
sd_noise <- 1  # Standard deviation of random fluctuations (noise)

# Create a data frame to store the simulation
simulated_data <- tibble(
  date = seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = n_days),
  support_rate = initial_support + (1:n_days * daily_trend) + rnorm(n_days, mean = 0, sd = sd_noise)
)

# Ensure support rate is within bounds (0% to 100%)
simulated_data <- simulated_data %>%
  mutate(support_rate = pmin(pmax(support_rate, 0), 100))

# Plot the simulated support rate over time
ggplot(simulated_data, aes(x = date, y = support_rate)) +
  geom_line(color = "blue") +
  labs(title = "Simulated Trump Support Rate Over Time",
       x = "Date",
       y = "Support Rate (%)") +
  theme_minimal()


#### Save data ####
write_parquet(simulated_data, "data/01-simulated_data/simulation.parquet")


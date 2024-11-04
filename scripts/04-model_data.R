#### Preamble ####
# Purpose: Model data for Donald Trump.
# Author: Yiyi Yao
# Date: 01 November 2024
# Contact: ee.yao@mail.utoronto.ca
# License: MIT


# Load necessary libraries
library(tidyverse)
library(lubridate)
library(rstanarm)

# Load the dataset
trump_data <- read_parquet("data/03-analysis_data/trump.parquet") %>%
  mutate(end_date = as.Date(end_date, format="%Y-%m-%d"))

# Model 1: pct as a function of end_date
model_date <- lm(pct ~ end_date, data = trump_data)

# Model 2: pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = trump_data)

# Augment data with model predictions
trump_data <- trump_data %>%
  mutate(
    fitted_date = predict(model_date),
    fitted_date_pollster = predict(model_date_pollster)
  )

# Plot model predictions

# Plot for Model 1
#| echo: false
#| warning: false
#| message: false
#| label: fig-trump-model1
#| fig-cap: Linear Model - Trump's Support Percentage as a Function of Date

ggplot(trump_data, aes(x = end_date, y = pct)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Trump's Percent", x = "Date", title = "Linear Model: pct ~ end_date")

# Plot for Model 2
#| echo: false
#| warning: false
#| message: false
#| label: fig-trump-model2
#| fig-cap: Linear Model - Trump's Support Percentage as a Function of Date and Pollster

ggplot(trump_data, aes(x = end_date, y = pct)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date_pollster), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Trump's Percent", x = "Date", title = "Linear Model: pct ~ end_date + pollster") +
  facet_wrap(vars(pollster))

# Save models if needed
saveRDS(model_date, file = "models/model_date_trump.rds")
saveRDS(model_date_pollster, file = "models/model_date_pollster_trump.rds")

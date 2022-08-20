suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(broom))

# Prepare the data for the models -----------------------------------------

# read the latest augmented monkeypox data
mp_df <- readRDS("data/proc/monkeypox-confirmed-augmented.rds") %>%
  filter(epiweek >= 20 & complete_epiweek) %>%
  group_by(
    Country,
    Country_ISO3,
    Date_confirmation,
    PopTotal
  ) %>%
  tally(name = "n_cases") %>%
  arrange(
    Country,
    Date_confirmation
  ) %>%
  group_by(
    Country,
    Country_ISO3,
    PopTotal
  ) %>%
  mutate(
    acc_cases = cumsum(n_cases)
  )

# select countries where the latest cumulative number cases >= 200
selected_countries <- mp_df %>%
  group_by(Country) %>%
  filter(acc_cases == max(acc_cases)) %>% # latest value for cummulative cases
  filter(acc_cases >= 200) %>%  # cumulative cases >= 200
  ungroup() %>%
  pull(Country_ISO3)

# - filter off dates where the cumulative number of cases >= 10
# - count the number of days since the earliest confirmation date for each country
mp_models_df <- mp_df %>%
  ungroup() %>%
  filter(Country_ISO3 %in% selected_countries) %>%
  filter(acc_cases >= 10) %>%
  select(
    Country,
    Date_confirmation,
    acc_cases
  ) %>%
  group_by(
    Country
  ) %>%
  mutate(
    days = as.numeric(Date_confirmation - min(Date_confirmation))
  )

# save this data as RDS and CSV
saveRDS(
  mp_models_df,
  "data/proc/monkeypox-selected-countries-model-data.rds"
)
write_csv(
  mp_models_df,
  "data/proc/monkeypox-selected-countries-model-data.csv"
)

# Generate de models and statistics ---------------------------------------

# Using a simple exponential growth equation: y = C*exp^(B*x)
# and converting to log: log(y) = log(C) + C*x = A + B*x [where: A = log(C)]

# 1. get the models parameters
mp_models_params <- mp_models_df %>%
  group_by(Country) %>%
  do(
    tidy(lm(log(acc_cases) ~ days, data = .))
  ) %>%
  rename(
    statistic_term = statistic,
    pvalue_term = p.value
  )

# 2. get the model statistics
mp_models_stats <- mp_models_df %>%
  group_by(Country) %>%
  do(
    glance(lm(log(acc_cases) ~ days, data = .))
  )

# 3. combine them into a dataframe
mp_models_results <- left_join(
  mp_models_params,
  mp_models_stats,
  by = "Country"
)

# save model results as RDS and CSV

saveRDS(
  mp_models_results,
  "data/proc/monkeypox-selected-countries-model-results.rds"
)

write_csv(
  mp_models_results,
  "data/proc/monkeypox-selected-countries-model-results.csv"
)

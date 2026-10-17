library(tidyquant)
library(tidyverse)

#Returns a Tibble
# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"   ~ "Oregon",
      symbol == "CAUR"   ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR"   ~ "New York"
    )
  )
# Replace October 2025 data
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Add column unemploymen_rate_change
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

# Tibble restricted from 2015 onwards
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

# Mean Minimum and Maximums of unemployment_rate for unemp and unemp_recent
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )

# Largest Increase and Decrease Month over Month for unemployment rate change.
unemp_top_increases <- unemp |>
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases <- unemp |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)


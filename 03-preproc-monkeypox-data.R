suppressPackageStartupMessages(library(tidyverse))

# data source: https://github.com/globaldothealth/monkeypox

# Monkeypox data ----------------------------------------------------------

gh_url <- "https://github.com/globaldothealth/monkeypox/raw/main/latest.csv"
gh_fn <- "data/orig/global_health_latest.csv"
# always get the latest data file
download.file(url = gh_url, destfile = gh_fn)

mp_raw <- read_csv(
  gh_fn,
  col_types = cols(
    .default = col_character(),
    Date_onset = col_date(format = ""),
    Date_confirmation = col_character(), # parse using lubridate
    Date_hospitalisation = col_date(format = ""),
    Date_isolation = col_date(format = ""),
    Date_entry = col_date(format = ""),
    Date_last_modified = col_date(format = "")
  )
) %>%
  mutate(
    # lubridate is more resilient to malformed dates
    Date_confirmation = lubridate::ymd(Date_confirmation),
    epiweek = lubridate::epiweek(Date_confirmation)
  )

saveRDS(
  mp_raw,
  "data/proc/monkeypox-data-raw.rds"
)

# combine with the UN WPP data
country_region_continent <- readRDS("data/proc/un-wpp-countries-2022.rds") %>%
  select(
    Country = Location,
    Country_ISO3 = ISO3_code,
    Continent,
    Region,
    PopTotal,
    PopDensity
  ) %>%
  group_by(Continent, Region) %>%
  mutate(
    RegionPopTotal = sum(PopTotal, na.rm = TRUE)
  ) %>%
  group_by(Continent) %>%
  mutate(
    ContinentPopTotal = sum(PopTotal, na.rm = TRUE)
  ) %>%
  distinct()

# Augment data
mp_confirmed_augmented <- mp_raw %>%
  filter(Status == "confirmed") %>%
  mutate(
    first_day_of_epi_week = lubridate::floor_date(Date_confirmation,
                                                  "weeks",
                                                  week_start = 7), # first dow
    last_day_of_epi_week = first_day_of_epi_week + 6, # last dow
    current_epiweek = lubridate::epiweek(Sys.Date()),
    complete_epiweek = (
      (current_epiweek > epiweek) &
        Date_confirmation <= last_day_of_epi_week
    )
  ) %>%
  select(-current_epiweek,
         -first_day_of_epi_week,
         -last_day_of_epi_week
  ) %>%
  left_join(
    country_region_continent %>% select(-Country),
    by = "Country_ISO3"
  )

saveRDS(
  mp_confirmed_augmented,
  "data/proc/monkeypox-confirmed-augmented.rds"
)

write_csv(
  mp_confirmed_augmented,
  "data/proc/monkeypox-confirmed-augmented.csv"
)

# get the date ranges for the confirmations for each country

country_confdates <- mp_confirmed_augmented %>%
  group_by(Continent, Country, Country_ISO3) %>%
  summarise(
    start_date = min(Date_confirmation, na.rm = TRUE),
    end_date = max(Date_confirmation, na.rm = TRUE)
  )

write_csv(
  country_confdates,
  "data/proc/country-confirmation-date-range.csv"
)

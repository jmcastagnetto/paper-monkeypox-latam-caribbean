library(tidyverse)

# data source: https://population.un.org/wpp/Download/Standard/CSV/

# Population using UN's WPP -----------------------------------------------
un_wpp_url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip"
un_wpp_fn <- "data/orig/WPP2022_TotalPopulationBySex.zip"

if (!file.exists(un_wpp_fn)) {
  download.file(url = un_wpp_url, destfile = un_wpp_fn)
}

un_pop_raw <- read_csv(
  unz(
    un_wpp_fn,
    "WPP2022_TotalPopulationBySex.csv"
  ),
  col_types = cols(
    SortOrder = col_integer(),
    LocID = col_character(),
    Notes = col_character(),
    ISO3_code = col_character(),
    ISO2_code = col_character(),
    SDMX_code = col_integer(),
    LocTypeID = col_skip(),
    LocTypeName = col_character(),
    ParentID = col_character(),
    Location = col_character(),
    VarID = col_integer(),
    Variant = col_character(),
    Time = col_integer(),
    MidPeriod = col_double(),
    PopMale = col_double(),
    PopFemale = col_double(),
    PopTotal = col_double(),
    PopDensity = col_double()
  )
)

regions <- un_pop_raw %>%
  filter(is.na(ISO3_code)) %>%
  select(
    LocID,
    LocTypeName,
    Location
  ) %>%
  distinct() %>%
  add_row( # add missing subregion
    LocID = "918",
    LocTypeName = "Subregion",
    Location = "Northern America"
  ) %>%
  rename(
    RegionType = LocTypeName,
    Region = Location
  ) %>%
  mutate( # map subregions to "Continents"
    Continent = if_else(
      RegionType == "Subregion",
      case_when(
        str_detect(Region, "Asia") ~ "Asia",
        str_detect(Region, "Africa") ~ "Africa",
        str_detect(Region, "(South America|Central America|Caribbean)") ~ "Latin America and the Caribbean",
        Region == "Northern America" ~ "Northern America",
        str_detect(Region, "Europe") ~ "Europe",
        str_detect(Region, "(Micronesia|Polynesia|Melanesia|Australia)") ~ "Oceania"
      ),
      NA_character_
    )
  )

# select population for 2022, under the Medium variant UN WPP model
countries_2022 <- un_pop_raw %>%
  filter(
    !is.na(ISO3_code) &
      Time == 2022 &
      Variant == "Medium"
  ) %>%
  select(
    ISO3_code,
    ISO2_code,
    Location,
    ParentID,
    Time,
    PopTotal,
    PopDensity
  ) %>%
  left_join(
    regions,
    by = c("ParentID" = "LocID")
  ) %>%
  mutate(
    PopTotal = 1000 * PopTotal
  ) %>%
  distinct()

saveRDS(
  un_pop_raw,
  "data/proc/un-wpp-raw.rds"
)

saveRDS(
  regions,
  "data/proc/un-wpp-regions.rds"
)

saveRDS(
  countries_2022,
  "data/proc/un-wpp-countries-2022.rds"
)


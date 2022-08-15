library(tidyverse)
library(readxl)

# data source: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

wb_url <- "http://databank.worldbank.org/data/download/site-content/CLASS.xlsx"
wb_fn <- "data/orig/country-by-income-CLASS.xlsx"

if (!file.exists(wb_fn)) {
  download.file(url = wb_url, destfile = wb_fn
  )
}

country_raw <- read_excel(
  wb_fn,
  sheet = 1
) %>%
  filter(!is.na(Region))

groups_raw <- read_excel(
  wb_fn
  sheet = 2
)

saveRDS(country_raw, "data/proc/wb-countries-income.rds")
saveRDS(groups_raw, "data/proc/wb-groups.rds")

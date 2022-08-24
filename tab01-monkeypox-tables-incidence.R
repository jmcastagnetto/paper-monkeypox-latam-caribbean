suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gt))


# When the original data was last downloaded ------------------------------

data_info <- file.info("data/orig/global_health_latest.csv")
date_last_download <- as.Date(data_info$ctime)

# Prepare data for the table ----------------------------------------------

wb_countries <- readRDS("data/proc/wb-countries-income.rds") %>%
  select(
    Country_ISO3 = Code,
    Income_group = `Income group`
  ) %>%
  distinct()

mp_df <- readRDS("data/proc/monkeypox-confirmed-augmented.rds") %>%
  filter(complete_epiweek)
most_recent_complete_epiweek <- max(mp_df$epiweek, na.rm = TRUE)

build_table <- function(tbl_df) {
  tbl_df%>%
    gt(rowname_col = "Country") %>%
    cols_label(
      ini_date = "Date of First Confirmed Case",
      n_cases = "Confirmed Cases",
      incidence = "Cummulative Incidence (per million)",
      Income_group = "Income Group"
    ) %>%
    sub_missing() %>%
    fmt_number(
      columns = c(incidence),
      decimals = 2
    ) %>%
    fmt_integer(
      columns = n_cases,
      use_seps = TRUE
    ) %>%
    cols_align(
      columns = Income_group,
      align = "right"
    ) %>%
    cols_align(
      columns = ini_date,
      align = "center"
    ) %>%
    tab_options(
      column_labels.font.weight = "bold"
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = list(
        cell_text(align = "center", v_align = "middle")
      )
    ) %>%
    tab_style(
      style = "padding-left:15px;padding-right:15px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      locations = cells_row_groups(),
      style = list(
        cell_text(style = "italic", weight = "bold")
      )
    ) %>%
    tab_footnote(
      locations = cells_column_labels(
        columns = c(n_cases, incidence)
      ),
      footnote = paste0("As of complete epidemiological week #", most_recent_complete_epiweek)
    ) %>%
    opt_row_striping()
}

tbl_df <- mp_df %>%
  left_join(
    wb_countries,
    by = "Country_ISO3"
  ) %>%
  group_by(
    Country,
    Income_group,
    Continent,
    Region,
    PopTotal,
    PopDensity
  ) %>%
  summarise(
    ini_date = min(Date_confirmation, na.rm = TRUE),
    n_cases = n()
  ) %>%
  mutate(
    incidence = 1e6 * n_cases / PopTotal
  ) %>%
  arrange(Continent, Region, desc(incidence)) %>%
  ungroup()

# Latam incidence table ---------------------------------------------------

latam_incid_tbl <- build_table(
  tbl_df %>%
    filter(Continent == "Latin America and the Caribbean") %>%
    select(-PopTotal, -Continent, -PopDensity) %>%
    relocate(
      Income_group,
      .after = last_col()
    ) %>%
    arrange(Region, desc(incidence)) %>%
    group_by(Region)
) %>%
  tab_footnote(
    locations = cells_body(
      columns = Income_group,
      rows = "Venezuela"
    ),
    footnote = "Venezuela has been temporarily unclassified as of July 2021 by the World Bank"
  ) %>%
  tab_source_note(
    source_note = glue::glue("Data sources: Global.health Monkeypox (accessed on {date_last_downloaded}), UN 2022 Revision of World Population Prospects, World Bank Income Classification (FY 2023)")
  ) %>%
  tab_header(
    title = "Monkeypox in Latinamerica and the Caribbean: Cummulative incidence per country"
  )

gtsave(
  latam_incid_tbl,
  filename = "output/tab01-monkeypox-incidence-latamcarib.docx"
)

gtsave(
  latam_incid_tbl,
  filename = "output/tab01-monkeypox-incidence-latamcarib.html"
)

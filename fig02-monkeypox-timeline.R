# timeline
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggh4x))

mp <- readRDS("data/proc/monkeypox-confirmed-augmented.rds")


# ref: https://www.who.int/emergencies/disease-outbreak-news/item/2022-DON385
endemic_countries <- c(
  "BEN", # Benin
  "CMR", # Cameroon
  "CAF", # Central African Republic
  "COD", # Democratic Republic of the Congo
  "GAB", # Gabon
  "GHA", # Ghana (identified in animals only)
  "CIV", # Ivory Coast
  "LBR", # Liberia
  "NGA", # Nigeria
  "COG", # Republic of the Congo
  "SLE", # Sierra Leone
  "SSD" # South Sudan
)

# non endemic countries
mp_non_endemic <- mp %>%
  filter(!Country_ISO3 %in% endemic_countries)

world_1st_confirmed <- min(mp_non_endemic$Date_confirmation, na.rm = TRUE)
location_1st_confirmed <- mp_non_endemic %>%
    filter(Date_confirmation == world_1st_confirmed)

per_continent_1st_confirmed <- mp_non_endemic %>%
  arrange(Continent, Date_confirmation) %>%
  group_by(Continent) %>%
  summarise(
    first_report = min(Date_confirmation, na.rm = TRUE)
  ) %>%
  mutate(
    tmp = paste(Continent, first_report)
  )

df_1st_confirmed <- mp_non_endemic %>%
  mutate(
    tmp = paste(Continent, Date_confirmation)
  ) %>%
  filter(
    tmp %in% per_continent_1st_confirmed$tmp
  ) %>%
  group_by(
    Continent,
    Country,
    Country_ISO3,
    Date_confirmation
  ) %>%
  tally(name = "first_cases") %>%
  rename(
    event_date = Date_confirmation
  ) %>%
  mutate(
    Continent = str_replace_all(
      Continent,
      c(
        "Latin America and the Caribbean" = "LATAM & Caribbean"
      )
    )
  ) %>%
  arrange(event_date) %>%
  mutate(
    event = glue::glue("First confirmed case in {Continent} ({Country}, n={first_cases})"),
    type = "first"
  )

mp_events <- df_1st_confirmed %>%
  ungroup() %>%
  select(event, event_date, type) %>%
  bind_rows(
    tribble(
      ~event, ~event_date, ~type,
      # https://www.who.int/emergencies/disease-outbreak-news/item/2022-DON385
      "WHO report on multi-country outbreak", "2022-05-22", "who",
      # https://www.who.int/europe/news/item/23-07-2022-who-director-general-declares-the-ongoing-monkeypox-outbreak-a-public-health-event-of-international-concern
      "WHO declares Monkeypox a Public Health Emergency of International Concern (PHEIC)", "2022-07-23", "who",
      # https://www.bbc.com/news/health-62350022 - first deaths
      # https://www.washingtonpost.com/world/2022/07/30/monkeypox-deaths-brazil-spain/
      "First death in Europe (Spain)", "2022-07-29", "death",
      "First death in LATAM & Caribbean (Brazil)", "2022-07-29", "death",
      # https://www.bbc.com/news/world-asia-india-62344928
      # https://www.anews.com.tr/world/2022/08/01/ghana-confirms-first-monkeypox-related-death
      "First death in Africa (Ghana)", "2022-08-01", "death",
      "First death in Asia (India)", "2022-07-30", "death"
    ) %>%
      mutate(
        event_date = as.Date(event_date)
      )
  )

# timeline label positions
positions <- c(
  .25,
  .5,
  .75,
  1,
  .15,
  .3,
  .65,
  .15,
  .3,
  .6,
  .75,
  1
)

plot_df <- mp_events %>%
  arrange(event_date) %>%
  mutate(
    position = positions,
    event = str_wrap(event, 25)
  )

mp_plot <- ggplot(
  plot_df,
  aes(x = event_date, y = 0, label = event, color = type)
) +
  geom_hline(yintercept = 0, size = 1,
             linetype = "dotted", color = "grey30") +
  geom_point(size = 5, shape = 18) +
  geom_segment(aes(xend = event_date, y = 0.015, yend = position),
               arrow = arrow(
                 length = unit(0.2, "cm"),
                 ends = "first", type = "closed"
               )) +
  geom_label(
    aes(y = position), hjust = .5, size = 5.5,
    family = "Roboto",
    fill = "lightyellow",
    label.padding = unit(.5, "lines")
  ) +
  scale_color_manual(
    values = c(
      "first" = "black",
      "who" = "blue",
      "death" = "darkred"
    )
  ) +
  scale_y_continuous(limits = c(0, 1.1)) +
  scale_x_date(
    date_breaks = "week",
    date_labels = "%b %d",
    guide = "axis_minor",
    minor_breaks = seq(
      as.Date("2022-04-25"),
      as.Date("2022-08-08"),
      by = "day"
    ),
    limits = c(
      as.Date("2022-04-25"),
      as.Date("2022-08-08")
    )
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(rep(1, 4), "cm"),
    axis.ticks.x = element_line(color = "grey30"),
    axis.ticks.length.x = unit(.5, "cm"),
    axis.text.x = element_text(color = "grey30", family = "Inconsolata"),
    plot.background = element_rect(fill = "white", color = "white"),
    ggh4x.axis.ticks.length.minor = rel(0.4)
  )

#mp_plot
ggsave(
  plot = mp_plot,
  filename = "output/fig02-mp-timeline.tiff",
  width = 16,
  height = 9,
  dpi = 300
)

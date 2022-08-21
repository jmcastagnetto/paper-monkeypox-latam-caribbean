suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(rnaturalearth))
suppressPackageStartupMessages(library(rnaturalearthdata))

lac_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_wb == "Latin America & Caribbean")

lac_iso3 <- as.character(lac_map$iso_a3)

mp_df <- readRDS("data/proc/monkeypox-confirmed-augmented.rds") %>%
  filter(
    epiweek >= 20 &                 # select from epiweek #20
      complete_epiweek &            # only complete epiweeks
      Country_ISO3 %in% lac_iso3    # latam & caribbean countries
  ) %>%
  arrange(
    Country_ISO3,
    PopTotal,
    epiweek
  ) %>%
  group_by(
    Country_ISO3,
    PopTotal,
    epiweek
  ) %>%
  tally(name = "n_cases") %>%
  mutate(
    n_accum = cumsum(n_cases),
    incid = 1e6 * n_accum / PopTotal
  ) %>%
  ungroup() %>%
  select(-PopTotal, -n_cases, -n_accum)

# complete the other countries
mp_df <- mp_df %>%
  add_row(
    Country_ISO3 = setdiff(lac_iso3, mp_df$Country_ISO3),
    epiweek = NA_integer_,
    incid = NA_real_
  )

min_epiweek <- min(mp_df$epiweek, na.rm = TRUE)
max_epiweek <- max(mp_df$epiweek, na.rm = TRUE)

plot_df <- mp_df %>%
  group_by(Country_ISO3) %>%
  complete( # fill epiweek series
    epiweek = seq(min_epiweek, max_epiweek, by = 1)
  ) %>%
  fill(incid) %>%
  ungroup() %>%
  filter(!is.na(epiweek)) %>%
  mutate(
    lbl_epiweek = paste0("Week: ", epiweek)
  )

# select data from every other epiweek
every_other_epiweek <- unique(plot_df$epiweek)[c(TRUE, FALSE)]

lac_map_incid <- lac_map %>%
  left_join(
    plot_df %>% filter(epiweek %in% every_other_epiweek),
    by = c("iso_a3" = "Country_ISO3")
  ) %>%
  filter(!is.na(epiweek))

lac_plot <- ggplot(lac_map_incid) +
  geom_sf(aes(fill = incid), size = 0.2) +
  scale_fill_viridis_b(
    direction = -1,
    n.breaks = 10,
    na.value = "white",
    show.limits = TRUE
  ) +
  labs(
    fill = "Incidence (per million) ",
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = unit(rep(0.5, 4), "cm"),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "grey80"),
    strip.text = element_text(size = 14, face = "bold", color = "black")
  ) +
  facet_wrap(~lbl_epiweek, nrow = 2)

ggsave(
  plot = lac_plot,
  filename = "output/fig02-map-evolution-incidence-latamcarib.tiff",
  dpi = 300,
  width = 16,
  height = 9
)

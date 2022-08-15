library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

mp_raw <- readRDS("data/proc/monkeypox-confirmed-augmented.rds")

last_update <- max(mp_raw %>% filter(complete_epiweek) %>% pull(epiweek))

mp_df <- mp_raw %>%
  filter(epiweek >= 20 & complete_epiweek) %>%
  arrange(
    Country,
    Country_ISO3,
    PopTotal,
    Date_confirmation
  ) %>%
  group_by(
    Country,
    Country_ISO3,
    PopTotal,
    Date_confirmation
  ) %>%
  tally(name = "n_cases") %>%
  mutate(
    n_accum = cumsum(n_cases),
    incid = 1e6 * n_accum / PopTotal
  ) %>%
  ungroup() %>%
  select(-PopTotal, -n_cases, -n_accum) %>%
  arrange(Country, desc(Date_confirmation)) %>%
  group_by(Country) %>%
  top_n(1, Date_confirmation)

# large version, to capture Gibraltar and other small places/countries
world <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_transform(crs = "+proj=comill")

world_incidence_current <- world %>%
  filter(sov_a3 != "ATA") %>%
  left_join(
    mp_df,
    by = c("iso_a3" = "Country_ISO3")
  )

world_plot <- ggplot(world_incidence_current) +
  geom_sf(aes(fill = incid), size = 0.2) +
  scale_fill_viridis_b(
    direction = -1,
    n.breaks = 10,
    na.value = "white",
    show.limits = TRUE
  ) +
  labs(
    fill = "Incidence (per million) ",
    caption = paste0("Map projection: Compact Miller (without Anctartica)\nData up to complete epidemiological week #", last_update)
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = unit(rep(0.5, 4), "cm"),
    legend.key.width = unit(3, "cm"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    plot.caption = element_text(color = "grey50", size = 10),
    axis.text = element_blank()
  )
#world_plot
ggsave(
  plot = world_plot,
  filename = "output/fig01-map-current-incidence-country.tiff",
  dpi = 300,
  width = 14,
  height = 10
)

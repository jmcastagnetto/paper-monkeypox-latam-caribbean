library(tidyverse)

mp_df <- readRDS("data/proc/monkeypox-confirmed-augmented.rds")

mp_region_continent <- mp_df %>%
  filter(epiweek >= 20) %>% # start from epi week #20
  mutate(
    monday_of_week = lubridate::floor_date(
      Date_confirmation,
      "week",
      week_start = 7
    ) + 1
  ) %>%
  arrange(
    Continent,
    Region,
    monday_of_week
  ) %>%
  group_by(
    Continent,
    Region,
    RegionPopTotal,
    monday_of_week
  ) %>%
  tally(name = "n_cases") %>%
  mutate(
    n_accum = cumsum(n_cases),
    incid = 1e6 * n_accum / RegionPopTotal
  )

# get last entry to position the sub-region labels
last_entry <- mp_region_continent %>%
  arrange(Continent, Region, desc(monday_of_week)) %>%
  group_by(Continent, Region) %>%
  top_n(1, monday_of_week)

min_date <- min(mp_region_continent$monday_of_week)
max_date <- max(mp_region_continent$monday_of_week)

set.seed(123)
incid_plot <- ggplot(
  mp_region_continent,
  aes(x = monday_of_week, y = incid, group = Region, color = Region)
) +
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(data = last_entry,
             size = 5,
             show.legend = FALSE) +
  ggrepel::geom_label_repel(
    data = last_entry,
    aes(x = monday_of_week, y = incid, label = Region),
    size = 7,
    hjust = 0,
    nudge_x = 7,
    direction = "y",
    label.size = 0,
    show.legend = FALSE,
    fill = rgb(1, 1, 1, 0.4),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    max.overlaps = 30
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(NA, 100)
  ) +
  annotation_logticks(sides = "l") +
  scale_x_date(
    date_labels = "W%V",
    date_breaks = "week",
    expand = expansion(0, c(7, 21)),
    limits = c(min_date, max_date)
  ) +
  scale_color_manual(
    values = paletteer::paletteer_d("Polychrome::dark")
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_line(color = "grey80", linetype = "dashed"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    plot.margin = unit(rep(1, 4), "cm")
  ) +
  labs(
    x = "Epidemiological Week (2022)",
    y = bquote("Incidence (per million,"~log[10]~" scale)")
  )

ggsave(
  plot = incid_plot,
  filename = "output/fig04-monkeypox-incidence-by-subregion.tiff",
  height = 16,
  width = 16,
  dpi = 300
)

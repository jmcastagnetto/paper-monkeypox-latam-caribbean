suppressPackageStartupMessages(library(tidyverse))

mp_rt <- readRDS(
  "data/proc/monkeypox-selected-countries-model-results.rds"
)  %>%
  filter(term == "days") %>%
  select(
    Country,
    B = estimate,
  ) %>%
  mutate(
    rt = exp(12.5 * B),
    rt_lo = exp(7.5 * B),
    rt_hi = exp(17.3 * B),
    label = glue::glue("widehat(R)[t]:{sprintf('%.2f', rt)}")
  )

top_3 <- mp_rt %>%
  ungroup() %>%
  top_n(n = 3, wt = rt) %>%
  pull(Country)

mp_rt_plot <- ggplot(
  mp_rt %>%
    mutate(top_country = (Country %in% top_3)),
  aes(y = fct_reorder(Country, rt), x = rt),
) +
  geom_linerange(
    aes(xmin = rt_lo, xmax = rt_hi),
    size = 1,
    color = "grey60"
  ) +
  geom_point(
    aes(color = top_country),
    shape = "bullet",
    size = 2,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 2.43, linetype = "dashed") +
  geom_label(
    aes(x = rt, label = label),
    parse = TRUE,
    hjust = 0,
    nudge_y = 0.25,
    size = 4.5,
    label.size = 0,
    label.padding = unit(0, "lines")
  ) +
  annotate(
    geom = "curve",
    xend = 2.43,
    yend = 9,
    x = 3,
    y = 10.5,
    curvature = 0.3,
    arrow = arrow(type = "closed", length = unit(2, "mm"))
  ) +
  annotate(
    geom = "label",
    label = "Value reported by Guzzetta et al. 2022 for Italy (2.43)",
    x = 3,
    y = 10.5,
    hjust = 0,
    vjust = 0.5
  ) +
  scale_x_continuous(n.breaks = 15) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  labs(
    y = "",
    x = "Estimated Rt (including lower and upper bounds)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(size = 16)
  )

ggsave(
  plot = mp_rt_plot,
  filename = "output/fig06-monkeypox-rt-selected-countries.tiff",
  dpi = 300,
  height = 10,
  width = 12
)

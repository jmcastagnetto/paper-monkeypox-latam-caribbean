suppressPackageStartupMessages(library(tidyverse))

mp_dup_time <- readRDS(
  "data/proc/monkeypox-selected-countries-model-results.rds"
)  %>%
  filter(term == "days") %>%
  select(
    Country,
    term,
    estimate,
    std.error,
    adj.r.squared,
    p.value,
    nobs
  ) %>%
  mutate(
    td = log(2) / estimate,
    td_low = log(2) / (estimate + 1.96 * std.error),
    td_high = log(2) / (estimate - 1.96 * std.error),
    label = glue::glue("widehat(T)[d]:{sprintf('%.1f', td)}~(N:{nobs})"
    )
  )

top_3 <- mp_dup_time %>%
  ungroup() %>%
  top_n(n = 3, wt = -td) %>%
  pull(Country)

mp_dup_time_plot <- ggplot(
  mp_dup_time %>%
    mutate(top_country = (Country %in% top_3)),
  aes(y = fct_reorder(Country, desc(td)), x = td),
) +
  geom_linerange(
    aes(xmin = td_low, xmax = td_high),
    size = 1,
    color = "grey60"
  ) +
  geom_point(
    aes(color = top_country),
    shape = "bullet",
    size = 2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = td, label = label),
    parse = TRUE,
    hjust = 0,
    nudge_y = 0.25,
    size = 4.5
  ) +
  scale_x_continuous(n.breaks = 15) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  labs(
    y = "",
    x = "Estimated duplication time (in days, with 95% CI)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(size = 16)
  )

ggsave(
  plot = mp_dup_time_plot,
  filename = "output/fig05-monkeypox-duplication-time-selected-countries.tiff",
  dpi = 300,
  height = 10,
  width = 10
)

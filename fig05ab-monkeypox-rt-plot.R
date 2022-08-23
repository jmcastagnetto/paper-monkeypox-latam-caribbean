suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(patchwork))

# Fig 5a, Td --------------------------------------------------------------

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

mp_dup_time <- mp_dup_time %>%
  mutate(top_country = if_else(Country %in% top_3, "Yes", "No"))

mp_dup_time_plot <- ggplot(
  mp_dup_time,
  aes(y = fct_reorder(Country, desc(td)), x = td),
) +
  geom_linerange(
    aes(xmin = td_low, xmax = td_high),
    size = 1.5,
    color = "grey60"
  ) +
  geom_point(
    aes(shape = top_country),
    size = 3,
    show.legend = FALSE,
    alpha = 0.7
  ) +
  geom_text(
    aes(x = td, label = label),
    parse = TRUE,
    hjust = 0,
    nudge_y = 0.3,
    nudge_x = 2,
    size = 4.5
  ) +
  scale_x_continuous(n.breaks = 15) +
  scale_shape_manual(values = c("No" = "bullet", "Yes" = "diamond")) +
  labs(
    y = "",
    x = "Estimated duplication time (in days, with 95% CI)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(size = 16)
  )

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
  ) %>%
  arrange(rt) %>%
  ungroup() %>%
  mutate(
    top_country = if_else(Country %in% top_3, "Yes", "No")
  )

mp_rt_plot <- ggplot(
  mp_rt,
  aes(y = fct_reorder(Country, rt), x = rt),
) +
  geom_linerange(
    aes(xmin = rt_lo, xmax = rt_hi),
    size = 1.5,
    color = "grey60"
  ) +
  geom_point(
    aes(shape = top_country),
    size = 3,
    show.legend = FALSE
  ) +
  scale_shape_manual(values = c("No" = "bullet", "Yes" = "diamond")) +
  geom_text(
    aes(x = rt, label = label),
    parse = TRUE,
    hjust = 0,
    nudge_y = 0.3,
    nudge_x = 0.05,
    size = 4.5
  ) +
  scale_x_continuous(n.breaks = 10) +
  labs(
    y = "",
    x = "Estimated Rt (including lower and upper bounds)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(size = 16),
    legend.position = "none"
  )

fig5ab <- (mp_dup_time_plot + mp_rt_plot) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 24, face = "bold")
  )

ggsave(
  plot = fig5ab,
  filename = "output/fig05ab-monkeypox-td-rt-selected-countries.tiff",
  dpi = 300,
  height = 10,
  width = 12
)

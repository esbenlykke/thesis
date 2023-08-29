library(tidyverse)
library(ggthemes)
library(showtext)
library(ggtext)
library(patchwork)
library(ggsci)

showtext_auto()
font_add_google("Montserrat", "Montserrat")

hip_thigh_nw <- read_rds("data/nw_episodes_thigh_hip.rds")
wrist_nw <- read_rds("data/nw_episodes_wrist.rds")

all_short <-
hip_thigh_nw %>%
  bind_rows(wrist_nw) %>%
  mutate(
    n = (n * 10),
    nw = time_length(n, unit = "minute")
  ) %>%
  filter(nw < 60) %>%
  ggplot(aes(nw, fill = location)) +
  geom_histogram(color = "black", binwidth = 1, alpha = .8, size = .1) +
  labs(
    title = "<60 minutes",
    x = "Non-wear episode duration (minutes)",
    y = NULL,
    fill = NULL
  ) +
  scale_fill_npg(labels = c("Hip", "Thigh", "Wrist")) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~location, ncol = 1) +
  theme_classic() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(size = 8, family = "Montserrat"),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 4),
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank()
  )

all_long <-
  hip_thigh_nw %>%
  bind_rows(wrist_nw) %>%
  mutate(
    n = (n * 10),
    nw = time_length(n, unit = "hour")
  ) %>%
  filter(nw >= 2) %>%
  ggplot(aes(nw, fill = location)) +
  geom_histogram(color = "black", binwidth = 1, alpha = .8, size = .1) +
  labs(
    title = "\u2265 60 minutes",
    x = "Non-wear episode duration (hours)",
    y = NULL,
    fill = NULL
  ) +
  scale_fill_npg(labels = c("Hip", "Thigh", "Wrist")) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~location, ncol = 1, scales = "free_y") +
  theme_classic() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(size = 8),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 4),
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank()
  ) +
  xlim(0, 80)

nw_dists <-
  all_short + all_long +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom"
    )

ggsave(plot = nw_dists, "figures/paper2_plot_nw_dists.pdf", dpi = 600,  width = 12.5, height = 6, units = "cm")

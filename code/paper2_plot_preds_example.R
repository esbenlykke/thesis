library(tidyverse)
library(ggthemes)
library(showtext)
library(ggtext)
library(patchwork)
library(ggsci)

showtext_auto()
font_add_google("Montserrat", "Montserrat")


preds_wrist <- read_rds("data/preds_wrist.rds")
preds_thigh <- read_rds("data/preds_thigh.rds")
preds_hip <- read_rds("data/preds_hip.rds")

times_all <-
  read_rds("data/all_nw_start_stop_times.rds")

times <-
  times_all %>%
  filter(name == "thigh_cri") %>%
  unnest(times) %>%
  mutate(
    start = as.POSIXct(start),
    stop = as.POSIXct(stop)
  ) %>% 
  filter(start > 481682 & stop < 481682 + 66960) # samples from id = 1043206

ml_plots <-
preds_thigh %>%
  filter(id == 2029012) %>%
  mutate(across(wear_criterion:syed_preds, as.numeric),
    idx_all = as.POSIXct(idx_all)
  ) %>%
  select(idx_all, wear_criterion:syed_preds) %>%
  pivot_longer(-idx_all) %>%
  filter(name %in% c("syed_preds", "sunda_preds")) %>%
  mutate(value = if_else(name == "syed_preds", value + .05, value)) %>%
  ggplot(aes(idx_all, value, color = name)) +
  scale_color_manual(values = c("#3C5488FF", "#F39B7FFF"), labels = c("sunda_RF", "syed_CNN")) +
  scale_y_continuous(n.breaks = 2, labels = c("Wear", "Non-wear")) +
  scale_x_datetime(breaks = "3 hours", date_labels = "%H:%M") +
  geom_rect(times,
    mapping = aes(
      xmin = start,
      xmax = stop,
      ymin = .8,
      ymax = 2.2
    ),
    fill = "grey75",
    alpha = .4,
    inherit.aes = FALSE
  ) +
  geom_line(size = .3,
            key_glyph = draw_key_rect,
            lineend = "round") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
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
  )

heu_plots <-
  preds_thigh %>%
  filter(id == 2029012) %>%
  mutate(across(wear_criterion:syed_preds, as.numeric),
         idx_all = as.POSIXct(idx_all)) %>%
  select(idx_all, wear_criterion:syed_preds) %>%
  pivot_longer(-idx_all) %>%
  filter(name %in% c("wear_heuristic", "wear_time_cz")) %>%
  mutate(value = if_else(name == "wear_heuristic", value + .05, value)) %>%
  ggplot(aes(idx_all, value, color = name)) +
  scale_color_manual(values = c("#4DBBD5FF", "#7E6148FF"), labels = c("cz_60", "heu_alg")) +
  scale_y_continuous(n.breaks = 2, labels = c("Wear", "Non-wear")) +
  scale_x_datetime(breaks = "3 hours", date_labels = "%H:%M") +
  geom_rect(times,
    mapping = aes(
      xmin = start,
      xmax = stop,
      ymin = .8,
      ymax = 2.2
    ),
    fill = "grey75",
    alpha = .4,
    inherit.aes = FALSE
  ) +
  geom_line(size = .3,
            key_glyph = draw_key_rect,
            lineend = "round") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
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
  )

tree_plots <-
  preds_thigh %>%
  filter(id == 2029012) %>%
  mutate(across(wear_criterion:syed_preds, as.numeric),
         idx_all = as.POSIXct(idx_all)) %>%
  select(idx_all, wear_criterion:syed_preds) %>%
  pivot_longer(-idx_all) %>%
  filter(name %in% c("tree_full_preds", "tree_imp6_preds")) %>%
  mutate(value = if_else(name == "tree_full_preds", value + .05, value)) %>%
  # value = if_else(name == "tree_no_temp_preds", value - .05, value)) %>%
  ggplot(aes(idx_all, value, color = name)) +
  scale_color_manual(values = c("#E64B35FF", "#00A087FF"),
                     labels = c("tree_full", "tree_imp6")) +
  scale_y_continuous(n.breaks = 2, labels = c("Wear", "Non-wear")) +
  scale_x_datetime(breaks = "3 hours", date_labels = "%H:%M") +
  geom_rect(times,
    mapping = aes(
      xmin = start,
      xmax = stop,
      ymin = .8,
      ymax = 2.2
    ),
    fill = "grey75",
    alpha = .4,
    inherit.aes = FALSE
  ) +
  geom_line(size = .3,
            key_glyph = draw_key_rect,
            lineend = "round") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
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
  ) 


plots <-
  ml_plots / heu_plots / tree_plots 

ggsave(plot = plots, "figures/paper2_plot_preds_example.pdf", dpi = 600, width = 12.5, height = 10, units = "cm")


library(tidyverse)
library(vip)
library(ggthemes)
library(showtext)
library(ggridges)
library(patchwork)
library(ggsci)
library(png)
library(vip)
library(ggdist)
library(prismatic)

theme_color <- "#F0F1EB"

my_theme <-
  theme_classic(base_size = 16) +
  theme(
    text = element_text(family = "Zilla Slab"),
    plot.background = element_rect(fill = theme_color, color = theme_color),
    panel.background = element_rect(fill = theme_color, color = theme_color),
    legend.position = "bottom",
    legend.background = element_rect(fill = theme_color, color = theme_color),
    axis.line = element_line(linewidth = .2),
    axis.ticks = element_line(linewidth = .2),
    strip.background = element_blank()
  )

# Hypnogram ---------------------------------------------------------------


hyp <-
  read_csv("data/yasa_example_night_young_hypno.csv")

hyp_plot <- hyp %>%
  mutate(
    time = 1:n(),
    stage = case_when(
      Stage == 0 ~ "Wake",
      Stage == 4 ~ "REM",
      Stage == 1 ~ "N1",
      Stage == 2 ~ "N2",
      Stage == 3 ~ "N3"
    ),
    stage = factor(stage, levels = c("N3", "N2", "N1", "REM", "Wake"))
  ) %>%
  ggplot(aes(time, stage, group = 1, fill = stage, color = stage)) +
  geom_tile(show.legend = FALSE) +
  scale_x_continuous(
    breaks = seq(1, 1000, 120),
    labels = c("22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00")
  ) +
  labs(
    x = "Time",
    y = "Stage"
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  my_theme



# Density ridge plots -----------------------------------------------------


self_fup_tb <- read_csv("data/fup_self_to_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man1_fup_tb <- read_csv("data/fup_man1_to_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man2_fup_tb <- read_csv("data/fup_man2_to_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)

dens_fup_tb <-
  tibble(
    x = rep(self_fup_tb$x, times = abs(self_fup_tb$y * 100)), # Adjust multiplier as needed
    rater = "self"
  ) %>%
  bind_rows(
    tibble(
      x = rep(man1_fup_tb$x, times = abs(man1_fup_tb$y * 100)), # Adjust multiplier as needed
      rater = "man1"
    )
  ) %>%
  bind_rows(
    tibble(
      x = rep(man2_fup_tb$x, times = abs(man2_fup_tb$y * 100)), # Adjust multiplier as needed
      rater = "man2"
    )
  ) %>%
  mutate(
    rater = factor(rater, levels = c("man1", "man2", "self"))
  )

p1 <-
  ggplot(dens_fup_tb, aes(x, rater, fill = rater)) +
  geom_density_ridges(alpha = .8, size = .1, show.legend = FALSE, color = "white") +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Going to bed, follow-up",
    x = ""
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  my_theme +
  theme(
    axis.title = element_blank()
  )

self_bsl_tb <- read_csv("data/bsl_self_to_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man1_bsl_tb <- read_csv("data/bsl_man1_to_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man2_bsl_tb <- read_csv("data/bsl_man2_to_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)

dens_bsl_tb <-
  tibble(
    x = rep(self_bsl_tb$x, times = abs(self_bsl_tb$y * 100)), # Adjust multiplier as needed
    rater = "self"
  ) %>%
  bind_rows(
    tibble(
      x = rep(man1_bsl_tb$x, times = abs(man1_bsl_tb$y * 100)), # Adjust multiplier as needed
      rater = "man1"
    )
  ) %>%
  bind_rows(
    tibble(
      x = rep(man2_bsl_tb$x, times = abs(man2_bsl_tb$y * 100)), # Adjust multiplier as needed
      rater = "man2"
    )
  ) %>%
  mutate(
    rater = factor(rater, levels = c("man1", "man2", "self"))
  )

p2 <-
  ggplot(dens_bsl_tb, aes(x, rater, fill = rater)) +
  geom_density_ridges(alpha = .8, size = .1, show.legend = FALSE, color = "white") +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Going to bed, baseline",
    x = ""
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  my_theme +
  theme(
    axis.title = element_blank()
  )


self_fup_ob <- read_csv("data/fup_self_out_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man1_fup_ob <- read_csv("data/fup_man1_out_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man2_fup_ob <- read_csv("data/fup_man2_out_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)

dens_fup_ob <-
  tibble(
    x = rep(self_fup_ob$x, times = abs(self_fup_ob$y * 100)), # Adjust multiplier as needed
    rater = "self"
  ) %>%
  bind_rows(
    tibble(
      x = rep(man1_fup_ob$x, times = abs(man1_fup_ob$y * 100)), # Adjust multiplier as needed
      rater = "man1"
    )
  ) %>%
  bind_rows(
    tibble(
      x = rep(man2_fup_ob$x, times = abs(man2_fup_ob$y * 100)), # Adjust multiplier as needed
      rater = "man2"
    )
  ) %>%
  mutate(
    rater = factor(rater, levels = c("man1", "man2", "self"))
  )

p3 <-
  ggplot(dens_fup_ob, aes(x, rater, fill = rater)) +
  geom_density_ridges(alpha = .8, size = .1, show.legend = FALSE, color = "white") +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Getting out of bed, follow-up"
    # x = "Diffence in minutes compared to ZM"
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  my_theme +
  theme(
    axis.title = element_blank()
  )


self_bsl_ob <- read_csv("data/bsl_self_out_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man1_bsl_ob <- read_csv("data/bsl_man1_out_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)
man2_bsl_ob <- read_csv("data/bsl_man2_out_bed.csv", col_names = c("x", "y")) %>%
  filter(x > -25 & x < 25)

dens_bsl_ob <-
  tibble(
    x = rep(self_bsl_ob$x, times = abs(self_bsl_ob$y * 100)), # Adjust multiplier as needed
    rater = "self"
  ) %>%
  bind_rows(
    tibble(
      x = rep(man1_bsl_ob$x, times = abs(man1_bsl_ob$y * 100)), # Adjust multiplier as needed
      rater = "man1"
    )
  ) %>%
  bind_rows(
    tibble(
      x = rep(man2_bsl_ob$x, times = abs(man2_bsl_ob$y * 100)), # Adjust multiplier as needed
      rater = "man2"
    )
  ) %>%
  mutate(
    rater = factor(rater, levels = c("man1", "man2", "self"))
  )

p4 <-
  ggplot(dens_fup_ob, aes(x, rater, fill = rater)) +
  geom_density_ridges(alpha = .8, size = .1, show.legend = FALSE, color = "white") +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Getting out of bed, baseline",
    x = ""
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  my_theme +
  theme(
    axis.title = element_blank()
  )


# paper 2 flowchart -------------------------------------------------------


sensor_img <- readPNG("img/wrist_sensor.png", native = TRUE)



data <-
  tibble(x = 1:100, y = 1:100)

annotate_text_size <- 4
annotate_lineheight <- 1
box_color <- "#F0F1EB"
font_family <- "Zilla Slab"


flow1 <-
  data |>
  ggplot(aes(x, y)) +
  # scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  # scale_y_continuous(minor_breaks = seq(10, 130, 10)) +
  # main boxes
  geom_rect(
    xmin = 10, xmax = 40, ymin = 117, ymax = 127, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 25, y = 122,
           label = "Complete PHASAR dataset\nhip + thigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 10, xmax = 40, ymin = 92, ymax = 102, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 25, y = 97,
           label = "PHASAR training set\nhip + thigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) + 
  geom_rect(
    xmin = 0, xmax = 15, ymin = 67, ymax = 77, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 7.5, y = 72,
           label = "Resample 1",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 17.5, xmax = 32.5, ymin = 67, ymax = 77, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 25, y = 72,
           label = "Resample 2",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 35, xmax = 50, ymin = 67, ymax = 77, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 42.5, y = 72,
           label = "Resample 5",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  annotate("text",
           x = 33.75, y = 72,
           label = "...",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  annotate("text",
           x = 46.5, y = 123,
           label = "20.8%",
           size = 3, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  annotate("text",
           x = 23.8, y = 109,
           label = "79.2%",
           size = 3, color = "grey15", lineheight = annotate_lineheight,
           angle = 90,
           family = font_family
  ) +
  # vertical arrows
  geom_segment(
    x = 25, xend = 25, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) +
  geom_segment(
    x = 25, xend = 25, y = 92, yend = 77,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) +
  geom_segment(
    x = 15, xend = 7.5, y = 92, yend = 77,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) +
  geom_segment(
    x = 35, xend = 42.5, y = 92, yend = 77,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) + 
  geom_segment(
    x = 57.5, xend = 57.5, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) +
  geom_segment(
    x = 75, xend = 75, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) +
  geom_segment(
    x = 92.5, xend = 92.5, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) +
  # horizontal arrows
  geom_segment(
    x = 40, xend = 52.5, y = 122, yend = 122,
    linewidth = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#4DBBD5"
  ) +
  # boxes to the side
  geom_rect(
    xmin = 52.5, xmax = 80, ymin = 117, ymax = 127, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 66.25, y = 122,
           label = "PHASAR test data\nhip + thigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 50, xmax = 65, ymin = 92, ymax = 102, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 57.5, y = 97,
           label = "PHASAR test\nhip",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 67.5, xmax = 82.5, ymin = 92, ymax = 102, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 75, y = 97,
           label = "PHASAR test\nthigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 85, xmax = 100, ymin = 92, ymax = 102, color = "#4DBBD5",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 92.5, y = 97,
           label = "In-house test\nwrist",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  # coord_cartesian(ylim = c(65, 125)) +
  ylim(69, 125) +
  theme_void() +
  patchwork::inset_element(p = sensor_img,
                           left = 0.83,
                           bottom = .8,
                           right = 0.95,
                           top = 0.97) &
  theme(
    text = element_text(family = "Zilla Slab"),
    plot.background = element_rect(fill = theme_color, color = theme_color),
    panel.background = element_rect(fill = theme_color, color = theme_color),
    legend.background = element_rect(fill = theme_color, color = theme_color)
  )


# VIP plot ----------------------------------------------------------------

vi_scores <- 
  read_rds("~/projects/nonwear/data/vi_scores.rds")

vip_plot <- 
  vi_scores %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(Importance, Variable, fill = Importance)) +
  geom_col(color = "grey50", show.legend = FALSE, size = .08) +
  scale_fill_gradient2(low = "white", high = "#3C5488") +
  scale_x_continuous(labels = scales::scientific, expand = c(0, 0)) +
  labs(x = "Relative Importance",
       y = "Feature") +
  my_theme


# nonwear distribution plots ----------------------------------------------

hip_thigh_nw <- read_rds("data/nw_episodes_thigh_hip.rds")
wrist_nw <- read_rds("data/nw_episodes_wrist.rds")

rain_short <-
  hip_thigh_nw %>%
  bind_rows(wrist_nw) %>%
  mutate(
    n = (n * 10),
    nw = time_length(n, unit = "minute")
  ) %>%
  filter(nw < 60) %>%
  ggplot(aes(x = nw, y = location, fill = location)) +
  geom_boxplot(
    aes(color = after_scale(clr_darken(fill, shift = .5))),
    width = 0.1,
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  stat_slab(
    position = position_nudge(y = 0.075),
    height = .9
  ) +
  scale_fill_npg(labels = c("Hip", "Thigh", "Wrist")) +
  labs(
    title = "<60 minutes",
    x = "Duration (minutes)",
    fill = NULL
  ) +
  my_theme +
  theme(
    axis.title = element_blank()
  )

rain_long <-
  hip_thigh_nw %>%
  bind_rows(wrist_nw) %>%
  mutate(
    n = (n * 10),
    nw = time_length(n, unit = "hour")
  ) %>%
  filter(nw >= 2) %>%
  ggplot(aes(x = nw, y = location, fill = location)) +
  geom_boxplot(
    aes(color = after_scale(clr_darken(fill, shift = .5))),
    width = 0.1,
    linewidth = 0.2,
    outlier.shape = NA,
    show.legend = FALSE
  ) +
  stat_slab(
    position = position_nudge(y = 0.075),
    height = .9
  ) +
  # scale_x_log10() +
  scale_fill_npg(labels = c("Hip", "Thigh", "Wrist")) +
  labs(
    title = "\u2265 60 minutes",
    x = "Duration (hours)",
    fill = NULL
  ) +
  xlim(0, 20) +
  my_theme +
  theme(
    axis.title = element_blank()
  )

nw_rain <-
  rain_short + rain_long +
  plot_layout(guides = "collect") &
  scale_y_discrete(expand = c(0.05, 0)) &
  my_theme +
  theme(
    legend.position = "bottom"
  )


# paper2 preds example ----------------------------------------------------

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
  ) 


preds_plots <-
  ml_plots / heu_plots / tree_plots &
  my_theme &
  theme(
    legend.position = "right"
  )


# paper2 performance plots ------------------------------------------------
all_metrics <- read_rds("data/all_metrics.rds")

plot_all <-
  all_metrics %>% 
  replace_na(replace = list(.estimate = 0)) %>%
  mutate(
    model = fct_relevel(model, c("syed_CNN", "sunda_RF", "tree_no_temp", "tree_imp6", "tree_full", "heuristic")),
    .metric = fct_relevel(.metric, c("precision", "sensitivity", "f_meas")),
    model = fct_reorder(model, .estimate)
  ) %>%
  ggplot(aes(model, .estimate, fill = dataset)) +
  geom_col(
    width = .75,
    alpha = .8,
    # color = "black",
    size = .1,
    position = "dodge2"
  ) +
  geom_text(aes(model, .estimate + .1,
                label = round(.estimate, 2)
  ),
  size = 3, 
  color = "grey20",
  position = position_dodge(width = .8)
  ) +
  expand_limits(y = c(0, 1.2)) +
  scale_fill_npg(
    labels = c("Hip", "Thigh", "Wrist")
  ) +
  scale_x_discrete(breaks = c("syed_CNN", "heuristic", "sunda_RF", "cz_60", "tree_no_temp", "tree_full", "tree_imp6"),
                   labels = c("syed_cnn", "hue_alg", "sunda_rf", "cv_60", "tree_no_temp", "tree_full", "tree_imp6")) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, .5, 1)) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = NULL,
  ) +
  facet_wrap(~.metric,
             labeller = labeller(.metric = c(
               precision = "Precision",
               sensitivity = "Sensitivity",
               accuracy = "Accuracy",
               f_meas = "F1 score"
             )),
             nrow = 4
  ) +
  my_theme

all_metrics <- read_rds("data/all_metrics_60.rds")

plot_short <-
  all_metrics %>%
  filter(.metric != "accuracy", model != "cz_60") %>% 
  replace_na(replace = list(.estimate = 0)) %>%
  mutate(
    model = fct_relevel(model, c("syed_cnn", "sunda_rf", "tree_no_temp", "tree_imp6", "tree_full", "heuristic")),
    .metric = fct_relevel(.metric, c("precision", "sensitivity", "f_meas")),
    model = fct_reorder(model, .estimate)
  ) %>%
  ggplot(aes(model, .estimate, fill = dataset)) +
  geom_col(
    width = .75,
    alpha = .8,
    # color = "black",
    size = .1,
    position = "dodge2"
  ) +
  geom_text(aes(model, .estimate + .1,
                label = round(.estimate, 2)
  ),
  size = 3,
  color = "grey20",
  position = position_dodge(width = .8)
  ) +
  expand_limits(y = c(0, 1.2)) +
  scale_fill_npg(
    labels = c("Hip", "Thigh", "Wrist")
  ) +
  scale_x_discrete(breaks = c("syed_cnn", "heuristic", "sunda_rf", "tree_no_temp", "tree_full", "tree_imp6"),
                   labels = c("syed_cnn", "hue_alg", "sunda_rf", "tree_no_temp", "tree_full", "tree_imp6")) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, .5, 1)) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = NULL,
  ) +
  facet_wrap(~.metric,
             labeller = labeller(.metric = c(
               precision = "Precision",
               sensitivity = "Sensitivity",
               accuracy = "Accuracy",
               f_meas = "F1 score"
             )),
             nrow = 4
  ) +
  my_theme


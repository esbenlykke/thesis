library(tidyverse)
library(ggthemes)
library(showtext)
library(ggtext)
library(patchwork)
library(ggsci)

showtext_auto()
font_add_google("Montserrat", "Montserrat")

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
    color = "black",
    size = .1,
    position = "dodge2"
  ) +
  geom_text(aes(model, .estimate + .08,
                label = round(.estimate, 2)
  ),
  size = 1.5, 
  position = position_dodge(width = .8)
  ) +
  expand_limits(y = c(0, 1.2)) +
  scale_fill_npg(
    labels = c("Hip", "Thigh", "Wrist")
  ) +
  scale_x_discrete(breaks = c("syed_CNN", "heuristic", "sunda_RF", "cz_60", "tree_no_temp", "tree_full", "tree_imp6"),
                   labels = c("syed_cnn", "hue_alg", "sunda_rf", "cv_60", "tree_no_temp", "tree_full", "tree_imp6")) +
  scale_y_continuous(expand = c(0, 0)) +
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
    axis.line = element_line(size = .2),
    axis.ticks = element_blank()
  )


ggsave(filename = "figures/paper2_performance_all.pdf", 
       plot = plot_all, dpi = 600, width = 10, height = 10, units = "cm")

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
    color = "black",
    size = .1,
    position = "dodge2"
  ) +
  geom_text(aes(model, .estimate + .08,
                label = round(.estimate, 2)
  ),
  size = 1.5,
  position = position_dodge(width = .8)
  ) +
  expand_limits(y = c(0, 1.2)) +
  scale_fill_npg(
    labels = c("Hip", "Thigh", "Wrist")
  ) +
  scale_x_discrete(breaks = c("syed_cnn", "heuristic", "sunda_rf", "tree_no_temp", "tree_full", "tree_imp6"),
                   labels = c("syed_cnn", "hue_alg", "sunda_rf", "tree_no_temp", "tree_full", "tree_imp6")) +
  scale_y_continuous(expand = c(0, 0)) +
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
    axis.line = element_line(size = .2),
    axis.ticks = element_blank()
  )


ggsave(filename = "figures/paper2_performance_short.pdf", 
       plot = plot_short, dpi = 600, width = 8.57, height = 7.5, units = "cm")
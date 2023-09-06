library(tidyverse)
library(ggridges)
library(patchwork)
library(ggtext)
library(ggthemes)
library(showtext)
library(ggsci)

showtext_auto()
font_add_google("Montserrat", "Montserrat")

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
  geom_density_ridges(alpha = .8, size = .1, show.legend = FALSE) +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Going to bed, follow-up",
    x = ""
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
  geom_density_ridges(alpha = .8, size = .1, show.legend = FALSE) +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Going to bed, baseline",
    x = ""
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
  geom_density_ridges(alpha = .8, size = .1, show.legend = TRUE) +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Getting out of bed, follow-up",
    x = "Diffence in minutes compared to ZM"
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
  geom_density_ridges(alpha = .8, size = .1, show.legend = FALSE) +
  scale_y_discrete(expand = c(0, 0), limits = c("self", "man2", "man1")) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Getting out of bed, baseline",
    x = ""
  )

ridge_plot <- (p2 / p1 / p4 / p3) +
  plot_layout(guides = "collect") &
  scale_fill_npg(labels = c("Self-report", "Manual, Round 1", "Manual, round 2"),
                       name = NULL) &
  theme_tufte() &
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(size = 8),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 4),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = NA),
    plot.subtitle = element_blank() # This will remove the subtitle
  )

ggsave(plot = ridge_plot, filename = "figures/paper1_ridge_plot.pdf", width = 12.5, height = 12, units = "cm")

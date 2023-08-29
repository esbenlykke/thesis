library(tidyverse)
library(ggtext)
library(showtext)
library(png)

sensor_img <- readPNG("img/wrist_sensor.png", native = TRUE)

showtext_auto()
font_add_google("Montserrat", family = "Montserrat")



data <-
  tibble(x = 1:100, y = 1:100)

annotate_text_size <- 2
annotate_lineheight <- 1
box_color <- "grey80"
font_family <- "Montserrat"


data |>
  ggplot(aes(x, y)) +
  # scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  # scale_y_continuous(minor_breaks = seq(10, 130, 10)) +
  # main boxes
  geom_rect(
    xmin = 10, xmax = 40, ymin = 117, ymax = 127, color = "#93A1A1",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 25, y = 122,
           label = "Complete PHASAR dataset\nhip + thigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 10, xmax = 40, ymin = 92, ymax = 102, color = "#93A1A1",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 25, y = 97,
           label = "PHASAR training set\nhip + thigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) + 
  geom_rect(
    xmin = 0, xmax = 15, ymin = 67, ymax = 77, color = "#93A1A1",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 7.5, y = 72,
           label = "Resample 1",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 17.5, xmax = 32.5, ymin = 67, ymax = 77, color = "#93A1A1",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 25, y = 72,
           label = "Resample 2",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 35, xmax = 50, ymin = 67, ymax = 77, color = "#93A1A1",
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
           size = 1.2, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  annotate("text",
           x = 24, y = 109,
           label = "79.2%",
           size = 1.2, color = "grey15", lineheight = annotate_lineheight,
           angle = 90,
           family = font_family
  ) +
  # vertical arrows
  geom_segment(
    x = 25, xend = 25, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 25, xend = 25, y = 92, yend = 77,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 15, xend = 7.5, y = 92, yend = 77,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 35, xend = 42.5, y = 92, yend = 77,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) + 
  geom_segment(
    x = 57.5, xend = 57.5, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 75, xend = 75, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 92.5, xend = 92.5, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  # horizontal arrows
  geom_segment(
    x = 40, xend = 52.5, y = 122, yend = 122,
    linewidth = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  # boxes to the side
  geom_rect(
    xmin = 52.5, xmax = 80, ymin = 117, ymax = 127, color = "#93A1A1",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 66.25, y = 122,
           label = "PHASAR test data\nhip + thigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 50, xmax = 65, ymin = 92, ymax = 102, color = "#93A1A1",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 57.5, y = 97,
           label = "PHASAR test\nhip",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 67.5, xmax = 82.5, ymin = 92, ymax = 102, color = "#93A1A1",
    fill = box_color, linewidth = 0.25
  ) +
  annotate("text",
           x = 75, y = 97,
           label = "PHASAR test\nthigh",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 85, xmax = 100, ymin = 92, ymax = 102, color = "#93A1A1",
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
                         top = 0.97)

ggsave(filename = "figures/paper2_flowchart.pdf", width = 12.5, height = 8, units = "cm", dpi = 600)

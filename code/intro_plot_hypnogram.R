library(tidyverse)
library(showtext)
library(lubridate)

showtext_auto()
font_add_google("Montserrat", "Montserrat")

hyp <- 
  read_csv("data/yasa_example_night_young_hypno.csv")

hyp %>% 
  mutate(
    time = 1:n(),
    stage = case_when(Stage == 0 ~ "Wake",
                      Stage == 4 ~ "REM",
                      Stage == 1 ~ "N1",
                      Stage == 2 ~ "N2",
                      Stage == 3 ~ "N3"),
    stage = factor(stage, levels = c("N3", "N2", "N1", "REM", "Wake"))
  ) %>% 
  ggplot(aes(time, stage, group = 1, fill = stage, color = stage)) +
  geom_tile(show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1, 1000, 120),
                     labels = c("22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00")) +
  ggsci::scale_fill_npg() +
  ggsci::scale_color_npg() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(linewidth = .1, linetype = 2),
    text = element_text(family = "Montserrat"),
    plot.title = element_text(size = 8),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6),
    axis.line = element_line(linewidth = .2),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

ggsave(filename = "figures/hypnogram.pdf", width = 12.5, height = 4, units = "cm", dpi = 600)
  
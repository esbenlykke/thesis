library(tidyverse)
library(vip)
library(ggthemes)
library(showtext)

showtext_auto()
font_add_google("Montserrat", "Montserrat")

vi_scores <- 
  read_rds("~/projects/nonwear/data/vi_scores.rds")

vi_scores %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(Importance, Variable, fill = Importance)) +
  geom_col(color = "grey50", show.legend = FALSE, size = .08) +
  scale_fill_gradient2(low = "white", high = "#3C5488") +
  scale_x_continuous(labels = scales::scientific, expand = c(0, 0)) +
  labs(x = "Relative Importance",
       y = "Feature") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(size = .1, linetype = 2),
    text = element_text(family = "Montserrat"),
    plot.title = element_text(size = 8),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 4),
    axis.line = element_line(size = .2),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    )


ggsave(filename = "figures/paper2_vip.pdf", width = 10, height = 4, units = "cm", dpi = 600)

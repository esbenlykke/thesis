library(tidyverse)
library(vip)
library(ggthemes)
library(showtext)

showtext_auto()
font_add_google("Zilla Slab", "Zilla Slab")

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
       y = "Feature")


# ggsave(filename = "figures/paper2_vip.pdf", width = 10, height = 4, units = "cm", dpi = 600)

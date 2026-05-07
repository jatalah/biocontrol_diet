# Boxplot ---
library(tidyverse)
library(ggpubr)

rm(list = ls())

# select variables
vars_fa <- c(
  "∑Saturated fatty acids SFA",
  "∑Mono-unsaturated fatty acids MUFA",
  "∑ poly-unsaturated fatty acids 3 PUFA",
  "∑omega 3 poly-unsaturated fatty acids n-3 PUFA",
  "Fat Content g/100g"
)


fig_3_data <- 
read_csv('data/fa_percent_long.csv') %>%
  drop_na(value) %>% 
  filter(name %in% vars_fa) %>% 
  mutate(name = fct_recode(name, 
                           SFA = "∑Saturated fatty acids SFA",
                           MUFA = "∑Mono-unsaturated fatty acids MUFA",
                           PUFA = "∑ poly-unsaturated fatty acids 3 PUFA",
                           `ω-3~PUFA` = "∑omega 3 poly-unsaturated fatty acids n-3 PUFA",
                           `Fat~content` = "Fat Content g/100g"),
         name = fct_relevel(name, "Fat~content", "SFA", "MUFA", "PUFA", "ω3~PUFA"))
  

# Boxplot ----
ggplot(fig_3_data, aes(y = value, x = factor(Age))) +
  geom_boxplot(
    aes (fill = fct_rev(Season)),
    size = .15,
    alpha = .7,
    outlier.size = .5
  ) +
  facet_wrap(~name, scales = 'free_y', ,  labeller = label_parsed) +
  theme_bw(base_size = 9) +
  labs(y = "%", x = "Age (days)") +
  theme(
    legend.position = c(.8,.2),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
    # legend.key.size = unit(.3, "cm")
  ) +
  labs(fill = "Season")

# save plot -----
ggsave(
  plot = last_plot(),
  filename = "figures/figure_fa_percent_boxplot.png",
  dpi = 300,
  width = 120,
  height = 90,
  units = "mm",
  bg = "White"
)

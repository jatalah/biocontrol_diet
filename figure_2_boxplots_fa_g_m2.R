# load libraries
library(tidyverse)
library(readxl)
library(ggpubr)

rm(list = ls())

# read data and recode season and agre factors
d <- 
  read_excel("data/FAME master sheet.xlsx", "RNM") %>% 
  mutate(
    Season = fct_recode(
      factor(Season),
      `Spring/Summer` = "1",
      `Autumn/Winter` = "2"
    ),
    Age = fct_recode(
      factor(Age),
      `13` = "1",
      `32` = "2",
      `54` = "3"
    ),
    Location = fct_recode(factor(Location), `Berth C` = "1", `Berth G` = "2")
  )

# select target variables 
vars <-
  c(
    "dry weight g/m2",
    "fat g/m2",
    "protein g/m2",
    "EPA_gm2",
    "SFA_gm2",
    "MUFA_gm2",
    "PUFA_gm2",
    "O3_PUFA_gm2"
  )

mcor <- 
  cor(d %>% select(vars)) %>% 
  as_tibble() %>% 
  mutate(across(where(is.numeric), ~round(.x, 2)))

mcor


# summary stats ---
d %>% 
  select(all_of(vars)) %>% 
  get_summary_stats(type = "common") %>% 
  write_csv('tables/fame_summary_stats.csv')

d %>% 
  group_by(Season) %>% 
  select(all_of(vars)) %>% 
  get_summary_stats(type = 'common') %>%
  mutate(across(min:ci, ~round(.x, 3))) |> 
  write_csv('tables/fame_summary_stat_by_season.csv')

d_long <- 
d %>%
  pivot_longer(cols = vars) %>%
  mutate(
    name = fct_recode(
      name,
      `Fat~(g~m^-2)` = "fat g/m2",
      `Dry~weight~(g~m^-2)` = "dry weight g/m2",
      `Protein~(g~m^-2)` = "protein g/m2",
      `EPA~(g~m^-2)` = "EPA_gm2",
      `SFA~(g~m^-2)` = "SFA_gm2",
      `MUFA~(g~m^-2)` = "MUFA_gm2",
      `PUFA~(g~m^-2)` = "PUFA_gm2",
      `ω-3~PUFA~(g~m^-2)` = "O3_PUFA_gm2"
      
    ),
    name = fct_relevel(name,
                       "Dry~weight~(g~m^-2)",
                       "Protein~(g~m^-2)",
                       "Fat~(g~m^-2)",
                       "EPA~(g~m^-2)",
                       "SFA~(g~m^-2)",
                       "MUFA~(g~m^-2)",
                       "PUFA~(g~m^-2)",
                       "ω3~PUFA~(g~m^-2)"
                       )
  ) %>% 
  write_csv('data/clean_data_biofouling_long.csv')


# Box-plot by treatments -------
ggplot(d_long, aes(y = value, x = factor(Age))) +
  geom_boxplot(
    aes (fill = Season),
    size = .15,
    alpha = .7,
    outlier.size = .5
  ) +
  facet_wrap(~name, scales = 'free',  labeller = label_parsed) +
  theme_bw(base_size = 9) +
  labs(y = NULL, x = "Age (days)") +
  theme(
    legend.position = c(.82,.15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.key.size = unit(.3, "cm")
  ) +
  scale_y_log10(labels = scales::comma) 

ggsave(
  plot = last_plot(),
  filename = "figures/figure_2_boxplot.png",
  dpi = 300,
  width = 120,
  height = 120,
  units = "mm",
  bg = "White"
)

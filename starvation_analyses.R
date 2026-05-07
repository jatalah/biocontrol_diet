library(tidyverse)
library(readxl)
library(ggpubr)
library(skimr)
library(lmerTest)
library(broom.mixed)
library(scales)
library(DHARMa)
library(emmeans)
library(rstatix)
library(janitor)

rm(list = ls())

source('theme_javier.R')
theme_set(theme_javier(base_size = 9))

dd <-
  read_excel('data/Snails.xlsx', na = 'NA') %>%
  select(-`Ash (% of DW)`) %>%
  mutate(treatment = fct_relevel(treatment, "Nil")) %>%
  rename(`Wet weight (g)` = "weight",
         `Carbohydrates (% of DW)` = "Carbs (% of DW)")

base_sum <- 
dd %>%
  filter(sampling=='Baseline') %>% 
  select(-tank, -rep) %>% 
  # group_by(sampling) %>%
  get_summary_stats(type = 'robust') %>% 
  # select(variable, median) %>% 
  rename(name = variable)



d_long_all <- 
  dd %>%
  pivot_longer(cols = c(contains('%'), `Wet weight (g)`)) %>% 
  mutate(treatment = if_else(sampling == "Baseline", "Baseline", treatment),
         treatment = fct_relevel(treatment, 'Baseline', 'Nil'))


ggplot(d_long_all, aes(treatment, value)) +
  geom_boxplot(alpha = .7, size = .25, outlier.size = .25) +
  facet_wrap(~name, scales = 'free') +
  theme(legend.position = 'bottom') +
  labs(fill = "Sampling", x = "Food treatment", y = NULL) +
  stat_compare_means(aes(label = after_stat(p.signif)),
                     method = "t.test", 
                     ref.group = "Baseline") 


# Baseline pairwise comparisons --------
d_long_all %>%
  drop_na(value) %>%
  group_by(name) %>%
  nest() %>%
  mutate(t_tests = map(
    data,
    ~ pairwise_t_test(data = .x, value ~ treatment, 
                      ref.group = 'Baseline', 
                      p.adjust.method = "none")
  )) %>% 
  select(t_tests) %>% 
  unnest(t_tests) %>% 
  select(-.y., -contains('adj'), - p.signif) %>% 
  mutate(p = round(p, 3)) %>% 
  write_csv('tables/baseline_pairwise_comparisons.csv')


dd_long <-
  dd %>%
  filter(sampling == 'Final') %>%
  pivot_longer(cols = c(contains('%'), `Wet weight (g)`)) %>%
  mutate(
    name = fct_relevel(
      name,
      "Wet weight (g)",
      "Protein (% of DW)",
      "Lipid (% of DW)",
      "Water Content (%)",
      "TOM (% of DW)",
      "Carbohydrates (% of DW)"
    )
  )

# Box-plot figure 3 -----
ggplot(dd_long, aes(treatment, value)) +
  geom_boxplot(alpha = .7, size = .25, outlier.size = .25) +
  geom_hline(data = base_sum, aes(yintercept = median), lty = 2, color = 'darkred') +
  geom_hline(data = base_sum, aes(yintercept = median - iqr), lty = 2, color = 'gray70') +
  geom_hline(data = base_sum, aes(yintercept = median + iqr), lty = 2, color = 'gray70') +
  facet_wrap(~name, scales = 'free') +
  theme(legend.position = 'bottom') +
  labs(fill = "Sampling", x = "Food treatment", y = NULL) +
  scale_x_discrete(labels = c("13" = "Low", "54" = "High"))


ggsave(
  plot = last_plot(),
  filename = "figures/figure_3_boxplot.svg",
  dpi = 600,
  width = 180,
  height = 90,
  units = "mm",
  bg = "White"
)

dd_long_final <- 
dd_long %>%
  filter(sampling == 'Final') %>%
  drop_na(value) %>% 
  mutate(tank = factor(tank))


# experimental design-----
dd_long_final %>% 
  group_by(name, treatment, tank) %>% 
  count()



# LMMs --------
lmms_lab <-
  dd_long_final%>%
  group_by(name) %>%
  nest() %>%
  mutate(
    lmms = map(
      data,
      ~ lmerTest::lmer(
        log(value) ~ treatment + (1|tank),
        data = .x
      )
    ),
    lmms_table = map(lmms, tidy),
    anova_tables = map(lmms, ~ tidy(anova(.x))),
    pairwise = map(lmms, ~ emmeans(.x, ~ treatment)),
    pairs = map(pairwise, ~ pairs(.x) %>% as_tibble()),
    p = map(lmms, ~ simulateResiduals(.x, plot = F))
  )

# summary tables----
lmm_tables_lab <- 
lmms_lab %>%
  select(lmms_table) %>%
  unnest(cols = c(lmms_table)) %>% 
  mutate(p.value  = scales::pvalue(p.value))

lmm_tables_lab

# anova tables--
anova_tables_lab <- 
  lmms_lab %>%
  select(anova_tables) %>%
  unnest(cols = c(anova_tables)) %>% 
  mutate(p.value  = scales::pvalue(p.value)) %>% 
  mutate(across(is.numeric, ~round(.x, 2)))

anova_tables_lab

anova_tables_lab %>% write_excel_csv('tables/anova_tables_starvation.csv')


pairwise_tables_lab <- 
  lmms_lab %>%
  select(pairs) %>%
  unnest(cols = c(pairs)) %>% 
  mutate(p.value  = scales::pvalue(p.value)) %>% 
  mutate(across(is.numeric, ~round(.x, 2)))

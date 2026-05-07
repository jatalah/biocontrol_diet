library(tidyverse)
library(broom.mixed)
library(emmeans)
library(DHARMa)

rm(list = ls())


fa_percent_long <-
  left_join(fa_percent, factors) %>%
  rename(name = FAME) %>%
  write_csv('data/fa_percent_long.csv')

lmms <-  
  fa_percent_long %>% 
  drop_na() %>% 
  group_by(name) %>% 
  mutate(Age = factor(Age)) %>% 
  nest() %>% 
  mutate(lmms = map(data, ~lmerTest::lmer(log(value + 1)~Season*Age+(1|Location/Round), data = .x)),
         lmms_table = map(lmms, tidy),
         anova_tables = map(lmms, ~tidy(anova(.x))),
         pairwise = map(lmms, ~emmeans(.x, ~Age|Season)),
         pairs = map(pairwise, ~pairs(.x) %>% as_tibble()),
         p = map(lmms, ~simulateResiduals(.x, plot = F)))

# summary tables----
lmm_tables <- 
  lmms %>%
  select(lmms_table) %>%
  unnest(cols = c(lmms_table)) %>% 
  mutate(p.value  = scales::pvalue(p.value))

lmm_tables %>% write_csv('tables/lmms_summary_tables_fa_percent.csv')

anova_tables_fa_percent <- 
  lmms %>%
  select(anova_tables) %>%
  unnest(cols = c(anova_tables)) %>% 
  mutate(p.value  = scales::pvalue(p.value)) %>% 
  mutate(across(is.numeric, ~round(.x, 2)))

anova_tables_fa_percent %>% write_excel_csv('tables/anova_tables_fa_percent.csv')

pairwise_tables_fa_percent <- 
  lmms %>%
  select(pairs) %>%
  unnest(cols = c(pairs)) %>% 
  mutate(p.value  = scales::pvalue(p.value)) %>% 
  mutate(across(is.numeric, ~round(.x, 2))) %>% 
  write_excel_csv('tables/pairwise_fa_percent.csv')

all_pairwise <- 
lmms %>%
  select(lmms) %>%
  mutate(
    pairwise = map(lmms, ~ emmeans(.x, ~ Age * Season)),
    pairs = map(pairwise, ~ pairs(.x) %>% as_tibble())
  ) %>% 
  select(pairs) %>%
  unnest(cols = c(pairs)) %>% 
  mutate(p.value  = scales::pvalue(p.value)) %>% 
  mutate(across(is.numeric, ~round(.x, 2))) 
    
write_excel_csv(all_pairwise, 'tables/all_pairwise_fa_percent.csv')
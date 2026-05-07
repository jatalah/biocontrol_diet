library(tidyverse)
library(broom.mixed)
library(emmeans)
library(DHARMa)

# vars <-
#   c(
#     "fat g/m2",
#     "dry weight g/m2",
#     "protein g/m2",
#     "EPA_gm2",
#     "SFA_gm2",
#     "MUFA_gm2",
#     "PUFA_gm2",
#     "O3_PUFA_gm2"
#   )

d_long <- read_csv('data/clean_data_biofouling_long.csv')

lmms <-  
  d_long %>% 
  drop_na() %>% 
  group_by(name) %>% 
  mutate(Age = factor(Age)) %>% 
  nest() %>% 
  mutate(lmms = map(data, ~lmerTest::lmer(log(value)~Season*Age+(1|Location/Round), data = .x)),
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

lmm_tables %>% write_csv('tables/lmms_summary_tables_biofouling.csv')

anova_tables <- 
  lmms %>%
  select(anova_tables) %>%
  unnest(cols = c(anova_tables)) %>% 
  mutate(p.value  = scales::pvalue(p.value)) %>% 
  mutate(across(is.numeric, ~round(.x, 2)))

anova_tables %>% write_excel_csv('tables/anova_tables_biofouling.csv')

map(lmms$p, plot)

pairwise_tables <- 
  lmms %>%
  select(pairs) %>%
  unnest(cols = c(pairs)) %>% 
  mutate(p.value  = scales::pvalue(p.value)) %>% 
  mutate(across(is.numeric, ~round(.x, 2)))

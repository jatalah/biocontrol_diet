# Boxplot ---
library(tidyverse)
library(ggpubr)
library(readxl)
library(janitor)
library(lmerTest)
library(skimr)
library(broom.mixed)
library(scales)
library(DHARMa)
library(emmeans)
library(rstatix)
library(janitor)


theme_set(theme_javier(base_size = 9))

#rm(list = ls())

tbars <- read_excel('data/Snails_TBARS.xlsx', na = "NA") %>% 
  clean_names() %>% 
  mutate(treatment = if_else(sampling == "Baseline", "Baseline", treatment),
         treatment = fct_relevel(treatment, 'Baseline', 'Nil'),
         tank = factor(tank) %>% droplevels())
  


tbars_sums <- 
  tbars %>%
  select(mda_nmol_g_fw, sampling) %>% 
  filter(sampling=='Baseline') %>% 
  get_summary_stats(type = 'robust') 


# Baseline pairwise comparisons --------
pairwise_t_test(data = tbars, mda_nmol_g_fw ~ treatment, 
                      ref.group = 'Baseline', 
                      p.adjust.method = "none") |> 
  write_csv('tables/tbars_baseline_pairwise_comparisons.csv')



# filter out final data -----
final_tbars <- tbars %>% filter(sampling=='Final') %>% drop_na() 


#Box-plot of final data ---
ggplot(final_tbars, aes(treatment, mda_nmol_g_fw)) +
  geom_boxplot(alpha = .7, size = .25, outlier.size = .25) +
  geom_hline(data = tbars_sums, aes(yintercept = median), lty = 2, color = 'darkred') +
  geom_hline(data = tbars_sums, aes(yintercept = median - iqr), lty = 2, color = 'gray70') +
  geom_hline(data = tbars_sums, aes(yintercept = median + iqr), lty = 2, color = 'gray70') +
  labs(x = "Food treatment", y = "MDA (nmol / g FW)") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  )+ scale_x_discrete(labels = c("13" = "Low", "54" = "High"))


# save plot -----
ggsave(
  plot = last_plot(),
  filename = "figures/TBARS_boxplot.png",
  dpi = 300,
  width = 80,
  height = 60,
  units = "mm",
  bg = "White"
)


# LMM with tanks as random effect -----
summary(final_tbars)
tbars_lmm <- lmer(log(mda_nmol_g_fw)~treatment + (1|tank), data = final_tbars)
anova(tbars_lmm) %>% tidy() %>% write_csv('tables/LMM_tbars.csv')

emmeans(tbars_lmm, ~ treatment) %>% pairs() %>% as_tibble() %>% write_csv('tables/tbars_pairs.csv')




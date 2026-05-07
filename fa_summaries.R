library(readxl)
library(tidyverse)
library(ggpubr)

# read the fa percentage data 
fa_percent <- 
  read_excel("data/FAME master sheet.xlsx", 
                         sheet = "FAME for paper ", n_max = 34) %>% 
  pivot_longer(cols = -1, names_to = "Unique ID") %>% 
  rename(FAME = 1) %>% 
  mutate(`Unique ID`  = as.numeric(`Unique ID`)) %>% 
  drop_na()

# read the fa mg per 100 g data 
fa_mg <- 
  read_excel("data/FAME master sheet.xlsx", 
             sheet = "FAME for paper ", skip = 34) %>% 
  pivot_longer(cols = -1, names_to = "Unique ID") %>%
  rename(FAME = 1) %>% 
  mutate(`Unique ID`  = as.numeric(`Unique ID`)) %>% 
  drop_na()

# read and prepare factors----
factors <- read_excel("data/FAME master sheet.xlsx", 
                      sheet = "factors") %>% 
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

# join dataset for summaries----------
all_fa <- 
bind_rows(`% FA` = fa_percent, `mg/100g` = fa_mg, .id = 'Units') %>% 
  left_join(factors) %>% 
  select(-`Deployment time (days)`)

fa_sum_stats <- 
all_fa %>% 
  group_by(Units, FAME, Age, Season) %>% 
  get_summary_stats(value, type = 'common') %>% 
  select(-variable)

write_excel_csv(fa_sum_stats, 'tables/all_fame_summary_stats.csv')

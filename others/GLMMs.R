library(glmmTMB)

betareg_scaler <-
  function(y) {
    n <- length(!is.na(y))
    (y / 100 * (n - 1)  + 0.5) / n
  }

glmms <-  
  d_long %>% 
  drop_na() %>% 
  group_by(name) %>% 
  mutate(Age = factor(Age)) %>% 
  nest() %>% 
  mutate(lmms = map(data, ~glmmTMB(value~Season*Age+(1|Location/Round), family = Gamma(link = "log"), data = .x)),
         lmms_table = map(lmms, tidy),
         # anova_tables = map(lmms, ~tidy(anova(.x))),
         pairwise = map(lmms, ~emmeans(.x, ~Age|Season)),
         pairs = map(pairwise, ~pairs(.x)),
         p = map(lmms, ~simulateResiduals(.x, plot = F)))


glmm_tables <- 
  glmms %>%
  select(lmms_table) %>%
  unnest(cols = c(lmms_table)) %>% 
  mutate(p.value  = scales::pvalue(p.value))

rm(list = ls())

# Load libraries

library(tidyverse)
library(lfe)
library(broom)
library(pbapply)

## Function to tidy felm output

tidy_felm <- function (model, add_glance = T, add_dv_stats = T, add_conf_90 = T) 
{
  n <- sum(!is.na(model$residuals))
  m_tidy <- broom::tidy(model, conf.int = T)
  out <- m_tidy %>% mutate(n = n)
  if (add_conf_90) {
    out <- out %>% mutate(conf.low90 = estimate - qnorm(0.95) * 
                            std.error, conf.high90 = estimate + qnorm(0.95) * 
                            std.error)
  }
  dv <- model$formula %>% as.character() %>% str_split(" ~ ", 
                                                       simplify = T) %>% .[2]
  dv_mean <- model %>% augment() %>% pull(!!dv) %>% mean(na.rm = T)
  dv_sd <- model %>% augment() %>% pull(!!dv) %>% sd(na.rm = T)
  dv_min <- model %>% augment() %>% pull(!!dv) %>% min(na.rm = T)
  dv_max <- model %>% augment() %>% pull(!!dv) %>% max(na.rm = T)
  if (add_dv_stats) {
    out <- out %>% mutate(dv_mean = dv_mean, dv_sd = dv_sd, 
                          dv_min = dv_min, dv_max = dv_max)
  }
  if (add_glance) {
    g <- model %>% glance() %>% dplyr::rename(f_stat = statistic, 
                                              f_pval = p.value) %>% dplyr::select(matches("squared|f_"))
    out <- out %>% mutate(rsq = g$r.squared, a_rsq = g$adj.r.squared, 
                          f_stat = g$f_stat, f_pval = g$f_pval)
    out
  }
  else {
    out
  }
}

### Get GLES

gles <- read_rds("data/gles_panel.rds")%>%
  mutate(hannover_dist = as.numeric(scale(hannover_dist))) %>% 
  mutate(state = bula) %>% 
  mutate(first_date_wave = as.Date("2014-01-01"))

## Subset

gles_subset <- gles %>% 
  dplyr::select(one_of(c('vote_afd_2', 'hannover_dist', 
                         'female', 'age', 
                         'unemployed', 'income', 'edu', 'rural', 'east', 
                         'attach_local', 'attach_nation', 
                         'id', 'wave')))

#### Over time ####

outvars <- colnames(gles) %>%
  .[str_detect(., 'afd|mig|multi|populism|pop_people')]

reg_list1 <- outvars %>% 
  pblapply(., function(v) {
    
    ## All
    
    print(v)
    
    
    ## Wave FE, cov
    
    f3 <- as.formula(paste0(v, '~hannover_dist + female + age + unemployed + 
                            log(income) + factor(edu) + factor(rural) + 
                            factor(east) | wave | 0 |id'))
    m3 <- felm(f3, data = gles) %>% 
      tidy_felm() %>%
      mutate(outcome = v, method= 'East + Wave FE, Cov')
    
    m3
    
  }) %>% reduce(bind_rows) %>% 
  filter(term == 'hannover_dist')

##

## Rename variables 

reg_list1 <- reg_list1 %>% 
  mutate(outcome = dplyr::recode(outcome,
                                 `mig_limit` = 'Limit Immigration',
                                 `mig_salience` = 'Salience of immigration',
                                 `multiculturalism` = 'Support for Multiculturalism'))

## Save this

write_rds(reg_list1,
          file = 'data/gles_attitude_results.rds')

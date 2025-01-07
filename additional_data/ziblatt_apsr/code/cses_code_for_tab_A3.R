rm(list = ls())

library(tidyverse)
library(broom)

## Get data

cses <- read_rds("data/cses.rds")

## Define variable labels

var.labels <- data.frame(outcome_lab = c("ATTITUDES ABOUT ELITES: ARE TRUSTWORTHY", 
                                         "ATTITUDES ABOUT ELITES: PEOPLE SHOULD MAKE POLICY DECISIONS", 
                                         "OUT-GROUP ATTITUDES: MINORITIES - CUSTOMS AND TRADITIONS", 
                                         "OUT-GROUP ATTITUDES: CULTURE HARMED BY IMMIGRANTS"),
                         outcome = c("E3004_3", "E3004_6", "E3005_1", "E3005_4"))

## Models

outvars <- var.labels$outcome

## Second part of formula

secpart <- ') ~ scale(hannover_dist) + gender + education + employment_status +
pop_den_km2 + unem_rate_2017 + hh_inc_quantile| region | 0 | region'

## loop over outcomes

out_reg <- pblapply(outvars, function(v) {
  
  ## Make formula and estimate model
  
  f <- as.formula(paste0('scale(', v, secpart))
  m1 <- felm(f, data = cses)
  n <- sum(!is.na(m1$residuals))
  
  ## Tidy
  
  m1 <- m1 %>%
    broom::tidy(conf.int = T) %>%
    filter(term == 'scale(hannover_dist)') %>%
    mutate(outcome = v, covars = 'covars', n = n)
  
  ## Return this
  
  m1
  
}) %>%
  reduce(rbind)

## Rename

out_reg <- out_reg %>% 
  left_join(var.labels) %>% 
  dplyr::select(-outcome) %>% 
  dplyr::rename(outcome = outcome_lab)

## Prep for saving / renaming ##

out_reg_save <- out_reg %>%
  dplyr::select(estimate, std.error, p.value, outcome, n)
colnames(out_reg_save) <- c('Estimate', 'SE', 'P-Val', 'Outcome', 'N')

## Save 

write_rds(out_reg_save, path = 'data/cses_results.rds')







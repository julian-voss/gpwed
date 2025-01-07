rm(list = ls())

##

library(CBPS)
library(cobalt)
library(tidyverse)
library(lfe)
library(broom)
library(stargazer)

## Load the data

df <- read_rds('data/data_main.rds') %>% 
  mutate(east = ifelse(as.numeric(state) > 11, 1, 0)) %>% 
  mutate(rb = substr(ags_2017, 1, 3))

#### Table A.8: Robustness ####

m1 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital  | state | 0 | 0,
           data = df)
m2 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital + any_prog_20s| state | 0 | 0,
           data = df)
m3 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital + nsdap33| state | 0 | 0,
           data = df)
m4 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | 0,
           data = df, subset = east == 0)
m5 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | 0,
           data = df, subset = east == 1)

## Make the final table 

model_list <- list(m1, m2, m3, m4, m5)
model_list %>% lapply(broom::tidy) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, 'hannover_dist'))

## Get mean and SD of DV 

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)
dv_sd <- sapply(model_list, function(m) sd(m$response, na.rm = T)) %>%
  round(2)
n <- model_list %>% 
  sapply(function(x) length(x$response))
n

## Return

stargazer(model_list, keep = 'hannover_dist',
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          covariate.labels = 'Similarity to Standard German',
          add.lines = list(c('Mean of DV', dv_means)),
          column.labels = c('Baseline', 'Control: 1920s progroms',
                            'Control: NSDAP voting',
                            'West Germany',
                            'East Germany'))

## w/ control coefficients

stargazer(model_list, 
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          covariate.labels = 'Similarity to Standard German',
          add.lines = list(c('Mean of DV', dv_means)),
          column.labels = c('Baseline', 'Control: 1920s progroms',
                            'Control: NSDAP voting',
                            'West Germany',
                            'East Germany'))



#### Table A.9: Jaro-Winkler distance ####

m1 <- felm(afd_party_17 ~ scale(hannover_dist) | 0 | 0 | 0,
           data = df)
m2 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital | state | 0 | 0,
           data = df)
m3 <- felm(afd_party_17 ~ scale(hannover_dist_jw) | 0 | 0 | 0,
           data = df)
m4 <- felm(afd_party_17 ~ scale(hannover_dist_jw) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | 0,
           data = df)
m5 <- felm(afd_party_17 ~ scale(hannover_dist_jw) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | 0,
           data = df, subset = east == 0)
m6 <- felm(afd_party_17 ~ scale(hannover_dist_jw) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | 0,
           data = df, subset = east == 1)

## List of models   

model_list <- list(m1, m2, m3, m4, m5, m6)
model_list %>% lapply(broom::tidy) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, 'hannover_dist'))

## Get mean and SD of DV

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)
dv_sd <- sapply(model_list, function(m) sd(m$response, na.rm = T)) %>%
  round(2)

## Return as table 

stargazer(model_list, keep = 'hannover_dist',
          covariate.labels = c('Dialectal distance', 
                               'Dialectal distance (Jaro-Winkler)'),
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV', dv_means)))

## w/ control coefficients

stargazer(model_list, 
          covariate.labels = c('Dialectal distance', 
                               'Dialectal distance (Jaro-Winkler)'),
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV', dv_means)))

#### Table A.10: Administrative district FE ####

m1 <- felm(afd_party_17 ~ scale(hannover_dist) | 0 | 0 | 0,
           data = df)
m2 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital | rb | 0 | 0,
           data = df)
m3 <- felm(afd_party_17 ~ scale(hannover_dist_jw) | 0 | 0 | 0,
           data = df)
m4 <- felm(afd_party_17 ~ scale(hannover_dist_jw) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| rb | 0,
           data = df)

## list of models

model_list <- list(m1, m2, m3, m4)

## Mean and SD of DV

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)
dv_sd <- sapply(model_list, function(m) sd(m$response, na.rm = T)) %>%
  round(2)

## Table ##

stargazer(model_list, keep = 'hannover_dist',
          covariate.labels = c('Dialectal distance', 
                               'Dialectal distance (Jaro-Winkler)'),
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV', dv_means)))

## w/ control coefficients

stargazer(model_list,
          covariate.labels = c('Dialectal distance', 
                               'Dialectal distance (Jaro-Winkler)'),
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV', dv_means)))

#### Table A.12: CBPS weights ####

## Declare controls

controls2 <- c('pop_density', 
               'pop_total', 
               'gdp_nominal_2016', 
               'wage_nominal2016', 
               'relig_cath_2011', 
               'unemp_rate_tot', 
               'cdu_csu_party_13', 
               'commuters_capita_in_2017', 
               'dist_to_state_capital')

## Remove missings

use_cases <- complete.cases(df[, c(controls2, 'afd_party_17')])

## Get weights 

w_cbps <- CBPS(scale(hannover_dist) ~
                 pop_total +
                 relig_cath_2011+
                 unemp_rate_tot + 
                 cdu_csu_party_13 +
                 commuters_capita_in_2017 +
                 dist_to_state_capital + 
                 state,
               method = 'exact',
               data = df[use_cases, ])

## Extract weights

w_cbps <- w_cbps$weights

## Estimate models with weights

m1 <- felm(afd_party_17 ~ scale(hannover_dist)  | state | 0 | 0,
           data = df[use_cases, ], weights = w_cbps)
m2 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital  | state | 0 | 0,
           data = df[use_cases, ], weights = w_cbps)

## only West

w_cbps <- CBPS(scale(hannover_dist) ~
                 pop_total +
                 relig_cath_2011+
                 unemp_rate_tot + 
                 cdu_csu_party_13 +
                 commuters_capita_in_2017 +
                 dist_to_state_capital + 
                 state,
               method = 'exact',
               data = df[use_cases & df$east == 0, ])

w_cbps <- w_cbps$weights

## Model w/ weights

m3 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital  | state | 0 | 0,
           data = df[use_cases & df$east == 0, ], weights = w_cbps)

## only East  

w_cbps <- CBPS(scale(hannover_dist) ~
                 pop_total +
                 relig_cath_2011+
                 unemp_rate_tot + 
                 cdu_csu_party_13 +
                 commuters_capita_in_2017 +
                 dist_to_state_capital + 
                 state,
               method = 'exact',
               data = df[use_cases & df$east == 1, ])

w_cbps <- w_cbps$weights

## Model w/ weights

m4 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital  | state | 0 | 0,
           data = df[use_cases & df$east == 1, ], weights = w_cbps)

## Models list

model_list <- list(m1, m2, m3, m4)

## DV means

dv_means <- sapply(model_list, function(m) weighted.mean(m$response, 
                                                         m$weights,
                                                         na.rm = T)) %>%
  round(2)

## Output

stargazer(model_list, keep = 'hannover_dist',
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          covariate.labels = 'Dialectal distance',
          add.lines = list(c('Mean of DV', dv_means)), 
          align = T, font.size = 'footnotesize')

## w/ control coefficients

stargazer(model_list, 
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          covariate.labels = 'Dialectal distance',
          add.lines = list(c('Mean of DV', dv_means)), 
          align = T, font.size = 'footnotesize')


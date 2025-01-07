library(tidyverse)
library(lfe)
library(stargazer)

## 

rm(list = ls())

## Load the data

df <- read_rds('data/data_main.rds') %>% 
  mutate(rb = substr(ags_2017, 1, 3)) %>% 
  mutate(hannover_dist_hist = max(hannover_sim, na.rm = T) - 
           hannover_sim) 

#### Table 1: Effects of dialectal distance on radical right voting in 2017 ####

vlist <- c('pop_density', 'gdp_nominal_2016',
           'pop_total', 'wage_nominal2016' , 'relig_cath_2011',
           'unemp_rate_tot', 'commuters_capita_in_2017',
           'dist_to_state_capital', 'cdu_csu_party_13',
           'dist_hannover_km')

## Models

m0 <- felm(afd_party_17 ~ scale(hannover_dist) | 0 |0 | ags_2017, 
           data = df)
m1 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | ags_2017,
           data = df)
m2 <- felm(afd_party_17 ~ scale(hannover_dist_hist) | 0 |0 | ags_2017, 
           data = df)
m3 <- felm(afd_party_17 ~ scale(hannover_dist_hist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | ags_2017,
           data = df)

## Table

model_list <- list(m0, m1, m2, m3)

## Mean and SD of DV

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)
dv_sd <- sapply(model_list, function(m) sd(m$response, na.rm = T)) %>%
  round(2)

## Return as table 

stargazer(model_list, keep = 'hannover_dist',
          keep.stat = c('rsq', "adj.rsq", 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV',
                             dv_means),
                           c('State FE', rep(c("No", "Yes"), 2)),
                           c('Covariates', rep(c("No", "Yes"), 2))),
          covariate.labels = c('Dialectal distance (contemporary)',
                               'Dialectal distance (historical)'))

## Full table (w/ control coefficients)

stargazer(model_list, 
          keep.stat = c('rsq', "adj.rsq", 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV',
                             dv_means),
                           c('State FE', rep(c("No", "Yes"), 2)),
                           c('Covariates', rep(c("No", "Yes"), 2))),
          covariate.labels = c('Dialectal distance (contemporary)',
                               'Dialectal distance (historical)'))

#### Table A.4: main results + covariates ####

## Scale all covariates for the table

df_ta4 <- df %>% 
  mutate(across(all_of(vlist), scale))

## Models

m0 <- felm(afd_party_17 ~ scale(hannover_dist) | 0 |0 | ags_2017, 
           data = df_ta4)
m1 <- felm(afd_party_17 ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | ags_2017,
           data = df_ta4)
m2 <- felm(afd_party_17 ~ scale(hannover_dist_hist) | 0 |0 | ags_2017, 
           data = df_ta4)
m3 <- felm(afd_party_17 ~ scale(hannover_dist_hist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital| state | 0 | ags_2017,
           data = df_ta4)

## Table

model_list <- list(m0, m1, m2, m3)

## Mean of DV

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round

## Table

stargazer(model_list,
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV',
                             dv_means),
                           c('State FE', rep(c("No", "Yes"), 2)),
                           c('Covariates', rep(c("No", "Yes"), 2))),
          covariate.labels = c('Dialectal distance (contemporary)',
                               'Pop. density / km2', 
                               'Tot. population',
                               'Nominal GDP (EUR)',
                               'Nominal Wage (EUR)',
                               'Share Catholic',
                               'Unemployment rate (\\%)', 
                               'CDU/CSU vote share, 2013',
                               'In-commuters / capita',
                               'Dist. to state capital (km)'))

#### Table A.11: control coefs, dist. to Hannover, dist. to the border ####

controls2 <- c('hannover_dist',
               'pop_density',
               'pop_total',
               'gdp_nominal_2016',
               'wage_nominal2016',
               'relig_cath_2011',
               'unemp_rate_tot', 
               'cdu_csu_party_13',
               'commuters_capita_in_2017',
               'dist_to_state_capital',
               'dist_hannover_km',
               'dist_to_border')

## Scale all covars

df_scaled <- df %>% 
  mutate_at(vars(one_of(controls2)), scale)

## Proper labels

controls2_proper <- c('Dialectal distance',
                      'Pop. density / km2', 
                      'Tot. population',
                      'Nominal GDP (EUR)',
                      'Share Catholic',
                      'Unemployment rate (\\%)', 
                      'CDU/CSU vote share, 2013',
                      'In-commuters / capita',
                      'Dist. to state capital (km)',
                      'Dist. to Hannover (km)',
                      'Dist. to Border (km)')

## Run regressions

m0 <- felm(afd_party_17 ~ hannover_dist | 0 |0 | 0, 
           data = df_scaled)
m01 <- felm(afd_party_17 ~ hannover_dist + 
              dist_hannover_km | 0 |0 | 0, 
            data = df_scaled)
m02 <- felm(afd_party_17 ~ hannover_dist + 
              dist_to_border | 0 |0 | 0, 
            data = df_scaled)
m03 <- felm(afd_party_17 ~ hannover_dist + dist_to_border + 
              dist_hannover_km| 0 |0 | 0, 
            data = df_scaled)
m4 <- felm(afd_party_17 ~ hannover_dist + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             commuters_capita_in_2017 +
             dist_to_state_capital + 
             dist_hannover_km +
             dist_to_border | state | 0 | 0,
           data = df_scaled)

## List of models 

model_list <- list(m0, m01, m02, m03, m4)

## Return as table

stargazer(model_list, keep = controls2,
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('State FE', rep('No', 4), 
                             rep('Yes', 1))),
          covariate.labels = controls2_proper)

#### Table A.7: Effects of dialectal distance on support for all parties ####

## Def variables

dv_list <- c("cdu_csu_party_17", "spd_party_17", "left_party_17", 
             "greens_party_17", 
             "fdp_party_17", "afd_party_17", "other_party_17")
parties <- c('CDU/CSU', 'SPD', 'Left', 'Greens', 'FDP', 'AfD', 'Other')

## Loop to run regression with different parties as outcomes

model_list <- dv_list %>% lapply(function(p) {
  
  df[, 'o'] <- df[, p]
  
  m1 <- felm(o ~ scale(hannover_dist) + 
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
  
  m1
  
})

## Mean and SD of DV

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)
dv_sd <- sapply(model_list, function(m) sd(m$response, na.rm = T)) %>%
  round(2)

## Return as table 

stargazer(model_list, keep = 'hannover_dist',
          column.labels = parties,
          model.numbers = F,
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'Vote share, 2017',
          add.lines = list(c('Mean of DV',
                             dv_means),
                           c('State FE', rep('Yes', 7)),
                           c('Covariates', rep('Yes', 7))),
          covariate.labels = 'Dialectal distance')

## w/ control coefficients

stargazer(model_list,
          column.labels = parties,
          model.numbers = F,
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'Vote share, 2017',
          add.lines = list(c('Mean of DV',
                             dv_means),
                           c('State FE', rep('Yes', 7)),
                           c('Covariates', rep('Yes', 7))),
          covariate.labels = 'Dialectal distance')

#### Table A.6:  Effects of dialectal distance on radical right voting ####
##               in three elections 

m1 <- felm(afd_party_13 ~ scale(hannover_dist) + 
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
m2 <- felm(afd_party_17 ~ scale(hannover_dist) + 
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
m3 <- felm(afd_party_21 ~ scale(hannover_dist) + 
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

## Make the Table 

model_list <- list(m1, m2, m3)

## Mean and SD of DV

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)
dv_sd <- sapply(model_list, function(m) sd(m$response, na.rm = T)) %>%
  round(2)

## Return as table 

stargazer(model_list, keep = 'dist',
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV',
                             dv_means),
                           c('State FE', rep('Yes', 3)),
                           c('Covariates', rep('Yes', 3))),
          covariate.labels = 'Dialectal distance')

## w/ control coefficients 

stargazer(model_list,
          keep.stat = c('rsq', 'n'), style = 'ajps',
          dep.var.labels = 'AfD Vote share, 2017',
          add.lines = list(c('Mean of DV',
                             dv_means),
                           c('State FE', rep('Yes', 3)),
                           c('Covariates', rep('Yes', 3))),
          covariate.labels = 'Dialectal distance')


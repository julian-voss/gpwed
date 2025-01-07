rm(list = ls())

# Load libraries

library(tidyverse)
library(lfe)
library(broom)

## Get the saved GLES results
## Code that produces these results is provided in file gles_regressions_for_tab_A3.R

res_gles <- read_rds('data/gles_attitude_results.rds') %>% 
  dplyr::select(outcome, estimate, std.error, matches('conf'), 
                p.value, n, method, dv_sd) 

## Prepare for plotting

res_gles_plot <- res_gles %>% 
  filter(outcome %in% c("populism", "pop_people", "Limit Immigration", 
                        "Support for Multiculturalism")) %>% 
  filter(str_detect(method, 'Cov') & 
           str_detect(method, 'East') & 
           str_detect(method, 'Wave')) %>% 
  mutate_at(vars(estimate, conf.low, conf.high),
            list(~. / dv_sd)) %>% 
  mutate(outcome = dplyr::recode(outcome,
                                 `populism` = 'Populism scale',
                                 `pop_people` = 'People should make policy',
                                 `Limit Immigration` = 'Limit immigration',
                                 `Support for Multiculturalism` = 'Oppose multiculturalism')) %>% 
  mutate(outcome = paste0(outcome, '\n(N = ', format(n, big.mark = ','), ')')) %>% 
  mutate_at(vars(estimate, matches('conf')),
            list(~ifelse(str_detect(outcome, 'multi'), -1*., .))) %>% 
  mutate(g = rep(c('Out-group\nhostility', 'Anti-elitism'), each = 2))

## Saved SOEP results on social status ##
## For details on the estimation, see manuscript -- SOEP data can not be provided

res_soep <- read_rds("data/soep_status_results.rds") %>% 
  mutate_at(vars(estimate, conf.low, conf.high),
            list(~. / dvsd)) %>% 
  mutate(outcome = dplyr::recode(model, `State FE` = 'Social status (no controls)',
                                 `County Covs + Indiv Covs + State FE` = 
                                   'Social status w/ county- and\nindividual-level controls')) %>% 
  mutate(outcome = paste0(outcome, "\n(N = ", format(n, big.mark = ','),')')) %>% 
  mutate(outcome = factor(outcome, levels = outcome[c(2,1)])) %>% 
  mutate(g = rep(c('Social status'), each = 2))

## Combine before plotting

res_full <- bind_rows(res_soep, res_gles_plot) %>% 
  mutate(g = factor(g, levels = unique(g)[c(1,3,2)]))

##### Figure 3: Dialectal distance, self-perceived social status, anti-elitism, and out-group hostility ####

p2 <- res_full %>% 
  ggplot(aes(outcome, estimate)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 2) +
  theme_bw() +
  coord_flip() +
  ylab('Effect of dialectal distance\n(standard deviations)') +
  facet_grid(rows = vars(g), scales = 'free_y') +
  theme(axis.title.y = element_blank())
p2

rm(res_full, res_gles_plot, p2)

## Load CSES results ##

cses <- read_rds('data/cses_results.rds') %>% 
  mutate(source = 'CSES') %>% 
  mutate(Year = 2017)

vars_cses <- c('ATTITUDES ABOUT ELITES: PEOPLE SHOULD MAKE POLICY DECISIONS',
               'ATTITUDES ABOUT ELITES: ARE TRUSTWORTHY',
               'OUT-GROUP ATTITUDES: MINORITIES - CUSTOMS AND TRADITIONS',
               'OUT-GROUP ATTITUDES: CULTURE HARMED BY IMMIGRANTS')

## Reverse estimates (outcome are reverse coded)

cses_use <- lapply(X = vars_cses, FUN =  function(x) cses[cses$Outcome == x, ]) %>%
  reduce(rbind) %>%
  mutate(Estimate = -Estimate) %>%
  mutate(Outcome = str_remove(Outcome, 'ATTITUDES ABOUT ELITES: |OUT-GROUP ATTITUDES: ')) %>%
  dplyr::select(-`P-Val`) %>% 
  rename(Source = source) %>% 
  dplyr::select(everything()/one_of(c('Year', 'Source')), Year, Source)
cses_use

## Gles results for the table ##

res_gles_table <- res_gles %>% 
  filter(outcome %in% c("Salience of immigration", "Limit Immigration", 
                        "Support for Multiculturalism")) %>% 
  filter(str_detect(method, 'Cov') & 
           str_detect(method, 'East') & 
           str_detect(method, 'Wave')) %>% 
  mutate_at(vars(estimate, conf.low, conf.high),
            list(~. / dv_sd)) %>% 
  mutate(outcome = dplyr::recode(outcome,
                                 `populism` = 'Populism scale',
                                 `pop_people` = 'People should make policy',
                                 `Limit Immigration` = 'Limit immigration',
                                 `Support for Multiculturalism` = 'Support for multiculturalism')) %>% 
  mutate(outcome = paste0(outcome, '\n(N = ', format(n, big.mark = ','), ')')) 

res_gles_table %>% 
  dplyr::select(outcome, estimate, std.error, n)

## Migration and commuting distance results

## Load the data
## Standardize variable of interest

df <- read_rds('data/data_main.rds') %>% 
  mutate(east = ifelse(as.numeric(state) > 11, 1, 0)) %>% 
  mutate_at(vars(commuters_capita_in_2017, 
                 mig_combined_capita,
                 mig_in_capita,
                 mig_out_capita),
            list(~(. - mean(., na.rm = T)) / sd(., na.rm = T)))

## Run models

m1 <- felm(scale(mig_out_capita) ~ scale(hannover_dist) + 
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
m2 <- felm(scale(mig_in_capita) ~ scale(hannover_dist) + 
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

## Commuting distance models

m3 <- felm(scale(commuters_in_dist_2017) ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             dist_to_state_capital  | state | 0 | 0,
           data = df)
m4 <- felm(scale(commuters_out_dist_2017) ~ scale(hannover_dist) + 
             pop_density +
             pop_total +
             gdp_nominal_2016+
             wage_nominal2016+
             relig_cath_2011+
             unemp_rate_tot + 
             cdu_csu_party_13 +
             dist_to_state_capital  | state | 0 | 0,
           data = df)

model_list <- list(m2, m1, m3, m4)
res_scope <- model_list %>% lapply(broom::tidy) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, 'hannover_dist')) %>% 
  mutate(n = 392, year = 2017, source = "Official data") %>% 
  dplyr::select(estimate, std.error, n, year, source) %>% 
  mutate(outcome = c("In-migration", 
                     "Out-migration", 
                     "Avg commuting distance (in)",
                     "Avg commuting distance (out)")) %>% 
  dplyr::select(outcome, everything())

#### Table A.3 Correlate of dialectal distance ####

## Rows 1-4
res_scope

# Rows 5 - 7 
res_gles_table %>% 
  dplyr::select(outcome, estimate, std.error, n) %>% 
  mutate(year = 'multiple', source = "Gles")

# Rows 8 - 11

cses_use %>% 
  slice(4, 3, 1, 2) %>% 
  dplyr::select(Outcome, Estimate, SE, N, Year, Source)


library(tidyverse)
library(lfe)
library(readstata13)
library(pbapply)

## 

rm(list = ls())

## Read main data set (provided w/ replication code)

df <- read_rds('data/data_main.rds') %>% 
  mutate(rb = substr(ags_2017, 1, 3)) %>% 
  mutate(hannover_dist = scale(hannover_dist))

## Get SOEP data (not provided w/ replication code)

soep <- readstata13::read.dta13('data/SOEP.dta', convert.factors = F) %>% 
  filter(!is.na(hid)) %>% 
  filter(syear > 2015)

## Add KKZ (this is only available at the DIW in Berlin)

kkz <- readstata13::read.dta13('data/kkz_2016.dta') %>% 
  mutate(kkz = as.character(kkz),
         kkz = ifelse(nchar(kkz) == 4, paste0('0', kkz), kkz))

## Merge

soep <- soep %>% 
  left_join(kkz)

## Get the dialect vars

v_dial <- colnames(soep) %>% 
  .[str_detect(., 'dial')]

## Check

v_dial %>% 
  lapply(function(x) table(soep[, x]))

## Negative needs to be removed

soep <- soep %>% 
  mutate_at(vars(one_of(v_dial)),
            list(~ifelse(.<0, NA, .)))

## Def vars

yvars <- c("p_afd")
treatvars <- c('dialect_work', 'dialect')
cvars <- c('income', 'unemployed', 'age', 'edu_time', 'sex', 'polint')
cvars_county <- c('pop_density', 'gdp_nominal_2016',
                  'pop_total', 'wage_nominal2016' , 'relig_cath_2011',
                  'unemp_rate_tot', 
                  'dist_to_state_capital', 'cdu_csu_party_13',
                  'share_abitur_2011', 'dist_hannover_km')

## Merge the main data

soep <- left_join(soep, df %>% dplyr::rename(kkz = ags_2017))

## Regressions

df_use <- soep

## First treatment: dialect:

treatvar <- "dialect"

f1 <-  paste('p_afd~', treatvar, '|0|0|0') %>% 
  as.formula()
f2 <- paste('p_afd~', treatvar,  '+', paste0(c(cvars, cvars_county), 
                                           collapse = '+'),
            '|state+syear|0|0') %>% 
  as.formula()
f3 <- paste('p_afd ~ ', treatvar,  '+hannover_dist+' 
            , paste0(c(cvars, cvars_county), 
                     collapse = '+'),
            '|state+syear|0|0') %>% 
  as.formula()

## Estimate models

m1 <- felm(f1, data = df_use)
m2 <- felm(f1, data = df_use)
m3 <- felm(f1, data = df_use)

## Second treatment : dialect at work

treatvar <- "dialect_work"

f4 <-  paste('p_afd~', treatvar, '|0|0|0') %>% 
  as.formula()
f5 <- paste('p_afd~', treatvar,  '+', paste0(c(cvars, cvars_county), 
                                             collapse = '+'),
            '|state+syear|0|0') %>% 
  as.formula()
f6 <- paste('p_afd ~ ', treatvar,  '+hannover_dist+' 
            , paste0(c(cvars, cvars_county), 
                     collapse = '+'),
            '|state+syear|0|0') %>% 
  as.formula()

## Estimate models

m4 <- felm(f4, data = df_use)
m5 <- felm(f5, data = df_use)
m6 <- felm(f6, data = df_use)

## Return (w/ covariates)
#### Table A.18: Association between individual–dialect and AfD party preference (SOEP data) ####

stargazer(list(m1, m2, m3 ,m4 ,m5, m6))
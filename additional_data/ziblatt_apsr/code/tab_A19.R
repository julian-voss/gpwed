library(tidyverse)
library(lfe)
library(readstata13)
library(pbapply)
library(AER)

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

## Negative needs to be removed

soep <- soep %>% 
  mutate_at(vars(one_of(v_dial)),
            list(~ifelse(.<0, NA, .)))

## Construct instruments

## RB instrument

soep <- soep %>% 
  mutate(rb = substr(kkz, 1, 3)) %>% 
  group_by(rb) %>% 
  mutate(instrument_rb = mean(dialect_classmates, na.rm = T),
         instrument_rb_bin = mean(dialect_classmates > 3, na.rm = T)) %>% 
  ungroup()

# Define variables

yvars <- c("p_afd")
treatvars <- c('dialect_work', 'dialect')
instvars <- "instrument_rb"
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

## Treatment: speaks any dialect

treatvar <- "dialect"

f1 <- paste('p_afd~1|0|(', treatvar, '~instrument_rb',
            ')|0') %>% 
  as.formula()

f2 <- paste('p_afd~', paste0(c(cvars, cvars_county), 
                           collapse = '+'),
            '|state+syear|(', treatvar, '~instrument_rb',
            ')|kkz') %>%  
  as.formula()

m1 <- felm(f1, data = df_use)
m2 <- felm(f2, data = df_use)

## Treatment: dialect at work

treatvar <- "dialect_work"

f3 <- paste('p_afd~1|0|(', treatvar, '~instrument_rb',
            ')|0') %>% 
  as.formula()

f4 <- paste('p_afd~', paste0(c(cvars, cvars_county), 
                             collapse = '+'),
            '|state+syear|(', treatvar, '~instrument_rb',
            ')|kkz') %>%  
  as.formula()

m3 <- felm(f3, data = df_use)
m4 <- felm(f4, data = df_use)

## Return as table 

#### Table A.19: Association between individual–dialect and AfD party preference (SOEP data) – IV estimates ####

stargazer::stargazer(list(m1, m2, m3,m4))


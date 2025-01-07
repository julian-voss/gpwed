rm(list = ls())

## Libraries

library(tidyverse)
library(readstata13)
library(lfe)

## Load main data set 

df <- read_rds('data/data_main.rds') %>% 
  mutate(rb = substr(ags_2017, 1, 3)) %>% 
  mutate(hannover_dist = scale(hannover_dist))

## Get SOEP
## Note that this is not provided in the replication archive
## It can be produced with the .do file in the replication archive

soep <- read.dta13('data/SOEP.dta', convert.factors = F) %>% 
  filter(!is.na(hid))

## Add KKZ
## This file can only be accessed at the DIW (regionl.data)

kkz <- readstata13::read.dta13('data/regionl.dta') %>% 
  dplyr::select(hid, syear, kkz) %>% 
  mutate(kkz = as.character(kkz),
         kkz = ifelse(nchar(kkz) == 4, paste0('0', kkz), kkz))

## Merge

soep <- soep %>% 
  left_join(kkz)

## Get the dialect vars

v_dial <- c('dialect_work', 
            'dialect', 
            'dialect_classmates')

## Negative values need to be removed

soep <- soep %>% 
  mutate_at(vars(one_of(v_dial)),
            list(~ifelse(.<0, NA, .)))

#### Table A.17: Correlation between the aggregate-level and three individual-level dialect measures ####

cor(soep$dialect_work, soep$hannover_dist)
cor(soep$dialect, soep$hannover_dist)
cor(soep$dialect_classmates, soep$hannover_dist)
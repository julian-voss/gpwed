rm(list = ls())

## Libraries

library(tidyverse)
library(lfe)
library(readstata13)
library(pbapply)
library(broom)
library(ggsignif)
library(ggh4x)

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

## Looks ok

yvars <- v_dial
cvars <- c('income', 'unemployed', 'age', 'edu_time', 'sex', 'polint')
cvars_county <- c('pop_density', 'gdp_nominal_2016',
                  'pop_total', 'wage_nominal2016' , 'relig_cath_2011',
                  'unemp_rate_tot', 
                  'dist_to_state_capital', 'cdu_csu_party_13',
                  'share_abitur_2011', 'dist_hannover_km')

## Merge the main data

soep <- left_join(soep, df %>% dplyr::rename(kkz = ags_2017))

## Function

get_stats <- function(data, v) {
  
  x <- data[, v] %>% na.omit() %>% 
    as.numeric()
  
  m = mean(x); s <- sd(x); n = length(x); med = median(x)
  q25 = quantile(x, 0.25); q75 = quantile(x, 0.75)
  
  ## Return as vector
  
  c('mean' = m, 'sd' = s, 'n' = n, 'med' = med, 'q25' = q25, 'q75' = q75)
}

## List of data sets

ss_list <- list(soep,
                soep %>% filter(hannover_dist > 0),
                soep %>% filter(hannover_dist < 0))

ss_labs <- c('Full data', 
             'Above average dist. to Hannover',
             'Below average dist. to Hannover')

## Do

r <- lapply(seq_along(ss_list), function(i) {
  
  ss <- ss_list[[i]]
  ss_lab <- ss_labs[i]
  
  ## Get some summary stats ##  
  
  out_agg <- yvars %>% 
    lapply(get_stats, data = ss) %>% 
    reduce(bind_rows) %>% 
    cbind(yvars) %>% 
    mutate(ss = ss_lab)
  
}) %>% 
  reduce(bind_rows)

## Proceed

r <- r %>% 
  mutate(yvars = dplyr::recode(yvars,
                               `dialect` =  "Knows any dialect", 
                               `dialect_work` = 'Speaks dialect at work',
                               `dialect_classmates` = "Dialect prevalence among classmates")) %>% 
  mutate(ss = dplyr::recode(ss,
                            `Below average dist. to Hannover` = 'Counties with\nsmaller dialectal\ndistance',
                            `Above average dist. to Hannover` = 'Counties with\ngreater dialectal\ndistance',
                            `Full data` = 'Full sample')) %>% 
  mutate(yvars = factor(yvars, levels = unique(yvars)[c(1,2,3)]))%>% 
  mutate(ss = factor(ss, levels = unique(ss)[c(1:3)])) %>% 
  mutate(se_mean = sd / sqrt(n)) %>% 
  mutate(ci_high = mean + 1.96*se_mean,
         ci_low = mean - 1.95*se_mean)

## Significance

know_1 <- r$yvars == 'Knows any dialect' & 
  str_detect(r$ss, "greater") 
know_2 <- r$yvars == 'Knows any dialect' & 
  str_detect(r$ss, "smaller") 

speak_1 <- r$yvars == 'Speaks dialect at work' & 
  str_detect(r$ss, "greater") 
speak_2 <- r$yvars == 'Speaks dialect at work' & 
  str_detect(r$ss, "smaller") 

class_1 <- r$yvars == 'Dialect prevalence among classmates' & 
  str_detect(r$ss, "greater") 
class_2 <- r$yvars == 'Dialect prevalence among classmates' & 
  str_detect(r$ss, "smaller") 

t_know <- tsum.test(mean.x = r$mean[know_1], s.x = r$sd[know_1], 
                    n.x = r$n[know_1],
                    mean.y = r$mean[know_2], s.y = r$sd[know_2], 
                    n.y = r$n[know_2])
t_speak <- tsum.test(mean.x = r$mean[speak_1], s.x = r$sd[speak_1], 
                     n.x = r$n[speak_1],
                     mean.y = r$mean[speak_2], s.y = r$sd[speak_2], 
                     n.y = r$n[speak_2])
t_class <- tsum.test(mean.x = r$mean[class_1], s.x = r$sd[class_1], 
                     n.x = r$n[class_1],
                     mean.y = r$mean[class_2], s.y = r$sd[class_2], 
                     n.y = r$n[class_2])

p_know <- t_know$p.value; p_speak <- t_speak$p.value; p_class <- t_class$p.value; 

## Signif DF

annotation_df <- data.frame(
  yvars = unique(r$yvars),
  start = rep(unique(r$ss)[2], 3),
  end = rep(unique(r$ss)[3], 3),
  y = c(0.875, 0.45, 3.45),
  label = c("***", "***", "***")
)

## Custom scales per facet 

yvars <- unique(r$yvars) %>% 
  as.character()
ylist <- list(c(0, 1), 
              c(0, 0.55), 
              c(0, 3.8))
names(ylist) <- yvars

## Scales

scales <- lapply(ylist, function(limits) scale_y_continuous(limits = limits))

#### Figure A.7: Dialect prevalence at the individual level ####

ggplot(r, aes(ss, mean)) +
  geom_bar(stat = 'identity', fill = 'grey95', color = 'black', width = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.2) +
  geom_signif(
    data = annotation_df,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 5, vjust = 0.2, tip_length = 0.1,
    manual = TRUE
  ) +
  facet_wrap(~yvars, ncol = 1, scales = 'free_y') +
  facetted_pos_scales(y = scales) +
  xlab('Sample') + ylab("Average") +
  theme_bw()



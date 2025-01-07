rm(list = ls())

## Libraries

library(tidyverse)
library(broom)
library(pbapply)
library(lfe)

## Load the data

df <- read_rds('data/data_main.rds')

## Load bootstrap samples 
## See section A.7.1 for the construction of these samples

bs_dist <- read_rds('data/bootstrap_distances.rds')

## Iterate over bootstrap data sets

reg_list <- pblapply(1:length(bs_dist), function(i) {
  
  ## Define temp df, merge distance based on bootstrap sample
  
  df_temp <- df
  bs_dist_temp <- bs_dist[[i]] %>% 
    dplyr::rename(hannover_dist_bs = hannover_dist) %>% 
    dplyr::select(ags, hannover_dist_bs) %>% 
    mutate(ags = as.character(ags)) %>% 
    rename(ags_2017 = ags)
  
  ## Merge to temp df
  
  df_temp <- df_temp %>% 
    left_join(bs_dist_temp)

  ## Models
  
  m0 <- felm(afd_party_17 ~ scale(hannover_dist_bs) | 0 |0 | 0, 
             data = df_temp)
  m1 <- felm(afd_party_17 ~ scale(hannover_dist_bs) + 
               pop_density +
               pop_total +
               gdp_nominal_2016+
               wage_nominal2016+
               relig_cath_2011+
               unemp_rate_tot + 
               cdu_csu_party_13 +
               commuters_capita_in_2017 +
               dist_to_state_capital| state | 0 | 0,
             data = df_temp)
  
  ## Tidy estimates
  
  list(m0, m1) %>% 
    lapply(tidy) %>% 
    reduce(rbind) %>% 
    filter(str_detect(term, 'hannover_dist_bs')) %>% 
    mutate(covars = c('No covariates', 'Covariates'))
})

## Reduce to DF

reg_final <- reg_list %>% 
  reduce(rbind) %>% 
  mutate(covars = factor(covars, levels = unique(covars)[1:2]))

## Get mean and quantiles

mean_est <- reg_final %>% 
  group_by(covars) %>% 
  summarise(mean_est = mean(estimate),
            conf.low = quantile(estimate, 0.025),
            conf.high = quantile(estimate, 0.975))

#### Figure A.4: distribution of bootstrapped estimates ####

p1 <- reg_final %>% 
  ggplot(aes(x = estimate)) +
  geom_density(fill = 'grey95') +
  geom_vline(data = mean_est, aes(xintercept = mean_est), linetype = 'dotted') +
  facet_wrap(~covars, scales = 'free') +
  theme_bw() +
  xlab('Effect of dialectal distance on 2017 AfD vote share') +
  ylab('Frequency')
p1

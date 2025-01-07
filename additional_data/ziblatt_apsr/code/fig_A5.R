library(tidyverse) 
library(stargazer)
library(lfe)

## Clear

rm(list = ls())

## Load the data

df <- read_rds('data/data_main.rds')

## Main results

m0 <- felm(afd_party_17 ~ scale(hannover_dist) | 0 |0 | 0, 
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
             dist_to_state_capital| state | 0 | 0,
           data = df)

# With Pre-treatment variables

m1 <- lm(afd_party_17~ scale(hannover_dist) + 
           dist_to_state_capital,
         data = df)

# With Pre/Post-treatment variables

m1f <- lm(afd_party_17~
            scale(hannover_dist) + 
            dist_to_state_capital +
            #Post-treatment
            pop_density +
            pop_total +
            gdp_nominal_2016+
            wage_nominal2016+
            relig_cath_2011+
            unemp_rate_tot + 
            cdu_csu_party_13 +
            commuters_capita_in_2017,
          data = df)

# G-estimator 1st stage

m1s <- lm(I(afd_party_17 - 
              coef(m1f)['pop_density']*pop_density -
              coef(m1f)['pop_total']*pop_total -
              coef(m1f)['gdp_nominal_2016']*gdp_nominal_2016 - 
              coef(m1f)['wage_nominal2016']*wage_nominal2016 - 
              coef(m1f)['relig_cath_2011']*relig_cath_2011 -
              coef(m1f)['unemp_rate_tot']*unemp_rate_tot - 
              coef(m1f)['cdu_csu_party_13']*cdu_csu_party_13 - 
              coef(m1f)['commuters_capita_in_2017']*commuters_capita_in_2017) ~ 
            scale(hannover_dist) + 
            dist_to_state_capital,
          data = df)

#Full G-estimator

boots <- 1000
set.seed(123)

fl.boots <- matrix(NA, nrow=boots, ncol=3)

for(b in 1:boots){
  d.star <- df[sample(1:nrow(df), replace=TRUE),]
  
  cat(round(b * 100 / boots, 2))
  
  #G-estimator 1st stage
  
  boot.first <- lm(afd_party_17~
                     scale(hannover_dist) + 
                     dist_to_state_capital +
                     #Post-treatment
                     pop_density +
                     pop_total +
                     gdp_nominal_2016+
                     wage_nominal2016+
                     relig_cath_2011+
                     unemp_rate_tot + 
                     cdu_csu_party_13 +
                     commuters_capita_in_2017,
                   data = d.star)
  
  #G-estimator 2nd stage
  
  boot.direct <- lm(I(afd_party_17 - 
                        coef(m1f)['pop_density']*pop_density -
                        coef(m1f)['pop_total']*pop_total -
                        coef(m1f)['gdp_nominal_2016']*gdp_nominal_2016 - 
                        coef(m1f)['wage_nominal2016']*wage_nominal2016 - 
                        coef(m1f)['relig_cath_2011']*relig_cath_2011 -
                        coef(m1f)['unemp_rate_tot']*unemp_rate_tot - 
                        coef(m1f)['cdu_csu_party_13']*cdu_csu_party_13 - 
                        coef(m1f)['commuters_capita_in_2017']*commuters_capita_in_2017) ~ 
                      scale(hannover_dist) + 
                      dist_to_state_capital,
                    data = d.star)
  
  fl.boots[b,] <- coef(boot.direct)
}

# Extracting bootstrapped SEs

SEs1 <- apply(fl.boots, 2, sd)

## Make plot DF

plot_df <- data.frame(estimate = c(m0$coefficients[2],
                                   m1s$coefficients[2]),
                      se = c(summary(m0)$coefficients[2,2],
                             apply(fl.boots, 2, sd)[2]),
                      label = c('Base specification\n(no post-treatment covariates)',
                                'Sequential-g'),
                      stringsAsFactors = F)

## Print estimates ##

plot_df

#### Figure A.5: Comparing sequential-g estimates with unadjusted estimated ####

p1 <- ggplot(plot_df, aes(x = label, y = estimate)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = estimate - 1.96 * se,
                    ymax = estimate + 1.96 * se),
                width = 0) +
  geom_point(shape = 21, fill = 'white') +
  theme_bw() +
  ylab("Estimate") +
  theme(axis.title.y = element_blank()) +
  coord_flip()
p1

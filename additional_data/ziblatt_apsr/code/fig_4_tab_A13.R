library(tidyverse)
library(lfe)
library(sensemakr)
library(broom)
library(knitr)
library(kableExtra)

## 

rm(list = ls())

## Load the data
## Scale main treatment 

df <- read_rds('data/data_main.rds') %>% 
  mutate(hannover_dist = as.numeric(scale(hannover_dist)))

## Recode treatment - we need a binary treatment ## 
## Rename covars for plotting

df <- df %>% 
  mutate(hannover_dist_binary = ifelse(hannover_dist > 
                                         mean(hannover_dist, na.rm = T), 
                                       1, 0)) %>% 
  rename(Unemployment_Rate = unemp_rate_tot,
         Pop.density = pop_density,
         CDU_2013 = cdu_csu_party_13)

## Main model

ymodel <- lm(afd_party_17 ~ hannover_dist + 
               Pop.density +
               pop_total +
               gdp_nominal_2016+
               wage_nominal2016+
               relig_cath_2011+
               Unemployment_Rate + 
              CDU_2013 +
               commuters_capita_in_2017 +
               dist_to_state_capital +
               state,
             data = df)

## Check

ymodel %>% tidy() %>% 
  print(n = nrow(.))

## Run sensitivity analysis

ymodel$coefficients %>% names()
out <- sensemakr::sensemakr(model = ymodel, 
                            treatment = 'hannover_dist',
                            kd = c(1, 2),
                            benchmark_covariates= c('Unemployment_Rate'))
out$bounds

#### Figure 4: sensitivity analysis ####

plot(out, nlevels = 4, lim = 0.5, label.bump.x = 0.05)

## Table with additional details

out2 <- sensemakr::sensemakr(model = ymodel, 
                            treatment = 'hannover_dist',
                            kd = c(1),
                            benchmark_covariates = names(ymodel$coefficients)[c(-1:-2, -12:-24)])
s <- summary(out2)

## Table

vars <- names(ymodel$coefficients)[c(-1:-2, -12:-24)]
vars_proper <- c('Pop. density / km2', 'Tot. population',  'Nominal GDP',
                 'Nominal wage' , 'Share Catholic (2011)',
                 'Unemployment rate', 'CDU/CSU vote share 2017 (party)',
                 'Commuters / capita (2017)',
                 'Dist. to state capital (km)')

## Extract quantities 

r2_treat <- s$R2dz.x
r2_outcome <- s$R2yz.dx
estimate <- s$`Adjusted Estimate`
se <- s$`Adjusted Se`
t_adj <- s$`Adjusted T`

## Make table

tab_out <- data.frame(vars_proper, r2_treat, r2_outcome, estimate, se, t_adj,
                      stringsAsFactors = F) %>% 
  mutate_if(is.numeric, 
            list(~round(., 2)))

#### Table A.13: Sensitivity analysis – full results ####

kable(tab_out, "latex", longtable = F, 
      booktabs = T, col.names = c('',
                                  'Partial $R^2$ w.r.t. treatment',
                                  'Partial $R^2$ w.r.t. outcome', 
                                  'Adj. estimate',
                                  'Adj. SE',
                                  'Adj. t-stat'),
      linesep = "",
      caption = 'Sensitivity analysis\\label{tab:sumstats}',
      escape = F) %>%
  kable_styling(latex_options = c("repeat_header")) %>% 
  # collapse_rows() %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = "HOLD_position")


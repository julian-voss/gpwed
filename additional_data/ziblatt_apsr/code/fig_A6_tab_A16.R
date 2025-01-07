rm(list = ls())

## Libraries

library(tidyverse)

## Main data set 

df <- read_rds('data/data_main.rds') %>% 
  mutate(rb = substr(ags_2017, 1, 3))

## IS data set

is <- read_csv("data/soep_is_coverage.csv") %>% 
  mutate(ags_2017 = ifelse(nchar(kkz) == 4, paste0("0", kkz),kkz))

## Note that Berlin is coded as two difference counties
## Also Goettingen needs to be recoded

is <- is %>% 
  mutate(ags_2017 = ifelse(substr(ags_2017, 1, 2) == '11', '11000',
                           ags_2017)) %>% 
  mutate(ags_2017 = ifelse(ags_2017 == "03152", 
                           "03159",
                           ags_2017))

## Which counties are not covered ?

sum(is$ags_2017 %in% df$ags_2017)
is$ags_2017[!is$ags_2017 %in% df$ags_2017]

## Create dummy

df <- df %>% 
  mutate(in_soep_is = ifelse(ags_2017 %in% is$ags_2017, 1, 0))

## Looks ok

## Define function that gets balance stats that we need

bvars <- c('pop_density', 
           'pop_total', 
           'gdp_nominal_2016',
           'wage_nominal2016' , 
           'relig_cath_2011',
           'unemp_rate_tot', 
           'commuters_capita_in_2017',
           'cdu_csu_party_13',
           'dist_to_state_capital', 
           'dist_hannover_km')
bvars_proper <- c('Population density / km2', 
                  'Total population',
                  'Nominal GDP/capita',
                  'Nominal wages',
                  'Share Catholic',
                  'Unemployment rate',
                  'In-commuters / capita',
                  'CDU/CSU vote share, 2013',
                  'Distance to state capital',
                  'Distance to Hannover')
order = 1:length(bvars_proper)

## function

get_bal <- function (treatvar, cov_list, data.df, 
                     FE = NULL, weights = NULL) {
  out_temp <- lapply(cov_list, function(cv) {
    if (length(FE) == 0) {
      f <- as.formula(paste0(cv, " ~", treatvar))
    }
    else {
      f <- as.formula(paste0(cv, " ~", treatvar, "+", FE))
    }
    data.df[, cov_list] <- scale(data.df[, cov_list])
    if (!is.null(weights)) {
      m <- lm(f, data = data.df, weight = data.df %>% pull(!!weights))
    }
    else {
      m <- lm(f, data = data.df)
    }
    coef_list <- summary(m)$coefficients[2, 1]
    lower_list <- coef_list - 1.96 * summary(m)$coefficients[2, 
                                                             2]
    upper_list <- coef_list + 1.96 * summary(m)$coefficients[2, 
                                                             2]
    data.frame(cov = cv, lower = lower_list, upper = upper_list, 
               coef = coef_list, tv = treatvar, stringsAsFactors = F)
  })
  do.call("rbind", out_temp)
}

## Balance table 

btab <- df %>% 
  get_bal(treatvar = "in_soep_is", cov_list = bvars) %>% 
  mutate(var_name = bvars_proper,
         order = order) %>% 
  mutate(var_name = fct_reorder(var_name, rev(order)))

#### Figure A.6: Correlates of SOEP-IS coverage ####

bplot <- ggplot(btab, aes(var_name, coef)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  geom_point(shape = 21, fill = 'white', size = 2) +
  xlab("") + ylab("Standardized difference") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  coord_flip()
bplot

#### Table A.16 ####

## We note that the quantities given in the first 4 rows were conveyed to use via email
## These can be verified by contacting MGerike@diw.de

## The final quantity is calculated below 
## Ie share of population in counties covered by the SOEP IS data

df %>% 
  group_by(in_soep_is) %>% 
  summarise(pop = sum(pop_total, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pop = pop/sum(pop)) %>% 
  filter(in_soep_is == 1)


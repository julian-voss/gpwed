## Libraries

library(tidyverse)
library(knitr)
library(kableExtra)

## Clear

rm(list = ls())

## Get data 

df <- read_rds('data/data_main.rds') %>% 
  mutate(nsdap33 = nsdap33 * 100) %>% 
  mutate(pop_total = pop_total / 1000)

## Declare variables and labels

iv_list <- c('hannover_dist', 'hannover_dist_jw')
iv_proper <- c('Distance from standard German',
               'Distance from standard German (Jaro-Winkler)')
dv <- c('afd_party_17', 
        'cdu_csu_party_13')
dv_proper <- c('AfD vote share 2017 (\\%, party)',
               'CDU/CSU vote share 2017 (\\%, party)')
controls2 <- c('pop_total', 'pop_density', 'gdp_nominal_2016',
               'wage_nominal2016' , 'relig_cath_2011',
               'unemp_rate_tot',
               'mig_out_capita',
               'mig_in_capita',
               'mig_combined_capita',
               'commuters_capita_in_2017',
               'commuters_capita_out_2017',
               "commuters_in_dist_2017", 
               "commuters_out_dist_2017",
               'dist_to_state_capital', 
               'any_prog_20s',
               'nsdap33')
controls2_proper <- c('Tot. population (1000s)', 'Pop. density / km2', 'Nominal GDP (EUR)',
                      'Nominal wage (EUR)' , 'Share Catholic (2011)',
                      'Unemployment rate (\\%)', 
                      'Out-migration / capita (internal, 2017)', 
                      'In-migration / capita (internal, 2017)', 
                      'Combined migration / capita (internal, 2017)', 
                      'In-commuters / capita (2017)',
                      'Out-commuters / capita (2017)',
                      'Avg. in-commuting distance (km, 2017)',
                      'Avg. out-commuting distance (km, 2017)',
                      'Dist. to state capital (km)', 
                      'Pogroms in 1920s (0/1)',
                      'NSDAP vote share, 1933 (\\%)')
allvars <- c(iv_list, dv, controls2)
allvars_proper <- c(iv_proper, dv_proper, controls2_proper)

#### Figure 1: Map ####

#### Table A.1: summary stats ####

ss <- lapply(allvars, function(v) {
  x <- df %>% pull(!!v)
  c(mean(x, na.rm = T) %>% round(2),
    sd(x, na.rm = T) %>% round(2),
    min(x, na.rm = T) %>% round(2),
    max(x, na.rm = T) %>% round(2),
    sum(!is.na(x)))
}) %>% 
  reduce(rbind) %>% data.frame(stringsAsFactors = F) %>% 
  cbind(allvars_proper, .) %>% 
  rename(Variable = 1, Mean = 2, SD = 3, Min = 4, Max = 5, Valid_Obs = 4)
row.names(ss) <- NULL

## To table

kable(ss, "latex", longtable = F, 
      booktabs = T, col.names = c('',
                                  'Mean',
                                  'SD',
                                  'Min',
                                  'Max',
                                  'Valid obs.'),
      linesep = "",
      caption = 'Summary statistics\\label{tab:sumstats}',
      escape = F) %>%
  kable_styling(latex_options = c("repeat_header"),
                font_size = 9) %>% 
  # collapse_rows() %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  footnote(general = "Summary statistics") %>% 
  pack_rows('Dialectal distance', 1, 2) %>%
  pack_rows('Aggregate voting outcomes', 3, 4) %>%
  pack_rows('County-level covariates', 5, nrow(ss))

#### Figure A.1: Histogram of responses by county ####

p1 <- ggplot(df, aes(x = n_replies)) +
  geom_histogram(fill = 'grey93', color = 'black') +
  xlab('Dialect survey respondents per county\n(logarithmic scale)') +
  scale_x_log10() +
  theme_bw() +
  ylab('Number of counties')
p1




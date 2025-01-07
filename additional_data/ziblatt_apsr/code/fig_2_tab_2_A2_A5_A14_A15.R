rm(list = ls())

# Load libraries

library(tidyverse)
library(lfe)
library(estimatr)
library(broom)
library(stargazer)
library(haven)

# Read data
gles_panel <- read_rds("data/gles_panel.rds")

#### Table 2: Effects of dialectal distance (survey data) ####

m1 <- felm(vote_afd_2 ~ std_hannover_dist + female + 
             age + unemployed + income + factor(edu) + factor(rural) + 
             east | 0 | 0 | id, 
           data = gles_panel)
m2 <- update(m1, . ~ . + nat_gen)
m3 <- update(m1, . ~ . + attach_local + attach_nation)
m4 <- update(m1, skalafd ~ .)
m5 <- update(m4, . ~ . + nat_gen)
m6 <- update(m4, . ~ . + attach_local + attach_nation)

## Get results

model_list <- list(m1, m2, m3, m4, m5, m6)

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)
dv_sd <- sapply(model_list, function(m) sd(m$response, na.rm = T)) %>%
  round(2)

## Table

stargazer::stargazer(model_list, keep = 'dist|nat_gen|attach_local|attach_nation',
                     keep.stat = c('rsq',"adj.rsq", 'n'), style = 'ajps',
                     dep.var.labels = c('AfD vote intentions',
                                        'AfD scalometer'),
                     add.lines = list(c('Mean of DV',
                                        dv_means),
                                      c("East-West FE", rep("Yes", 6)),
                                      c("Covariates", rep("Yes", 6))), 
                     covariate.labels = c("Dialectal distance",
                                          "Nationalism scale", 
                                          "Local attachment",
                                          "National attachment"))

#### Figure 2: AfD likability over time ####

# List to store models

models_list <- list()

# Run models for each wave (1 to 18)
for (x in 1:18) {
  models_list[[x]] <- felm(skalafd ~ std_hannover_dist + 
                             female + age + unemployed + income + 
                             factor(edu) + factor(rural) + east, 
                           data = gles_panel %>% filter(wave == !!x))
}

# Store results

results <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results) <- c("wave", "term", "estimate", "conf.low", "conf.high")

for (x in 1:length(models_list)) {
  model <- models_list[[x]]
  coef_df <- tidy(model, conf.int = TRUE) %>% 
    dplyr::select(term, estimate, conf.low, conf.high) %>%
    filter(term == "std_hannover_dist") %>% 
    mutate(wave = x) %>% 
    dplyr::select(wave, everything())
  results <- rbind(results, coef_df)
}

## Plot

p1 <- ggplot(data = results %>% 
               mutate(ref = ifelse(wave> 7, "During the refugee crisis",
                                   "Prior to the refugee crisis")), 
             aes(wave, estimate)) + 
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(width = 0, aes(color = ref, ymin = conf.low,
                               ymax = conf.high)) +
  geom_point(aes(color = ref), size = 2) +
  xlab('') +
  ylab('Effect of dialectal distance\non AfD likability') +
  theme_bw() +
  scale_color_grey(name = '', start = 0, end = 0.7) +
  theme(legend.position = 'bottom') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.key.size = unit(2,"line")) +
  scale_x_continuous(breaks= 1:20)
p1

#### Table A.2: GLES summary stats ####

# Summary statistics

variable_labels <- c(
  "vote_afd_2" = "Vote for AfD (party vote)",
  "skalafd" = "AfD likability scalometer (1-11)",
  "vote_rr_2" = "Vote for radical right (party vote)",
  "female" = "Female (binary)",
  "age" = "Age (18-88)",
  "unemployed" = "Unemployed (binary)",
  "income" = "Income (1-13)",
  "edu" = "Education (0-4)",
  "rural" = "Rurality (1-5)",
  "east" = "East (binary)",
  "nat_gen" = "Nationalism scale (1-5)",
  "attach_local" = "Local attachment (1-5)",
  "attach_nation" = "National attachment (1-5)"
)

# Summary statistics

summary_stats <- gles_panel %>%
  dplyr::select(all_of(names(variable_labels))) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ) %>%
  ungroup() %>%
  mutate(
    variable = recode(variable, !!!variable_labels),
    mean = round(mean, 4),
    sd = round(sd, 4)
  ) %>%
  mutate(variable = factor(variable, levels = variable_labels)) %>%
  arrange(variable)

print(summary_stats)

#### Table A.5: Correlations between GLES variables ####

correlation_labels <- c(
  "std_hannover_dist" = "Dialectal Distance",
  "nat_gen" = "Nationalism scale (1-5)",
  "attach_local" = "Local attachment (1-5)",
  "attach_nation" = "National attachment (1-5)",
  "female" = "Female (binary)",
  "age" = "Age (18-88)",
  "unemployed" = "Unemployed (binary)",
  "income" = "Income (1-13)",
  "edu" = "Education (0-4)",
  "rural" = "Rurality (1-5)",
  "east" = "East (binary)"
)

# Correlations using only valid observations
correlation_matrix <- cor(
  gles_panel[, c("std_hannover_dist", "nat_gen", "attach_local", 
                 "attach_nation",
                 "female", "age", "unemployed", "income", "edu", 
                 "rural", "east")],
  use = "pairwise.complete.obs"
)

correlation_results <- as.data.frame(correlation_matrix)

# Update row and column names with proper labels
rownames(correlation_results) <- correlation_labels
colnames(correlation_results) <- correlation_labels
correlation_results %>% round(2)

#### Table A.14: main GLES results w/ covariates ####

clabs <- c("Dialectal distance", "Female (vs. male)", "Age", 
           "Unemployed (vs. employed)", "Income (1-13)", "Hauptschule", 
           "Realschule", "Fachhochschulreife", "Abitur", 
           "Suburban towns in metro areas", 
           "Suburban towns in less dense areas", 
           "Rural areas close to larger towns", 
           "Rural areas close to small towns", "East (vs. West)", 
           "Nationalism scale (1-5)", "Local attachment (1-5)", 
           "National attachment (1-5)", "Constant")

stargazer::stargazer(model_list,
                     keep.stat = c('rsq', 'n'), style = 'ajps',
                     dep.var.labels = c('AfD vote intentions',
                                        'AfD scalometer'),
                     add.lines = list(c('Mean of DV',
                                        dv_means),
                                      c("East-West FE", rep("Yes", 6)),
                                      c("Covariates", rep("Yes", 6))), 
                     covariate.labels = clabs)

#### Table A.15: Radical right voting ####

m1 <- felm(vote_rr_2 ~ std_hannover_dist + female + 
             age + unemployed + income + factor(edu) + factor(rural) + 
             east | 0 | 0 | id, 
           data = gles_panel)
m2 <- update(m1, . ~ . + nat_gen)
m3 <- update(m1, . ~ . + attach_local + attach_nation)

## Get results

model_list <- list(m1, m2, m3)

dv_means <- sapply(model_list, function(m) mean(m$response, na.rm = T)) %>%
  round(2)

## Table

stargazer::stargazer(model_list, keep = 'dist|nat_gen|attach_local|attach_nation',
                     keep.stat = c('rsq', 'n'), style = 'ajps',
                     dep.var.labels = 'RR vote intentions',
                     add.lines = list(c('Mean of DV',
                                        dv_means),
                                      c("East-West FE", rep("Yes", 3)),
                                      c("Covariates", rep("Yes", 3))), 
                     covariate.labels = c("Dialectal distance",
                                          "Nationalism scale", 
                                          "Local attachment",
                                          "National attachment"))
## w/ covariates

stargazer::stargazer(model_list, 
                     keep.stat = c('rsq', 'n'), style = 'ajps',
                     dep.var.labels = 'RR vote intentions',
                     add.lines = list(c('Mean of DV',
                                        dv_means),
                                      c("East-West FE", rep("Yes", 3)),
                                      c("Covariates", rep("Yes", 3))))


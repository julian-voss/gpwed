library(tidyverse)

# Assess measurement error using historical data ------------------------------

## Process 1953 ----

# Read imputed data
btw_53_imputed <- read_rds("./output/cleaned/53_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  relocate(cdu, spd, fdp, .after = imputed) %>%
  mutate(other = rowSums(select(., dp:center), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(dp:center)) %>%
  drop_na(votes_valid)

# Aggregate counts to county level
vote_counts_scraped <- btw_53_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(county_name, regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

# Read official county returns
btw_53_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1953_counties.rds") %>%
  mutate(other = rowSums(select(., gb_bhe:ssw), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(csu, gb_bhe:ssw))

# Match county keys to official data
counties53_keys <- read_rds("./additional_data/administrative_info/counties1953_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_53_counties_admin, counties53_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

# Combine curated and official data to compare
vote_counts_compare53 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  group_by(regional_key) %>% filter(n() == 1) %>% ungroup() %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1953)


## Process 1957 ----

btw_57_imputed <- read_rds("./output/cleaned/57_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  relocate(fdp, .after = spd) %>%
  mutate(other = rowSums(select(., dp:ssw), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(dp:ssw)) %>%
  drop_na(votes_valid)

btw_57_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1957_counties.rds") %>%
  mutate(other = rowSums(select(., dp:other), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(csu, dp:gb_bhe))

counties57_keys <- read_rds("./additional_data/administrative_info/counties1957_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_57_counties_admin, counties57_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_57_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(county_name, regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare57 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  group_by(regional_key) %>% filter(n() == 1) %>% ungroup() %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1957)

## Process 1961 ----

btw_61_imputed <- read_rds("./output/cleaned/61_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  mutate(other = rowSums(select(., gdp:other), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(gdp:ssw)) %>%
  drop_na(votes_valid)

btw_61_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1961_counties.rds") %>%
  mutate(other = rowSums(select(., gdp:ssw), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(csu, gdp:ssw))

counties61_keys <- read_rds("./additional_data/administrative_info/counties1961_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_61_counties_admin, counties61_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_61_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(county_name, regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare61 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  group_by(regional_key) %>% filter(n() == 1) %>% ungroup() %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1961)

## Process 1965 ----

btw_65_imputed <- read_rds("./output/cleaned/65_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  mutate(other = rowSums(select(., dfu:uap), na.rm = T),
         key_state = str_sub(ags, 1, 2)) %>%
  filter(key_state != "07") %>%
  relocate(other, .after = fdp) %>%
  select(-c(dfu:uap)) %>%
  drop_na(votes_valid)

btw_65_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1965_counties.rds")

counties65_keys <- read_rds("./additional_data/administrative_info/counties1965_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_65_counties_admin, counties65_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_65_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare65 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1965)

## Process 1969 ----

btw_69_imputed <- read_rds("./output/cleaned/69_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  mutate(other = rowSums(select(., adf:uap), na.rm = T), key_state = str_sub(ags, 1, 2)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(adf:uap)) %>%
  drop_na(votes_valid)

btw_69_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1969_counties.rds")

counties69_keys <- read_rds("./additional_data/administrative_info/counties1969_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_69_counties_admin, counties69_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  mutate(other = rowSums(select(., npd, adf, other))) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_69_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare69 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1969)

## Table 2 ----

vote_counts_compare <-
  bind_rows(vote_counts_compare53, vote_counts_compare57,
            vote_counts_compare61, vote_counts_compare65,
            vote_counts_compare69) %>%
  mutate(error = error*100,
         error_flag = ifelse(error > 1 | error < -1, 1, 0)) %>%
  arrange(county_name_scraped, year) %>%
  mutate(state = str_sub(regional_key, 1, 2),
         state = ifelse(state == "48", "10", state))

# Tabulate errors
validation_tab_dat <- vote_counts_compare %>%
  group_by(measure) %>%
  summarise(
    Mean = mean(error, na.rm = T),
    SD = sd(error, na.rm = T),
    Min = min(error, na.rm = T),
    p25 = quantile(error, probs = .25, na.rm = T),
    Median = quantile(error, probs = .50, na.rm = T),
    p75 = quantile(error, probs = .75, na.rm = T),
    Max = max(error, na.rm = T)) %>%
  mutate(across(Mean:Max, ~ round(.x , 2))) %>%
  rename(Party = measure,
         `$P_{25}$` = p25,
         `$P_{75}$` = p75) %>%
  mutate(Party = toupper(Party),
         Party = case_when(Party == "CDU" ~ "CDU/CSU", Party == "OTHER" ~ "Other", T ~ Party),
         Party = factor(Party, levels = c("CDU/CSU", "SPD", "FDP", "Other"))) %>%
  arrange(Party)

#Save as a LaTeX table
validation_tab <-
  kable(validation_tab_dat, format = "latex", booktabs = TRUE, caption = "County-level deviations by Party", escape = F) %>%
  as.character() %>%
  str_remove_all("\\\\begin\\{table\\}|\\\\end\\{table\\}")

# Write to a .tex file
cat(validation_tab, file = "./output/validation/measurement_error_table.tex")



btw_53_imputed %>% distinct(ags_22, completeness_mun) %>%
  ggplot(data = ., aes(x = completeness_mun))



## Analyze measurement error using historical data (without imputation) --------

### Process 1953 ----

# Read imputed data
btw_53_imputed <- read_rds("./output/cleaned/53_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  relocate(cdu, spd, fdp, .after = imputed) %>%
  mutate(other = rowSums(select(., dp:center), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(dp:center)) %>%
  drop_na(votes_valid) %>%
  filter(!imputed)

# Aggregate counts to county level
vote_counts_scraped <- btw_53_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(county_name, regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

# Read official county returns
btw_53_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1953_counties.rds") %>%
  mutate(other = rowSums(select(., gb_bhe:ssw), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(csu, gb_bhe:ssw))

# Match county keys to official data
counties53_keys <- read_rds("./additional_data/administrative_info/counties1953_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_53_counties_admin, counties53_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

# Combine curated and official data to compare
vote_counts_compare53 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  group_by(regional_key) %>% filter(n() == 1) %>% ungroup() %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1953)


### Process 1957 ----

btw_57_imputed <- read_rds("./output/cleaned/57_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  relocate(fdp, .after = spd) %>%
  mutate(other = rowSums(select(., dp:ssw), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(dp:ssw)) %>%
  drop_na(votes_valid) %>%
  filter(!imputed)

btw_57_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1957_counties.rds") %>%
  mutate(other = rowSums(select(., dp:other), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(csu, dp:gb_bhe))

counties57_keys <- read_rds("./additional_data/administrative_info/counties1957_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_57_counties_admin, counties57_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_57_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(county_name, regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare57 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  group_by(regional_key) %>% filter(n() == 1) %>% ungroup() %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1957)

### Process 1961 ----

btw_61_imputed <- read_rds("./output/cleaned/61_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  mutate(other = rowSums(select(., gdp:other), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(gdp:ssw)) %>%
  drop_na(votes_valid) %>%
  filter(!imputed)

btw_61_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1961_counties.rds") %>%
  mutate(other = rowSums(select(., gdp:ssw), na.rm = T)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(csu, gdp:ssw))

counties61_keys <- read_rds("./additional_data/administrative_info/counties1961_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_61_counties_admin, counties61_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_61_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(county_name, regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare61 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  group_by(regional_key) %>% filter(n() == 1) %>% ungroup() %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1961)

### Process 1965 ----

btw_65_imputed <- read_rds("./output/cleaned/65_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  mutate(other = rowSums(select(., dfu:uap), na.rm = T),
         key_state = str_sub(ags, 1, 2)) %>%
  filter(key_state != "07") %>%
  relocate(other, .after = fdp) %>%
  select(-c(dfu:uap)) %>%
  drop_na(votes_valid) %>%
  filter(!imputed)

btw_65_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1965_counties.rds")

counties65_keys <- read_rds("./additional_data/administrative_info/counties1965_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_65_counties_admin, counties65_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_65_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare65 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1965)

### Process 1969 ----

btw_69_imputed <- read_rds("./output/cleaned/69_btw_imputed.rds") %>%
  filter(first_second_vote == "second") %>%
  mutate(other = rowSums(select(., adf:uap), na.rm = T), key_state = str_sub(ags, 1, 2)) %>%
  relocate(other, .after = fdp) %>%
  select(-c(adf:uap)) %>%
  drop_na(votes_valid) %>%
  filter(!imputed)

btw_69_imputed %>% filter(completeness_mun == 0) %>% count(state_name)

btw_69_counties_admin <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1969_counties.rds")

counties69_keys <- read_rds("./additional_data/administrative_info/counties1969_admin.rds") %>%
  select(key_state, regional_key, county_name)

vote_counts_admin <-
  left_join(btw_69_counties_admin, counties69_keys, by = c("county_name", "key_state")) %>%
  relocate(matches("key"), .before = everything()) %>%
  mutate(other = rowSums(select(., npd, adf, other))) %>%
  select(county_name, regional_key, voters_eligible, votes_valid, cdu, spd, fdp, other) %>%
  drop_na(regional_key)

vote_counts_scraped <- btw_69_imputed %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  group_by(regional_key) %>%
  summarise(across(c(votes_valid, cdu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop")

vote_counts_compare69 <-
  left_join(vote_counts_scraped, vote_counts_admin, by = "regional_key", suffix = c("_scraped", "_admin")) %>%
  relocate(matches("county_name|key"), .before = everything()) %>%
  mutate(across(cdu_scraped:other_scraped, ~ .x/votes_valid_scraped)) %>%
  mutate(across(cdu_admin:other_admin, ~ .x/votes_valid_admin)) %>%
  pivot_longer(names_to = c("measure", "scraped"), names_sep = "_(?=scraped)|_(?=admin)",
               cols = matches("spd|cdu|fdp|other")) %>%
  pivot_wider(names_from = scraped, values_from = value) %>%
  mutate(error = scraped-admin) %>%
  mutate(year = 1969)

### Table 2 (without imputation) ----

vote_counts_compare <-
  bind_rows(vote_counts_compare53, vote_counts_compare57,
            vote_counts_compare61, vote_counts_compare65,
            vote_counts_compare69) %>%
  mutate(error = error*100) %>%
  arrange(county_name_scraped, year) %>%
  mutate(state = str_sub(regional_key, 1, 2),
         state = ifelse(state == "48", "10", state))


# Tabulate errors
validation_tab_dat <-
  vote_counts_compare %>%
  group_by(measure) %>%
  summarise(
    Mean = mean(error, na.rm = T),
    SD = sd(error, na.rm = T),
    Min = min(error, na.rm = T),
    p25 = quantile(error, probs = .25, na.rm = T),
    Median = quantile(error, probs = .50, na.rm = T),
    p75 = quantile(error, probs = .75, na.rm = T),
    Max = max(error, na.rm = T)) %>%
  mutate(across(Mean:Max, ~ round(.x , 2))) %>%
  rename(Party = measure,
         `$P_{25}$` = p25,
         `$P_{75}$` = p75) %>%
  mutate(Party = toupper(Party),
         Party = case_when(Party == "CDU" ~ "CDU/CSU", Party == "OTHER" ~ "Other", T ~ Party),
         Party = factor(Party, levels = c("CDU/CSU", "SPD", "FDP", "Other"))) %>%
  arrange(Party)

# Save as a LaTeX table
validation_tab <-
  kable(validation_tab_dat, format = "latex", booktabs = TRUE, caption = "County-level deviations by Party", escape = F) %>%
  as.character() %>%
  str_remove_all("\\\\begin\\{table\\}|\\\\end\\{table\\}")

# Write to a .tex file
cat(validation_tab, file = "./output/figures/measurement_error_tab_without_imput.tex")

## Create linktable to add deviation info to main data -------------------------

vote_counts_compare_party <-
  vote_counts_compare %>%
  select(year, regional_key, measure, error) %>%
  pivot_wider(names_from = measure, values_from = error) %>%
  select(-other) %>%
  mutate(across(cdu:fdp, ~ as.integer(.x > 1 | .x < -1), .names = "{.col}_county_deviation"))

crossw <- list.files("./additional_data/administrative_info/crosswalks/", full.names = T) %>% map(read_rds)

crossw[[1]] <- crossw[[1]] %>% mutate(year = 1953)
crossw[[2]] <- crossw[[1]] %>% mutate(year = 1957)
crossw[[3]] <- crossw[[1]] %>% mutate(year = 1961)
crossw[[4]] <- crossw[[1]] %>% mutate(year = 1965)
crossw[[5]] <- crossw[[1]] %>% mutate(year = 1969)

crossw <- bind_rows(crossw)

crossw_error <- crossw %>%
  mutate(regional_key = str_sub(ags, 1, 5)) %>%
  distinct(year, ags_new, name_new, regional_key) %>%
  left_join(vote_counts_compare_party) %>%
  group_by(year, ags_new, name_new) %>%
  summarise(across(cdu_county_deviation:fdp_county_deviation, ~ any(.x == 1)), .groups = "drop") %>%
  mutate(across(cdu_county_deviation:fdp_county_deviation, ~ case_when(.x ~ 1, T ~ NA)))

write_rds(crossw_error, "./output/validation/county_deviation_data.rds")










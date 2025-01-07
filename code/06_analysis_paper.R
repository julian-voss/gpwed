# This script contains the code for all figures and analyses in the data
# descriptor
#
# Load libraries
library(tidyverse)
library(sf)
library(rmapshaper)
library(patchwork)
library(kableExtra)
library(fixest)

source("~/Dropbox/my_functions.R")

rm(list = ls())

gpwed <- read_rds("./output/final_database/gpwed.rds")

# Data overview: Figure 1 & A.1 ------------------------------------------------

na_info <-
  gpwed %>%
  mutate(non_missing = !is.na(votes_valid)) %>%
  group_by(key_state, year, vote_type) %>%
  summarise(non_missing = mean(non_missing), .groups = "drop") %>%
  mutate(
    state_lab = case_match(key_state, "01" ~ "Schleswig-Holstein", "02" ~ "Hamburg", "03" ~ "Niedersachsen",
                           "04" ~ "Bremen", "05" ~ "Nordrhein-Westfalen", "06" ~ "Hessen", "07" ~ "Rheinland-Pfalz",
                           "08" ~ "Baden-Württemberg", "09" ~ "Bayern", "10" ~ "Saarland"),
    state_lab = factor(state_lab, levels = rev(c("Schleswig-Holstein", "Hamburg", "Niedersachsen",
                                                 "Bremen", "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz",
                                                 "Baden-Württemberg", "Bayern", "Saarland"))),
    non_missing = round(non_missing, 2),
    non_missing = non_missing*100,
    non_missing_lab = paste0(non_missing, "%"))

# Figure 1
overview_data_second_votes <-
  na_info %>%
  filter(vote_type == "second") %>%
  ggplot(., aes(x = year, y = state_lab, fill = non_missing)) +
  geom_tile(color = "white") +
  geom_text(aes(label = non_missing_lab), size = 3) +
  scale_x_continuous(breaks  = seq(1949, 1969, 4)) +
  labs(x = "Election", y = "State") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        text = element_text(size = 15))

# Figure A.1
overview_data_first_votes <-
  na_info %>%
  filter(vote_type == "first") %>%
  ggplot(., aes(x = year, y = state_lab, fill = non_missing)) +
  geom_tile(color = "white") +
  geom_text(aes(label = non_missing_lab), size = 3) +
  scale_x_continuous(breaks  = seq(1949, 1969, 4)) +
  labs(x = "Election", y = "State") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        text = element_text(size = 15))

ggsave("./output/figures/overview_data_second_votes.pdf",
       overview_data_second_votes, width = 6, height = 3.5)

ggsave("./output/figures/overview_data_first_votes.pdf",
       overview_data_first_votes, width = 6, height = 3.5)


# Figure 2  --------------------------------------------------------------------

## Define right-wing nationalist parties
rw_parties_list <- c("dp", "drp", "gb_bhe", "pdgd_dns", "dg", "npd", "uap", "gpd", "wav", "vu")

state_geometries <- st_read("./additional_data/shapefiles/vg5000_12-31.gk3.shape.ebenen/vg5000_ebenen_1231/VG5000_LAN.shp") %>%
  # Remove coastal areas
  filter(!OBJID %in% c("DEBKGVG500000CTC", "DEBKGVG500000CTD", "DEBKGVG500000CTA", "DEBKGVG500000CTG", "DEBKGVG500000CTB")) %>%
  mutate(AGS_num = as.numeric(AGS))

municipality_geometries <-
  st_read("./additional_data/shapefiles/vg5000_12-31.gk3.shape.ebenen/vg5000_ebenen_1231/VG5000_GEM.shp") %>%
  select(AGS, GEN)

gpwed_plot_dat <-
  gpwed %>%
  filter(vote_type == "second") %>%
  mutate(rw_parties = rowSums(select(., all_of(rw_parties_list)), na.rm = T),
         rw_parties = ifelse(is.na(share_imputed), NA, rw_parties),) %>%
  left_join(municipality_geometries, by = c("ags" = "AGS"))

gpwed_plot_dat_sf_long <-
  gpwed_plot_dat %>%
  st_as_sf() %>%
  select(ags, year, votes_valid, cdu_csu, spd, fdp, rw_parties) %>%
  mutate(across(cdu_csu:rw_parties, ~ (.x/votes_valid)*100)) %>%
  pivot_longer(cdu_csu:rw_parties, values_to = "voteshare", names_to = "party") %>%
  mutate(party = case_match(party, "cdu_csu" ~ "CDU/CSU", "spd" ~ "SPD", "fdp" ~ "FDP", "rw_parties" ~ "Right-wing"),
         party = factor(party, levels = c("CDU/CSU", "SPD", "FDP", "Right-wing"))) %>%
  mutate(voteshare_quant = cut(voteshare, breaks = round(quantile(voteshare, probs = seq(0, 1, .25), na.rm = T)),
                               include.lowest = T))


# Showcase data plot
breaks <- c("[0,5]", "(5,15]", "(15,38]", "(38,100]")

plot <-
  ggplot(data = gpwed_plot_dat_sf_long) +
  geom_sf(data = state_geometries, fill = "grey", lwd = .01) +
  geom_sf(aes(fill = voteshare_quant, color = voteshare_quant)) +
  geom_sf(data = state_geometries, fill = NA, color = "black", lwd = .15) +
  scale_fill_viridis_d("Share of second\nvotes (%)", na.translate = T, na.value = "grey", breaks = breaks) +
  scale_color_viridis_d("Share of second\nvotes (%)", na.translate = T, na.value = "grey", breaks = breaks) +
  facet_grid(party~year) +
  theme(legend.position = "bottom", panel.grid = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(), text = element_text(size = 15))

ggsave("./output/figures/descriptive_maps.png", plot,
       width = 6, height = 6, dpi = 500)


# Section 4: Technical validation ----------------------------------------------

## Analyze measurement error using historical data ------------------------------

### Process 1953 ----

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


### Process 1957 ----

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

### Process 1961 ----

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

### Process 1965 ----

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

### Process 1969 ----

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

### Table 2 ----

vote_counts_compare <-
  bind_rows(vote_counts_compare53, vote_counts_compare57,
            vote_counts_compare61, vote_counts_compare65,
            vote_counts_compare69) %>%
  mutate(error = error*100) %>%
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
cat(validation_tab, file = "./output/figures/measurement_error_tab.tex")

## Analyze measurement error using Erfort data ----

# Read GPWED data
gpwed <- read_rds("./output/final_database/gpwed.rds")

# Aggregate to county level
gpwed_county <-
  gpwed %>%
  # Merge cdu_csu with cvp to be consistent with erfort
  mutate(cdu_csu = ifelse(key_state == "10" & year == 1957, rowSums(select(., cdu_csu, cvp)), cdu_csu),
         cvp = ifelse(key_state == "10" & year == 1957, NA, cvp)) %>%
  mutate(ags_county = str_sub(ags, 1, 5)) %>%
  relocate(cdu_csu, spd, fdp, .after = votes_valid) %>%
  mutate(other = rowSums(select(., dp:center_fu), na.rm = T)) %>%
  group_by(ags_county, year) %>%
  summarise(across(c(votes_valid, cdu_csu, spd, fdp, other), ~ sum(.x, na.rm = T)), .groups = "drop") %>%
  mutate(across(cdu_csu:other, ~ ifelse(votes_valid == 0, NA, .x)),
         votes_valid = ifelse(votes_valid == 0, NA, votes_valid),
         across(cdu_csu:other, ~ .x/votes_valid))

# Read Erfort data
load("./additional_data/county_data_erfort/germany-53-21-districts/election-results-53-21.RData")

results_erfort <-
  results_long %>%
  filter(year %in% 1953:1969) %>%
  pivot_wider(names_from = party, values_from = vote_share) %>%
  rename_with(~ tolower(str_replace_all(.x, "-", "_"))) %>%
  select(ags, year, valid_votes, cdu_csu, spd, fdp) %>%
  mutate(other = 1 - rowSums(select(., cdu_csu:fdp)))

rm(list = c("results_long"))

# Merge GPWED and Erfort estimates
gpwed_erfort_comparison <-
  left_join(gpwed_county, results_erfort, by = c("ags_county" = "ags", "year"), suffix = c(".own", ".erfort")) %>%
  relocate(votes_valid, valid_votes, .after = year) %>%
  pivot_longer(cdu_csu.own:other.erfort, names_sep = "\\.", names_to = c("party", "source")) %>%
  pivot_wider(values_from = value, names_from = source) %>%
  mutate(error = (own - erfort)*100)

# Model measurement error

# Read county covariates from ziblatt et al
ziblatt_covs <- read_rds("./additional_data/ziblatt_apsr/data/data_main.rds")

# Combine with error data
gpwed_erfort_comparison <-
  left_join(gpwed_erfort_comparison, ziblatt_covs, by = c("ags_county" = "ags_2017")) %>%
  mutate(party = factor(party), state = factor(state))


# Model error
coef_labels <- c("nsdap33" = "Nazi vote share 1933",
                 "any_prog_20s" = "Anti-jewish progrom 1920s (0/1)",
                 "relig_cath_2011" = "Share catholic (2011)",
                 "dist_to_state_capital" = "Dist. to state capital (km)",
                 "gdp_nominal_2016" = "Nominal GDP (2016)",
                 "pop_density" = "Pop. density",
                 "pop_total" = "Tot. population")

model_measurement_error <-
  feols(error ~ nsdap33 + any_prog_20s + dist_to_state_capital + pop_total + pop_density + relig_cath_2011 + gdp_nominal_2016,
        fixef = c("state", "year"),
        split = ~party,
        data = gpwed_erfort_comparison %>%
          mutate(across(all_of(c("nsdap33", "dist_to_state_capital",
                                 "pop_density", "relig_cath_2011", "gdp_nominal_2016",
                                 "pop_total")), scale))) %>%
  imap(~ broom::tidy(.x, conf.int = T, conf.level = .95) %>% mutate(party = .y)) %>%
  bind_rows() %>%
  filter(term != "(Intercept)", term != "erfort") %>%
  mutate(party = str_extract(party, "(?<=sample: ).*"),
         party = factor(party, levels = rev(c("cdu_csu", "spd", "fdp", "other")), labels = rev(c("CDU/CSU", "SPD", "FDP", "Other"))),
         term = coef_labels[term],
         term = factor(term, levels = coef_labels))

# Figure 3
coefplot_error <-
  model_measurement_error %>%
  ggplot(data = ., aes(x = estimate, y = term, color = party)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = .5), #shape = 21, fill = "white"
  ) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), size = .3,
                 position = position_dodge(width = .5)) +
  scale_x_continuous(breaks = seq(-1, 1, .5)) +
  scale_y_discrete(labels = label_wrap_gen(width = 20)) +
  scale_color_discrete("Party", limits = c("CDU/CSU", "SPD", "FDP", "Other")) +
  labs(y = "", x = "Estimate") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "bottom", text = element_text(size = 14))

ggsave("./output/figures/coefplot_error.pdf", width = 6, height = 4)



# Section 6: Use case ----------------------------------------------------------

## Calculate population density

municipality_geometries$area_km2 <- as.numeric(st_area(municipality_geometries)) / 1e6

gpwed <- gpwed %>%
  mutate(rw_parties = rowSums(select(., all_of(rw_parties_list)), na.rm = T),
         rw_parties = ifelse(is.na(share_imputed), NA, rw_parties),
         rw_parties = ifelse(key_state == "08" & year == 1949, NA, rw_parties))  %>%
  mutate(across(c(cdu_csu, fdp, spd, rw_parties), ~ .x/votes_valid, .names = "{.col}_share")) %>%
  left_join(municipality_geometries %>% st_drop_geometry() %>% select(AGS, area_km2), by = c("ags" = "AGS")) %>%
  mutate(pop_density = voters_eligible/area_km2)

# Compare upper two deciles to lower two deciles

urban_rural_descriptive_dat <-
  gpwed %>%
  group_by(year) %>%
  mutate(decile = ntile(pop_density, 10)) %>%
  ungroup() %>%
  filter(decile %in% c(1, 2, 9, 10)) %>%
  mutate(region = ifelse(decile < 3, "rural", "urban")) %>%
  group_by(region, year) %>%
  summarise(across(c(votes_valid, cdu_csu, spd, fdp, rw_parties), sum), .groups = "drop") %>%
  mutate(across(cdu_csu:rw_parties, ~ .x/votes_valid)) %>%
  rename(cducsu = cdu_csu, rwparties = rw_parties) %>%
  pivot_wider(values_from = votes_valid:rwparties, names_from = region) %>%
  select(-matches("votes_valid")) %>%
  pivot_longer(
    cols = -year, # Keep the "year" column as is
    names_to = c("party", "region"), # Split names into two columns
    names_sep = "_", # Separator between party and region in column names
    values_to = "value" # Values column
  ) %>%
  pivot_wider(
    names_from = region, # Pivot on the "region" column
    values_from = value # Use "value" column for the data
  ) %>%
  mutate(urban_rural_diff = urban - rural,
         party = case_match(party, "cducsu" ~ "CDU/CSU", "spd" ~ "SPD",
                            "fdp" ~ "FDP", "rwparties" ~ "Right-wing nationalist"),
         party = factor(party, levels = c("CDU/CSU", "SPD", "FDP", "Right-wing nationalist")))

# Estimate
model_urban_rural <-
  feols(c(cdu_csu_share, spd_share, fdp_share, rw_parties_share) ~ pop_density,
        fixef = "key_state",
        split = ~year,
        data = gpwed %>% filter(vote_type == "second") %>%
          group_by(year) %>%
          mutate(voters_eligible = scale(voters_eligible),
                 pop_density = scale(pop_density)
          ) %>%
          ungroup())

model_urban_rural_tidy <- model_urban_rural %>%
  imap(~ broom::tidy(.x, conf.int = T) %>% mutate(spec = .y)) %>%
  bind_rows() %>%
  mutate(year = as.numeric(str_extract(spec, "(?<=sample: ).*(?=;)")),
         lhs = str_extract(spec, "(?<=lhs: ).*"),
         lhs = case_match(lhs, "cdu_csu_share" ~ "CDU/CSU",
                          "spd_share" ~ "SPD", "fdp_share" ~ "FDP",
                          "rw_parties_share" ~ "Right-wing nationalist",
                          .default = lhs),
         lhs = factor(lhs, c("CDU/CSU", "SPD", "FDP", "Right-wing nationalist")))


# Figure 4

urban_rural_descriptive <-
  ggplot(data = urban_rural_descriptive_dat, aes(x = year, y = urban_rural_diff, color = party)) +
  geom_point() +
  geom_line(show.legend = F) +
  scale_x_continuous(breaks = seq(1949, 1969, 4)) +
  scale_color_discrete("") +
  labs(y = expression(Delta * " vote share (urban - rural)"), x= "Election year") +
  theme_bw()

urban_rural_coefplot <-
  ggplot(data = model_urban_rural_tidy, aes(x = year, y = estimate, color = lhs)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 1.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 1.5), show.legend = F) +
  scale_x_continuous(breaks = seq(1949, 1969, 4)) +
  scale_color_discrete("") +
  labs(y = "Estimate", x = "Election year") +
  theme_bw()

# Combined plot

urban_rural_combined_plot <-
  urban_rural_descriptive + urban_rural_coefplot +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "bottom", text = element_text(size = 12))

ggsave("./output/figures/urban_rural_divide.pdf", urban_rural_combined_plot,
       width = 6, height = 3)

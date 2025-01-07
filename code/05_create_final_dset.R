# This script creates the final data set files
#
# Load libraries
library(tidyverse)

rm(list = ls())

# Read admin data
elections_admin <-
  read_rds("./additional_data/state_provided_files/admin_all_elections.rds") %>%
  select(-voters) %>%
  mutate(share_imputed = 0)

# Read curated data
elections_extracted_all <-
  map(seq(49, 69, 4), ~ read_rds(paste0("./output/cleaned/", .x, "_btw_mapped.rds")) %>%
        mutate(year = paste0("19", .x))) %>%
  map(~ if("dkp_drp" %in% names(.x)){rename(.x, drp = dkp_drp)} else{.x}) %>%
  bind_rows() %>%
  select(-pop_help) %>%
  mutate(
    year = as.numeric(year),
    first_second_vote = ifelse(year == 1949, "second", first_second_vote),
    flag = rowSums(select(., share_complete, share_imputed))) %>%
  # Keep only complete rows after imputation
  filter(flag == 1) %>%
  select(-flag) %>%
  relocate(matches("share"), year, first_second_vote, .after = rgs_22) %>%
  rename(ags = ags_22, municipality_name = name_22, cdu_csu = cdu, dms = mittelstand)

# Combine
elections_combined_all <-
  bind_rows(elections_extracted_all, elections_admin) %>%
  # Make variables consistent
  mutate(
    other = rowSums(select(., other, independent, gehr, pfr, nbayg, eb_matjak, dvg, pöhn,
                                 wv_69, eb_fossing, eb_bartsch),
                          na.rm = T),
    pdgd_dns = rowSums(select(., pdgd, dns), na.rm = T),
    center_fu = rowSums(select(., fu_center, center, fu), na.rm = T),
    cvp = rowSums(select(., csu_cvp, cvp), na.rm = T),
    gpd = rowSums(select(., gdp, gpd), na.rm = T)) %>%
  select(-c(independent, gehr, pfr, nbayg, eb_matjak, dvg, pöhn, wv_69, eb_fossing, eb_bartsch,
            pdgd, dns, fu, center, fu_center, csu_cvp, gdp)) %>%
  mutate(key_state = str_sub(ags, 1, 2)) %>%
  # Replace missing values with zeros
  mutate(across(spd:center_fu, ~ replace_na(.x, 0))) %>%
  pivot_longer(spd:center_fu) %>%
  # Properly code missing values and zeros
  group_by(key_state, year, name, first_second_vote) %>%
  mutate(flag = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(value = ifelse(flag == 0, NA, value)) %>%
  select(-flag) %>%
  pivot_wider() %>%
  # Replace 'other' category with zero in non-missing rows
  mutate(other = ifelse(if_any(spd:center_fu, ~ !is.na(.x)) & is.na(other), 0, other)) %>%
  # Properly order variables, drop not needed variables
  select(-c(share_complete, rgs_22, pop, municipality_name)) %>%
  relocate(key_state, .before = everything()) %>%
  relocate(year, first_second_vote) %>%
  rename(vote_type = first_second_vote) %>%
  arrange(ags, year, vote_type)

# Read list of 2022 municipalities
municipalities2022 <- read_rds("./additional_data/administrative_info/municipalities2022_admin.rds") %>%
  filter(key_state %in% sprintf("%02d", 1:10)) %>%
  unite("ags", key_state, key_district, key_county, key_municipality, sep = "") %>%
  select(ags, municipality_name = name)

# Create empty panel
empty_panel <-
  expand_grid(
    municipalities2022,
    year = seq(1949, 1969, 4),
    vote_type = c("first", "second")) %>%
  arrange(ags, year, vote_type) %>%
  filter(!(year == 1949 & vote_type == "first"))

# Create final dset
gpwed_final <-
  left_join(empty_panel, elections_combined_all, by = c("ags", "year", "vote_type")) %>%
  mutate(key_state = str_sub(ags, 1, 2)) %>%
  select(-c(srp)) %>%
  relocate(key_state, .before = ags) %>%
  relocate(other, .after = everything())

# Save

# RDS
write_rds(gpwed_final, "./output/final_database/gpwed.rds")

# CSV
write.csv(gpwed_final, "./output/final_database/gpwed.csv", fileEncoding = "UTF-8")

# STATA
haven::write_dta(gpwed_final, "./output/final_database/gpwed.dat")





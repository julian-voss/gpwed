# This script creates crosswalks for individual election years to map historical
# election results to modern municipalities
#
# Load libraries
library(tidyverse)
library(readxl)
library(ags)
library(furrr)
library(magrittr)
source('~/Dropbox/my_functions.R')

options(readxl.show_progress = FALSE)

clear_environment()

# Clean files ------------------------------------------------------------------

## Municipality lists ----
municipality_lists <-
  list.files("./additional_data/administrative_info/municipality_lists/", full.names = T, pattern = "Auszug_GV")

## Define column names and type for different table layouts
col_names1 <- c('type', 'key_state', 'key_district', 'key_county', 'key_municipality',
                'name', 'area', 'pop_total', 'pop_density')
col_types1 <- c(rep("text", 6), rep("numeric", 3))

col_names2 <- c('type', 'key_state', 'key_district', 'key_county', 'key_municipality',
                'name', 'key_assoc', 'assoc_name',
                'area', 'pop_total', 'pop_male', 'pop_female', 'pop_density')
col_types2 <- c(rep("text", 8), rep("numeric", 5))

col_names3 <- c('type', 'key_state', 'key_district', 'key_county', 'key_municipality',
                'name', 'city', 'key_assoc', 'assoc_name',
                'area', 'pop_total', 'pop_male', 'pop_female', 'pop_density')
col_types3 <- c(rep("text", 9), rep("numeric", 5))

col_names4 <- c('type', 'type_text', 'key_state', 'key_district', 'key_county', 'key_assoc_new', 'key_municipality',
                'name', 'area', 'pop_total', 'pop_male', 'pop_female', 'pop_density')
col_types4 <- c(rep("text", 8), rep("numeric", 5))

municipality_lists_years <-
  municipality_lists %>%
  future_map_at(1:6, ~ read_xlsx(.x, sheet = 2, skip = 6, col_names = col_names1, col_types = col_types1) %>%
                  filter(type == 60) %>%select(-type)) %>%
  future_map_at(7:32, ~ read_xlsx(.x, sheet = 2, col_names = col_names2, range = cell_cols("A:M"), col_types = col_types2) %>% filter(type == 60) %>% select(-type)) %>%
  future_map_at(33:43, ~ read_xlsx(.x, sheet = 2, col_names = col_names3, range = cell_cols("A:N"), col_types = col_types3) %>% filter(type == 60) %>% select(-type)) %>%
  future_map_at(44:71, ~ read_xlsx(.x, sheet = 2, col_names = col_names4, range = cell_cols("A:M"), col_types = col_types4) %>% filter(type == 60) %>% select(-matches("type"))) %>%
  future_map(~ unite(.x, "ags", key_state, key_district, key_county, key_municipality, sep = "", remove = F)) %>%
  future_map(~ select(.x, ags, key_state:key_municipality, name, pop_total, area)) %>%
  # remove municipalities in east Germany
  future_map_at(40:71,  ~ filter(.x, as.numeric(key_state)<12)) %>%
  # remove municipalities from bavaria, hessen, and baden-württemberg
  future_map_at(1:2, ~ filter(.x, !as.numeric(key_state) %in% c(16:18, 22:24))) %>%
  future_map_at(3, ~ filter(.x, !as.numeric(key_state) %in% c(8:9, 16))) %>%
  future_map_at(4:71, ~ filter(.x, !as.numeric(key_state) %in% c(6, 8:9))) %>%
  # correct errors
  future_map(function(x) {
    mutate(x, name = case_when(
      name == "Goldebeck" ~ "Goldebek",
      name == "Jakobidrebber" ~ "Jacobidrebber",
      T ~ name
    ),
    across(matches("name"), str_squish))
  })

names(municipality_lists_years) <- map_vec(municipality_lists, ~ str_sub(basename(.x), 1, 10))

write_rds(municipality_lists_years, "./additional_data/administrative_info/municipality_lists/municipality_lists_clean.rds")

## County lists ----
### 1950

regunits1950 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/1950_12_31_Auszug_GV.xlsx', sheet = 2,
            skip = 6, col_names = col_names1, col_types = col_types1) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything())

counties1950 <-
  regunits1950 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  # rename(county_name = name) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits1950 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_total, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities1950 <-
  filter(regunits1950, type == "municipality") %>%
  left_join(
    counties1950 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities1950, "./additional_data/administrative_info/municipalities1950_admin.rds")
write_rds(counties1950, "./additional_data/administrative_info/counties1950_admin.rds")

### 1953

regunits1953 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/1953_12_31_Auszug_GV.xlsx', sheet = 2,
            skip = 6, col_names = col_names1, col_types = col_types1) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything())

counties1953 <-
  regunits1953 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  # rename(county_name = name) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits1953 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_total, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities1953 <-
  filter(regunits1953, type == "municipality") %>%
  left_join(
    counties1953 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities1953, "./additional_data/administrative_info/municipalities1953_admin.rds")
write_rds(counties1953, "./additional_data/administrative_info/counties1953_admin.rds")

### 1957

regunits1957 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/1957_12_31_Auszug_GV.xlsx', sheet = 2,
            skip = 6, col_names = col_names2, col_types = col_types2) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything())

regunits1957 %>% group_by(pick(key_state:key_county)) %>%
  summarise(across(area:pop_female, ~ sum(.x, na.rm=T)), .groups = "drop")

counties1957 <-
  regunits1957 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  # rename(county_name = name) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits1957 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_female, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities1957 <-
  filter(regunits1957, type == "municipality") %>%
  left_join(
    counties1957 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities1957, "./additional_data/administrative_info/municipalities1957_admin.rds")
write_rds(counties1957, "./additional_data/administrative_info/counties1957_admin.rds")

### 1961

regunits1961 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/1961_12_31_Auszug_GV.xlsx', sheet = 2,
            skip = 6, col_names = col_names2, col_types = col_types2) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything())

counties1961 <-
  regunits1961 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  # rename(county_name = name) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits1961 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_female, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities1961 <-
  filter(regunits1961, type == "municipality") %>%
  left_join(
    counties1961 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities1961, "./additional_data/administrative_info/municipalities1961_admin.rds")
write_rds(counties1961, "./additional_data/administrative_info/counties1961_admin.rds")

### 1965

regunits1965 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/1965_12_31_Auszug_GV.xlsx',
            sheet = 2, range = cell_cols("A:M"), col_names = col_names2, col_types = col_types2) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything()) %>%
  drop_na(state_name)

counties1965 <-
  regunits1965 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  # rename(county_name = name) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits1965 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_female, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities1965 <-
  filter(regunits1965, type == "municipality") %>%
  left_join(
    counties1965 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities1965, "./additional_data/administrative_info/municipalities1965_admin.rds")
write_rds(counties1965, "./additional_data/administrative_info/counties1965_admin.rds")

### 1968

regunits1968 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/1968_12_31_Auszug_GV.xlsx',
            sheet = 2, range = cell_cols("A:M"), col_names = col_names2, col_types = col_types2) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything()) %>%
  drop_na(state_name)

counties1968 <-
  regunits1968 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  # rename(county_name = name) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits1968 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_female, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities1968 <-
  filter(regunits1968, type == "municipality") %>%
  left_join(
    counties1968 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities1968, "./additional_data/administrative_info/municipalities1968_admin.rds")
write_rds(counties1968, "./additional_data/administrative_info/counties1968_admin.rds")

### 1969/1970

regunits1970 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/1970_01_01_Auszug_GV.xlsx',
            sheet = 2, range = cell_cols("A:M"), col_names = col_names2, col_types = col_types2) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything()) %>%
  drop_na(state_name)

counties1970 <-
  regunits1970 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  # rename(county_name = name) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits1970 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_female, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities1970 <-
  filter(regunits1970, type == "municipality") %>%
  left_join(
    counties1970 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities1970, "./additional_data/administrative_info/municipalities1970_admin.rds")
write_rds(counties1970, "./additional_data/administrative_info/counties1970_admin.rds")

### 2022

regunits2022 <-
  read_xlsx('./additional_data/administrative_info/municipality_lists/2022_12_31_Auszug_GV.xlsx',
            sheet = 2, range = cell_cols("A:M"), col_names = col_names4, col_types = col_types4) %>%
  mutate(
    type = case_when(
      type == 10 ~ 'state',
      type == 20 ~ 'district',
      type == 40 ~ 'county',
      type == 60 ~ 'municipality'),
    county_name = case_when(type == "county" ~ name),
    state_name = case_when(type == "state" ~ name)) %>%
  fill(county_name, state_name) %>%
  mutate(county_name = str_remove(county_name, "Früherer Stadtkreis "),
         county_name = ifelse(type == "state", NA, county_name)) %>%
  relocate(state_name, county_name, .before = everything()) %>%
  drop_na(state_name)

counties2022 <-
  regunits2022 %>%
  filter(type == 'county') %>%
  select(-c(name, key_municipality, type, area, matches("assoc|pop_"))) %>%
  arrange(state_name, county_name) %>%
  left_join(
    regunits2022 %>% group_by(pick(key_state:key_county)) %>%
      summarise(across(area:pop_female, ~ sum(.x, na.rm=T)), .groups = "drop")
    , by = c("key_state", "key_county", "key_district")
  ) %>%
  mutate(regional_key = paste0(key_state, key_district, key_county),
         across(matches("name"), str_trim)) %>%
  relocate(regional_key, .after = key_county)

municipalities2022 <-
  filter(regunits2022, type == "municipality") %>%
  left_join(
    counties2022 %>% select(key_state, key_district, key_county, county_name) %>% rename(key_district_county = key_district),
    by = c("key_state", "key_county", "county_name")
  ) %>%
  mutate(key_district_county = ifelse(is.na(key_district_county), key_district, key_district_county)) %>%
  relocate(key_district_county, .after = key_district)

write_rds(municipalities2022, "./additional_data/administrative_info/municipalities2022_admin.rds")
write_rds(counties2022, "./additional_data/administrative_info/counties2022_admin.rds")

## Admin changes ----

# change_type == 1: Auflösung
# change_type == 2: Teilausgliederung
# change_type == 3: Schlüsseländerung
# change_type == 4: Namensänderung

col_names <- c("merge_id", "unit_type", "regional_key_old", "ags_old", "name_old", "change_type", "area_ha", "pop",
               "regional_key_new", "ags_new", "name_new", "date_effective_legal", "date_effective_stat")

col_types <- c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "date", "date")

admin_changes_years <- list.files("./additional_data/administrative_info/changes/", full.names = T, pattern = "\\.xlsx") %>%
  future_map(~ read_xlsx(.x, sheet = 2, skip = 7, col_names = F,
                         range = cell_cols("A:M"), col_types = col_types)  %>%
               `colnames<-`(col_names) %>% filter(unit_type == "Gemeinde") %>%
               mutate(regional_key_old = str_sub(ags_old, 1, 5),
                      regional_key_new = str_sub(ags_new, 1, 5),
                      across(matches("ags"), ~ str_replace(.x, "n\\.a\\.", "NA")),
                      across(matches("name"), str_squish)
               ),
             .progress = T) %>%
  # Remove east germany
  future_map_at(40:74, ~ filter(.x, as.numeric(str_sub(ags_new, 1, 2)) < 12)) %>%
  # Remove Hesse, Bavaria, BW
  future_map(~ filter(.x, !as.numeric(str_sub(ags_new, 1, 2)) %in% c(6, 8:9))) %>%
  # Fix errors
  future_map(~ mutate(.x, across(matches("name"), function(x){
    str_replace_multiple(x, c("Seeth-Eckholt" = "Seeth-Ekholt", "Ostfriedland" = "Ostfriesland",
                              "Remiguisberg" = "Remigiusberg", "Neunkirchen am Porzberg" = "Neunkirchen am Potzberg",
                              "Sankt Aldegrund" = "Sankt Aldegund", "Sendenhost" = "Sendenhorst"))
  })))

names(admin_changes_years) <- list.files("./additional_data/administrative_info/changes/",
                                         pattern = "\\.xlsx") %>% map(~ str_remove(.x, "\\.xlsx"))

admin_changes_years <-
  future_map(admin_changes_years, ~ distinct(.x, pick(regional_key_old:name_new), .keep_all = T),
             .progress = T)

write_rds(admin_changes_years, "./additional_data/administrative_info/admin_changes_years.rds")

# Functions to build crosswalks ------------------------------------------------

# Function for crosswalk between two years
crosswalk_dyad <- function(municipalities, admin_changes) {


  if("pop" %in% names(municipalities)) {
    municipalities %<>% rename(pop_total = pop)
  }

  admin_changes <-
    admin_changes %>%
    arrange(ags_old, date_effective_stat) %>% drop_na(ags_old) %>%
    separate_rows(change_type, sep = "\\.") %>%
    mutate(across(c(area_ha, pop), ~ ifelse(change_type %in% c("3", "4"), NA, .x)),
           name_new = ifelse(change_type == "3", name_old, name_new))


  admin_changes_keys <- admin_changes %>% filter(grepl("3", change_type))
  admin_changes_names <- admin_changes %>% filter(grepl("4", change_type))
  admin_changes_mergers <- admin_changes %>% filter(grepl("1|2", change_type))


  # Build crosswalk
  crosswalk <-
    municipalities %>%
    select(-matches("key")) %>%
    # Join merger info with old municipality list
    left_join(admin_changes_mergers %>% select(change_type, ags_old, name_old, ags_new, name_new, pop), by = c("ags" = "ags_old")) %>%
    mutate(ags_new = ifelse(is.na(ags_new), ags, ags_new),
           name_new = ifelse(is.na(name_new), name, name_new),
           pop = ifelse(is.na(pop), pop_total, pop)) %>%
    # Keep only merger info for main absorbing municipality
    arrange(ags, desc(pop)) %>%
    group_by(ags) %>% filter(row_number() == 1) %>% ungroup() %>%
    # Join key changes
    left_join(admin_changes_keys %>% select(ags_old, ags_new, name_old),
              by = c("ags_new" = "ags_old", "name_new" = "name_old")) %>%
    mutate(ags_new.y = ifelse(is.na(ags_new.y), ags_new, ags_new.y)) %>%
    select(-ags_new) %>%
    rename(ags_new = ags_new.y) %>%
    # Join name changes
    left_join(admin_changes_names %>% select(ags_old, name_old, name_new),
              by = c("ags_new" = "ags_old", "name_new" = "name_old")) %>%
    mutate(name_new.y = ifelse(is.na(name_new.y), name_new, name_new.y)) %>%
    select(-name_new) %>%
    rename(name_new = name_new.y) %>%
    select(-name_old)

  return(crosswalk)

}

# Aggregate crosswalk to municipalities
aggregate_crosswalk <- function(crosswalk) {

  municipalities <-
    crosswalk %>%
    group_by(ags_new, name_new) %>%
    summarise(pop = first(pop), .groups = "drop") %>%
    rename_with(~ str_remove_all(.x, pattern = "_new")) %>%
    mutate(key_state = str_sub(ags, 1, 2), key_district = str_sub(ags, 3, 3),
           key_county = str_sub(ags, 4, 5), key_municipality = str_sub(ags, 6, 8),
           # key_district_county = case_when(
           #   key_state == "05" & key_district %in% c("1", "2") & key_county == "32" ~ "1/2",
           #   key_state == "05" & key_district %in% c("1", "2") & key_county == "38" ~ "1/2",
           #   key_state == "05" & key_district %in% c("8", "9") & key_county == "35" ~ "8/9",
           #   T ~ key_district)
    ) %>%
    relocate(key_state, key_district, #key_district_county,
             key_municipality, .before = everything())

  return(municipalities)

}

# Merge two crosswalks
merge_crosswalk_dyad <- function(crossA, crossB) {

  crossAB <-
    left_join(crossA %>% select(ags, name, ags_new),
              crossB %>% select(ags, ags_new, name_new),
              by = c("ags_new" = "ags")) %>%
    select(-ags_new) %>% rename(ags_new = ags_new.y) #%>%
  # mutate(weight = weight.x*weight.y) %>% select(-matches("weight\\."))

  return(crossAB)

}

# Main function
get_crosswalk <- function(municipalities, base_year, admin_changes_lists) {


  # Create empty lists to store crosswalk dyads
  crosswalk_list <- list()

  idx <- 1 # Index

  start_idx <- which(names(admin_changes_lists) == as.character(base_year+1))
  end_idx <- which(names(admin_changes_lists) == as.character(2022))

  idx <- 1

  municipalities_temp <- municipalities

  # Loop through years and create crosswalk dyads
  for (admin_changes in admin_changes_lists[start_idx:end_idx]) {

    print(idx)

    crossw <- crosswalk_dyad(municipalities_temp, admin_changes)

    crosswalk_list[[idx]] <- crossw

    municipalities_temp <- aggregate_crosswalk(crossw)

    idx <- idx + 1

  }

  # names(crosswalk_list) <- map2_vec(1968:2021, 1969:2022, ~ paste0(.x, "_", .y))

  # # Create function to merge to crosswalk dyads
  # merge_crosswalk_dyad <- function(crossA, crossB) {
  #
  #   crossAB <-
  #     left_join(crossA %>% select(ags, name, ags_new),
  #               crossB %>% select(ags, ags_new, name_new),
  #               by = c("ags_new" = "ags")) %>%
  #     select(-ags_new) %>% rename(ags_new = ags_new.y) #%>%
  #   # mutate(weight = weight.x*weight.y) %>% select(-matches("weight\\."))
  #
  #   return(crossAB)
  #
  # }

  # Merge all crosswalk dyads into single crosswalk linking first and last year

  cross <- crosswalk_list[[1]]

  idx <- 2

  for (idx in seq(2, length(crosswalk_list))) {

    print(idx)

    crossB <- crosswalk_list[[idx]]

    if (idx == 2) {
      cross_final <- merge_crosswalk_dyad(cross, crossB)
    } else{
      cross_final <- merge_crosswalk_dyad(cross_final, crossB)
    }

    idx <- idx+1

  }

  # Add population info to crosswalk

  cross_final <- left_join(cross_final, municipalities %>% select(ags, matches("pop")), by = "ags")

  return(cross_final)

}

# Create missing lists  ----

municipality_lists <- read_rds("./additional_data/administrative_info/municipality_lists/municipality_lists_clean.rds")
admin_changes_lists <- read_rds("./additional_data/administrative_info/admin_changes_years.rds")

## 1969 units ----

# Counties
counties68 <- read_rds("./additional_data/administrative_info/counties1968_admin.rds")

col_names <- c("merge_id", "unit_type", "regional_key_old", "ags_old", "name_old", "change_type", "area_ha", "pop",
               "regional_key_new", "ags_new", "name_new", "date_effective_legal", "date_effective_stat")

col_types <- c(rep("text", 6), rep("numeric", 2), rep("text", 3), rep("date", 2))

county_changes_69 <- read_xlsx("./additional_data/administrative_info/changes/1969.xlsx", sheet = 2, skip = 7, col_names = F,
                               range = cell_cols("A:M"), col_types = col_types) %>%
  `colnames<-`(col_names) %>% filter(unit_type == "Kreis") %>%
  select(-matches("ags")) %>%
  select(-c(area_ha, pop))

counties69 <-
  counties68 %>%
  left_join(county_changes_69 %>% select(change_type, regional_key_old, regional_key_new, name_new),
            by = c("regional_key" = "regional_key_old")) %>%
  mutate(regional_key_new = ifelse(is.na(regional_key_new), regional_key, regional_key_new),
         name_new = ifelse(is.na(name_new), county_name, name_new)) %>%
  group_by(state_name, regional_key_new, name_new) %>%
  summarise(across(matches("pop"), sum), .groups = "drop") %>%
  rename_with(~ str_remove_all(.x, pattern = "_new")) %>%
  mutate(key_state = str_sub(regional_key, 1, 2),
         key_district = ifelse(str_length(regional_key) == 7, str_sub(regional_key, 3, 5), str_sub(regional_key, 3, 3)),
         key_county = ifelse(str_length(regional_key) == 7, str_sub(regional_key, 6, 7), str_sub(regional_key, 4, 5))) %>%
  relocate(name, regional_key, key_state:key_county, .before = everything()) %>%
  rename(county_name = name)

write_rds(counties69, "./additional_data/administrative_info/counties1969_admin.rds")

# Municipalities
municipalities69 <-
  crosswalk_dyad(municipality_lists[["1968_12_31"]], admin_changes_lists[["1969"]]) %>%
  aggregate_crosswalk() %>%
  group_by(ags) %>% filter(n() == 1) %>% ungroup()

# Create crosswalk from 69' to 71'

crossw_69_70 <- crosswalk_dyad(municipalities69, admin_changes_lists[["1970"]])

municipalities70 <- aggregate_crosswalk(crossw_69_70)

crossw_70_71 <- crosswalk_dyad(municipalities70, admin_changes_lists[["1971"]])

municipalities71 <- municipality_lists[["1971_12_31"]]

crossw_69_71 <- merge_crosswalk_dyad(crossw_69_70, crossw_70_71)

# Add population data from 71 to 69 municipalities

municipalities69_pop <-
  left_join(municipalities69,
            crossw_69_71 %>% select(ags, ags_new),
            by = "ags") %>%
  relocate(key_county, .before = key_municipality) %>%
  rename(pop61 = pop) %>%
  group_by(ags_new) %>% mutate(weight = pop61/sum(pop61)) %>% ungroup() %>%
  left_join(municipalities71 %>% select(ags_new = ags, pop71_new = pop_total),
            by = "ags_new") %>%
  mutate(pop = round(weight*pop71_new)) %>%
  select(-c(pop61, pop71_new, weight, ags_new)) %>%
  mutate(key_district_county = case_when(
           key_state == "05" & key_district %in% c("1", "2") & key_county == "32" ~ "1/2",
           key_state == "05" & key_district %in% c("1", "2") & key_county == "38" ~ "1/2",
           key_state == "05" & key_district %in% c("8", "9") & key_county == "35" ~ "8/9",
           T ~ key_district
         )) %>%
  relocate(key_state, key_district, key_district_county, key_municipality, .before = everything())

write_rds(municipalities69_pop, "./additional_data/administrative_info/municipalities1969_admin.rds")

# Build crosswalks ----

municipalities69 <- read_rds("./additional_data/administrative_info/municipality_lists/1969_12_31.rds")

crosswalk_69_22 <- get_crosswalk(municipalities = municipalities69,
                                 base_year = 1969,
                                 admin_changes_lists = admin_changes_lists)

write_rds(crosswalk_69_22, "./additional_data/administrative_info/crosswalks/crosswalk_69_22.rds")

crosswalk_65_22 <- get_crosswalk(municipalities = municipality_lists[["1965_12_31"]],
                                 base_year = 1965,
                                 admin_changes_lists = admin_changes_lists)


write_rds(crosswalk_65_22, "./additional_data/administrative_info/crosswalks/crosswalk_65_22.rds")

crosswalk_61_22 <- get_crosswalk(municipalities = municipality_lists[["1961_12_31"]],
                                 base_year = 1961,
                                 admin_changes_lists = admin_changes_lists)

write_rds(crosswalk_61_22, "./additional_data/administrative_info/crosswalks/crosswalk_61_22.rds")


crosswalk_57_22 <- get_crosswalk(municipalities = municipality_lists[["1957_12_31"]],
                                 base_year = 1957,
                                 admin_changes_lists = admin_changes_lists)


write_rds(crosswalk_57_22, "./additional_data/administrative_info/crosswalks/crosswalk_57_22.rds")

crosswalk_53_22 <- get_crosswalk(municipalities = municipality_lists[["1953_12_31"]],
                                 base_year = 1953,
                                 admin_changes_lists = admin_changes_lists)

write_rds(crosswalk_53_22, "./additional_data/administrative_info/crosswalks/crosswalk_53_22.rds")


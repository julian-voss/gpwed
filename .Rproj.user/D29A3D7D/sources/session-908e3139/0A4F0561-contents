# This file merges the data for individual election years and maps the municipal
# results to German municipalities as of 2022
#
# Load libraries
library(tidyverse)
library(readxl)
library(magrittr)
library(future)
library(furrr)
library(fuzzyjoin)
library(xlsx)

# Additional functions
source("./code/scripts/functions.R")

# Python setup
library(reticulate)
use_condaenv("ocr_history")

# Merge 1949 ----

clear_environment()

# Combine cleaned municipality returns from different states
btw_49_municipalities <-
  list.files("./output/cleaned/", pattern = "49_btw.*clean", full.names = T) %>%
  map(read_rds) %>%
  map(~ if("dkp" %in% names(.x)){.x %>% rename(dkp_drp = dkp)}else {.x}) %>%
  map(~ if("drp" %in% names(.x)){.x %>% rename(dkp_drp = drp)}else {.x}) %>%
  bind_rows() %>%
  select(-matches("votes_invalid"), -c(turnout, voters, key_municipality)) %>%
  mutate(county_name = str_trim(str_remove_all(county_name, "Ldkr\\.|Ldkr,|,$|\\.|Landkreis|Handkreis")),
         county_name = str_replace(county_name, "Grf\\s", "Grafschaft "),
         key_state = case_match(state_name, "Niedersachsen" ~ "13", "Nordrhein-Westfalen" ~ "14",
                                "Schleswig-Holstein" ~ "11", "Rheinland-Pfalz" ~ "21"),
         municipality_name = str_replace_multiple(municipality_name, c("Ue" = "Ü", "ue" = "ü", "Oe" = "Ö", "oe" = "ö",
                                                                       "ksp|Ksp" = "Kirchspiel", "Gr\\s|Cr\\s|Gr\\." = "Groß ",
                                                                       "ae" = "ä", "Ae" = "Ä"))) %>%
  relocate(key_state, type, .after = state_name)

# Recode counties to match official names
btw_49_municipalities <- btw_49_municipalities %>%
  mutate(county_name = case_when(
    county_name == "Gottingen" ~ "Göttingen",
    county_name == "Gottingen,  Stadt" ~ "Göttingen, Stadt",
    county_name == "Mappen" ~ "Meppen",
    county_name == "Oanabruck" ~ "Osnabrück",
    county_name == "Osnabruck, Stadt" ~ "Osnabrück, Stadt",
    county_name == "Weserminde" ~ "Wesermünde",
    county_name == "resermarach" ~ "Wesermarsch",
    county_name == "Booholt, Stadt" ~ "Bocholt, Stadt",
    county_name == "Boohum, Stadt" ~ "Bochum, Stadt",
    grepl("Det.*old", county_name) ~ "Detmold",
    grepl("Priim|Priin|Prum", county_name) ~ "Prüm",
    grepl("Oberberg.*Kreis", county_name) ~ "Oberbergischer Kreis",
    grepl("Rhein.*Berg.*Kreis", county_name) ~ "Rheinisch-Bergischer Kreis",
    county_name == "Buskirchen" ~ "Euskirchen",
    county_name == "Diren" ~ "Düren",
    county_name == "Dusseldorf-Mettmann" ~ "Düsseldorf-Mettmann",
    county_name == "II Gelsenkirohen, Stadt" ~ "Gelsenkirchen, Stadt",
    county_name == "Jittizenstein" ~ "Wittgenstein",
    county_name == "Kempen-Xrefeld" ~ "Kempen-Krefeld",
    county_name == "Lengo" ~ "Lemgo",
    county_name == "MGladbach, Stadt" ~ "München-Gladbach, Stadt",
    county_name == "Varendorf" ~ "Warendorf",
    county_name == "noahs  Lengo" ~ "Lemgo",
    county_name == "Aunterwesterwaldkreis" | county_name == "S  Unterwesterwaldkreis" ~ "Unterwesterwaldkreis",
    county_name == "Berakastel" ~ "Bernkastel",
    county_name == "Kaiseralantern" ~ "Kaiserslautern",
    county_name == "St Goarshausen" ~ "Sankt Goarshausen",
    county_name == "Stgoar" ~ "Sankt Goar",
    county_name == "Flensburg-Land" ~ "Flensburg",
    county_name == "Hzgt Lauenburg" ~ "Herzogtum Lauenburg",
    county_name == "Dannenberg" ~ "Lüchow-Dannenberg",
    county_name == "Luneburg" ~ "Lüneburg",
    county_name == "Wolfsburg, Stadt" ~ "Gifhorn",
    county_name == "Mulheim adRuhr, Stadt" ~ "Mülheim a.d. Ruhr, Stadt",
    TRUE ~ county_name  # Keep original value if no match
  ),
  county_name = str_replace_multiple(county_name, c("Koln" = "Köln", "Künster" = "Münster", "Reascheid" = "Remscheid",
                                                    "Reoklinghausen" = "Recklinghausen", "Kaisers-lautern" = "Kaiserslautern")))

# Gen municipality and county IDs
btw_49_municipalities <- btw_49_municipalities %>%
  arrange(key_state, county_name) %>%
  group_by(key_state, county_name) %>%
  mutate(county_id = paste0(key_state, str_pad(cur_group_id(), width = 3, pad = 0))) %>%
  mutate(municipality_id = paste0(county_id, str_pad(row_number(), width = 3, pad = 0))) %>%
  ungroup() %>%
  relocate(county_id, .after = county_name)

# Get county names from election returns
btw_49_counties_unique <- btw_49_municipalities %>% drop_na(county_name) %>%
  distinct(state_name, key_state, county_name, county_id) %>% arrange(state_name, county_name)

# Read list of counties & municipalities
counties53_admin <- read_rds("./additional_data/administrative_info/counties1953_admin.rds") %>% arrange(state_name, county_name)
municipalities53_admin <- read_rds("./additional_data/administrative_info/municipalities1953_admin.rds")

# Subset municipalities and counties for which I have to to do string matching
counties53_admin_sub <-
  counties53_admin %>% filter(key_state %in% c("01", "03", "05", "07")) %>%
  rename(key_district_county = key_district) %>%
  filter(!county_name %in% c("Wolfsburg, Stadt"))

# Create linktable for counties from election returns to official list

# counties49_sub_linktable <-
#   bind_cols(btw_49_counties_unique %>%
#               select(state_name, county_name) %>%
#               rename_with(~ paste0(.x, "_elec")) %>%
#               slice_head(n = nrow(counties53_admin_sub)),
#             counties53_admin_sub %>% select(state_name, county_name) %>%
#               slice_head(n = nrow(btw_49_counties_unique))) %>%
#   relocate(matches("county"), .after = everything()) %>%
#   mutate(dist = stringdist::stringdist(county_name_elec, county_name))

counties49_sub_linktable <-
  bind_cols(btw_49_counties_unique %>% rename(county_name_elec = county_name) %>% select(-key_state),
            counties53_admin_sub %>% select(county_name, key_state, key_district_county, key_county)) %>%
  select(county_id, matches("key"))

# Subset list of municipalities with admin county keys and export for string matching
btw_49_municipalities_sub <-
  btw_49_municipalities %>%
  drop_na(county_name) %>%
  select(municipality_name, municipality_id, county_id) %>%
  left_join(counties49_sub_linktable, by = "county_id") %>%
  mutate(municipality_name_match = tolower(municipality_name))

# Stringmatch
municipalities53_admin_sub <-
  filter(municipalities53_admin, key_state %in% c("01", "03", "05", "07")) %>%
  select(key_state, key_district, key_district_county, key_county, key_municipality, name) %>%
  rename(municipality_name = name) %>%
  mutate(municipality_name_match = tolower(municipality_name))

# lt <- import("linktransformer")
# model <- 'dell-research-harvard/lt-wikidata-comp-de' # BEST MODEL SO FAR!
#
# btw_49_municipalities_sub_matched <-
#   lt$merge_blocking(df1 = municipalities53_admin_sub,
#                     df2 = btw_49_municipalities_sub,
#                     merge_type = "1:1",
#                     model = model,
#                     on = list("municipality_name_match"),
#                     blocking_vars = list("key_state", "key_district_county", "key_county"))
#
# write_rds(btw_49_municipalities_sub_matched, "./output/linktables/matched_municipalities_49.rds")

btw_49_municipalities_sub_matched <- read_rds("./output/linktables/matched_municipalities_49.rds")

# Clean linktable
btw_49_municipalities_sub_matched <-
  btw_49_municipalities_sub_matched %>%
  select(-matches("match")) %>%
  mutate(across(matches("municipality_name"), ~ str_trim(str_remove_all(.x, "Stadt")), .names = "{.col}_help")) %>%
  mutate(across(matches("help"), ~ str_trim(str_remove_all(.x, "\\([^)]*\\)|^\\p{P}+|\\p{P}+$")))) %>%
  mutate(across(matches("help"), ~ str_replace_multiple(.x, c("ue|Ue" = "ü")))) %>%
  mutate(stringdist = stringdist::stringdist(municipality_name_x_help, municipality_name_y_help)) %>%
  relocate(score, stringdist, .after = municipality_name_y) %>%
  select(key_state_x, key_district, key_county_x, key_municipality, municipality_id,
         matches("municipality_name_"), score, stringdist, -matches("help")) %>%
  rename_with(~ str_remove(.x, "_x$")) %>%
  drop_na(municipality_id) %>%
  # For multiple matches, keep only match with the highest score
  arrange(municipality_id) %>%
  group_by(municipality_id) %>%
  arrange(desc(score)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(score > .5 | stringdist < 4)

# # Export unmatched for manual matching
# unmatched <-
#   # Filter municipalities without a match
#   filter(btw_49_municipalities,
#          !municipality_id %in% btw_49_municipalities_sub_matched$municipality_id, key_state != "01") %>%
#   select(municipality_id, county_id, county_name, municipality_name, voters_eligible) %>%
#   # Do fuzzy join
#   stringdist_left_join(
#     municipalities53_admin_sub %>% left_join(counties49_sub_linktable) %>% select(key_state, key_district, key_county, key_municipality, municipality_name, county_id),
#     by = c("county_id" = "county_id", "municipality_name" = "municipality_name"),
#     method = "jw",          # Jaro-Winkler distance for fuzzy matching
#     max_dist = 0.5,         # Adjust this threshold based on how strict you want the matching to be
#     distance_col = "dist"   # Optional: adds a column showing the distance between the names
#   )
#
# # Export to validate manually
# unmatched %>%
#   # Pick match with the lowest string distance
#   group_by(municipality_id) %>%
#   arrange(county_id.dist, municipality_name.dist) %>%
#   filter(row_number() == 1, county_id.dist == 0) %>%
#   ungroup() %>%
#   # count(key_state, key_district, key_county) %>%
#   # arrange(-n) %>%
#   # view()
#   # write.xlsx("~/Desktop/inspect.xlsx")
#   # Select variables
#   select(municipality_id, key_state, key_district, key_county, key_municipality, municipality_name.x, municipality_name.y, municipality_name.dist) %>%
#   arrange(desc(municipality_name.dist)) %>%
#   mutate(invalid = "", municipality_name.dist = round(municipality_name.dist, 2)) %>%
#   # Export to excel
#   write.xlsx("./output/linktables/unmatched_municipalities_49.xlsx")

# Read validated matches
unmatched_manual <-
  read_xlsx("./output/linktables/unmatched_municipalities_49_checked.xlsx", sheet = 1,
            col_types = c(rep("text", 9), "numeric")) %>%
  filter(is.na(invalid))

# Add to matched
btw_49_municipalities_sub_matched_complete <-
  bind_rows(
    btw_49_municipalities_sub_matched,
    unmatched_manual %>% select(municipality_id, key_state, key_district, key_county,
                                key_municipality,
                                municipality_name = municipality_name.x,
                                municipality_name_y = municipality_name.y)) %>%
  # If one admin entry has been matched to multiple returns, keep the most similar one
  mutate(stringdist = stringdist::stringdist(municipality_name, municipality_name_y)) %>%
  arrange(key_state, key_district, key_county, key_municipality) %>%
  group_by(key_state, key_district, key_county, key_municipality) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(municipality_name_y != "")


# Add with data containing keys
btw_49_municipalities_keys <-
  btw_49_municipalities %>% select(-key_state) %>%
  left_join(
    btw_49_municipalities_sub_matched_complete %>%
      select(key_state, key_district, key_county, key_municipality, municipality_id, municipality_name_match = municipality_name),
    by = "municipality_id"
  ) %>%
  relocate(municipality_id, county_id, key_state, key_district, key_county, key_municipality, .after = municipality_name) %>%
  unite(ags, key_state, key_district, key_county, key_municipality, sep = "") %>%
  group_by(ags) %>% filter(n() == 1) %>% ungroup()

# Imputation and map to 2022 municipalities

rm(list = Filter(function(x){grepl("counties|unmatched", x)}, ls()))

# Read crosswalk
crosswalk_53_22 <-
  read_rds("./additional_data/administrative_info/crosswalks/crosswalk_53_22.rds")

btw_49_impute <-
  btw_49_municipalities_keys  %>%
  select(state_name:municipality_name, ags, voters_eligible, votes_valid, spd:ssw) %>%
  # Join with crosswalk
  right_join(crosswalk_53_22, by = c("ags" = "ags")) %>%
  # Remove duplicates
  group_by(ags) %>% filter(n() == 1) %>% ungroup() %>%
  # Variable management
  rename(name_matched = name, name_22 = name_new, ags_22 = ags_new, pop = pop_total) %>%
  mutate(rgs_22 = str_sub(ags_22, 1, 5),
         rgs = str_sub(ags, 1, 5)) %>%
  relocate(name_matched:name_22, rgs, rgs_22, .after = municipality_name) %>%
  relocate(pop, .before = voters_eligible) %>%
  # Validate returns through row sums and eligible voters
  mutate(vote_totals = rowSums(select(., spd:ssw), na.rm = T),
         flag = abs(1-(vote_totals/votes_valid)),
         flag_eligible = votes_valid/voters_eligible) %>%
  mutate(across(matches("flag"), ~ round(.x, 4))) %>%
  select(-matches("vote_totals")) %>%
  relocate(matches("flag"), .after = ags) %>%
  # Drop observations if row sums do not match valid votes
  mutate(across(c(voters_eligible, votes_valid:ssw), ~ ifelse(flag > .01, NA, .x)),
         voters_eligible = case_when(
           flag > .01 | flag > .01 ~ NA,
           flag_eligible > 1 & flag <= .01 ~ NA,
           T ~ voters_eligible)) %>%
  select(-matches("flag"))

btw_49_impute <- btw_49_impute %>%
  # Calculate imputation values for voting age population
  # By municipality
  group_by(ags_22) %>%
  mutate(voters_eligible_mun = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_mun = round(voters_eligible_mun*pop)) %>%
  # By county
  group_by(rgs) %>%
  mutate(voters_eligible_county = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_county = round(voters_eligible_county*pop)) %>%
  relocate(matches("voters_eligible_"), .after = ags) %>%
  # Replace missing values in valid rows with zero
  mutate(across(spd:ssw, function(var){case_when(is.na(var) & if_any(spd:ssw, ~ !is.na(.x)) ~ 0, T ~ var)})) %>%
  # Calculate completeness
  # By municipality
  group_by(ags_22) %>%
  mutate(completeness_mun = sum(if_all(spd:ssw, ~ !is.na(.x))*pop, na.rm = T)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # By historical county
  group_by(rgs) %>%
  mutate(completeness_county = sum(if_all(spd:ssw, ~ !is.na(.x))*pop)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # Pivot
  pivot_longer(spd:ssw, names_to = "party") %>%
  mutate(pop_help = ifelse(is.na(votes_valid), NA, pop)) %>%
  # Calculate municipality and county means
  # Municipality
  group_by(ags_22) %>%
  mutate(votes_valid_mun = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_mun = round(votes_valid_mun*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_mun = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # County
  group_by(rgs) %>%
  mutate(votes_valid_county = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_county = round(votes_valid_county*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_county = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # Replace missing values with county or municipality means
  mutate(
    imputed = is.na(votes_valid) & (completeness_mun >= .5 | completeness_county >= .5),
    votes_valid = case_when(
      is.na(votes_valid) & completeness_mun >= .5 ~ round(votes_valid_mun),
      is.na(votes_valid) & completeness_county >= .5 ~ round(votes_valid_county),
      T ~ votes_valid
    ),
    voters_eligible = case_when(
      is.na(voters_eligible) & completeness_mun >= .5 ~ voters_eligible_mun,
      is.na(voters_eligible) & completeness_county >= .5 ~ voters_eligible_county,
      T ~ voters_eligible
    ),
    value = case_when(
      is.na(value) & completeness_mun >= .5 ~ round(value_mun*votes_valid),
      is.na(value) & completeness_county >= .5 ~ round(value_county*votes_valid),
      T ~ value
    )
  ) %>%
  select(-matches("value_"), -c(votes_valid_mun, votes_valid_county)) %>%
  pivot_wider(names_from = "party", values_from = "value")

write_rds(btw_49_impute, "./output/cleaned/49_btw_imputed.rds")

btw_49_mapped <-
  btw_49_impute %>%
  mutate(pop_help2 = ifelse(imputed, pop, NA)) %>%
  relocate(matches("pop_help"), .after = pop) %>%
  group_by(pick(matches("22"))) %>%
  summarise(across(c(pop, pop_help, voters_eligible, votes_valid, spd:ssw), ~ sum(.x, na.rm = T)),
            share_complete = sum(pop_help, na.rm = T),
            share_imputed = sum(pop_help2, na.rm = T),
            .groups = "drop") %>%
  mutate(
    across(c(votes_valid, voters_eligible, spd:ssw), ~ ifelse(if_all(spd:ssw, ~ .x == 0), NA, .x)),
    voters_eligible = ifelse(voters_eligible == 0 & pop != 0, NA, voters_eligible),
    share_complete = share_complete/pop, share_imputed = share_imputed/pop) %>%
  select(-pop_help)

write_rds(btw_49_mapped, "./output/cleaned/49_btw_mapped.rds")

# Merge 1953 ----

clear_environment()

# Combine cleaned municipality returns from different states
btw_53_municipalities <-
  list.files("./output/cleaned/", pattern = "53_btw.*clean", full.names = T) %>%
  map(read_rds) %>%
  bind_rows() %>%
  select(-matches("votes_invalid"), -c(turnout, district_name, voters, voters_first, voters_second)) %>%
  mutate(county_name = str_trim(str_remove_all(county_name, "Ldkr\\.|Ldkr,|,$|\\.|Landkreis")),
         county_name = str_replace(county_name, "Grf\\s", "Grafschaft "),
         key_state = case_match(state_name, "Niedersachsen" ~ "03", "Nordrhein-Westfalen" ~ "05", "Rheinland-Pfalz" ~ "07"),
         municipality_name = str_replace_multiple(municipality_name, c("Ue" = "Ü", "ue" = "ü", "Oe" = "Ö", "oe" = "ö",
                                                                       "ksp|Ksp" = "Kirchspiel", "Gr\\s|Cr\\s" = "Groß ",
                                                                       "ae" = "ä", "Ae" = "Ä"))) %>%
  relocate(matches("key"), type, .after = municipality_name) %>%
  relocate(matches("second"), .after = voters_eligible)

# Recode counties to match official names
btw_53_municipalities <- btw_53_municipalities %>%
  mutate(county_name = case_when(
    county_name == "Aurich" ~ "Aurich (Ostfriesland)",
    county_name == "Hildesheim Marienburg" ~ "Hildesheim-Marienburg",
    county_name == "Id Aachen" ~ "Aachen",
    county_name == "D'dorf-Mettmann" | county_name == "Dusseldorf-Mettmann" ~ "Düsseldorf-Mettmann",
    county_name == "Ahave" ~ "Ahaus",
    county_name == "Id Altena" ~ "Altena",
    county_name == "Detaold" ~ "Detmold",
    county_name == "Duren" ~ "Düren",
    county_name == "Ennepe=Ruhr-Kreis" ~ "Ennepe-Ruhr-Kreis",
    county_name == "Erkelens" ~ "Erkelenz",
    county_name == "Hameln Pyrmont" ~ "Hameln-Pyrmont",
    county_name == "Goldern" ~ "Geldern",
    county_name == "Ferford" ~ "Herford",
    county_name %in% c("Hoxter", "Horter") ~ "Höxter",
    county_name == "K8ln" ~ "Köln",
    grepl("Ke.*pen-Krefeld", county_name) ~ "Kempen-Krefeld",
    county_name == "Lidenscheid, Stadt" ~ "Lüdenscheid, Stadt",
    county_name == "Liinen, Stadt" ~ "Lünen, Stadt",
    county_name == "Ludinghausen" ~ "Lüdinghausen",
    county_name == "MGladbach, Stadt" ~ "Mönchen-Gladbach, Stadt",
    county_name == "Milhein ad, Stadt" ~ "Mülheim a.d. Ruhr, Stadt",
    county_name == "Monschaw" ~ "Monschau",
    county_name == "Romscheid, Stadt" ~ "Remscheid, Stadt",
    county_name == "Rocklinghausen" ~ "Recklinghausen",
    county_name == "Speet" ~ "Soest",
    county_name == "Altenkircher" ~ "Altenkirchen",
    county_name == "Aml Winterburg   Kreuznach" ~ "Kreuznach",
    county_name == "Davn" ~ "Daun",
    county_name == "Landavi D Pfalz" ~ "Landau in der Pfalz",
    county_name == "Ludwigshafen A Rh" | county_name == "Ludwigshafen Am Rhein" ~ "Ludwigshafen am Rhein",
    county_name == "Landau" ~ "Landau in der Pfalz",
    county_name == "Neustadt A D Weinstr" | county_name == "Neustadt" ~ "Neustadt an der Weinstraße",
    county_name == "Prim" ~ "Prüm",
    county_name == "Rockenhsusen" ~ "Rockenhausen",
    county_name == "Simmam" ~ "Simmern",
    county_name == "Zell" ~ "Zell (Mosel)",
    TRUE ~ county_name  # Keep original value if no match
  ),
  county_name = str_trim(str_remove_all(county_name, "^Id |^tId |^i |Noch\\s*|Noch;  |^Stadtkreis |Vom  |;  ")),
  county_name = str_replace_multiple(county_name, c("Koln" = "Köln", "Minster" = "Münster", "Bergiacher" = "Bergischer",
                                                    "Rheinisoh" = "Rheinisch")))

# Gen municipality and county IDs
btw_53_municipalities <- btw_53_municipalities %>%
  arrange(key_state, county_name) %>%
  group_by(key_state, county_name) %>%
  mutate(county_id = paste0(key_state, str_pad(cur_group_id(), width = 3, pad = 0))) %>%
  mutate(municipality_id = paste0(county_id, str_pad(row_number(), width = 3, pad = 0))) %>%
  ungroup() %>%
  relocate(county_id, .after = county_name)

# Get county names from election returns
btw_53_counties_unique <- btw_53_municipalities %>% drop_na(county_name) %>%
  distinct(state_name, key_state, county_name, county_id) %>% arrange(state_name, county_name)

# Read list of counties & municipalities
counties53_admin <- read_rds("./additional_data/administrative_info/counties1953_admin.rds") %>% arrange(state_name, county_name)
municipalities53_admin <- read_rds("./additional_data/administrative_info/municipalities1953_admin.rds")

# Subset municipalities and counties for which I have to to do string matching
counties53_admin_sub <-
  counties53_admin %>% filter(key_state %in% c("03", "05", "07")) %>%
  rename(key_district_county = key_district)

# Create linktable for counties from election returns to official list

# counties53_sub_linktable <-
#   bind_cols(btw_53_counties_unique %>%
#               select(state_name, county_name) %>%
#               rename_with(~ paste0(.x, "_elec")) %>%
#               slice_head(n = nrow(counties53_admin_sub)),
#             counties53_admin_sub %>% select(state_name, county_name) %>%
#               slice_head(n = nrow(btw_53_counties_unique))) %>%
#   relocate(matches("county"), .after = everything()) %>%
#   mutate(dist = stringdist::stringdist(county_name_elec, county_name))

counties53_sub_linktable <-
  bind_cols(btw_53_counties_unique %>% rename(county_name_elec = county_name),
            counties53_admin_sub %>% select(county_name, key_district_county, key_county)) %>%
  select(county_id, matches("key"))

# Subset list of municipalities with admin county keys and export for string matching
btw_53_municipalities_sub <-
  btw_53_municipalities %>%
  drop_na(county_name) %>%
  select(municipality_name, municipality_id, county_id) %>%
  left_join(counties53_sub_linktable, by = "county_id") %>%
  mutate(municipality_name_match = tolower(municipality_name))

# Stringmatch
municipalities53_admin_sub <-
  filter(municipalities53_admin, key_state %in% c("03", "05", "07")) %>%
  select(key_state, key_district, key_district_county, key_county, key_municipality, name) %>%
  rename(municipality_name = name) %>%
  mutate(municipality_name_match = tolower(municipality_name))

# lt <- import("linktransformer")
# model <- 'dell-research-harvard/lt-wikidata-comp-de' # BEST MODEL SO FAR!
#
# btw_53_municipalities_sub_matched <-
#   lt$merge_blocking(df1 = municipalities53_admin_sub,
#                     df2 = btw_53_municipalities_sub,
#                     merge_type = "1:1",
#                     model = model,
#                     on = list("municipality_name_match"),
#                     blocking_vars = list("key_state", "key_district_county", "key_county"))
#
# write_rds(btw_53_municipalities_sub_matched, "./output/linktables/matched_municipalities_53.rds")

btw_53_municipalities_sub_matched <- read_rds("./output/linktables/matched_municipalities_53.rds")

# Clean linktable
btw_53_municipalities_sub_matched <-
  btw_53_municipalities_sub_matched %>%
  select(-matches("match")) %>%
  mutate(across(matches("municipality_name"), ~ str_trim(str_remove_all(.x, "Stadt")), .names = "{.col}_help")) %>%
  mutate(across(matches("help"), ~ str_trim(str_remove_all(.x, "\\([^)]*\\)|^\\p{P}+|\\p{P}+$")))) %>%
  mutate(across(matches("help"), ~ str_replace_multiple(.x, c("ue|Ue" = "ü")))) %>%
  mutate(stringdist = stringdist::stringdist(municipality_name_x_help, municipality_name_y_help)) %>%
  relocate(score, stringdist, .after = municipality_name_y) %>%
  select(key_state_x, key_district, key_county_x, key_municipality, municipality_id,
         matches("municipality_name_"), score, stringdist, -matches("help")) %>%
  rename_with(~ str_remove(.x, "_x$")) %>%
  drop_na(municipality_id) %>%
  # For multiple matches, keep only match with the highest score
  arrange(municipality_id) %>%
  group_by(municipality_id) %>%
  # filter(n() > 1)
  arrange(desc(score)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# # Export unmatched for manual matching
# unmatched <-
#   # Filter municipalities without a match
#   filter(btw_53_municipalities,
#          !municipality_id %in% btw_53_municipalities_sub_matched$municipality_id, key_state != "01") %>%
#   select(municipality_id, county_id, county_name, municipality_name, voters_eligible) %>%
#   # Do fuzzy join
#   stringdist_left_join(
#     municipalities53_admin_sub %>% left_join(counties53_sub_linktable) %>% select(key_state, key_district, key_county, key_municipality, municipality_name, county_id),
#     by = c("county_id" = "county_id", "municipality_name" = "municipality_name"),
#     method = "jw",          # Jaro-Winkler distance for fuzzy matching
#     max_dist = 0.5,         # Adjust this threshold based on how strict you want the matching to be
#     distance_col = "dist"   # Optional: adds a column showing the distance between the names
#   )
#
# # Export to validate manually
# unmatched %>%
#   # Pick match with the lowest string distance
#   group_by(municipality_id) %>%
#   arrange(county_id.dist, municipality_name.dist) %>%
#   filter(row_number() == 1, county_id.dist == 0) %>%
#   ungroup() %>%
#   # Select variables
#   select(municipality_id, key_state, key_district, key_county, key_municipality, municipality_name.x, municipality_name.y, municipality_name.dist) %>%
#   arrange(desc(municipality_name.dist)) %>%
#   mutate(invalid = "", municipality_name.dist = round(municipality_name.dist, 2)) %>%
#   # Export to excel
#   write.xlsx("./output/linktables/unmatched_municipalities_53.xlsx")

# Read validated matches
unmatched_manual <-
  read_xlsx("./output/linktables/unmatched_municipalities_53_checked.xlsx", sheet = 1,
            col_types = c(rep("text", 9), "numeric")) %>%
  filter(is.na(invalid))

# Add to matched
btw_53_municipalities_sub_matched_complete <-
  bind_rows(
    btw_53_municipalities_sub_matched,
    unmatched_manual %>% select(municipality_id, key_state, key_district, key_county,
                                key_municipality,
                                municipality_name = municipality_name.x,
                                municipality_name_y = municipality_name.y)) %>%
  # If one admin entry has been matched to multiple returns, keep the most similar one
  mutate(stringdist = stringdist::stringdist(municipality_name, municipality_name_y)) %>%
  arrange(key_state, key_district, key_county, key_municipality) %>%
  group_by(key_state, key_district, key_county, key_municipality) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(municipality_name_y != "")

# Add with data containing keys
btw_53_municipalities_keys <-
  btw_53_municipalities %>%
  left_join(
    btw_53_municipalities_sub_matched_complete %>%
      select(key_district, key_county, key_municipality, municipality_id, municipality_name_match = municipality_name),
    by = "municipality_id"
  ) %>%
  relocate(municipality_id, county_id, key_state, key_district, key_county, key_municipality, .after = municipality_name) %>%
  unite(ags, key_state, key_district, key_county, key_municipality, sep = "") %>%
  group_by(ags) %>% filter(n() == 1) %>% ungroup()

# Map to 2022 municipalities

rm(list = Filter(function(x){grepl("counties|unmatched", x)}, ls()))

# Read crosswalk
crosswalk_53_22 <-
  read_rds("./additional_data/administrative_info/crosswalks/crosswalk_53_22.rds") %>%
  filter(str_sub(ags, 1, 2) != "01")

btw_53_impute <-
  btw_53_municipalities_keys  %>%
  select(state_name:municipality_name, ags, voters_eligible, matches("second|first")) %>%
  # Join with crosswalk
  right_join(crosswalk_53_22, by = c("ags" = "ags")) %>%
  # Remove duplicates
  group_by(ags) %>% filter(n() == 1) %>% ungroup() %>%
  # Variable management
  rename(name_matched = name, name_22 = name_new, ags_22 = ags_new, pop = pop_total) %>%
  mutate(rgs_22 = str_sub(ags_22, 1, 5),
         rgs = str_sub(ags, 1, 5)) %>%
  relocate(name_matched:name_22, rgs, rgs_22, .after = municipality_name) %>%
  relocate(pop, .before = voters_eligible) %>%
  # Validate returns through row sums and eligible voters
  mutate(vote_totals_first = rowSums(select(., spd_first:pdgd_first), na.rm = T),
         flag_first = abs(1-(vote_totals_first/votes_valid_first)),
         vote_totals_second = rowSums(select(., spd_second:pdgd_second), na.rm = T),
         flag_second = abs(1-(vote_totals_second/votes_valid_second)),
         flag_first_second = abs(1-(votes_valid_first/votes_valid_second)),
         flag_eligible_first = votes_valid_first/voters_eligible,
         flag_eligible_second = votes_valid_second/voters_eligible) %>%
  mutate(across(matches("flag"), ~ round(.x, 4))) %>%
  select(-matches("vote_totals")) %>%
  relocate(matches("flag"), .after = ags) %>%
  # Drop observations if row sums do not match valid votes
  mutate(across(c(votes_valid_first:pdgd_first), ~ ifelse(flag_first > .01, NA, .x)),
         across(c(votes_valid_second:pdgd_second), ~ ifelse(flag_second > .01, NA, .x)),
         across(c(voters_eligible, votes_valid_first:pdgd_first, votes_valid_second:pdgd_second), ~ if_else(flag_first_second > .5, NA, .x, .x)),
         voters_eligible = case_when(
           flag_second > .01 | flag_first > .01 ~ NA,
           flag_eligible_first > 1 & flag_first <= .01 ~ NA,
           flag_eligible_second > 1 & flag_second <= .01 ~ NA,
           T ~ voters_eligible)) %>%
  select(-matches("flag"))

btw_53_impute <- btw_53_impute %>%
  # Calculate imputation values for voting age population
  # By municipality
  group_by(ags_22) %>%
  mutate(voters_eligible_mun = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_mun = round(voters_eligible_mun*pop)) %>%
  # By county
  group_by(rgs) %>%
  mutate(voters_eligible_county = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_county = round(voters_eligible_county*pop)) %>%
  relocate(matches("voters_eligible_"), .after = ags) %>%
  pivot_longer(cols = starts_with("votes_valid") | ends_with("_first") | ends_with("_second"),
               names_to = c("vote_type", "first_second_vote"),
               names_pattern = "(.*)_(first|second)",
               values_to = "votes") %>%
  pivot_wider(names_from = vote_type, values_from = votes) %>%
  # Replace missing values in valid rows with zero
  mutate(across(cdu:center, function(var){case_when(is.na(var) & if_any(spd:center, ~ !is.na(.x)) ~ 0, T ~ var)})) %>%
  # Calculate completeness
  # By municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(completeness_mun = sum(if_all(spd:center, ~ !is.na(.x))*pop, na.rm = T)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # By historical county
  group_by(rgs, first_second_vote) %>%
  mutate(completeness_county = sum(if_all(spd:center, ~ !is.na(.x))*pop)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # Pivot
  pivot_longer(spd:center, names_to = "party") %>%
  mutate(pop_help = ifelse(is.na(votes_valid), NA, pop)) %>%
  # Calculate municipality and county means
  # Municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(votes_valid_mun = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_mun = round(votes_valid_mun*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_mun = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # County
  group_by(rgs, first_second_vote) %>%
  mutate(votes_valid_county = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_county = round(votes_valid_county*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_county = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # Replace missing values with county or municipality means
  mutate(
    imputed = is.na(votes_valid) & (completeness_mun >= .5 | completeness_county >= .5),
    votes_valid = case_when(
      is.na(votes_valid) & completeness_mun >= .5 ~ round(votes_valid_mun),
      is.na(votes_valid) & completeness_county >= .5 ~ round(votes_valid_county),
      T ~ votes_valid
    ),
    voters_eligible = case_when(
      is.na(voters_eligible) & completeness_mun >= .5 ~ voters_eligible_mun,
      is.na(voters_eligible) & completeness_county >= .5 ~ voters_eligible_county,
      T ~ voters_eligible
    ),
    value = case_when(
      is.na(value) & completeness_mun >= .5 ~ round(value_mun*votes_valid),
      is.na(value) & completeness_county >= .5 ~ round(value_county*votes_valid),
      T ~ value
    )
  ) %>%
  select(-matches("value_"), -c(votes_valid_mun, votes_valid_county)) %>%
  pivot_wider(values_from = value, names_from = party)

write_rds(btw_53_impute, "./output/cleaned/53_btw_imputed.rds")

btw_53_mapped <-
  btw_53_impute %>%
  mutate(pop_help2 = ifelse(imputed, pop, NA)) %>%
  relocate(matches("pop_help"), .after = pop) %>%
  group_by(pick(matches("22")), first_second_vote) %>%
  summarise(across(c(pop, pop_help, voters_eligible, votes_valid, spd:center), ~ sum(.x, na.rm = T)),
            share_complete = sum(pop_help, na.rm = T),
            share_imputed = sum(pop_help2, na.rm = T),
            .groups = "drop") %>%
  mutate(
    across(c(votes_valid, voters_eligible, spd:center), ~ ifelse(if_all(spd:center, ~ .x == 0), NA, .x)),
    voters_eligible = ifelse(voters_eligible == 0 & pop != 0, NA, voters_eligible),
    share_complete = share_complete/pop, share_imputed = share_imputed/pop,
    ) %>%
  select(-pop_help) %>%
  # Remove values for first vote for States with no info
  mutate(across(c(votes_valid, spd:center), ~ ifelse(first_second_vote == "first" & str_sub(ags_22, 1, 2) == "05", NA, .x))) %>%
  rename(pdgd_dns = pdgd)

write_rds(btw_53_mapped, "./output/cleaned/53_btw_mapped.rds")

# Merge 1957 ----

clear_environment()

# Combine cleaned municipality returns from different states
btw_57_municipalities <-
  list.files("./output/cleaned/", pattern = "57_btw.*clean", full.names = T) %>%
  map(read_rds) %>%
  bind_rows() %>%
  select(-matches("votes_invalid"), -c(pop, sheet)) %>%
  mutate(county_name = str_trim(str_remove_all(county_name, "Ldkr\\.|Ldkr,|,$|\\.")),
         county_name = str_replace(county_name, "Grf\\s", "Grafschaft "),
         key_state = case_match(state_name, "Niedersachsen" ~ "03", "Nordrhein-Westfalen" ~ "05",
                                "Schleswig-Holstein" ~ "01", "Rheinland-Pfalz" ~ "07",
                                "Saarland" ~ "48")) %>%
  relocate(matches("key"), type, .after = municipality_id) %>%
  relocate(turnout, .after = voters) %>%
  relocate(matches("first"), .after = turnout) %>%
  select(-c(key_municipality, voters, voters_first, voters_second))


# Recode counties to match administrative
btw_57_municipalities <- btw_57_municipalities %>%
  mutate(county_name = case_when(
    county_name == "Rannover" ~ "Hannover",
    county_name == "Luneburg" ~ "Lüneburg",
    county_name == "D'dorf-Mettmann" ~ "Düsseldorf-Mettmann",
    county_name == "is Gladbeck, Stadt" ~ "Gladbeck, Stadt",
    county_name == "IF Münster, Stadt" ~ "Münster, Stadt",
    county_name == "Id Tecklenburg" ~ "Tecklenburg",
    county_name == "Hoxter" ~ "Höxter",
    county_name == "Koln, Stadt" ~ "Köln, Stadt",
    county_name == "MGladbach   , Stadt" ~ "Mönchen-Gladbach, Stadt",
    county_name == "Rhein-Berg Kreis" ~ "Rheinisch-Bergischer Kreis",
    county_name == "Saarbrücken-Stadt" ~ "Saarbrücken, Stadt",
    county_name == "Saarbrücken-Land" ~ "Saarbrücken",
    county_name == "Flensburg Stadt" ~ "Flensburg, Stadt",
    county_name == "Flensburg-Land" ~ "Flensburg",
    county_name == "Lauenburg" ~ "Herzogtum Lauenburg",
    county_name == "Sudtondern" ~ "Südtondern",
    county_name == "Lineburg, Stadt" ~ "Lüneburg, Stadt",
    county_name == "Bernkaste" ~ "Bernkastel",
    county_name == "Kirchl" ~ "Kirchheimbolanden",
    county_name == "Neustadt An" ~ "Neustadt An Der Weinstraße",
    TRUE ~ county_name  # Keep original value if no match
  ),
  county_name = str_replace_multiple(county_name,
                                     c("Gottingen" = "Göttingen", "01db" = "Oldenburg",
                                       "Osnabruck" = "Osnabrück",
                                       "Altenkirche.*(Westerw)" = "Altenkirchen (Westerwald)")))



# Gen municipality and county IDs
btw_57_municipalities <- btw_57_municipalities %>%
  arrange(key_state, county_name) %>%
  group_by(key_state, county_name) %>%
  mutate(county_id = paste0(key_state, str_pad(cur_group_id(), width = 3, pad = 0))) %>%
  mutate(municipality_id = paste0(county_id, str_pad(row_number(), width = 3, pad = 0))) %>%
  ungroup() %>%
  relocate(county_id, .after = county_name)

# Get county names from election returns
btw_57_counties_unique <- btw_57_municipalities %>% drop_na(county_name) %>%
  distinct(state_name, key_state, county_name, county_id) %>% arrange(state_name, county_name)

# Read list of counties & municipalities
counties57_admin <- read_rds("./additional_data/administrative_info/counties1957_admin.rds") %>% arrange(state_name, county_name)
municipalities57_admin <- read_rds("./additional_data/administrative_info/municipalities1957_admin.rds")

# Subset municipalities and counties for which I have to to do string matching
counties57_admin_sub <-
  counties57_admin %>% filter(key_state %in% c("01", "03", "05", "07", "48")) %>%
  rename(key_district_county = key_district)

# Create linktable for counties from election returns to official list

# counties57_sub_linktable <-
#   bind_cols(btw_57_counties_unique %>%
#               select(state_name, county_name) %>%
#               rename_with(~ paste0(.x, "_elec")) %>%
#               slice_head(n = nrow(counties57_admin_sub)),
#             counties57_admin_sub %>% select(state_name, county_name) %>%
#               slice_head(n = nrow(btw_57_counties_unique))) %>%
#   relocate(matches("county"), .after = everything()) %>%
#   mutate(dist = stringdist::stringdist(county_name_elec, county_name))

counties57_sub_linktable <-
  bind_cols(btw_57_counties_unique %>% rename(county_name_elec = county_name),
            counties57_admin_sub %>% select(county_name, key_district_county, key_county)) %>%
  select(county_id, matches("key"))

# Subset list of municipalities with admin county keys and export for string matching
btw_57_municipalities_sub <-
  btw_57_municipalities %>%
  drop_na(county_name) %>%
  select(municipality_name, municipality_id, county_id) %>%
  left_join(counties57_sub_linktable, by = "county_id") %>%
  mutate(municipality_name_match = tolower(municipality_name))

# Stringmatch
municipalities57_admin_sub <-
  filter(municipalities57_admin, key_state %in% c("01", "03", "05", "07", "48")) %>%
  select(key_state, key_district, key_district_county, key_county, key_municipality, name) %>%
  rename(municipality_name = name) %>%
  mutate(municipality_name_match = tolower(municipality_name))

# lt <- import("linktransformer")
# model <- 'dell-research-harvard/lt-wikidata-comp-de' # BEST MODEL SO FAR!
#
# btw_57_municipalities_sub_matched <-
#   lt$merge_blocking(df1 = municipalities57_admin_sub,
#                     df2 = btw_57_municipalities_sub,
#                     merge_type = "1:1",
#                     model = model,
#                     on = list("municipality_name_match"),
#                     blocking_vars = list("key_state", "key_district_county", "key_county"))
#
# write_rds(btw_57_municipalities_sub_matched, "./output/linktables/matched_municipalities_57.rds")

btw_57_municipalities_sub_matched <- read_rds("./output/linktables/matched_municipalities_57.rds")

# Clean linktable
btw_57_municipalities_sub_matched <-
  btw_57_municipalities_sub_matched %>%
  select(-matches("match")) %>%
  mutate(across(matches("municipality_name"), ~ str_trim(str_remove_all(.x, "Stadt")), .names = "{.col}_help")) %>%
  mutate(across(matches("help"), ~ str_trim(str_remove_all(.x, "\\([^)]*\\)|^\\p{P}+|\\p{P}+$")))) %>%
  mutate(across(matches("help"), ~ str_replace_multiple(.x, c("ue|Ue" = "ü")))) %>%
  mutate(stringdist = stringdist::stringdist(municipality_name_x_help, municipality_name_y_help)) %>%
  relocate(score, stringdist, .after = municipality_name_y) %>%
  select(key_state_x, key_district, key_county_x, key_municipality, municipality_id,
         matches("municipality_name_"), score, stringdist, -matches("help")) %>%
  rename_with(~ str_remove(.x, "_x$")) %>%
  drop_na(municipality_id) %>%
  # For multiple matches, keep only match with the highest score
  arrange(municipality_id) %>%
  group_by(municipality_id) %>%
  # filter(n() > 1)
  arrange(desc(score)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# # Export unmatched for manual matching
# unmatched <-
#   # Filter municipalities without a match
#   filter(btw_57_municipalities,
#          !municipality_id %in% btw_57_municipalities_sub_matched$municipality_id, key_state != "01") %>%
#   select(municipality_id, county_id, county_name, municipality_name, voters_eligible) %>%
#   # Do fuzzy join
#   stringdist_left_join(
#     municipalities57_admin_sub %>% left_join(counties57_sub_linktable) %>% select(key_state, key_district, key_county, key_municipality, municipality_name, county_id),
#     by = c("county_id" = "county_id", "municipality_name" = "municipality_name"),
#     method = "jw",          # Jaro-Winkler distance for fuzzy matching
#     max_dist = 0.5,         # Adjust this threshold based on how strict you want the matching to be
#     distance_col = "dist"   # Optional: adds a column showing the distance between the names
#   )
#
# # Export to validate manually
# unmatched %>%
#   # Pick match with the lowest string distance
#   group_by(municipality_id) %>%
#   arrange(county_id.dist, municipality_name.dist) %>%
#   filter(row_number() == 1, county_id.dist == 0) %>%
#   ungroup() %>%
#   # Select variables
#   select(municipality_id, key_state, key_district, key_county, key_municipality, municipality_name.x, municipality_name.y, municipality_name.dist) %>%
#   arrange(desc(municipality_name.dist)) %>%
#   mutate(invalid = "", municipality_name.dist = round(municipality_name.dist, 2)) %>%
#   # Export to excel
#   write.xlsx("./output/linktables/unmatched_municipalities_57.xlsx")

# Read validated matches
unmatched_manual <-
  read_xlsx("./output/linktables/unmatched_municipalities_57_checked.xlsx", sheet = 1,
            col_types = c(rep("text", 9), "numeric")) %>%
  filter(is.na(invalid))

# Add to matched
btw_57_municipalities_sub_matched_complete <-
  bind_rows(
    btw_57_municipalities_sub_matched,
    unmatched_manual %>% select(municipality_id, key_state, key_district, key_county,
                                key_municipality,
                                municipality_name = municipality_name.x,
                                municipality_name_y = municipality_name.y)) %>%
  # If one admin entry has been matched to multiple returns, keep the most similar one
  mutate(stringdist = stringdist::stringdist(municipality_name, municipality_name_y)) %>%
  arrange(key_state, key_district, key_county, key_municipality) %>%
  group_by(key_state, key_district, key_county, key_municipality) %>%
  filter(row_number() == 1) %>%
  ungroup()


# Add with data containing keys
btw_57_municipalities_keys <-
  btw_57_municipalities %>%
  left_join(
    btw_57_municipalities_sub_matched_complete %>%
      select(key_district, key_county, key_municipality, municipality_id, municipality_name_match = municipality_name),
    by = "municipality_id"
  ) %>%
  relocate(municipality_id, county_id, key_state, key_district, key_county, key_municipality, .after = municipality_name) %>%
  select(-c(turnout)) %>%
  unite(ags, key_state, key_district, key_county, key_municipality, sep = "") %>%
  group_by(ags) %>% filter(n() == 1) %>% ungroup()

# Map to 2022 municipalities

rm(list = Filter(function(x){grepl("counties|unmatched", x)}, ls()))

# Read crosswalk
crosswalk_57_22 <- read_rds("./additional_data/administrative_info/crosswalks/crosswalk_57_22.rds")

btw_57_impute <-
  btw_57_municipalities_keys  %>%
  select(state_name:municipality_name, ags, voters_eligible, matches("second|first")) %>%
  # Join with crosswalk
  right_join(crosswalk_57_22, by = c("ags" = "ags")) %>%
  # Remove duplicates
  group_by(ags) %>% filter(n() == 1) %>% ungroup() %>%
  # Variable management
  rename(name_matched = name, name_22 = name_new, ags_22 = ags_new, pop = pop_total) %>%
  mutate(rgs_22 = str_sub(ags_22, 1, 5),
         rgs = str_sub(ags, 1, 5)) %>%
  relocate(name_matched:name_22, rgs, rgs_22, .after = municipality_name) %>%
  relocate(pop, .before = voters_eligible) %>%
  # Validate returns through row sums and eligible voters
  mutate(vote_totals_first = rowSums(select(., cdu_first:pdgd_first), na.rm = T),
         flag_first = abs(1-(vote_totals_first/votes_valid_first)),
         vote_totals_second = rowSums(select(., cdu_second:ssw_second), na.rm = T),
         flag_second = abs(1-(vote_totals_second/votes_valid_second)),
         flag_first_second = abs(1-(votes_valid_first/votes_valid_second)),
         flag_eligible_first = votes_valid_first/voters_eligible,
         flag_eligible_second = votes_valid_second/voters_eligible) %>%
  mutate(across(matches("flag"), ~ round(.x, 4))) %>%
  select(-matches("vote_totals")) %>%
  relocate(matches("flag"), .after = ags) %>%
  # Drop observations if row sums do not match valid votes
  mutate(across(c(votes_valid_first:pdgd_first), ~ ifelse(flag_first > .01, NA, .x)),
         across(c(votes_valid_second:ssw_second), ~ ifelse(flag_second > .01, NA, .x)),
         across(c(voters_eligible, votes_valid_first:pdgd_first, votes_valid_second:ssw_second), ~ if_else(flag_first_second > .5, NA, .x, .x)),
         voters_eligible = case_when(
           flag_second > .01 | flag_first > .01 ~ NA,
           flag_eligible_first > 1 & flag_first <= .01 ~ NA,
           flag_eligible_second > 1 & flag_second <= .01 ~ NA,
           T ~ voters_eligible,
         )
  ) %>%
  select(-matches("flag"))

btw_57_impute <- btw_57_impute %>%
  # Calculate imputation values for voting age population
  # By municipality
  group_by(ags_22) %>%
  mutate(voters_eligible_mun = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_mun = round(voters_eligible_mun*pop)) %>%
  # By county
  group_by(rgs) %>%
  mutate(voters_eligible_county = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_county = round(voters_eligible_county*pop)) %>%
  relocate(matches("voters_eligible_"), .after = ags) %>%
  pivot_longer(cols = starts_with("votes_valid") | ends_with("_first") | ends_with("_second"),
               names_to = c("vote_type", "first_second_vote"),
               names_pattern = "(.*)_(first|second)",
               values_to = "votes") %>%
  pivot_wider(names_from = vote_type, values_from = votes) %>%
  # Replace missing values in valid rows with zero
  mutate(across(cdu:ssw, function(var){case_when(is.na(var) & if_any(cdu:ssw, ~ !is.na(.x)) ~ 0, T ~ var)})) %>%
  # Calculate completeness
  # By municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(completeness_mun = sum(if_all(cdu:ssw, ~ !is.na(.x))*pop, na.rm = T)/sum(pop)) %>%
  ungroup() %>%
  # By historical county
  group_by(rgs, first_second_vote) %>%
  mutate(completeness_county = sum(if_all(cdu:ssw, ~ !is.na(.x))*pop)/sum(pop)) %>%
  ungroup() %>%
  # Pivot
  pivot_longer(cdu:ssw, names_to = "party") %>%
  mutate(pop_help = ifelse(is.na(votes_valid), NA, pop)) %>%
  # Calculate municipality and county means
  # Municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(votes_valid_mun = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_mun = round(votes_valid_mun*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_mun = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # County
  group_by(rgs, first_second_vote) %>%
  mutate(votes_valid_county = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_county = round(votes_valid_county*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_county = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # Replace missing values with county or municipality means
  mutate(
    imputed = is.na(votes_valid) & (completeness_mun >= .5 | completeness_county >= .5),
    votes_valid = case_when(
      is.na(votes_valid) & completeness_mun >= .5 ~ round(votes_valid_mun),
      is.na(votes_valid) & completeness_county >= .5 ~ round(votes_valid_county),
      T ~ votes_valid
    ),
    voters_eligible = case_when(
      is.na(voters_eligible) & completeness_mun >= .5 ~ voters_eligible_mun,
      is.na(voters_eligible) & completeness_county >= .5 ~ voters_eligible_county,
      T ~ voters_eligible
    ),
    value = case_when(
      is.na(value) & completeness_mun >= .5 ~ round(value_mun*votes_valid),
      is.na(value) & completeness_county >= .5 ~ round(value_county*votes_valid),
      T ~ value
    )
  ) %>%
  select(-matches("value_"), -c(votes_valid_mun, votes_valid_county)) %>%
  pivot_wider(values_from = value, names_from = party)

write_rds(btw_57_impute, "./output/cleaned/57_btw_imputed.rds")

btw_57_mapped <-
  btw_57_impute %>%
  mutate(pop_help2 = ifelse(imputed, pop, NA)) %>%
  relocate(matches("pop_help"), .after = pop) %>%
  group_by(pick(matches("22")), first_second_vote) %>%
  summarise(across(c(pop, pop_help, voters_eligible, votes_valid, cdu:ssw), ~ sum(.x, na.rm = T)),
            share_complete = sum(pop_help, na.rm = T),
            share_imputed = sum(pop_help2, na.rm = T),
            .groups = "drop") %>%
  mutate(
    across(c(votes_valid, voters_eligible, cdu:ssw), ~ ifelse(if_all(cdu:ssw, ~ .x == 0), NA, .x)),
    voters_eligible = ifelse(voters_eligible == 0 & pop != 0, NA, voters_eligible),
    share_complete = share_complete/pop, share_imputed = share_imputed/pop) %>%
  select(-pop_help) %>%
  # Remove values for first vote for States with no info
  mutate(across(c(votes_valid, cdu:ssw), ~ ifelse(first_second_vote == "first" & !str_sub(ags_22, 1, 2) %in% c("03", "07"), NA, .x)))

write_rds(btw_57_mapped, "./output/cleaned/57_btw_mapped.rds")

# Merge 1961 ----

clear_environment()

# Combine cleaned municipality returns from different states
btw_61_municipalities <-
  list.files("./output/cleaned/", pattern = "61_btw.*clean", full.names = T) %>%
  map(read_rds) %>%
  bind_rows() %>%
  select(-matches("votes_invalid"), -c(turnout, voters)) %>%
  mutate(county_name = str_trim(str_remove_all(county_name, "Ldkr\\.|Ldkr,|,$|\\.|Landkreis")),
         county_name = str_replace(county_name, "Grf\\s", "Grafschaft "),
         key_state = case_match(state_name, "Niedersachsen" ~ "03", "Nordrhein-Westfalen" ~ "05",
                                "Schleswig-Holstein" ~ "01", "Rheinland-Pfalz" ~ "07",
                                "Saarland" ~ "10"),
         municipality_name = str_replace_multiple(municipality_name, c("Ue" = "Ü", "ue" = "ü", "Oe" = "Ö", "oe" = "ö",
                                                                       "ksp|Ksp" = "Kirchspiel", "Gr\\s|Cr\\s" = "Groß ",
                                                                       "ae" = "ä", "Ae" = "Ä"))) %>%
  relocate(matches("key"), type, .after = municipality_id) %>%
  relocate(matches("second"), .after = voters_eligible)

# Recode counties to match official names
btw_61_municipalities <- btw_61_municipalities %>%
  mutate(county_name = case_when(
    county_name == "Goettingen" ~ "Göttingen",
    county_name == "Hildesheim Marienag" ~ "Hildesheim-Marienburg",
    county_name == "Oldenburg Oldenaurg" ~ "Oldenburg (Oldenburg)",
    county_name == "01pe" | county_name == "nooh:  Olpe" ~ "Olpe",
    county_name == "Julich" ~ "Jülich",
    county_name == "Velzen" ~ "Uelzen",
    county_name == "Soeat" ~ "Soest",
    county_name == "Rhein-Berg Kreis" ~ "Rheinisch-Bergischer Kreis",
    county_name == "Flensburg Land" | county_name == "Flensburg-Land" ~ "Flensburg",
    county_name == "Flensburg~Stadt" ~ "Flensburg, Stadt",
    county_name == "Hzgt Lauenburg" | county_name == "Hzgtlauenburg" ~ "Herzogtum Lauenburg",
    county_name == "Plon" ~ "Plön",
    county_name == "Hameln Pyrmont" ~ "Hameln-Pyrmont",
    # county_name %in% c("Hzgt Lauenburg", "Hzgtlauenburg") ~ "Herzogtum Lauenburg"
    TRUE ~ county_name  # Keep original value if no match
  ))

# Gen municipality and county IDs
btw_61_municipalities <- btw_61_municipalities %>%
  mutate(county_name_help = case_when(key_state == "07" ~ str_sub(key_municipality, 1, 3),
                                      T ~ county_name)) %>%
  arrange(key_state, county_name_help) %>%
  group_by(key_state, county_name_help) %>%
  mutate(county_id = paste0(key_state, str_pad(cur_group_id(), width = 3, pad = 0))) %>%
  mutate(municipality_id = paste0(county_id, str_pad(row_number(), width = 3, pad = 0))) %>%
  ungroup() %>%
  relocate(county_id, .after = county_name) %>% select(-county_name_help)

# Get county names from election returns
btw_61_counties_unique <- btw_61_municipalities %>% drop_na(county_name) %>%
  filter(key_state != "07") %>%
  distinct(state_name, key_state, county_name, county_id) %>% arrange(state_name, county_name)

# Read list of counties & municipalities
counties61_admin <- read_rds("./additional_data/administrative_info/counties1961_admin.rds") %>% arrange(state_name, county_name)
municipalities61_admin <- read_rds("./additional_data/administrative_info/municipalities1961_admin.rds")

# Subset municipalities and counties for which I have to to do string matching
counties61_admin_sub <-
  counties61_admin %>% filter(key_state %in% c("01", "03", "05", "10")) %>%
  rename(key_district_county = key_district)

# Create linktable for counties from election returns to official list

# counties61_sub_linktable <-
#   bind_cols(btw_61_counties_unique %>%
#               select(state_name, county_name) %>%
#               rename_with(~ paste0(.x, "_elec")) %>%
#               slice_head(n = nrow(counties61_admin_sub)),
#             counties61_admin_sub %>% select(state_name, county_name) %>%
#               slice_head(n = nrow(btw_61_counties_unique))) %>%
#   relocate(matches("county"), .after = everything()) %>%
#   mutate(dist = stringdist::stringdist(county_name_elec, county_name))

counties61_sub_linktable <-
  bind_cols(btw_61_counties_unique %>% rename(county_name_elec = county_name),
            counties61_admin_sub %>% select(county_name, key_district_county, key_county)) %>%
  select(county_id, matches("key"))

# Subset list of municipalities with admin county keys and export for string matching
btw_61_municipalities_sub <-
  btw_61_municipalities %>%
  drop_na(county_name) %>% filter(key_state != "07") %>%
  select(municipality_name, municipality_id, county_id) %>%
  left_join(counties61_sub_linktable, by = "county_id") %>%
  mutate(municipality_name_match = tolower(municipality_name))

# Stringmatch
municipalities61_admin_sub <-
  filter(municipalities61_admin, key_state %in% c("01", "03", "05", "10")) %>%
  select(key_state, key_district, key_district_county, key_county, key_municipality, name) %>%
  rename(municipality_name = name) %>%
  mutate(municipality_name_match = tolower(municipality_name))

# lt <- import("linktransformer")
# model <- 'dell-research-harvard/lt-wikidata-comp-de' # BEST MODEL SO FAR!
#
# btw_61_municipalities_sub_matched <-
#   lt$merge_blocking(df1 = municipalities61_admin_sub,
#                     df2 = btw_61_municipalities_sub,
#                     merge_type = "1:1",
#                     model = model,
#                     on = list("municipality_name_match"),
#                     blocking_vars = list("key_state", "key_district_county", "key_county"))
#
# write_rds(btw_61_municipalities_sub_matched, "./output/linktables/matched_municipalities_61.rds")

btw_61_municipalities_sub_matched <- read_rds("./output/linktables/matched_municipalities_61.rds")

# Clean linktable
btw_61_municipalities_sub_matched <-
  btw_61_municipalities_sub_matched %>%
  select(-matches("match")) %>%
  mutate(across(matches("municipality_name"), ~ str_trim(str_remove_all(.x, "Stadt")), .names = "{.col}_help")) %>%
  mutate(across(matches("help"), ~ str_trim(str_remove_all(.x, "\\([^)]*\\)|^\\p{P}+|\\p{P}+$")))) %>%
  mutate(across(matches("help"), ~ str_replace_multiple(.x, c("ue|Ue" = "ü")))) %>%
  mutate(stringdist = stringdist::stringdist(municipality_name_x_help, municipality_name_y_help)) %>%
  relocate(score, stringdist, .after = municipality_name_y) %>%
  select(key_state_x, key_district, key_county_x, key_municipality, municipality_id,
         matches("municipality_name_"), score, stringdist, -matches("help")) %>%
  rename_with(~ str_remove(.x, "_x$")) %>%
  drop_na(municipality_id) %>%
  # For multiple matches, keep only match with the highest score
  arrange(municipality_id) %>%
  group_by(municipality_id) %>%
  # filter(n() > 1)
  arrange(desc(score)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# # Export unmatched for manual matching
# unmatched <-
#   # Filter municipalities without a match
#   filter(btw_61_municipalities,
#          !municipality_id %in% btw_61_municipalities_sub_matched$municipality_id, key_state != "01") %>%
#   select(municipality_id, county_id, county_name, municipality_name, voters_eligible, key_municipality) %>%
#   # Do fuzzy join
#   stringdist_left_join(
#     municipalities61_admin_sub %>% left_join(counties61_sub_linktable) %>% select(key_state, key_district, key_county, key_municipality, municipality_name, county_id),
#     by = c("county_id" = "county_id", "municipality_name" = "municipality_name"),
#     method = "jw",          # Jaro-Winkler distance for fuzzy matching
#     max_dist = 0.5,         # Adjust this threshold based on how strict you want the matching to be
#     distance_col = "dist"   # Optional: adds a column showing the distance between the names
#   )
#
# # Export to validate manually
# unmatched %>%
#   # Pick match with the lowest string distance
#   group_by(municipality_id) %>%
#   arrange(county_id.dist, municipality_name.dist) %>%
#   filter(row_number() == 1, county_id.dist == 0) %>%
#   ungroup() %>%
#   # Select variables
#   select(municipality_id, key_state, key_district, key_county, key_municipality.y, municipality_name.x, municipality_name.y, municipality_name.dist) %>%
#   arrange(desc(municipality_name.dist)) %>%
#   mutate(invalid = "", municipality_name.dist = round(municipality_name.dist, 2)) %>%
#   # Export to excel
#   write.xlsx("./output/linktables/unmatched_municipalities_61.xlsx")

# Read validated matches
unmatched_manual <-
  read_xlsx("./output/linktables/unmatched_municipalities_61_checked.xlsx", sheet = 1,
            col_types = c(rep("text", 9), "numeric")) %>%
  filter(is.na(invalid))

# Add to matched
btw_61_municipalities_sub_matched_complete <-
  bind_rows(
    btw_61_municipalities_sub_matched,
    unmatched_manual %>% select(municipality_id, key_state, key_district, key_county,
                                key_municipality  = key_municipality.y,
                                municipality_name = municipality_name.x,
                                municipality_name_y = municipality_name.y))

# Add with data containing keys
btw_61_municipalities_keys <-
  btw_61_municipalities %>%
  left_join(
    btw_61_municipalities_sub_matched_complete %>%
      select(key_district, key_county, key_municipality, municipality_id, municipality_name_match = municipality_name),
    by = "municipality_id"
  ) %>%
  mutate(key_district = ifelse(is.na(key_district.x), key_district.y, key_district.x),
         key_county = ifelse(is.na(key_county.x), key_county.y, key_county.x),
         key_municipality = ifelse(is.na(key_municipality.x), key_municipality.y, key_municipality.x)) %>%
  select(-matches("key_.*\\.")) %>%
  relocate(municipality_id, county_id, key_state, key_district, key_county, key_municipality, .after = municipality_name) %>%
  unite(ags, key_state, key_district, key_county, key_municipality, sep = "") %>%
  group_by(ags) %>% filter(n() == 1) %>% ungroup()

# Map to 2022 municipalities

rm(list = Filter(function(x){grepl("counties|unmatched", x)}, ls()))

# Read crosswalk
crosswalk_61_22 <- read_rds("./additional_data/administrative_info/crosswalks/crosswalk_61_22.rds")

btw_61_impute <-
  btw_61_municipalities_keys  %>%
  select(state_name:municipality_name, ags, voters_eligible, matches("second|first")) %>%
  # Join with crosswalk
  right_join(crosswalk_61_22, by = c("ags" = "ags")) %>%
  # Remove duplicates
  group_by(ags) %>% filter(n() == 1) %>% ungroup() %>%
  # Variable management
  rename(name_matched = name, name_22 = name_new, ags_22 = ags_new, pop = pop_total) %>%
  mutate(rgs_22 = str_sub(ags_22, 1, 5),
         rgs = str_sub(ags, 1, 5)) %>%
  relocate(name_matched:name_22, rgs, rgs_22, .after = municipality_name) %>%
  relocate(pop, .before = voters_eligible) %>%
  # Validate returns through row sums and eligible voters
  mutate(vote_totals_first = rowSums(select(., cdu_first:ssw_first), na.rm = T),
         flag_first = abs(1-(vote_totals_first/votes_valid_first)),
         vote_totals_second = rowSums(select(., cdu_second:ssw_second), na.rm = T),
         flag_second = abs(1-(vote_totals_second/votes_valid_second)),
         flag_first_second = abs(1-(votes_valid_first/votes_valid_second)),
         flag_eligible_first = votes_valid_first/voters_eligible,
         flag_eligible_second = votes_valid_second/voters_eligible) %>%
  mutate(across(matches("flag"), ~ round(.x, 4))) %>%
  select(-matches("vote_totals")) %>%
  relocate(matches("flag"), .after = ags) %>%
  # Drop observations if row sums do not match valid votes
  mutate(across(c(votes_valid_first:ssw_first), ~ ifelse(flag_first > .01, NA, .x)),
         across(c(votes_valid_second:ssw_second), ~ ifelse(flag_second > .01, NA, .x)),
         across(c(voters_eligible, votes_valid_first:ssw_first, votes_valid_second:ssw_second), ~ if_else(flag_first_second > .5, NA, .x, .x)),
         voters_eligible = case_when(
           flag_second > .01 | flag_first > .01 ~ NA,
           flag_eligible_first > 1 & flag_first <= .01 ~ NA,
           flag_eligible_second > 1 & flag_second <= .01 ~ NA,
           T ~ voters_eligible,
         )
  ) %>%
  select(-matches("flag"))

btw_61_impute <- btw_61_impute %>%
  # Calculate imputation values for voting age population
  # By municipality
  group_by(ags_22) %>%
  mutate(voters_eligible_mun = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_mun = round(voters_eligible_mun*pop)) %>%
  # By county
  group_by(rgs) %>%
  mutate(voters_eligible_county = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_county = round(voters_eligible_county*pop)) %>%
  relocate(matches("voters_eligible_"), .after = ags) %>%
  pivot_longer(cols = starts_with("votes_valid") | ends_with("_first") | ends_with("_second"),
               names_to = c("vote_type", "first_second_vote"),
               names_pattern = "(.*)_(first|second)",
               values_to = "votes") %>%
  pivot_wider(names_from = vote_type, values_from = votes) %>%
  # Replace missing values in valid rows with zero
  mutate(across(cdu:other, function(var){case_when(is.na(var) & if_any(cdu:other, ~ !is.na(.x)) ~ 0, T ~ var)})) %>%
  # Calculate completeness
  # By municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(completeness_mun = sum(if_all(cdu:other, ~ !is.na(.x))*pop, na.rm = T)/sum(pop)) %>%
  ungroup() %>%
  # By historical county
  group_by(rgs, first_second_vote) %>%
  mutate(completeness_county = sum(if_all(cdu:other, ~ !is.na(.x))*pop)/sum(pop)) %>%
  ungroup() %>%
  # Pivot
  pivot_longer(cdu:other, names_to = "party") %>%
  mutate(pop_help = ifelse(is.na(votes_valid), NA, pop)) %>%
  # Calculate municipality and county means
  # Municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(votes_valid_mun = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_mun = round(votes_valid_mun*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_mun = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # County
  group_by(rgs, first_second_vote) %>%
  mutate(votes_valid_county = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_county = round(votes_valid_county*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_county = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # Replace missing values with county or municipality means
  mutate(
    imputed = is.na(votes_valid) & (completeness_mun >= .5 | completeness_county >= .5),
    votes_valid = case_when(
      is.na(votes_valid) & completeness_mun >= .5 ~ round(votes_valid_mun),
      is.na(votes_valid) & completeness_county >= .5 ~ round(votes_valid_county),
      T ~ votes_valid
    ),
    voters_eligible = case_when(
      is.na(voters_eligible) & completeness_mun >= .5 ~ voters_eligible_mun,
      is.na(voters_eligible) & completeness_county >= .5 ~ voters_eligible_county,
      T ~ voters_eligible
    ),
    value = case_when(
      is.na(value) & completeness_mun >= .5 ~ round(value_mun*votes_valid),
      is.na(value) & completeness_county >= .5 ~ round(value_county*votes_valid),
      T ~ value
    )
  ) %>%
  select(-matches("value_"), -c(votes_valid_mun, votes_valid_county)) %>%
  pivot_wider(values_from = value, names_from = party)

write_rds(btw_61_impute, "./output/cleaned/61_btw_imputed.rds")

btw_61_mapped <-
  btw_61_impute %>%
  mutate(pop_help2 = ifelse(imputed, pop, NA)) %>%
  relocate(matches("pop_help"), .after = pop) %>%
  group_by(pick(matches("22")), first_second_vote) %>%
  summarise(across(c(pop, pop_help, voters_eligible, votes_valid, cdu:other), ~ sum(.x, na.rm = T)),
            share_complete = sum(pop_help, na.rm = T),
            share_imputed = sum(pop_help2, na.rm = T),
            .groups = "drop") %>%
  mutate(
    across(c(votes_valid, voters_eligible, cdu:other), ~ ifelse(if_all(cdu:other, ~ .x == 0), NA, .x)),
    voters_eligible = ifelse(voters_eligible == 0 & pop != 0, NA, voters_eligible),
    share_complete = share_complete/pop, share_imputed = share_imputed/pop) %>%
  select(-pop_help)


write_rds(btw_61_mapped, "./output/cleaned/61_btw_mapped.rds")

# Merge 1965 ----

## All states but RLP ----

# Rheinland-Pfalz data is from the 1969 release of returns and is therefore
# reported within 1969 territorial units. So I have to match it to the 1969
# list of municipalities.

# Schleswig-Holstein returns contain keys

clear_environment()

# Combine cleaned municipality returns from different states
btw_65_municipalities <-
  list.files("./output/cleaned/", pattern = "65_btw.*clean", full.names = T) %>%
  map(read_rds) %>%
  bind_rows() %>%
  select(-matches("votes_invalid"), -c(turnout, voters, county_id)) %>%
  mutate(county_name = str_trim(str_remove_all(county_name, "Ldkr\\.|Ldkr,|,$|\\.|Landkreis")),
         county_name = str_replace(county_name, "Grf\\s", "Grafschaft "),
         key_state = case_match(state_name, "Niedersachsen" ~ "03", "Nordrhein-Westfalen" ~ "05",
                                "Schleswig-Holstein" ~ "01", "Rheinland-Pfalz" ~ "07",
                                "Saarland" ~ "10"),
         municipality_name = str_replace_multiple(municipality_name, c("Ue" = "Ü", "ue" = "ü", "Oe" = "Ö", "oe" = "ö",
                                                                       "ksp|Ksp" = "Kirchspiel", "Gr\\s|Cr\\s" = "Groß ",
                                                                       "ae" = "ä", "Ae" = "Ä"))) %>%
  relocate(matches("key"), type, .after = municipality_name) %>%
  relocate(matches("second"), .after = voters_eligible)

# Recode counties to match official names
btw_65_municipalities <- btw_65_municipalities %>%
  mutate(county_name = case_when(
    county_name == "01pe" ~ "Olpe",
    county_name == "11 Münster (Westf), Stadt" ~ "Münster (Westf), Stadt",
    county_name == "Dusseldorf-Mettmann" ~ "Düsseldorf-Mettmann",
    county_name == "Ennope=Ruhr-Kreis" ~ "Ennepe-Ruhr-Kreis",
    county_name == "Grevenbrcich" ~ "Grevenbroich",
    county_name == "Julich" ~ "Jülich",
    county_name == "lierne, Stadt" ~ "Herne, Stadt",
    county_name == "11 Bottrop, Stadt" ~ "Bottrop, Stadt",
    county_name == "Ennope-Ruhr-Kreis" ~ "Ennepe-Ruhr-Kreis",
    county_name == "Litdenscheid, Stadt" ~ "Lüdenscheid, Stadt",
    county_name == "Lilneburg, Stadt" ~ "Lüneburg, Stadt",
    county_name == "Oldenburg (01db) Stadt" ~ "Oldenburg (Oldenburg), Stadt",
    TRUE ~ county_name  # Keep original value if no match
  ),
  county_name = str_replace_multiple(county_name, c("Koln" = "Köln")))

# Gen municipality and county IDs
btw_65_municipalities <- btw_65_municipalities %>%
  arrange(key_state, county_name) %>%
  group_by(key_state, county_name) %>%
  mutate(county_id = paste0(key_state, str_pad(cur_group_id(), width = 3, pad = 0))) %>%
  mutate(municipality_id = paste0(county_id, str_pad(row_number(), width = 3, pad = 0))) %>%
  ungroup() %>%
  relocate(county_id, .after = county_name)

# Get county names from election returns
btw_65_counties_unique <- btw_65_municipalities %>% drop_na(county_name) %>%
  filter(key_state %in% c("03", "05", "10")) %>%
  distinct(state_name, key_state, county_name, county_id) %>% arrange(state_name, county_name)

# Read list of counties & municipalities
counties65_admin <- read_rds("./additional_data/administrative_info/counties1965_admin.rds") %>% arrange(state_name, county_name)
municipalities65_admin <- read_rds("./additional_data/administrative_info/municipalities1965_admin.rds")

# Subset municipalities and counties for which I have to to do string matching
counties65_admin_sub <-
  counties65_admin %>% filter(key_state %in% c("03", "05", "10")) %>%
  rename(key_district_county = key_district)

# Create linktable for counties from election returns to official list
counties65_sub_linktable <-
  bind_cols(btw_65_counties_unique %>% rename(county_name_elec = county_name),
            counties65_admin_sub %>% select(county_name, key_district_county, key_county)) %>%
  select(county_id, matches("key"))

# counties65_sub_linktable <-
#   bind_cols(btw_65_counties_unique %>%
#               select(state_name, county_name) %>%
#               rename_with(~ paste0(.x, "_elec")) %>%
#               slice_head(n = nrow(counties65_admin_sub)),
#             counties65_admin_sub %>% select(state_name, county_name) %>%
#               slice_head(n = nrow(btw_65_counties_unique))) %>%
#   relocate(matches("county"), .after = everything()) %>%
#   mutate(dist = stringdist::stringdist(county_name_elec, county_name))

# Subset list of municipalities with admin county keys and export for string matching
btw_65_municipalities_sub <-
  btw_65_municipalities %>%
  drop_na(county_name) %>% filter(key_state != "07" & key_state != "01") %>%
  select(municipality_name, municipality_id, county_id) %>%
  left_join(counties65_sub_linktable, by = "county_id") %>%
  mutate(municipality_name_match = tolower(municipality_name))

# Stringmatch
municipalities65_admin_sub <-
  filter(municipalities65_admin, key_state %in% c("03", "05", "10")) %>%
  select(key_state, key_district, key_district_county, key_county, key_municipality, name) %>%
  rename(municipality_name = name) %>%
  mutate(municipality_name_match = tolower(municipality_name))

# lt <- import("linktransformer")
# model <- 'dell-research-harvard/lt-wikidata-comp-de' # BEST MODEL SO FAR!
#
# btw_65_municipalities_sub_matched <-
#   lt$merge_blocking(df1 = municipalities65_admin_sub,
#                     df2 = btw_65_municipalities_sub,
#                     merge_type = "1:1",
#                     model = model,
#                     on = list("municipality_name_match"),
#                     blocking_vars = list("key_state", "key_district_county", "key_county"))
#
# write_rds(btw_65_municipalities_sub_matched, "./output/linktables/matched_municipalities_65.rds")

btw_65_municipalities_sub_matched <- read_rds("./output/linktables/matched_municipalities_65.rds")

# Clean linktable
btw_65_municipalities_sub_matched <-
  btw_65_municipalities_sub_matched %>%
  select(-matches("match")) %>%
  mutate(across(matches("municipality_name"), ~ str_trim(str_remove_all(.x, "Stadt")), .names = "{.col}_help")) %>%
  mutate(across(matches("help"), ~ str_trim(str_remove_all(.x, "\\([^)]*\\)|^\\p{P}+|\\p{P}+$")))) %>%
  mutate(across(matches("help"), ~ str_replace_multiple(.x, c("ue|Ue" = "ü")))) %>%
  mutate(stringdist = stringdist::stringdist(municipality_name_x_help, municipality_name_y_help)) %>%
  relocate(score, stringdist, .after = municipality_name_y) %>%
  select(key_state_x, key_district, key_county_x, key_municipality, municipality_id,
         matches("municipality_name_"), score, stringdist, -matches("help")) %>%
  rename_with(~ str_remove(.x, "_x$")) %>%
  drop_na(municipality_id) %>%
  # For multiple matches, keep only match with the highest score
  arrange(municipality_id) %>%
  group_by(municipality_id) %>%
  # filter(n() > 1)
  arrange(desc(score)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# # Export unmatched for manual matching
# unmatched <-
#   # Filter municipalities without a match
#   filter(btw_65_municipalities,
#          !municipality_id %in% btw_65_municipalities_sub_matched$municipality_id, !key_state %in% c("01", "07")) %>%
#   select(municipality_id, county_id, county_name, municipality_name, voters_eligible, key_municipality) %>%
#   # Do fuzzy join
#   stringdist_left_join(
#     municipalities65_admin_sub %>% left_join(counties65_sub_linktable) %>% select(key_state, key_district, key_county, key_municipality, municipality_name, county_id),
#     by = c("county_id" = "county_id", "municipality_name" = "municipality_name"),
#     method = "jw",          # Jaro-Winkler distance for fuzzy matching
#     max_dist = 0.5,         # Adjust this threshold based on how strict you want the matching to be
#     distance_col = "dist"   # Optional: adds a column showing the distance between the names
#   )
#
# # Export to validate manually
# unmatched %>%
#   # Pick match with the lowest string distance
#   group_by(municipality_id) %>%
#   arrange(county_id.dist, municipality_name.dist) %>%
#   filter(row_number() == 1, county_id.dist == 0) %>%
#   ungroup() %>%
#   # Select variables
#   select(municipality_id, key_state, key_district, key_county, key_municipality.y, municipality_name.x, municipality_name.y, municipality_name.dist) %>%
#   arrange(desc(municipality_name.dist)) %>%
#   mutate(invalid = "", municipality_name.dist = round(municipality_name.dist, 2)) %>%
#   # Export to excel
#   write.xlsx("./output/linktables/unmatched_municipalities_65.xlsx")

# Read validated matches
unmatched_manual <-
  read_xlsx("./output/linktables/unmatched_municipalities_65_checked.xlsx", sheet = 1,
            col_types = c(rep("text", 9), "numeric")) %>%
  filter(is.na(invalid))

# Add to matched
btw_65_municipalities_sub_matched_complete <-
  bind_rows(
    btw_65_municipalities_sub_matched,
    unmatched_manual %>% select(municipality_id, key_state, key_district, key_county,
                                key_municipality  = key_municipality.y,
                                municipality_name = municipality_name.x,
                                municipality_name_y = municipality_name.y))

## RLP ----

# Get county names from election returns
btw_65_counties_unique_rlp <- btw_65_municipalities %>% drop_na(county_name) %>%
  filter(key_state %in% c("07")) %>%
  distinct(state_name, key_state, county_name, county_id) %>% arrange(state_name, county_name)

# Read list of counties & municipalities
counties69_admin <- read_rds("./additional_data/administrative_info/counties1969_admin.rds") %>% arrange(state_name, county_name)
municipalities69_admin <- read_rds("./additional_data/administrative_info/municipalities1969_admin.rds")

# Subset municipalities and counties for which I have to to do string matching
counties69_admin_rlp <-
  counties69_admin %>% filter(key_state %in% c("07")) %>%
  rename(key_district_county = key_district)

# Create linktable for counties from election returns to official list
counties65_rlp_linktable <-
  bind_cols(btw_65_counties_unique_rlp %>% rename(county_name_elec = county_name),
            counties69_admin_rlp %>% select(county_name, key_district_county, key_county)) %>%
  select(county_id, matches("key"))

# counties65_rlp_linktable <-
#   bind_cols(btw_65_counties_unique_rlp %>%
#               select(state_name, county_name) %>%
#               rename_with(~ paste0(.x, "_elec")) %>%
#               slice_head(n = nrow(counties69_admin_rlp)),
#             counties69_admin_rlp %>% select(state_name, county_name) %>%
#               slice_head(n = nrow(btw_65_counties_unique_rlp))) %>%
#   relocate(matches("county"), .after = everything()) %>%
#   mutate(dist = stringdist::stringdist(county_name_elec, county_name))

# Subset list of municipalities with admin county keys and export for string matching
btw_65_municipalities_rlp <-
  btw_65_municipalities %>%
  drop_na(county_name) %>% filter(key_state == "07") %>%
  select(municipality_name, municipality_id, county_id) %>%
  left_join(counties65_rlp_linktable, by = "county_id") %>%
  mutate(municipality_name_match = tolower(municipality_name))

# Stringmatch
municipalities69_admin_rlp <-
  filter(municipalities69_admin, key_state == "07") %>%
  select(key_state, key_district, key_district_county, key_county, key_municipality, name) %>%
  rename(municipality_name = name) %>%
  mutate(municipality_name_match = tolower(municipality_name))


# lt <- import("linktransformer")
# model <- 'dell-research-harvard/lt-wikidata-comp-de' # BEST MODEL SO FAR!
#
# btw_65_municipalities_rlp_matched <-
#   lt$merge_blocking(df1 = municipalities69_admin_rlp,
#                     df2 = btw_65_municipalities_rlp,
#                     merge_type = "1:1",
#                     model = model,
#                     on = list("municipality_name_match"),
#                     blocking_vars = list("key_state", "key_district_county", "key_county"))
#
# write_rds(btw_65_municipalities_rlp_matched, "./output/linktables/matched_municipalities_65_rlp.rds")

btw_65_municipalities_rlp_matched <- read_rds("./output/linktables/matched_municipalities_65_rlp.rds")

# Clean linktable
btw_65_municipalities_rlp_matched <-
  btw_65_municipalities_rlp_matched %>%
  select(-matches("match")) %>%
  mutate(across(matches("municipality_name"), ~ str_trim(str_remove_all(.x, "Stadt")), .names = "{.col}_help")) %>%
  mutate(across(matches("help"), ~ str_trim(str_remove_all(.x, "\\([^)]*\\)|^\\p{P}+|\\p{P}+$")))) %>%
  mutate(across(matches("help"), ~ str_replace_multiple(.x, c("ue|Ue" = "ü")))) %>%
  mutate(stringdist = stringdist::stringdist(municipality_name_x_help, municipality_name_y_help)) %>%
  relocate(score, stringdist, .after = municipality_name_y) %>%
  select(key_state_x, key_district, key_county_x, key_municipality, municipality_id,
         matches("municipality_name_"), score, stringdist, -matches("help")) %>%
  rename_with(~ str_remove(.x, "_x$")) %>%
  drop_na(municipality_id) %>%
  # For multiple matches, keep only match with the highest score
  arrange(municipality_id) %>%
  group_by(municipality_id) %>%
  # filter(n() > 1)
  arrange(desc(score)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# # Export unmatched for manual matching
# unmatched <-
#   # Filter municipalities without a match
#   filter(btw_65_municipalities,
#          !municipality_id %in% btw_65_municipalities_rlp_matched$municipality_id, key_state == "07") %>%
#   select(municipality_id, county_id, county_name, municipality_name, voters_eligible, key_municipality) %>%
#   # Do fuzzy join
#   stringdist_left_join(
#     municipalities69_admin_rlp %>% left_join(counties65_rlp_linktable) %>% select(key_state, key_district, key_county, key_municipality, municipality_name, county_id),
#     by = c("county_id" = "county_id", "municipality_name" = "municipality_name"),
#     method = "jw",          # Jaro-Winkler distance for fuzzy matching
#     max_dist = 0.5,         # Adjust this threshold based on how strict you want the matching to be
#     distance_col = "dist"   # Optional: adds a column showing the distance between the names
#   )
#
# # Export to validate manually
# unmatched %>%
#   # Pick match with the lowest string distance
#   group_by(municipality_id) %>%
#   arrange(county_id.dist, municipality_name.dist) %>%
#   filter(row_number() == 1, county_id.dist == 0) %>%
#   ungroup() %>%
#   # Select variables
#   select(municipality_id, key_state, key_district, key_county, key_municipality.y, municipality_name.x, municipality_name.y, municipality_name.dist) %>%
#   arrange(desc(municipality_name.dist)) %>%
#   mutate(invalid = "", municipality_name.dist = round(municipality_name.dist, 2)) %>%
#   # Export to excel
#   write.xlsx("./output/linktables/unmatched_municipalities_65_rlp.xlsx")

# Read validated matches
unmatched_manual <-
  read_xlsx("./output/linktables/unmatched_municipalities_65_rlp_checked.xlsx", sheet = 1,
            col_types = c(rep("text", 9), "numeric")) %>%
  filter(is.na(invalid))

# Add to matched
btw_65_municipalities_rlp_matched_complete <-
  bind_rows(
    btw_65_municipalities_rlp_matched,
    unmatched_manual %>% select(municipality_id, key_state, key_district, key_county,
                                key_municipality  = key_municipality.y,
                                municipality_name = municipality_name.x,
                                municipality_name_y = municipality_name.y))

## Merge States and map to 2022 ----

btw_65_municipalities_matched_complete <-
  bind_rows(
    btw_65_municipalities_sub_matched_complete,
    btw_65_municipalities_rlp_matched_complete
  )

# Add with data containg keys
btw_65_municipalities_keys <-
  btw_65_municipalities %>%
  left_join(
    btw_65_municipalities_matched_complete %>%
      select(key_district, key_county, key_municipality, municipality_id, municipality_name_match = municipality_name_y),
    by = "municipality_id"
  ) %>%
  mutate(key_district = ifelse(is.na(key_district.x), key_district.y, key_district.x),
         key_county = ifelse(is.na(key_county.x), key_county.y, key_county.x),
         key_municipality = ifelse(is.na(key_municipality.x), key_municipality.y, key_municipality.x)) %>%
  select(-matches("key_.*\\.")) %>%
  relocate(municipality_id, county_id, key_state, key_district, key_county, key_municipality, .after = municipality_name) %>%
  unite(ags, key_state, key_district, key_county, key_municipality, sep = "") %>%
  group_by(ags) %>% filter(n() == 1) %>% ungroup()

# Map to 22' municipalities

rm(list = Filter(function(x){grepl("counties|unmatched", x)}, ls()))

# Read crosswalk, combine 69 for RLP and 65 for all other states
crosswalk_69_22 <- read_rds("./additional_data/administrative_info/crosswalks/crosswalk_69_22.rds")
crosswalk_65_22 <- read_rds("./additional_data/administrative_info/crosswalks/crosswalk_65_22.rds")

crosswalk <-
  bind_rows(crosswalk_69_22 %>% filter(str_sub(ags, 1, 2) == "07"),
            crosswalk_65_22 %>% filter(str_sub(ags, 1, 2) != "07") %>% rename(pop = pop_total))

btw_65_impute <-
  btw_65_municipalities_keys  %>%
  select(state_name:municipality_name, ags, voters_eligible, matches("second|first")) %>%
  # Join with crosswalk
  right_join(crosswalk, by = c("ags" = "ags")) %>%
  # Remove duplicates
  group_by(ags) %>% filter(n() == 1) %>% ungroup() %>%
  # Variable management
  rename(name_matched = name, name_22 = name_new, ags_22 = ags_new) %>%
  mutate(rgs_22 = str_sub(ags_22, 1, 5),
         rgs = str_sub(ags, 1, 5)) %>%
  relocate(name_matched:name_22, rgs, rgs_22, .after = municipality_name) %>%
  relocate(pop, .before = voters_eligible) %>%
  # Validate returns through row sums and eligible voters
  mutate(vote_totals_first = rowSums(select(., cdu_first:fsu_first), na.rm = T),
         flag_first = abs(1-(vote_totals_first/votes_valid_first)),
         vote_totals_second = rowSums(select(., cdu_second:uap_second), na.rm = T),
         flag_second = abs(1-(vote_totals_second/votes_valid_second)),
         flag_first_second = abs(1-(votes_valid_first/votes_valid_second)),
         flag_eligible_first = votes_valid_first/voters_eligible,
         flag_eligible_second = votes_valid_second/voters_eligible) %>%
  mutate(across(matches("flag"), ~ round(.x, 4))) %>%
  select(-matches("vote_totals")) %>%
  relocate(matches("flag"), .after = ags) %>%
  # Drop observations if row sums do not match valid votes
  mutate(across(c(votes_valid_first:fsu_first), ~ ifelse(flag_first > .01, NA, .x)),
         across(c(votes_valid_second:uap_second), ~ ifelse(flag_second > .01, NA, .x)),
         across(c(voters_eligible, votes_valid_first:fsu_first, votes_valid_second:uap_second), ~ if_else(flag_first_second > .5, NA, .x, .x)),
         voters_eligible = case_when(
           flag_second > .01 | flag_first > .01 ~ NA,
           flag_eligible_first > 1 & flag_first <= .01 ~ NA,
           flag_eligible_second > 1 & flag_second <= .01 ~ NA,
           T ~ voters_eligible,
         )
  ) %>%
  select(-matches("flag"))

btw_65_impute <- btw_65_impute %>%
  # Calculate imputation values for voting age population
  # By municipality
  group_by(ags_22) %>%
  mutate(voters_eligible_mun = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_mun = round(voters_eligible_mun*pop)) %>%
  # By county
  group_by(rgs) %>%
  mutate(voters_eligible_county = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_county = round(voters_eligible_county*pop)) %>%
  relocate(matches("voters_eligible_"), .after = ags) %>%
  pivot_longer(cols = starts_with("votes_valid") | ends_with("_first") | ends_with("_second"),
               names_to = c("vote_type", "first_second_vote"),
               names_pattern = "(.*)_(first|second)",
               values_to = "votes") %>%
  pivot_wider(names_from = vote_type, values_from = votes) %>%
  # Replace missing values in valid rows with zero
  mutate(across(cdu:uap, function(var){case_when(is.na(var) & if_any(cdu:uap, ~ !is.na(.x)) ~ 0, T ~ var)})) %>%
  # Calculate completeness
  # By municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(completeness_mun = sum(if_all(cdu:uap, ~ !is.na(.x))*pop, na.rm = T)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # By historical county
  group_by(rgs, first_second_vote) %>%
  mutate(completeness_county = sum(if_all(cdu:uap, ~ !is.na(.x))*pop)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # Pivot
  pivot_longer(cdu:uap, names_to = "party") %>%
  mutate(pop_help = ifelse(is.na(votes_valid), NA, pop)) %>%
  # Calculate municipality and county means
  # Municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(votes_valid_mun = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_mun = round(votes_valid_mun*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_mun = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # County
  group_by(rgs, first_second_vote) %>%
  mutate(votes_valid_county = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_county = round(votes_valid_county*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_county = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # Replace missing values with county or municipality means
  mutate(
    imputed = is.na(votes_valid) & (completeness_mun >= .5 | completeness_county >= .5),
    votes_valid = case_when(
      is.na(votes_valid) & completeness_mun >= .5 ~ round(votes_valid_mun),
      is.na(votes_valid) & completeness_county >= .5 ~ round(votes_valid_county),
      T ~ votes_valid
    ),
    voters_eligible = case_when(
      is.na(voters_eligible) & completeness_mun >= .5 ~ voters_eligible_mun,
      is.na(voters_eligible) & completeness_county >= .5 ~ voters_eligible_county,
      T ~ voters_eligible
    ),
    value = case_when(
      is.na(value) & completeness_mun >= .5 ~ round(value_mun*votes_valid),
      is.na(value) & completeness_county >= .5 ~ round(value_county*votes_valid),
      T ~ value
    )
  ) %>%
  select(-matches("value_"), -c(votes_valid_mun, votes_valid_county)) %>%
  pivot_wider(values_from = value, names_from = party)

write_rds(btw_65_impute, "./output/cleaned/65_btw_imputed.rds")

btw_65_mapped <-
  btw_65_impute %>%
  mutate(pop_help2 = ifelse(imputed, pop, NA)) %>%
  relocate(matches("pop_help"), .after = pop) %>%
  group_by(pick(matches("22")), first_second_vote) %>%
  summarise(across(c(pop, pop_help, voters_eligible, votes_valid, cdu:uap), ~ sum(.x, na.rm = T)),
            share_complete = sum(pop_help, na.rm = T),
            share_imputed = sum(pop_help2, na.rm = T),
            .groups = "drop") %>%
  mutate(
    across(c(votes_valid, voters_eligible, cdu:uap), ~ ifelse(if_all(cdu:uap, ~ .x == 0), NA, .x)),
    voters_eligible = ifelse(voters_eligible == 0 & pop != 0, NA, voters_eligible),
    share_complete = share_complete/pop, share_imputed = share_imputed/pop) %>%
  select(-pop_help)

write_rds(btw_65_mapped, "./output/cleaned/65_btw_mapped.rds")

# Merge 1969 ----


# Major changes within RLP in 1969, results seem to be reported in new admin units
# Major changes within NRW in 1970, 1969 results are reported in old units

# SH: 1970 units
# NDS: 1970 units
# NRW: 1969 units
#

clear_environment()

# Combine cleaned municipality returns from different states
btw_69_municipalities <-
  list.files("./output/cleaned/", pattern = "69_btw.*clean", full.names = T) %>%
  map(read_rds) %>%
  bind_rows() %>%
  select(-matches("votes_invalid"), -c(turnout, voters, county_id)) %>%
  mutate(county_name = str_trim(str_remove_all(county_name, "Ldkr\\.|Ldkr,|,$|\\.|Landkreis")),
         county_name = str_replace(county_name, "Grf\\s", "Grafschaft "),
         key_state = case_match(state_name, "Niedersachsen" ~ "03", "Nordrhein-Westfalen" ~ "05",
                                "Schleswig-Holstein" ~ "01", "Rheinland-Pfalz" ~ "07",
                                "Saarland" ~ "10"),
         municipality_name = str_replace_multiple(municipality_name, c("Ue" = "Ü", "ue" = "ü", "Oe" = "Ö", "oe" = "ö",
                                                                       "ksp|Ksp" = "Kirchspiel", "Gr\\s|Cr\\s" = "Groß ",
                                                                       "ae" = "ä", "Ae" = "Ä"))) %>%
  relocate(matches("key"), type, .after = municipality_name) %>%
  relocate(matches("second"), .after = voters_eligible)

# Recode counties to match official names
btw_69_municipalities <- btw_69_municipalities %>%
  mutate(county_name = case_when(
    county_name == "Buren" ~ "Büren",
    county_name == "Disseldorf-Mettmanr" ~ "Düsseldorf-Mettmann",
    county_name == "Sceat" ~ "Soest",
    county_name == "nooh: Kreis Minster" ~ "Münster",
    county_name == "Hzgt Lauenburg" | county_name == "Hzgt Lau'enburg" ~ "Herzogtum Lauenburg",
    county_name == "Plon" ~ "Plön",
    county_name == "Suderdithmarschen" ~ "Süderdithmarschen",
    county_name == "Dusseldorf1, Stadt" ~ "Düsseldorf, Stadt",
    county_name == "Mulheim, Stadt" ~ "Mülheim a.d. Ruhr, Stadt",
    county_name == "noch Kreis Lippstadt" ~ "Lippstadt",
    county_name == "Lobeck" ~ "Lübeck, Hansestadt",
    county_name == "Sudtondern" ~ "Südtondern",
    TRUE ~ county_name  # Keep original value if no match
  ),
  county_name = str_replace_multiple(county_name, c("Koln" = "Köln")))

# Gen municipality and county IDs
btw_69_municipalities <- btw_69_municipalities %>%
  arrange(key_state, county_name) %>%
  group_by(key_state, county_name) %>%
  mutate(county_id = paste0(key_state, str_pad(cur_group_id(), width = 3, pad = 0))) %>%
  mutate(municipality_id = paste0(county_id, str_pad(row_number(), width = 3, pad = 0))) %>%
  ungroup() %>%
  relocate(county_id, .after = county_name)

# Get county names from election returns
btw_69_counties_unique <- btw_69_municipalities %>% drop_na(county_name) %>%
  distinct(state_name, key_state, county_name, county_id) %>% arrange(state_name, county_name)

# Read list of counties & municipalities
municipalities69_admin <- read_rds("./additional_data/administrative_info/municipalities1969_admin.rds")
counties69_admin <- read_rds("./additional_data/administrative_info/counties1969_admin.rds")


# Subset municipalities and counties for which I have to to do string matching
counties69_admin_sub <-
  counties69_admin %>% filter(key_state %in% c("01", "03", "05", "07", "10")) %>%
  arrange(state_name, county_name) %>%
  rename(key_district_county = key_district)

# Create linktable for counties from election returns to official list
counties69_sub_linktable <-
  bind_cols(btw_69_counties_unique %>% rename(county_name_elec = county_name),
            counties69_admin_sub %>% select(county_name, key_district_county, key_county)) %>%
  select(county_id, matches("key"))

# counties69_sub_linktable <-
#   bind_cols(btw_69_counties_unique %>%
#               select(state_name, county_name) %>%
#               rename_with(~ paste0(.x, "_elec")) %>%
#               slice_head(n = nrow(counties69_admin_sub)),
#             counties69_admin_sub %>% select(state_name, county_name) %>%
#               slice_head(n = nrow(btw_69_counties_unique))) %>%
#   relocate(matches("county"), .after = everything()) %>%
#   mutate(dist = stringdist::stringdist(county_name_elec, county_name))

# Subset list of municipalities with admin county keys and export for string matching
btw_69_municipalities_sub <-
  btw_69_municipalities %>%
  drop_na(county_name) %>% filter(key_state != "01") %>%
  select(municipality_name, municipality_id, county_id) %>%
  left_join(counties69_sub_linktable, by = "county_id") %>%
  mutate(municipality_name_match = tolower(municipality_name))

# Stringmatch
municipalities69_admin_sub <-
  filter(municipalities69_admin, key_state %in% c("03", "05", "07", "10")) %>%
  select(key_state, key_district, key_district_county, key_county, key_municipality, name) %>%
  rename(municipality_name = name) %>%
  mutate(municipality_name_match = tolower(municipality_name))

# lt <- import("linktransformer")
# model <- 'dell-research-harvard/lt-wikidata-comp-de' # BEST MODEL SO FAR!
#
# btw_69_municipalities_sub_matched <-
#   lt$merge_blocking(df1 = municipalities69_admin_sub,
#                     df2 = btw_69_municipalities_sub,
#                     merge_type = "1:1",
#                     model = model,
#                     # openai_key = openai_key,
#                     on = list("municipality_name_match"),
#                     blocking_vars = list("key_state", "key_district_county", "key_county"))

# write_rds(btw_69_municipalities_sub_matched, "./output/linktables/matched_municipalities_69.rds")

btw_69_municipalities_sub_matched <- read_rds("./output/linktables/matched_municipalities_69.rds")

# Read linktable
btw_69_municipalities_sub_matched <-
  btw_69_municipalities_sub_matched %>%
  select(-matches("match")) %>%
  mutate(across(matches("municipality_name"), ~ str_trim(str_remove_all(.x, "Stadt")), .names = "{.col}_help")) %>%
  mutate(across(matches("help"), ~ str_trim(str_remove_all(.x, "\\([^)]*\\)|^\\p{P}+|\\p{P}+$")))) %>%
  mutate(across(matches("help"), ~ str_replace_multiple(.x, c("ue|Ue" = "ü")))) %>%
  mutate(stringdist = stringdist::stringdist(municipality_name_x_help, municipality_name_y_help)) %>%
  relocate(score, stringdist, .after = municipality_name_y) %>%
  select(key_state_x, key_district, key_county_x, key_municipality, municipality_id,
         matches("municipality_name_"), score, stringdist, -matches("help")) %>%
  rename_with(~ str_remove(.x, "_x$")) %>%
  drop_na(municipality_id) %>%
  # For multiple matches, keep only match with the highest score
  arrange(municipality_id) %>%
  group_by(municipality_id) %>%
  # filter(n() > 1)
  arrange(desc(score)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# # Export unmatched for manual matching
# unmatched <-
#   # Filter municipalities without a match
#   filter(btw_69_municipalities,
#          !municipality_id %in% btw_69_municipalities_sub_matched$municipality_id, key_state != "01") %>%
#   select(municipality_id, county_id, county_name, municipality_name, voters_eligible, key_municipality) %>%
#   # Do fuzzy join
#   stringdist_left_join(
#     municipalities69_admin_sub %>% left_join(counties69_sub_linktable) %>% select(key_municipality, municipality_name, county_id),
#     by = c("county_id" = "county_id", "municipality_name" = "municipality_name"),
#     method = "jw",          # Jaro-Winkler distance for fuzzy matching
#     max_dist = 0.5,         # Adjust this threshold based on how strict you want the matching to be
#     distance_col = "dist"   # Optional: adds a column showing the distance between the names
#   )
#
# # Export to validate manually
# unmatched %>%
#   # Pick match with the lowest string distance
#   group_by(municipality_id) %>%
#   arrange(county_id.dist, municipality_name.dist) %>%
#   filter(row_number() == 1, county_id.dist == 0) %>%
#   ungroup() %>%
#   # Select variables
#   select(municipality_id, key_municipality.y, municipality_name.x, municipality_name.y, municipality_name.dist) %>%
#   arrange(desc(municipality_name.dist)) %>%
#   mutate(invalid = "", municipality_name.dist = round(municipality_name.dist, 2)) %>%
#   # Export to excel
#   write.xlsx("./output/linktables/unmatched_municipalities_69.xlsx")

# Read validated matches
unmatched_manual <-
  read_xlsx("./output/linktables/unmatched_municipalities_69_checked.xlsx", sheet = 1,
            col_types = c(rep("text", 6), "numeric")) %>%
  filter(is.na(invalid))

# Add to matched
btw_69_municipalities_sub_matched_complete <-
  bind_rows(
    btw_69_municipalities_sub_matched,
    unmatched_manual %>% select(municipality_id,
                                key_municipality  = key_municipality.y,
                                municipality_name = municipality_name.x,
                                municipality_name_y = municipality_name.y))

# Add with data with existing keys (Schleswig Holstein)
btw_69_municipalities_keys <-
  btw_69_municipalities %>%
  # Join county key info
  left_join(counties69_sub_linktable %>% select(-key_state) %>% rename(key_district = key_district_county),
            by = "county_id") %>%
  relocate(municipality_id, county_id, key_state, key_district, key_county, key_municipality, .after = municipality_name) %>%
  # Join municipality_key_info
  left_join(btw_69_municipalities_sub_matched_complete %>% select(municipality_id, key_municipality, municipality_name_y),
            by = "municipality_id", suffix = c("", ".y")) %>%
  mutate(key_municipality = ifelse(!is.na(key_municipality.y), key_municipality.y, key_municipality)) %>%
  rename(municipality_name_match = municipality_name_y) %>%
  select(-key_municipality.y) %>%
  drop_na(key_municipality) %>%
  unite(ags, key_state, key_district, key_county, key_municipality, sep = "") %>%
  group_by(ags) %>% filter(n() == 1) %>% ungroup()

# Map to 2022 municipalities

rm(list = Filter(function(x){grepl("counties|unmatched", x)}, ls()))

# Read crosswalk
crosswalk_69_22 <- read_rds("./additional_data/administrative_info/crosswalks/crosswalk_69_22.rds")

btw_69_impute <-
  btw_69_municipalities_keys  %>%
  select(state_name:municipality_name, ags, voters_eligible, matches("second|first")) %>%
  # Join with crosswalk
  right_join(crosswalk_69_22, by = c("ags" = "ags")) %>%
  # Remove duplicates
  group_by(ags) %>% filter(n() == 1) %>% ungroup() %>%
  # Variable management
  rename(name_matched = name, name_22 = name_new, ags_22 = ags_new) %>%
  mutate(rgs_22 = str_sub(ags_22, 1, 5),
         rgs = str_sub(ags, 1, 5)) %>%
  relocate(name_matched:name_22, rgs, rgs_22, .after = municipality_name) %>%
  relocate(pop, .before = voters_eligible) %>%
  # Validate returns through row sums and eligible voters
  mutate(vote_totals_first = rowSums(select(., cdu_first:fsu_first), na.rm = T),
         flag_first = abs(1-(vote_totals_first/votes_valid_first)),
         vote_totals_second = rowSums(select(., cdu_second:uap_second), na.rm = T),
         flag_second = abs(1-(vote_totals_second/votes_valid_second)),
         flag_first_second = abs(1-(votes_valid_first/votes_valid_second)),
         flag_eligible_first = votes_valid_first/voters_eligible,
         flag_eligible_second = votes_valid_second/voters_eligible) %>%
  mutate(across(matches("flag"), ~ round(.x, 4))) %>%
  select(-matches("vote_totals")) %>%
  relocate(matches("flag"), .after = ags) %>%
  # Drop observations if row sums do not match valid votes
  mutate(across(c(votes_valid_first:fsu_first), ~ ifelse(flag_first > .01, NA, .x)),
         across(c(votes_valid_second:uap_second), ~ ifelse(flag_second > .01, NA, .x)),
         across(c(voters_eligible, votes_valid_first:fsu_first, votes_valid_second:uap_second), ~ if_else(flag_first_second > .5, NA, .x, .x)),
         voters_eligible = case_when(
           flag_second > .01 | flag_first > .01 ~ NA,
           flag_eligible_first > 1 & flag_first <= .01 ~ NA,
           flag_eligible_second > 1 & flag_second <= .01 ~ NA,
           T ~ voters_eligible,
         )
  ) %>%
  select(-matches("flag"))

btw_69_impute <- btw_69_impute %>%
  # Calculate imputation values for voting age population
  # By municipality
  group_by(ags_22) %>%
  mutate(voters_eligible_mun = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_mun = round(voters_eligible_mun*pop)) %>%
  # By county
  group_by(rgs) %>%
  mutate(voters_eligible_county = sum(voters_eligible, na.rm = T)/sum(ifelse(!is.na(voters_eligible), pop, NA), na.rm = T),
         voters_eligible_county = round(voters_eligible_county*pop)) %>%
  relocate(matches("voters_eligible_"), .after = ags) %>%
  pivot_longer(cols = starts_with("votes_valid") | ends_with("_first") | ends_with("_second"),
               names_to = c("vote_type", "first_second_vote"),
               names_pattern = "(.*)_(first|second)",
               values_to = "votes") %>%
  pivot_wider(names_from = vote_type, values_from = votes) %>%
  # Replace missing values in valid rows with zero
  mutate(across(cdu:uap, function(var){case_when(is.na(var) & if_any(cdu:uap, ~ !is.na(.x)) ~ 0, T ~ var)})) %>%
  # Calculate completeness
  # By municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(completeness_mun = sum(if_all(cdu:uap, ~ !is.na(.x))*pop, na.rm = T)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # By historical county
  group_by(rgs, first_second_vote) %>%
  mutate(completeness_county = sum(if_all(cdu:uap, ~ !is.na(.x))*pop)/sum(pop, na.rm = T)) %>%
  ungroup() %>%
  # Pivot
  pivot_longer(cdu:uap, names_to = "party") %>%
  mutate(pop_help = ifelse(is.na(votes_valid), NA, pop)) %>%
  # Calculate municipality and county means
  # Municipality
  group_by(ags_22, first_second_vote) %>%
  mutate(votes_valid_mun = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_mun = round(votes_valid_mun*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_mun = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # County
  group_by(rgs, first_second_vote) %>%
  mutate(votes_valid_county = sum(votes_valid, na.rm = T)/sum(pop_help, na.rm = T),
         votes_valid_county = round(votes_valid_county*pop)) %>%
  group_by(party, .add = T) %>%
  mutate(value_county = sum(value, na.rm = T)/sum(votes_valid, na.rm = T)) %>%
  ungroup() %>%
  # Replace missing values with county or municipality means
  mutate(
    imputed = is.na(votes_valid) & (completeness_mun >= .5 | completeness_county >= .5),
    votes_valid = case_when(
      is.na(votes_valid) & completeness_mun >= .5 ~ round(votes_valid_mun),
      is.na(votes_valid) & completeness_county >= .5 ~ round(votes_valid_county),
      T ~ votes_valid
    ),
    voters_eligible = case_when(
      is.na(voters_eligible) & completeness_mun >= .5 ~ voters_eligible_mun,
      is.na(voters_eligible) & completeness_county >= .5 ~ voters_eligible_county,
      T ~ voters_eligible
    ),
    value = case_when(
      is.na(value) & completeness_mun >= .5 ~ round(value_mun*votes_valid),
      is.na(value) & completeness_county >= .5 ~ round(value_county*votes_valid),
      T ~ value
    )
  ) %>%
  select(-matches("value_"), -c(votes_valid_mun, votes_valid_county)) %>%
  pivot_wider(values_from = value, names_from = party)

write_rds(btw_69_impute, "./output/cleaned/69_btw_imputed.rds")

btw_69_mapped <-
  btw_69_impute %>%
  mutate(pop_help2 = ifelse(imputed, pop, NA)) %>%
  relocate(matches("pop_help"), .after = pop) %>%
  group_by(pick(matches("22")), first_second_vote) %>%
  summarise(across(c(pop, pop_help, voters_eligible, votes_valid, cdu:uap), ~ sum(.x, na.rm = T)),
            share_complete = sum(pop_help, na.rm = T),
            share_imputed = sum(pop_help2, na.rm = T),
            .groups = "drop") %>%
  mutate(
    across(c(votes_valid, voters_eligible, cdu:uap), ~ ifelse(if_all(cdu:uap, ~ .x == 0), NA, .x)),
    share_complete = share_complete/pop, share_imputed = share_imputed/pop)

write_rds(btw_69_mapped, "./output/cleaned/69_btw_mapped.rds")

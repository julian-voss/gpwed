# This script cleans all election data provided by state statistical offices
#
# Load libraries
library(tidyverse)
library(readxl)

source("./code/scripts/functions.R")

# Cities / county returns ------------------------------------------------------

## 1953

col_names <- c("key_state", "county_id", "county_name", "voters_eligible", "voters", "votes_invalid", "votes_valid",
               "cdu", "spd", "fdp", "csu", "gb_bhe", "bp", "dp", "drp", "center", "gvp", "kpd", "dns", "ssw")

btw_1953_counties <- read_delim("./additional_data/county_data_bundeswahlleiter/btw1953kreis.csv",
                                skip = 5, delim = ";", col_names = col_names) %>%
  mutate(type = ifelse(grepl(", stadt|hansestadt|hauptstadt|universitätsstadt", tolower(county_name)), "city", "county")) %>%
  drop_na(county_name)

write_rds(btw_1953_counties, "./additional_data/county_data_bundeswahlleiter/btw_1953_counties.rds")

## 1957

col_names <- c("key_state", "county_id", "county_name", "voters_eligible", "voters", "votes_invalid", "votes_valid",
               "cdu", "spd", "csu", "fdp", "dp", "fu", "gb_bhe", "other")

btw_1957_counties <- read_delim("./additional_data/county_data_bundeswahlleiter/btw1957kreis.csv",
                                skip = 6, delim = ";", col_names = col_names) %>%
  mutate(type = ifelse(grepl(", stadt|hansestadt|hauptstadt|universitätsstadt", tolower(county_name)), "city", "county")) %>%
  drop_na(county_name)

write_rds(btw_1957_counties, "./additional_data/county_data_bundeswahlleiter/btw_1957_counties.rds")

## 1961

col_names <- c("key_state", "county_id", "county_name", "voters_eligible", "voters", "votes_invalid", "votes_valid",
               "spd", "cdu", "csu", "fdp", "gdp", "dfu", "drp", "dg", "ssw")

btw_1961_counties <- read_delim("./additional_data/county_data_bundeswahlleiter/btw1961kreis.csv",
                                skip = 6, delim = ";", col_names = col_names) %>%
  mutate(type = ifelse(grepl(", stadt|hansestadt|hauptstadt|universitätsstadt", tolower(county_name)), "city", "county")) %>%
  drop_na(county_name)

write_rds(btw_1961_counties, "./additional_data/county_data_bundeswahlleiter/btw_1961_counties.rds")

## 1965

col_names <- c("key_state", "county_id", "county_name", "voters_eligible", "voters", "votes_invalid", "votes_valid",
               "spd", "cdu", "csu", "fdp", "other")

btw_1965_counties <- read_delim("./additional_data/county_data_bundeswahlleiter/btw1965kreis.csv",
                                skip = 6, delim = ";", col_names = col_names) %>%
  mutate(type = ifelse(grepl(", stadt|hansestadt|hauptstadt|universitätsstadt", tolower(county_name)), "city", "county")) %>%
  drop_na(county_name)

write_rds(btw_1965_counties, "./additional_data/county_data_bundeswahlleiter/btw_1965_counties.rds")

## 1969

col_names <- c("key_state", "county_id", "county_name", "voters_eligible", "voters", "votes_invalid", "votes_valid",
               "spd", "cdu", "csu", "fdp", "npd", "adf", "other")

btw_1969_counties <- read_delim("./additional_data/county_data_bundeswahlleiter/btw1969kreis.csv",
                                skip = 6, delim = ";", col_names = col_names) %>%
  mutate(type = ifelse(grepl(", stadt|hansestadt|hauptstadt|universitätsstadt", tolower(county_name)), "city", "county")) %>%
  drop_na(county_name)

write_rds(btw_1969_counties, "./additional_data/county_data_bundeswahlleiter/btw_1969_counties.rds")

# Bavaria ----

# Read raw data
bavaria_all_elections <-
  map(as.character(seq(1949, 1969, 4)), function(year) {
    read_xlsx("./additional_data/state_provided_files/Bayern_20240529_BTW_Zeitreihe_1949-2021.xlsx",
              sheet = year, skip = 6, col_names = T) %>%
      rename(ags = 1, municipality_name = 2, voters_eligible = 3, voters = 4, first_second_vote = 5) %>%
      fill(ags:voters) %>%
      mutate(first_second_vote = ifelse(grepl("Erst", first_second_vote), "first", "second"), year = year) %>%
      relocate(year, .after = municipality_name) %>%
      filter(grepl("^[0-9]+$", ags))
  })

# Vector with original party variable names
parties <- bavaria_all_elections %>% map(~ select(.x, -c(ags:first_second_vote)) %>% names()) %>% unlist() %>% unique()

# Vector with new party variable names and mappings

new_names <- c("csu", "spd", "fdp", "bp", "kpd", "wav", "gehr",
               "gb_bhe", "dns", "dp", "drp", "pfr", "vu", "gvp",
               "nbayg", "fu", "dg", "bdd", "dms", "gpd",
               "dfu", "eb_matjak", "npd", "aud", "fsu", "adf",
               "ep", "wv_69", "eb_fossing", "eb_bartsch", "gpd")

names(new_names) <- parties

# Rename
bavaria_all_elections <- map(bavaria_all_elections, function(df) {
  common_names <- intersect(names(df), names(new_names))
  names(df)[names(df) %in% common_names] <- new_names[common_names]
  df <- mutate(df, across(csu:last_col(), as.numeric))
  return(df)
})

# Combine
bavaria_all_elections <- bind_rows(bavaria_all_elections) %>% mutate(year = as.numeric(year))

# Baden-Württemberg ----

# Read raw data

bw_all_elections <-
  read_delim("./additional_data/state_provided_files/BTW-1949-1969.csv",
             delim = ";", skip = 8, col_names = T, locale = locale(encoding = "latin1"))


# Rename variables
varnames <- names(bw_all_elections) %>% map_vec(~ str_trim(str_remove_all(.x, "\\.{3}[0-9]*$")))
varnames[1] <- "ags"
varnames[2] <- "municipality_name"
varnames[3] <- "first_second_vote"

idx_csu <- which(varnames == "Christlich Demokratische Union Deutschlands (CDU)")

varnames <-
  varnames %>%
  map_at(seq(idx_csu[1], idx_csu[2]-1), ~ paste0(.x, "_1949")) %>%
  map_at(seq(idx_csu[2], idx_csu[3]-1), ~ paste0(.x, "_1953")) %>%
  map_at(seq(idx_csu[3], idx_csu[4]-1), ~ paste0(.x, "_1957")) %>%
  map_at(seq(idx_csu[4], idx_csu[5]-1), ~ paste0(.x, "_1961")) %>%
  map_at(seq(idx_csu[5], idx_csu[6]-1), ~ paste0(.x, "_1965")) %>%
  map_at(seq(idx_csu[6], length(varnames)), ~ paste0(.x, "_1969")) %>%
  unlist()

names(bw_all_elections) <- varnames


# Reshape data
bw_all_elections <-
  bw_all_elections %>%
  fill(ags:municipality_name) %>%
  filter(!if_all(4:last_col(), is.na)) %>%
  mutate(across(4:last_col(), as.numeric)) %>%
  pivot_longer(4:last_col(), names_to = c("party", "year"), names_sep = "_") %>%
  pivot_wider(names_from = party, values_from = value, id_cols = c(ags, municipality_name, first_second_vote, year)) %>%
  select(where(~ !all(is.na(.)))) %>%
  arrange(year, ags, first_second_vote) %>%
  mutate(first_second_vote = ifelse(grepl("Erst", first_second_vote), "first", "second"),
         ags = paste0("08", ags), year = as.numeric(year))

names_new <- c(
  "cdu",          # Christlich Demokratische Union Deutschlands (CDU)
  "spd",          # Sozialdemokratische Partei Deutschlands (SPD)
  "fdp",          # Freie Demokratische Partei (FDP)
  "npd",          # Nationaldemokratische Partei Deutschlands (NPD)
  "independent",  # Einzelbewerber/-innen
  "adf",          # Aktion Demokratischer Fortschritt (ADF)
  "aud",          # Aktionsgemeinschaft Unabhängiger Deutscher (AUD)
  "bdd",          # Bund der Deutschen (BdD)
  "dfu",          # Deutsche Friedens-Union (DFU)
  "dg",           # Deutsche Gemeinschaft (DG)
  "dp",           # Deutsche Partei (DP)
  "drp",          # Deutsche Reichs-Partei (DRP)
  "ep",           # Europa Partei (EP)
  "fsu",          # Freisoziale Union -Demokratische Mitte- (FSU)
  "gpd",          # Gesamtdeutsche Partei (GPD)
  "gvp",          # Gesamtdeutsche Volkspartei (GVP)
  "gb_bhe",       # Gesamtdeutscher Block - BHE (GB/BHE)
  "kpd",          # Kommunistische Partei Deutschlands (KPD)
  "dns",          # Nationale Sammlung (DNS)
  "other"         # Sonstige
)

names(bw_all_elections)[5:length(names(bw_all_elections))] <- names_new

# Hessen ----

## 1949 ----
he_49 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1949.xlsx", sheet = 1, skip = 2, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_name = 6,
         voters = 8, votes_invalid = ungültig, votes_valid = gültig, independent_a = 18, independent_b = 19) %>%
  select(-all_of(c(5, 7, 9, 10))) %>%
  rename_with(tolower) %>%
  # Clean
  drop_na(ags) %>%
  mutate(across(voters:independent_b, ~ replace_na(as.numeric(.x), 0))) %>%
  mutate(independent = rowSums(select(., independent_a:independent_b))) %>%
  select(-c(independent_a, independent_b)) %>%
  # Aggregation by contemporary municipality
  group_by(ags) %>%
  summarise(across(voters:independent, ~ sum(.x, na.rm = T)), .groups = "drop") %>%
  # Add year info
  mutate(year = 1949, first_second_vote = "second") %>% relocate(year, .after = ags)

## 1953 ----

he_53 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1953.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_name = 5,
         voters_eligible = 10, voters = 11, votes_invalid_first = 13, votes_valid_first = 14,
         votes_invalid_second = 24, votes_valid_second = 25
  ) %>%
  select(-all_of(c(6:9, 12))) %>%
  rename_with(tolower) %>%
  drop_na(ags) %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_first"), `spd...15`:dns) %>%
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_second"), `spd...26`:`dp...32`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  # Pivot
  mutate(across(voters_eligible:dp_second, as.numeric)) %>%
  pivot_longer(votes_invalid_first:dp_second, names_to = c("variable", "first_second_vote"), names_sep = "_(?=first)|_(?=second)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  # Aggregation by contemporary municipality
  group_by(ags, first_second_vote) %>%
  summarise(across(c(starts_with("vote"), spd:dns), ~ sum(.x, na.rm = T)), .groups = "drop") %>%
  # Add year info
  mutate(year = 1953) %>% relocate(year, .after = ags)

## 1957 ----

he_57 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1957.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_name = 5,
         voters_eligible = 10, voters = 11,
         votes_invalid_first = 13, votes_valid_first = 14,
         votes_invalid_second = 23, votes_valid_second = 24
  ) %>%
  select(-all_of(c(6:9, 12))) %>%
  rename_with(tolower) %>%
  drop_na(ags) %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_first"), `spd...15`:dg) %>%
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_second"), `spd...25`:`drp...31`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  # Pivot
  mutate(across(voters_eligible:drp_second, as.numeric)) %>%
  pivot_longer(votes_invalid_first:drp_second, names_to = c("variable", "first_second_vote"), names_sep = "_(?=first)|_(?=second)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  # Aggregation by contemporary municipality
  group_by(ags, first_second_vote) %>%
  summarise(across(c(starts_with("vote"), spd:dg), ~ sum(.x, na.rm = T)), .groups = "drop") %>%
  # Add year info
  mutate(year = 1957) %>% relocate(year, .after = ags)

## 1961 ----

he_61 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1961.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_key = 5, county_name = 6,
         voters_eligible = 11, voters = 12,
         votes_invalid_first = 14, votes_valid_first = 15,
         votes_invalid_second = 24, votes_valid_second = 25
  ) %>%
  select(-all_of(c(7:10, 13))) %>%
  rename_with(tolower) %>%
  drop_na(ags) %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_first"), `cdu...16`:dvg) %>%
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_second"), `cdu...26`:`drp...31`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  # Pivot
  mutate(across(voters_eligible:drp_second, as.numeric)) %>%
  pivot_longer(votes_invalid_first:drp_second, names_to = c("variable", "first_second_vote"), names_sep = "_(?=first)|_(?=second)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  # Aggregation by contemporary municipality
  group_by(ags, first_second_vote) %>%
  summarise(across(c(starts_with("vote"), cdu:dvg), ~ sum(.x, na.rm = T)), .groups = "drop") %>%
  # Add year info
  mutate(year = 1961) %>% relocate(year, .after = ags)

## 1965 ----

he_65 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1965.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_name = 5,
         voters_eligible = 10, voters = 11,
         votes_invalid_first = 13, votes_valid_first = 14,
         votes_invalid_second = 22, votes_valid_second = 23
  ) %>%
  select(-all_of(c(6:9, 12))) %>%
  rename_with(tolower) %>%
  drop_na(ags) %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_first"), `spd...15`:pöhn) %>%
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_second"), `spd...24`:`npd...29`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  # Pivot
  mutate(across(voters_eligible:npd_second, as.numeric)) %>%
  pivot_longer(votes_invalid_first:npd_second, names_to = c("variable", "first_second_vote"), names_sep = "_(?=first)|_(?=second)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  # Aggregation by contemporary municipality
  group_by(ags, first_second_vote) %>%
  summarise(across(c(starts_with("vote"), spd:pöhn), ~ sum(.x, na.rm = T)), .groups = "drop") %>%
  # Add year info
  mutate(year = 1965) %>% relocate(year, .after = ags)

## 1969 ----

he_69 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1969.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 2, district = 1, stat_key = 3, municipality_name = 4, county_name = 5,
         voters_eligible = 9, voters = 10,
         votes_invalid_first = 12, votes_valid_first = 13,
         votes_invalid_second = 21, votes_valid_second = 22
  ) %>%
  select(-all_of(c(6:8, 11))) %>%
  rename_with(tolower) %>%
  drop_na(ags) %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "[0-9]|\\.") %>% paste0(., "_first"), `spd...14`:uap) %>%
  rename_with(~ str_remove_all(.x, "[0-9]|\\.") %>% paste0(., "_second"), `spd...23`:`ep...29`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  # Pivot
  mutate(across(voters_eligible:ep_second, as.numeric)) %>%
  pivot_longer(votes_invalid_first:ep_second, names_to = c("variable", "first_second_vote"), names_sep = "_(?=first)|_(?=second)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  # Aggregation by contemporary municipality
  group_by(ags, first_second_vote) %>%
  summarise(across(c(starts_with("vote"), spd:gpd), ~ sum(.x, na.rm = T)), .groups = "drop") %>%
  # Add year info
  mutate(year = 1969) %>% relocate(year, .after = ags)

## Combine----

# municipalities2022 <- read_rds("./additional_data/administrative_info/municipalities2022_admin.rds") %>%
#   unite("ags", key_state, key_district, key_county, key_municipality, sep = "") %>%
#   select(ags, municipality_name = name)

he_all_elections <-
  bind_rows(he_49, he_53, he_57, he_61, he_65, he_69) %>%
  relocate(voters_eligible, voters, first_second_vote, votes_valid, .after = year) %>%
  select(-c(votes_invalid)) %>%
  filter(!grepl("[a-z]", ags), grepl("[0-9]", ags)) %>%
  arrange(year, ags, first_second_vote) %>%
  mutate(ags = paste0("06", ags)) #%>%
  # # Add info on municipality names
  # left_join(municipalities2022) %>%
  # relocate(municipality_name, .after = ags) %>%
  # arrange(ags, year)

# Combine and save ----

clear_environment("all_elections")

# Hamburg
hh_all_elections <-
  read_csv("./additional_data/state_provided_files/hh.csv") %>%
  mutate(across(cdu_csu:independent, as.numeric)) %>%
  rename(municipality_name = state) %>%
  mutate(ags = "02000000")

# Bremen
hb_all_elections <-
  read_xlsx("./additional_data/state_provided_files/HB_BTW_ab1949_Städte_alleParteien.xlsx",
            skip = 1) %>%
  mutate(Wahl = as.numeric(str_remove(Wahl, "BTW"))) %>%
  filter(Wahl <= 1969) %>%
  select(where(~ any(!is.na(.)))) %>%
  select(where(~ !all(. == 0))) %>%

  select(
    Region,
    year = Wahl,
    voters_eligible = Wahlber,
    voters = Waehl,
    votes_valid = Guel,
    ADF:SPD
  ) %>%
  rename_with(tolower) %>%
  rename(gb_bhe = bhe, cdu_csu = cdu, gpd = gdp) %>%
  mutate(ep = rowSums(select(., ep, efp), na.rm = T), ep = ifelse(ep == 0, NA, ep)) %>% select(-efp) %>%
  mutate(first_second_vote = "second",
         ags = case_when(region == "1" ~ "04011000", T ~ "04012000"),
         municipality_name = case_when(region == "1" ~ "Stadt Bremen", T ~ "Stadt Bremerhaven")) %>%
    relocate(ags, municipality_name, first_second_vote, .before = everything()) %>%
  select(-region)

hb_1949 <-
  read_delim("./additional_data/state_provided_files/hb.csv", delim = ";",
             locale = locale(encoding = "latin3"), skip = 4, col_names = F) %>%
  drop_na()

names(hb_1949) <-
  c("ags", "municipality_name", "year", "voters_eligible", "voters", "turnout",
    "votes_valid", "spd", "cdu_csu", "fdp", "greens", "left", "npd", "afd", "piraten", "other")

hb_1949 <-
  mutate(hb_1949, across(voters_eligible:other, as.numeric),
         ags = paste0(ags, "000"),
         year = year(year), ags = case_when(municipality_name == "Stadt Bremen" ~ "04011000", T ~ "04012000"),
         first_second_vote = "second") %>%
  filter(year == 1949) %>%
  select(-c(turnout, greens:piraten))

admin_all_elections <-
  bind_rows(bavaria_all_elections %>% rename(cdu_csu = csu),
            he_all_elections %>% rename(cdu_csu = cdu),
            bw_all_elections %>% rename(cdu_csu = cdu)) %>%
  relocate(votes_valid, .after = first_second_vote) %>%
  mutate(across(cdu_csu:other, ~ replace_na(.x, 0), .names = "{.col}_help")) %>%
  mutate(votes_valid = rowSums(select(., cdu_csu_help:other_help))) %>%
  select(-matches("help")) %>%
  mutate(votes_valid = ifelse(if_all(cdu_csu:other, is.na), NA, votes_valid)) %>%
  bind_rows(hh_all_elections, hb_all_elections, hb_1949)

write_rds(admin_all_elections, "./additional_data/state_provided_files/admin_all_elections.rds")

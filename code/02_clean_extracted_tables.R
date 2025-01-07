# This script cleans all raw municipality tables extracted from scanned PDFs
#
# Load libraries
library(tidyverse)
library(readxl)
library(magrittr)
library(future)
library(furrr)
library(fuzzyjoin)
library(xlsx)

source("./code/scripts/functions.R")

# Additional setup
plan(multisession, workers = 8)
options(readxl.show_progress = FALSE)

## BTW 1949 ----
### NDS ----

clear_environment()

# Create list of file paths
xl_files_nds49 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "49_btw_nds", full.names = T)) %>%
  mutate(
    sheet = str_extract(path,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet), as.numeric)) %>%
  arrange(sheet)

# Read city data
btw49_nds_cities <-
  read_xlsx("./additional_data/election_returns_cities/nds_49_cities.xlsx",
            col_names = c("name", "city", "voters_eligible", "voters", "turnout",
                          "votes_valid", "spd", "dp", "cdu", "drp", "fdp", "center",
                          "kpd", "rsf", "independent", "spd_share", "dp_share",
                          "cdu_share", "drp_share", "fdp_share", "center_share", "kpd_share",
                          "rsf_share", "independent_share"),
            skip = 1) %>%
  filter(city == 1) %>%
  select(name, matches("vote"), spd:independent) %>%
  mutate(county_name = name, type = "city") %>%
  rename(municipality_name = name)

# Variable names for extracted tables
varnames <-
  c("municipality_name", "voters_eligible", "voters", "votes_valid",
    "spd", "cdu", "dp", "fdp", "kpd", "center", "drp", "rsf", "independent")

# Read individual tables
xl_files_nds49_clean <-
  future_imap(xl_files_nds49$path, function(xl_file, idx) {


    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    file_df %<>% mutate(sheet = xl_file)

  })

# Combine and clean
btw49_nds_municipalities_clean <-
  bind_rows(xl_files_nds49_clean) %>%
  `colnames<-`(c(varnames, "sheet")) %>%
  filter(municipality_name != "-") %>%
  # County
  mutate(
    county_name = if_else(if_all(voters_eligible:independent, is.na), municipality_name, NA)
  ) %>%
  fill(county_name) %>%
  relocate(county_name, .before = everything()) %>%
  mutate(type = "municipality") %>%
  bind_rows(btw49_nds_cities) %>%
  filter(!if_all(voters_eligible:independent, is.na)) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:independent, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:independent), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:independent), as.numeric),
         across(spd:independent, ~ replace_na(.x, 0))) %>%
  mutate(state_name = "Niedersachsen") %>% relocate(state_name, .before = everything()) %>%
  arrange(type, county_name, municipality_name) %>%
  select(-sheet)


write_rds(btw49_nds_municipalities_clean, "./output/cleaned/49_btw_nds_clean.rds")

### NRW ----

clear_environment()

xl_files_nrw49 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "49_btw_nrw", full.names = T)) %>%
  mutate(sheet = str_extract(path,"[0-9]*(?=\\.xlsx)"),
         sheet = as.numeric(sheet)) %>%
  arrange(sheet)

# Subset files for left pages and clean
xl_files_nrw49_left <- xl_files_nrw49 %>%
  filter(sheet %in% seq(1, 246, 2)) %>%
  pull(path)

varnames_left <-
  c("row_id", "entity_name", "voters_eligible", "voters", "turnout",
    "votes_valid", "cdu", "cdu_share",
    "spd", "spd_share",
    "fdp", "fdp_share")

xl_files_nrw49_left_clean <-
  future_map(xl_files_nrw49_left, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1)

    if (ncol(file_df) == 13) {
      file_df %<>% select(-1)
    }

    file_df %<>%
      `colnames<-`(varnames_left) %>%
      mutate(sheet = file)

  })

xl_files_nrw49_left_clean_df <-
  xl_files_nrw49_left_clean  %>%
  bind_rows() %>%
  # Identify type of unit
  mutate(
    county_row = str_detect(entity_name, "^kr\\.|Kreis|kreis$|kreis\\s|Ldkr|ldkr|Ldks"),
    type = case_when(
      str_detect(tolower(entity_name), "^krfr\\.st|stkr\\.|reg\\.") ~ "city",
      county_row ~ "municipality"
    )
  ) %>%
  mutate(county_name = case_when(county_row ~ entity_name)) %>%
  relocate(county_name, type, .before = entity_name) %>%
  fill(county_name, type) %>%
  filter(if_any(voters_eligible:fdp_share, ~ !is.na(.x))) %>%
  # Clean municipality identifiers
  filter(!grepl("^Amt", entity_name)) %>%
  group_by(sheet) %>%
  mutate(
    helper_id = case_when(grepl("^a", entity_name) ~ 1,
                          grepl("\\sb$|\\.b$", lead(entity_name)) ~ 1,
                          lead(entity_name, 2) == "c" ~ 1,
                          T ~ 0),
    helper_id = cumsum(helper_id)
  ) %>%
  relocate(helper_id, .before = everything()) %>%
  mutate(municipality_name = ifelse(str_length(entity_name) < 3, NA, entity_name)) %>%
  group_by(helper_id, .add = T) %>%
  mutate(municipality_name = paste(ifelse(!is.na(municipality_name), municipality_name, ""), collapse = " "),
         municipality_name = str_trim(municipality_name),
         municipality_name = str_remove_all(municipality_name, "\\sb$|\\.b$")) %>%
  fill(municipality_name, row_id, .direction = "downup") %>%
  relocate(municipality_name, .after = entity_name) %>%
  mutate(
    election = case_when(row_number() == 1 | entity_name == "a" ~ "1948",
                         row_number() == 2 | entity_name == "b" ~ "1949",
                         row_number() == 3 | entity_name == "c" ~ "1950",
                         row_number() == 5 | entity_name == "d" ~ "other")
  ) %>%
  relocate(election, .after = municipality_name) %>%
  ungroup() %>%
  filter(
    !grepl("Ldkr|ldkr|in.*gesa.*t|in.*amt|inagesent", municipality_name)
  ) %>%
  mutate(across(c(county_name, municipality_name), ~ str_remove_all(.x, "Stkr|^[0-9]+|kr\\.|^Krfr\\.St|^Kreis|Amt|Ldkr\\.|nooh:|Ldks|nochs|Ldkr,|noch|moch|aoch|Landkreis") %>% str_trim())) %>%
  mutate(across(c(county_name, municipality_name), ~ str_remove_all(.x, "^[[:punct:]]|[[:punct:]]$|(\\s[[:punct:]]\\s|)") %>% str_trim()),
         county_name = ifelse(type == "city", municipality_name, county_name)) %>%
  group_by(sheet, helper_id) %>%
  filter(n() == 3) %>%
  ungroup() %>%
  filter(election == "1949") %>%
  mutate(row_id = as.numeric(row_id)) %>%
  mutate(sheet = as.numeric(str_extract(sheet, "[0-9]*(?=\\.xlsx)"))) %>%
  select(-county_row)

# Subset files for right pages and clean

xl_files_nrw49_right <- xl_files_nrw49 %>%
  filter(sheet %in% seq(2, 246, 2)) %>% pull(path)

varnames_right <- c("kpd", "kpd_share", "center", "center_share",
                    "dkp_drp", "dkp_drp_share", "rsf", "rsf_share",
                    "rvwp", "rvwp_share", "dp", "dp_share",
                    "srp", "srp_share", "other", "other_share",
                    "independent", "independent_share", "row_id")

xl_files_nrw49_right_clean <-
  future_map(xl_files_nrw49_right, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1)

    # Remove missing leading or trailing columns

    # Check which columns have only missing values
    missing_cols <- sapply(file_df %>% slice_head(n= nrow(.)-1), function(x) all(is.na(x)))

    if (missing_cols[1] & !is.na(missing_cols[1])) {

      file_df %<>% select(-1)

    } else if (missing_cols[20] & !is.na(missing_cols[20])) {

      file_df %<>% select(-20)

    }

    file_df %>%
      `colnames<-`(varnames_right) %>%
      mutate(sheet = file)


  })

xl_files_nrw49_right_clean_df  <-
  xl_files_nrw49_right_clean  %>%
  bind_rows() %>%
  mutate(row_id = as.numeric(row_id)) %>%
  drop_na(row_id) %>%
  mutate(sheet = str_extract(sheet, "[0-9]*(?=\\.xlsx)"),
         sheet = as.numeric(sheet)-1)

btw49_nrw_municipalities_clean <-
  left_join(xl_files_nrw49_left_clean_df, xl_files_nrw49_right_clean_df, by = c("row_id", "sheet")) %>%
  group_by(row_id, sheet) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  mutate(
    across(c(voters_eligible:independent_share), ~ str_replace_multiple(.x, c("," = "\\.", "C" = "0", "-" = "0", "o" = "0", "l" = "1", "\\s" = ""))),
    across(c(voters_eligible:independent_share),as.numeric),
    across(c(cdu:independent_share), ~ replace_na(.x, 0)),
    across(matches("share"), ~ ifelse(between(.x, 0, 100), .x, NA)),
    voters_eligible = ifelse(municipality_name == "Kerpen, Stadt", NA, voters_eligible),
    municipality_name = ifelse(type == "city", paste0(municipality_name, ", Stadt"), municipality_name),
    county_name = ifelse(type == "city", municipality_name, county_name)) %>%
  relocate(sheet, .before = everything()) %>%
  relocate(matches("share"), .after = everything()) %>%
  select(-c(helper_id, row_id, entity_name, election)) %>%
  # Caluclate flags based on reported totals and shares
  mutate(flag_share = abs(100- rowSums(select(., matches("share"))))) %>%
  mutate(across(cdu:independent, ~ .x/votes_valid*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  # If totals seem to contain errors, calculate totals based on shares
  mutate(across(cdu_share:independent_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu = ifelse(flag_total>2 | is.na(flag_total), cdu_share_replace, cdu),
    spd = ifelse(flag_total>2 | is.na(flag_total), spd_share_replace, spd),
    fdp = ifelse(flag_total>2 | is.na(flag_total), fdp_share_replace, fdp),
    kpd = ifelse(flag_total>2 | is.na(flag_total), kpd_share_replace, kpd),
    center = ifelse(flag_total>2 | is.na(flag_total), center_share_replace, center),
    dkp_drp = ifelse(flag_total>2 | is.na(flag_total), dkp_drp_share_replace, dkp_drp),
    rsf = ifelse(flag_total>2 | is.na(flag_total), rsf_share_replace, rsf),
    rvwp = ifelse(flag_total>2 | is.na(flag_total), rvwp_share_replace, rvwp),
    dp = ifelse(flag_total>2 | is.na(flag_total), dp_share_replace, dp),
    srp = ifelse(flag_total>2 | is.na(flag_total), srp_share_replace, srp),
    other = ifelse(flag_total>2 | is.na(flag_total), other_share_replace, other),
    independent = ifelse(flag_total>2 | is.na(flag_total), independent_share_replace, independent),
  ) %>%
  select(-matches("flag|share|row_id")) %>%
  mutate(state_name = "Nordrhein-Westfalen") %>% relocate(state_name, .before = everything()) %>%
  select(-sheet)

write_rds(btw49_nrw_municipalities_clean, "./output/cleaned/49_btw_nrw_clean.rds")

### RLP ----

clear_environment()

xl_files_rlp49 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "49_btw_rlp", full.names = T)) %>%
  mutate(
    table = str_extract(path,"(?<=[0-9]_)[0-9]*(?=\\.xlsx)"),
    sheet = str_replace(path,"(?<=[0-9])_[0-9]*(?=\\.xlsx)", ""),
    sheet = str_extract(sheet,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet, table), as.numeric)) %>%
  arrange(sheet, table)

varnames <-
  c("key_municipality", "municipality_name", "voters_eligible", "voters", "votes_valid",
    "cdu", "spd", "fdp",
    "kpd", "independent")

xl_files_rlp49_clean <-
  future_imap(xl_files_rlp49$path, function(xl_file, idx) {


    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    if (ncol(file_df) == 9) {
      file_df$independent <- NA
    }

    colnames(file_df) <- varnames

    file_df$sheet <- xl_file

    return(file_df)

  })

btw49_rlp_municipalities_clean <-
  bind_rows(xl_files_rlp49_clean) %>%
  mutate(county_row = case_when(grepl("kreis", tolower(municipality_name)) ~ T),
         type = case_when(grepl("stadtkreis", municipality_name, ignore.case = T) ~ "city", T ~ "municipality"),
         municipality_name = ifelse(type == "city", paste0(municipality_name, ", Stadt"), municipality_name),
         county_name = case_when(county_row ~ paste(municipality_name, voters_eligible)),
         county_name = str_remove_all(county_name, "NA"),
         county_name = str_remove_all(tolower(county_name), ".*(?=landkreis)|,[^,]*$|[0-9]|noch:|nooh|noohs"),
         county_name = str_remove_all(county_name, "(?<![[:alnum:]])[[:punct:]](?![[:alnum:]])"),
         county_name = str_remove(county_name, "landkreis"),
         county_name = str_trim(str_to_title(county_name)),
         county_name = str_remove_all(county_name, "\\.$|Amt"),
         county_name = ifelse(grepl("stadtkreis", tolower(municipality_name)), municipality_name, county_name),
         across(county_name:municipality_name, ~ str_remove_all(.x, ".*(?=Stadtkreis)")),
         across(county_name:municipality_name, ~ str_remove_all(.x, "Stadtkreis") %>% str_trim())) %>%
  fill(county_name, type) %>%
  relocate(type, county_row, county_name, .before = everything()) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:independent, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:independent), ~ str_replace_multiple(tolower(.x), c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:independent), as.numeric),
  ) %>%
  filter(!if_all(voters_eligible:independent, is.na)) %>%
  drop_na(municipality_name) %>%
  filter(!grepl("usammen|zusanmens|kreis", municipality_name)) %>%
  select(-county_row) %>%
  mutate(across(cdu:independent, ~ replace_na(.x, 0))) %>%
  mutate(votes_valid = ifelse(is.na(votes_valid), rowSums(select(., cdu:independent)), votes_valid)) %>%
  select(-sheet) %>%
  mutate(state_name = "Rheinland-Pfalz") %>% relocate(state_name, .before = everything())

write_rds(btw49_rlp_municipalities_clean, "./output/cleaned/49_btw_rlp_clean.rds")

### SH ----

clear_environment()

xl_files_sh49 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "49_btw_sh", full.names = T)) %>%
  mutate(
    table = str_extract(path,"(?<=[0-9]_)[0-9]*(?=\\.xlsx)"),
    sheet = str_replace(path,"(?<=[0-9])_[0-9]*(?=\\.xlsx)", ""),
    sheet = str_extract(sheet,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet, table), as.numeric)) %>%
  arrange(sheet, table)

btw49_sh_cities <- read_xlsx("./additional_data/election_returns_cities/sh_49_cities.xlsx") %>%
  mutate(type = "city", across(voters_eligible:independent, as.character),
         across(matches("name"), ~ paste0(.x, ", Stadt")))

varnames <-
  c("key_municipality", "municipality_name", "voters_eligible", "voters", "votes_invalid", "votes_valid",
    "spd", "cdu",  "ssw", "fdp",
    "kpd", "dkp", "center", "dp", "rsf", "independent")

xl_files_sh49_clean <-
  future_imap(xl_files_sh49$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    col1 <- select(file_df, 1) %>% pull()

    col2 <- select(file_df, 2) %>% pull()


    if(all(col1 == col2, na.rm = T)) {

      file_df %<>% select(-1)

    }


    file_df %>%
      select(all_of(1:16)) %>%
      `colnames<-`(c(varnames)) %>%
      mutate(sheet = xl_file)

  })

btw49_sh_municipalities_clean <-
  bind_rows(xl_files_sh49_clean) %>%
  mutate(county_row = case_when(grepl("kreis", tolower(municipality_name)) ~ T),
         na_row = if_all(voters_eligible:independent, ~ is.na(.x)),
         county_row = case_when(
           lag(county_row) & na_row ~ T,
           lag(county_row) & lag(na_row) ~ T,
           T ~ county_row),
         county_name = case_when(county_row ~ municipality_name),
         county_id = cumsum(case_when(lag(county_row) ~ 0, county_row ~ 1, T ~ 0))
  ) %>%
  group_by(county_row, county_id, sheet) %>%
  mutate(county_name = ifelse(county_row, paste(municipality_name, collapse = " "), NA)) %>%
  ungroup() %>%
  fill(county_name, .direction = "up") %>%
  mutate(county_name = str_remove(county_name, "^Kreis")) %>%
  relocate(county_row, county_name, county_id, .before = everything()) %>%
  filter(is.na(county_row)) %>%
  # Deal with municipaltiy names split over rows
  mutate(
    municipality_name = case_when(
      lag(na_row) & is.na(key_municipality) ~ paste(lag(municipality_name), municipality_name, sep = " "),
      T ~ municipality_name
    )
  ) %>%
  filter(!na_row) %>%
  select(-c(na_row, county_row, county_id)) %>%
  mutate(type = "municipality") %>%
  bind_rows(btw49_sh_cities) %>%
  # CLean numeric cols
  mutate(across(voters_eligible:independent, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:independent), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:independent), as.numeric),
  ) %>%
  mutate(across(spd:independent, ~ replace_na(.x, 0))) %>%
  mutate(state_name = "Schleswig-Holstein") %>% relocate(state_name, .before = everything()) %>%
  select(-key_municipality, -sheet)


write_rds(btw49_sh_municipalities_clean, "./output/cleaned/49_btw_sh_clean.rds")

## BTW 1953 ----
### NDS ----

clear_environment()

xl_files_nds53 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "53_btw_nds", full.names = T)) %>%
  mutate(
    sheet = str_extract(path,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet), as.numeric)) %>%
  arrange(sheet)

varnames <-
  c("municipality_name", "voters_eligible", "voters", "votes_valid",
    "spd", "dp", "cdu", "drp",
    "fdp", "kpd", "gb_bhe", "gvp")

btw53_nds_cities <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1953_counties.rds") %>%
  filter(type == "city", key_state == "03") %>%
  select(county_name:kpd, type, -c(csu, bp, center), -votes_invalid) %>%
  rename_with(~ paste0(.x, "_second"), votes_valid:kpd) %>%
  mutate(municipality_name = county_name)

xl_files_nds53_clean <-
  future_imap(xl_files_nds53$path, function(xl_file, idx) {


    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    file_df %<>% mutate(sheet = basename(xl_file))

    if (ncol(file_df) == 14) {

      file_df %<>%
        unite("municipality_name", c(1, 2), sep = " ", na.rm = T) %>%
        mutate(municipality_name = ifelse(municipality_name == "", NA, municipality_name))

    }

    names(file_df) <- c(varnames, "sheet")

    return(file_df)

  })

btw53_nds_municipalities_clean <-
  bind_rows(xl_files_nds53_clean) %>%
  fill(municipality_name, .direction = "down") %>%
  # Counties
  unite("county_name", voters_eligible:gvp, na.rm = T, sep = " ", remove = F) %>%
  mutate(county_name = ifelse(if_any(voters_eligible:gvp, ~ grepl("land", tolower(.x))), county_name, NA),
         county_name = str_trim(str_remove_all(county_name, "[0-9]|landkreis|Landkreis"))) %>%
  fill(county_name, .direction = "down") %>%
  filter(!if_any(voters_eligible:gvp, ~ grepl("land", tolower(.x)))) %>%
  relocate(county_name, .before = everything()) %>%
  # Remove invalid rows
  rowwise() %>%
  mutate(missing_count = sum(is.na(c_across(voters_eligible:gvp)))) %>%
  ungroup() %>%
  filter(missing_count < 6) %>%
  select(-missing_count) %>%
  # Differentiate between first/second vote
  mutate(first_second_vote = case_when(grepl("\\sI$", municipality_name) | municipality_name == "I" ~ "first",
                                       grepl("II$", municipality_name) | municipality_name == "II" | municipality_name == "Il" ~ "second")) %>%
  relocate(first_second_vote, .after = municipality_name) %>%
  # Identify rows for same municipality
  mutate(across(first_second_vote:voters, ~ replace_na(.x, "NA"))) %>%
  mutate(municipality_id = voters_eligible == lead(voters_eligible) | voters == lead(voters) | (first_second_vote == "first" & lead(first_second_vote) == "second"),
         municipality_id = cumsum(municipality_id),
         municipality_id = paste0(municipality_id, str_extract(sheet, "[0-9]*(?=\\.xlsx)"))) %>%
  relocate(municipality_id, .after = municipality_name) %>%
  group_by(municipality_id) %>%
  filter(n() == 2) %>%
  mutate(first_second_vote = ifelse(row_number() == 1, "first", "second")) %>%
  # Clean municipality names
  mutate(municipality_name = str_remove_all(municipality_name, "I$|II|\\sI\\s$|Il$"),
         municipality_name = ifelse(municipality_name == "I", "", municipality_name),
         municipality_name = paste(municipality_name, collapse = ""),
         municipality_name = str_remove_all(municipality_name, "-\\s[:lower:]|(?<=-)\\s")) %>%
  ungroup() %>%
  # Clean numeric cols
  mutate(across(voters_eligible:gvp, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:gvp), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:gvp), as.numeric),
         across(spd:gvp, ~ replace_na(.x, 0))) %>%
  group_by(municipality_id) %>%
  mutate(across(voters_eligible:voters, ~ round(mean(.x)))) %>%
  ungroup() %>%
  # select(-sheet, -municipality_id) %>%
  # Pivot
  pivot_wider(names_from = first_second_vote, values_from = votes_valid:gvp) %>%
  # Add cities
  mutate(type = "municipality") %>%
  bind_rows(btw53_nds_cities) %>%
  mutate(state_name = "Niedersachsen") %>% relocate(state_name, .before = everything())

write_rds(btw53_nds_municipalities_clean, "./output/cleaned/53_btw_nds_clean.rds")

### NRW ----

clear_environment()

xl_files_nrw53 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "53_btw_nrw", full.names = T)) %>%
  mutate(sheet = str_extract(path,"[0-9]*(?=\\.xlsx)"),
         sheet = as.numeric(sheet)) %>%
  arrange(sheet)

# Left pages

xl_files_nrw53_left <- xl_files_nrw53 %>%
  filter(sheet %in% seq(1, 124, 2)) %>%
  pull(path)


varnames_left <-
  c("row_id", "entity_name", "voters_eligible", "voters", "turnout",
    "votes_valid_second", "cdu_second", "cdu_second_share",
    "spd_second", "spd_second_share")

xl_files_nrw53_left_clean <-
  future_map(xl_files_nrw53_left, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1)

    # Remove missing leading columns

    # Check which columns have only missing values
    missing_cols <- sapply(file_df, function(x) all(is.na(x)))
    # Identify the first columns to remove
    first_col <- which(missing_cols)[1]
    # Remove columns with only missing values from the data frame
    if (!is.na(first_col) & first_col == 1) {
      file_df <- file_df %>%
        select(-all_of(first_col))
    }
    #
    file_df %<>%
      `colnames<-`(varnames_left) %>%
      mutate(sheet = file)

    return(file_df)


  })

xl_files_nrw53_left_clean_df <-
  xl_files_nrw53_left_clean  %>%
  bind_rows() %>%
  # Identify type of unit
  mutate(
    sheet = as.numeric(str_extract(sheet, "[0-9]+(?=\\.xlsx)")),
    entity_name = str_replace(entity_name, "noch:", ""),
    entity_name = str_trim(entity_name),
    entity_name = ifelse(row_number() != 1 & lag(entity_name) == "Kempen-Krefeld", "Ldkr. Kempen-Krefeld", entity_name),
    district_row = str_detect(entity_name, "^Reg"),
    city_row = str_detect(tolower(entity_name), "^krfr\\.st"),
    county_row = str_detect(entity_name, "^kr\\.|Kreis|kreis$|kreis\\s|Ldkr|ldkr|Ldks|Idkr|kreia"),
  ) %>%
  filter(!str_detect(entity_name, "^Amt"),
         !str_detect(entity_name, "in.*gesa.*t|in.*amt|inegeaant")) %>%
  mutate(district_id = cumsum(district_row)) %>%
  group_by(district_id) %>%
  mutate(county_id = cumsum(county_row)) %>%
  relocate(district_id:county_id, .before = everything()) %>%
  mutate(
    county_name = ifelse(county_row, entity_name, NA),
    district_name = ifelse(district_row, entity_name, NA)
  ) %>%
  mutate(type = ifelse(county_id == 0, "city", "municipality")) %>%
  group_by(district_id) %>%
  fill(district_name, county_name) %>%
  ungroup() %>%
  filter(!if_all(voters_eligible:spd_second_share, is.na),
         !county_row, !district_row) %>%
  select(-c(district_row:county_row, district_id, county_id)) %>%
  relocate(county_name:type, .before = everything()) %>%
  mutate(across(c(county_name, entity_name), ~ str_remove_all(.x, "^[0-9]+|kr\\.|^Krfr\\.St|^Kreis|Amt|Ldkr\\.|nooh:|Ldks|nochs|Ldkr,|noch|moch|aoch|Landkreis") %>% str_trim())) %>%
  mutate(across(c(county_name, entity_name), ~ str_remove_all(.x, "^[[:punct:]]|[[:punct:]]$|(\\s[[:punct:]]\\s|)") %>% str_trim()),
         county_name = ifelse(type == "city", entity_name, county_name),
         district_name = str_trim(str_remove_all(district_name, "Re.{1}|Be.{1}|[:punct:]"))) %>%
  filter(!if_all(voters_eligible:spd_second_share, ~ is.na(.x)) & type == "municipality" | type == "city") %>%
  filter(!grepl("zus.*en", tolower(entity_name)))

# Right pages

xl_files_nrw53_right <- xl_files_nrw53 %>%
  filter(sheet %in% seq(2, 124, 2)) %>% pull(path)

varnames_right <- c("center_second", "center_second_share", "fdp_second", "fdp_second_share",
                    "kpd_second", "kpd_second_share", "gb_bhe_second", "gb_bhe_second_share",
                    "dp_second", "dp_second_share", "gvp_second", "gvp_second_share", "row_id")

xl_files_nrw53_right_clean <-
  future_map(xl_files_nrw53_right, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1) #%>%

    # Remove missing leading columns

    # Check which columns have only missing values
    missing_cols <- sapply(file_df, function(x) all(is.na(x)))
    # Identify the first columns to remove
    first_col <- which(missing_cols)[1]
    # Remove columns with only missing values from the data frame
    if (!is.na(first_col) & (first_col == 1 | first_col == 14)) {
      file_df <- file_df %>%
        select(-all_of(first_col))
    }
    #
    file_df %<>%
      select(all_of(1:13)) %>%
      `colnames<-`(varnames_right) %>%
      mutate(sheet = basename(file))

    return(file_df)


  })

xl_files_nrw53_right_clean_df  <-
  xl_files_nrw53_right_clean  %>%
  bind_rows() %>%
  mutate(sheet = str_extract(sheet, "[0-9]+(?=\\.xlsx)"),
         sheet = as.numeric(sheet) - 1) %>%
  drop_na(row_id)

btw53_nrw_municipalities_clean <-
  left_join(xl_files_nrw53_left_clean_df, xl_files_nrw53_right_clean_df, by = c("row_id", "sheet")) %>%
  group_by(row_id, sheet) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  select(-sheet) %>%
  rename(municipality_name = entity_name) %>%
  mutate(across(c(voters_eligible:gvp_second_share), ~ str_replace_multiple(.x, c("," = "\\.", "-" = "0", "\\s" = ""))),
         across(c(voters_eligible:gvp_second_share),as.numeric),
         across(c(cdu_second:gvp_second_share), ~ replace_na(.x, 0)),
         across(matches("share"), ~ ifelse(between(.x, 0, 100), .x, NA))) %>%
  relocate(matches("second_share"), .after = gvp_second) %>%
  # Caluclate flags based on reported totals and shares
  mutate(flag_share = abs(100- rowSums(select(., matches("second_share"))))) %>%
  mutate(across(cdu_second:gvp_second, ~ .x/votes_valid_second*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("second_share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%

  mutate(across(cdu_second_share:gvp_second_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid_second), .x),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu_second = ifelse(flag_total>2, cdu_second_share_replace, cdu_second),
    spd_second = ifelse(flag_total>2, spd_second_share_replace, spd_second),
    fdp_second = ifelse(flag_total>2, fdp_second_share_replace, fdp_second),
    gb_bhe_second = ifelse(flag_total>2, gb_bhe_second_share_replace, gb_bhe_second),
    center_second = ifelse(flag_total>2, center_second_share_replace, center_second),
    kpd_second = ifelse(flag_total>2, kpd_second_share_replace, kpd_second),
    dp_second = ifelse(flag_total>2, dp_second_share_replace, dp_second),
    gvp_second = ifelse(flag_total>2, gvp_second_share_replace, gvp_second),
  ) %>%
  select(-matches("flag|share"), -row_id) %>%
  mutate(state_name = "Nordrhein-Westfalen",
         across(c(municipality_name, county_name), ~ ifelse(type == "city", paste0(.x, ", Stadt"), .x))) %>% relocate(state_name, .before = everything())

write_rds(btw53_nrw_municipalities_clean, "./output/cleaned/53_btw_nrw_clean.rds")

### RLP ----

clear_environment()

xl_files_rlp53 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "53_btw_rlp", full.names = T) %>% sort()) %>%
  mutate(
    sheet = as.numeric(str_extract(path, "[0-9]+(?=_[0-9]+\\.xlsx)")),
    table = ifelse(!is.na(sheet), as.numeric(str_extract(path, "[0-9]+(?=\\.xlsx)")), NA),
    sheet = ifelse(is.na(sheet), as.numeric(str_extract(path, "[0-9]+(?=\\.xlsx)")), sheet)
  ) %>%
  arrange(sheet)

varnames <-
  c("municipality_key", "municipality_name", "voters_eligible", "voters", "first_second_vote",
    "votes_valid", "cdu", "spd", "fdp", "kpd", "gb_bhe", "dp", "gvp", "drp", "pdgd")

files_clean_rlp53 <-
  future_imap(xl_files_rlp53$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1) %>%
      mutate(sheet = basename(xl_file)) %>%
      mutate(across(everything(), as.character))
  })

btw53_rlp_municipalities_clean <-
  bind_rows(files_clean_rlp53) %>%
  `colnames<-`(c(varnames, "sheet"))  %>%
  mutate(
    county_row = if_any(everything(), ~ grepl("landkr.*s|lahnkreis|waldkreis", tolower(.x))),
    type = case_when(county_row ~ "municipality", if_any(everything(), ~ grepl("stadtkr.*s", tolower(.x))) ~ "city"),
    row_num = row_number()
  ) %>%
  unite(col = county_name, municipality_key:pdgd, sep = " ", na.rm = T, remove = F) %>%
  mutate(county_name = ifelse(!county_row, NA, county_name),
         county_name = str_to_title(str_trim(str_remove_all(tolower(county_name), "landkreis|noch:")))
  ) %>%
  fill(type, county_name) %>%
  relocate(county_row, county_name, type, row_num, .before = everything()) %>%
  filter(!county_row) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:pdgd, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:pdgd), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = ""))),
         across(c(voters_eligible:pdgd), as.numeric)) %>%
  filter(if_any(voters_eligible:pdgd, ~ !is.na(.x))) %>%
  # Deal with muns split over multiple rows
  select(-c(county_row, row_num)) %>%
  # Deal with municipalities split over
  mutate(
    helper_id_voters = voters_eligible,
    helper_id_key = municipality_key
  ) %>%
  fill(helper_id_voters, helper_id_key) %>%
  mutate(helper_id_voters = helper_id_voters != lag(helper_id_voters),
         helper_id_voters = replace_na(helper_id_voters, F),
         helper_id_voters = cumsum(helper_id_voters),
         helper_id_key = helper_id_key != lag(helper_id_key),
         helper_id_key = replace_na(helper_id_key, F),
         helper_id_key = cumsum(helper_id_key)) %>%
  relocate(helper_id_voters, helper_id_key, .before = everything()) %>%
  group_by(helper_id_voters, helper_id_key) %>%
  filter(row_number() < 3) %>%
  mutate(municipality_name = paste(ifelse(!is.na(municipality_name), municipality_name, ""), collapse = " "),
         municipality_name = ifelse(municipality_name == "", NA, municipality_name),
         first_second_vote = ifelse(row_number() == 1, "first", "second"),
         county_name = ifelse(type == "city", municipality_name, county_name)) %>%
  fill(municipality_name, municipality_key, voters_eligible) %>%
  ungroup() %>%
  select(-matches("helper")) %>%
  # Additional numeric clean and pivot
  mutate(across(cdu:pdgd, ~ replace_na(.x, 0))) %>%
  pivot_wider(names_from = first_second_vote, values_from = c(voters, votes_valid, cdu:pdgd)) %>%
  select(-sheet, -municipality_key) %>%
  mutate(state_name = "Rheinland-Pfalz",
         across(c(county_name, municipality_name), ~ ifelse(type == "city", paste0(.x, ", Stadt"), .x))) %>% relocate(state_name, .before = everything())

write_rds(btw53_rlp_municipalities_clean, "./output/cleaned/53_btw_rlp_clean.rds")

## BTW 1957 ----

### NDS ----

clear_environment()

# Create list of filepaths
xl_files_nds57 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "57_btw_nds", full.names = T)) %>%
  mutate(
    sheet = str_extract(path,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet), as.numeric)) %>%
  arrange(sheet)

# Read city data
btw57_nds_cities <-
  read_xlsx("./additional_data/election_returns_cities/nds_57_cities.xlsx",
            col_names = c("name", "city", "election", "voters_eligible", "voters", "turnout",
                          "votes_valid_second", "cdu", "spd", "dp",  "gb_bhe", "fdp", "fu_center",
                          "bdd", "dg", "drp", "mittelstand", "other",
                          "cdu_share", "spd_share", "dp_share",  "gb_bhe_share", "fdp_share", "fu_center_share",
                          "bdd_share", "dg_share", "drp_share", "mittelstand_share", "other_share"),
            skip = 1) %>%
  mutate(name = str_trim(str_remove(name, "^[0-9]*"))) %>%
  fill(name, .direction = "down") %>%
  filter(election == "1957", city == 1) %>%
  select(name, matches("vote"), cdu:mittelstand) %>%
  mutate(county_name = name, type = "city") %>%
  rename(municipality_name = name) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:mittelstand, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:mittelstand), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:mittelstand), as.numeric),
         across(cdu:mittelstand, ~ replace_na(.x, 0))) %>%
  rename_with(~ paste0(.x, "_second"), cdu:mittelstand)

varnames <-
  c("municipality_name", "voters_eligible", "voters", "votes_valid",
    "cdu", "spd", "dp",  "gb_bhe",
    "fdp", "fu_center", "bdd", "dg", "drp", "mittelstand")

xl_files_nds57_clean <-
  future_imap(xl_files_nds57$path, function(xl_file, idx) {


    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    file_df %<>% mutate(sheet = basename(xl_file))

    names(file_df) <- c(varnames, "sheet")

    return(file_df)

  })

btw57_nds_municipalities_clean <-
  bind_rows(xl_files_nds57_clean) %>%
  # Counties
  mutate(county_name = ifelse(grepl("ldkr", tolower(municipality_name)), municipality_name, NA)) %>%
  fill(county_name, .direction = "down") %>%
  relocate(county_name, .before = everything()) %>%
  filter(!grepl("ldkr", tolower(municipality_name))) %>%
  # Identify rows of same municipality
  mutate(
    municipality_name = str_trim(municipality_name),
    municipality_name = str_remove_all(municipality_name, "II$|I$"),
    voters_eligible = str_remove_all(voters_eligible, "I"),
    voters_eligible = ifelse(voters_eligible == "", NA, voters_eligible),
    municipality_id = (!is.na(voters_eligible) & is.na(lead(voters_eligible))) | (!is.na(voters) & is.na(lead(voters))),
    municipality_id = cumsum(municipality_id),
    municipality_id = paste0(municipality_id, str_extract(sheet, "[0-9]*(?=\\.xlsx)"))
  ) %>%
  relocate(municipality_id, .after = municipality_name) %>%
  group_by(municipality_id) %>%
  mutate(municipality_name = paste0(municipality_name, collapse = ""),
         municipality_name = str_remove_all(municipality_name, "NA"),
         municipality_name = str_remove(municipality_name, "^\\d+\\)"),
         county_name = ifelse(row_number() != 1, NA, county_name)) %>%
  fill(county_name, .direction = "down") %>%
  ungroup() %>%
  # # Remove invalid rows
  rowwise() %>%
  mutate(missing_count = sum(is.na(c_across(voters_eligible:drp)))) %>%
  ungroup() %>%
  relocate(missing_count, .before = everything()) %>%
  filter(missing_count < 10) %>%
  select(-missing_count) %>%
  group_by(municipality_id) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  # Clean numeric cols
  mutate(across(voters_eligible:mittelstand, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:mittelstand), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:mittelstand), as.numeric),
         across(cdu:mittelstand, ~ replace_na(.x, 0))) %>%
  group_by(municipality_id) %>%
  mutate(first_second_vote = ifelse(row_number() == 1, "first", "second")) %>%
  fill(voters_eligible, voters, .direction = "down") %>%
  ungroup() %>%
  relocate(first_second_vote, .after = voters) %>%
  pivot_wider(values_from = votes_valid:mittelstand, names_from = first_second_vote) %>%
  relocate(matches("second"), .before = votes_valid_first) %>%
  # Add cities
  mutate(type = "municipality") %>%
  bind_rows(btw57_nds_cities) %>%
  mutate(state_name = "Niedersachsen") %>% relocate(state_name, .before = everything())

write_rds(btw57_nds_municipalities_clean, "./output/cleaned/57_btw_nds_clean.rds")

### NRW ----

clear_environment()

xl_files_nrw57 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "57_btw_nrw", full.names = T)) %>%
  mutate(sheet = str_extract(path,"[0-9]*(?=\\.xlsx)"),
         sheet = as.numeric(sheet)) %>%
  arrange(sheet)

# Left pages

xl_files_nrw57_left <- xl_files_nrw57 %>%
  filter(sheet %in% seq(1, 94, 2)) %>%
  pull(path)

varnames_left <-
  c("row_id", "entity_name", "voters_eligible", "voters", "turnout",
    "votes_valid_second", "cdu_second", "cdu_second_share",
    "spd_second", "spd_second_share")

xl_files_nrw57_left_clean <-
  future_map(xl_files_nrw57_left, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1)

    # Remove missing leading columns

    # Check which columns have only missing values
    missing_cols <- sapply(file_df, function(x) all(is.na(x)))
    # Identify the first columns to remove
    first_col <- which(missing_cols)[1]
    # Remove columns with only missing values from the data frame
    if (!is.na(first_col) & first_col == 1) {
      file_df <- file_df %>%
        select(-all_of(first_col))
    }

    file_df %<>%
      select(everything(all_of(1:10))) %>%
      `colnames<-`(varnames_left) %>%
      mutate(sheet = file)

    return(file_df)


  })



xl_files_nrw57_left_clean_df <-
  xl_files_nrw57_left_clean  %>%
  bind_rows() %>%
  # Identify type of unit
  mutate(
    entity_name = str_replace(entity_name, "noch:", ""),
    entity_name = str_trim(entity_name),
    entity_name = ifelse(row_number() != 1 & lag(entity_name) == "Kempen-Krefeld", "Ldkr. Kempen-Krefeld", entity_name),
    type = case_when(
      str_detect(tolower(entity_name), "^krfr\\.st") ~ "city",
      str_detect(entity_name, "^kr\\.|Kreis|kreis$|kreis\\s|Ldkr|ldkr|^Idkr") ~ "county",
      str_detect(entity_name, "Amt") ~ "amt",
      T ~ ""
    )
  ) %>%
  mutate(sheet = as.numeric(str_extract(sheet, "[0-9]+(?=\\.xlsx)"))) %>%
  relocate(type, .after = row_id) %>%
  group_by(sheet) %>%
  mutate(cumsum_county = cumsum(type == "county")) %>%
  ungroup() %>%
  relocate(matches("cumsum"), sheet, .before = everything()) %>%
  mutate(type = case_when(
    type == "" & cumsum_county == 0 ~ "city",
    type == "" & cumsum_county != 0 ~ "municipality",
    T ~ type
  )) %>%
  select(-cumsum_county) %>%
  mutate(county_name = case_when(type == "county" | type == "city" ~ entity_name)) %>%
  relocate(county_name, .before = entity_name) %>%
  fill(county_name, .direction = "down") %>%
  mutate(across(c(county_name, entity_name), ~ str_remove(.x, "^[0-9]+|kr\\.|^Krfr\\.St|^Kreis|Amt|Ldkr\\.") %>% str_trim())) %>%
  mutate(across(c(county_name, entity_name), ~ str_remove(.x, "(^[[:punct:]]\\s)|(\\s[[:punct:]]$)|(\\s[[:punct:]]\\s)"))) %>%
  filter(!grepl("in.*gesa.*t", entity_name)) %>%
  filter(!if_all(voters_eligible:spd_second_share, is.na) & (type == "municipality" | type == "city")) %>%
  filter(!grepl("zus.*en", tolower(entity_name)))

# Right pages

xl_files_nrw57_right <- xl_files_nrw57 %>%
  filter(sheet %in% seq(2, 94, 2)) %>% pull(path)

varnames_right <- c("fdp_second", "fdp_second_share", "gb_bhe_second", "gb_bhe_second_share",
                    "dp_second", "dp_second_share", "fu_center_second", "fu_center_second_share",
                    "bdd_second", "bdd_second_share", "drp_second", "drp_second_share",
                    "mittelstand_second", "mittelstand_second_share", "row_id")

xl_files_nrw57_right_clean <-
  future_map(xl_files_nrw57_right, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1) %>%
      rowwise() %>%
      mutate(numeric_share = sum(c_across(everything()) %>% str_count("[0-9]"), na.rm = t) / sum(c_across(everything()) %>% str_length(), na.rm = T)) %>%
      ungroup() %>%
      filter(numeric_share > .2) %>%
      select(-numeric_share) %>%
      select(where(function(x) any(!is.na(x)))) %>%
      `colnames<-`(varnames_right)
  }) %>%
  imap(~ mutate(.x, sheet = xl_files_nrw57_right[.y]))


xl_files_nrw57_right_clean_df  <-
  xl_files_nrw57_right_clean  %>%
  bind_rows() %>%
  mutate(sheet = str_extract(sheet, "[0-9]+(?=\\.xlsx)"),
         sheet = as.numeric(sheet) - 1) %>%
  drop_na(row_id)


btw57_nrw_municipalities_clean <-
  left_join(xl_files_nrw57_left_clean_df, xl_files_nrw57_right_clean_df, by = c("row_id", "sheet")) %>%
  group_by(row_id, sheet) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  # Clean numeric cols
  mutate(across(c(voters_eligible:mittelstand_second_share), ~ str_replace_multiple(.x, c("," = "\\.", "-" = "0", "\\s" = ""))),
         across(c(voters_eligible:mittelstand_second_share),as.numeric),
         across(c(cdu_second:mittelstand_second_share), ~ replace_na(.x, 0)),
         across(matches("share"), ~ ifelse(between(.x, 0, 100), .x, NA))) %>%
  mutate(flag_share = abs(100- rowSums(select(., matches("second_share"))))) %>%
  relocate(matches("second_share"), .after = mittelstand_second) %>%
  mutate(across(cdu_second:mittelstand_second, ~ .x/votes_valid_second*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("second_share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  mutate(across(cdu_second_share:mittelstand_second_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid_second), .x),
           .names = "{.col}_replace")) %>%
  mutate(
    cdu_second = ifelse(flag_total>2, cdu_second_share_replace, cdu_second),
    spd_second = ifelse(flag_total>2, spd_second_share_replace, spd_second),
    fdp_second = ifelse(flag_total>2, fdp_second_share_replace, fdp_second),
    gb_bhe_second = ifelse(flag_total>2, gb_bhe_second_share_replace, gb_bhe_second),
    dp_second = ifelse(flag_total>2, dp_second_share_replace, dp_second),
    fu_center_second = ifelse(flag_total>2, fu_center_second_share_replace, fu_center_second),
    bdd_second = ifelse(flag_total>2, bdd_second_share_replace, bdd_second),
    drp_second = ifelse(flag_total>2, drp_second_share_replace, drp_second),
    mittelstand_second = ifelse(flag_total>2, mittelstand_second_share_replace, mittelstand_second)
  ) %>%
  select(-matches("flag|share"), -row_id) %>%
  mutate(state_name = "Nordrhein-Westfalen") %>%
  relocate(state_name, .before = everything()) %>%
  rename(municipality_name = entity_name) %>%
  mutate(
    across(c(county_name, municipality_name), ~ ifelse(type == "city", paste0(.x, ", Stadt"), .x))
  ) %>%
  select(-sheet)

write_rds(btw57_nrw_municipalities_clean, "./output/cleaned/57_btw_nrw_clean.rds")

### SH ----

clear_environment()

xl_files_sh57 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "57_btw_sh", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]+(?=_[0-9]+\\.xlsx)")),
    table = as.numeric(str_extract(path, "[0-9]+(?=\\.xlsx)"))
  ) %>%
  arrange(sheet, table)

varnames <-
  c("municipality_name", "voters_eligible", "voters",
    "votes_invalid", "votes_valid",
    "cdu", "spd", "gb_bhe", "fdp", "dp", "bdd", "drp", "ssw")

files_clean_sh57 <-
  future_imap(xl_files_sh57$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1) %>%
      mutate(across(everything(), as.character))

  }) %>%
  imap(~ mutate(.x, sheet = xl_files_sh57$path[.y]))

sh57_municipalities_clean <-
  bind_rows(files_clean_sh57) %>%
  `colnames<-`(c(varnames, "sheet")) %>%
  # Identify county rows
  mutate(municipality_name = str_to_title(municipality_name),
         letter_first_ascii = str_extract(municipality_name, "^[:alpha:]")) %>%
  fill(letter_first_ascii, .direction = "down") %>%
  rowwise() %>%
  mutate(letter_first_ascii = as.integer(charToRaw(letter_first_ascii))) %>%
  ungroup() %>%
  fill(municipality_name, .direction = "down") %>%
  mutate(
    alphabet_start = letter_first_ascii < lag(letter_first_ascii) & lead(letter_first_ascii) < lag(letter_first_ascii),
    county_row = case_when(
      if_all(voters_eligible:ssw, is.na) & alphabet_start ~ T & !grepl("[0-9]", municipality_name),
      grepl("^noch", tolower(municipality_name)) ~ T,
      if_all(voters_eligible:ssw, is.na) & municipality_name %in% c("Plön", "Schleswig", "Segeberg") ~ T
    )) %>%
  relocate(county_row, letter_first_ascii, .before = everything()) %>%
  # Create col for county name
  mutate(county_name = ifelse(county_row, municipality_name, NA),
         county_name = str_trim(str_remove(county_name, "noch:|Noch:"))) %>%
  fill(county_name, .direction = "down") %>%
  relocate(county_name, .before = everything()) %>%
  filter(is.na(county_row), if_any(voters_eligible:ssw, ~ !is.na(.x))) %>%
  select(-c(county_row, letter_first_ascii, alphabet_start)) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:ssw, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:ssw), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = ""))),
         across(c(voters_eligible:ssw), as.numeric)) %>%
  filter(if_any(voters_eligible:ssw, ~ !is.na(.x))) %>%
  # County var for cities
  mutate(county_name = ifelse(is.na(county_name), municipality_name, county_name),
         type = ifelse(municipality_name == county_name, "city", "municipality")) %>%
  relocate(type, .after = municipality_name) %>%
  # Additional cleaning of numeric cols
  mutate(
    across(cdu:ssw, ~ replace_na(.x, 0)),
    state_name = "Schleswig-Holstein"
  ) %>%
  relocate(state_name, .before = everything()) %>%
  rename_with(~ paste0(.x, "_second"), votes_invalid:ssw) %>%
  select(-sheet)

write_rds(sh57_municipalities_clean, "./output/cleaned/57_btw_sh_clean.rds")

### RLP ----

clear_environment()

xl_files_rlp57 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "57_btw_rlp", full.names = T) %>% sort()) %>%
  mutate(
    sheet = as.numeric(str_extract(path, "[0-9]+(?=_[0-9]+\\.xlsx)")),
    table = ifelse(!is.na(sheet), as.numeric(str_extract(path, "[0-9]+(?=\\.xlsx)")), NA),
    sheet = ifelse(is.na(sheet), as.numeric(str_extract(path, "[0-9]+(?=\\.xlsx)")), sheet)
  ) %>%
  arrange(sheet)

varnames <-
  c("key_municipality", "municipality_name", "voters_eligible", "voters", "first_second_vote",
    "votes_valid", "cdu", "spd", "fdp", "gb_bhe", "dp", "bdd", "dg", "drp", "pdgd")

files_clean_rlp57 <-
  future_imap(xl_files_rlp57$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)


    if (all(is.na(file_df[[1]]))) {
      file_df %<>% select(-1)
    }

    file_df %>%
      select(1:15) %>%
      `colnames<-`(as.character(1:15)) %>%
      mutate(sheet = basename(xl_file)) %>%
      mutate(across(everything(), as.character))

  })

btw57_rlp_municipalities_clean <-
  bind_rows(files_clean_rlp57) %>%
  `colnames<-`(c(varnames, "sheet")) %>%
  mutate(
    county_row = if_any(voters_eligible:pdgd, ~ grepl("landkr.*s|lahnkreis|waldkreis|lahnkreis", tolower(.x))),
    type = case_when(county_row ~ "municipality", if_any(everything(), ~ grepl("kreisfrei", tolower(.x))) ~ "city"),
    row_num = row_number()
  ) %>%
  unite(col = county_name, key_municipality:pdgd, sep = " ", na.rm = T, remove = F) %>%
  mutate(county_name = ifelse(!county_row, NA, county_name),
         county_name = str_to_title(str_trim(str_remove_all(tolower(county_name), "landkreis|noch:|noch\\s|[0-9]")))
  ) %>%
  fill(type, county_name) %>%
  relocate(county_row, county_name, type, row_num, .before = everything()) %>%
  filter(!county_row) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:pdgd, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:pdgd), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "e" = "8"))),
         across(c(voters_eligible:pdgd), as.numeric)) %>%
  filter(if_any(voters_eligible:pdgd, ~ !is.na(.x))) %>%
  # Deal with muns split over multiple rows
  select(-c(county_row, row_num)) %>%
  mutate(
    helper_id_voters = voters_eligible,
    helper_id_key = key_municipality
  ) %>%
  fill(helper_id_voters, helper_id_key) %>%
  mutate(helper_id_voters = helper_id_voters != lag(helper_id_voters),
         helper_id_voters = replace_na(helper_id_voters, F),
         helper_id_voters = cumsum(helper_id_voters),
         helper_id_key = helper_id_key != lag(helper_id_key),
         helper_id_key = replace_na(helper_id_key, F),
         helper_id_key = cumsum(helper_id_key)) %>%
  relocate(helper_id_voters, helper_id_key, .before = everything()) %>%
  group_by(helper_id_voters, helper_id_key) %>%
  filter(row_number() < 3) %>%
  mutate(municipality_name = paste(ifelse(!is.na(municipality_name), municipality_name, ""), collapse = " "),
         municipality_name = ifelse(municipality_name == "", NA, municipality_name),
         first_second_vote = ifelse(row_number() == 1, "first", "second"),
         county_name = ifelse(type == "city", municipality_name, county_name),
         across(matches("name"), ~ ifelse(type == "city", paste0(str_trim(.x), ", Stadt"), .x))) %>%
  fill(municipality_name, key_municipality, voters_eligible) %>%
  ungroup() %>%
  select(-matches("helper")) %>%
  # Additional numeric clean and pivot
  mutate(across(cdu:pdgd, ~ replace_na(.x, 0))) %>%
  select(-sheet) %>%
  pivot_wider(names_from = first_second_vote, values_from = c(voters, votes_valid, cdu:pdgd)) %>%
  mutate(state_name = "Rheinland-Pfalz") %>% relocate(state_name, .before = everything())

write_rds(btw57_rlp_municipalities_clean, "./output/cleaned/57_btw_rlp_clean.rds")

### SA ----

clear_environment()

xl_files_sa57 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "57_btw_sa", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]+(?=\\.xlsx)"))
  ) %>%
  arrange(sheet)

varnames <-
  c("key_municipality", "municipality_name", "pop",
    "voters_eligible", "voters",
    "turnout", "votes_valid_second", "votes_valid_second_share",
    "cdu_second", "cdu_second_share",
    "csu_cvp_second", "csu_cvp_second_share",
    "spd_second", "spd_second_share",
    "fdp_second", "fdp_second_share",
    "drp_second", "drp_second_share",
    "bdd_second", "bdd_second_share",
    "dg_second", "dg_second_share",
    "dp_second", "dp_second_share",
    "gb_bhe_second", "gb_bhe_second_share")

files_clean_sa57_df <-
  future_imap(xl_files_sa57$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

  })


sa57_municipalities_clean <-
  bind_rows(files_clean_sa57_df) %>%
  `colnames<-`(varnames) %>%
  mutate(county_row = case_when(
    if_all(pop:gb_bhe_second_share, is.na) & is.na(key_municipality) & municipality_name != "1) in Faha" ~ T,
    T ~ F
  )) %>%
  mutate(county_name = ifelse(county_row, municipality_name, NA)) %>%
  fill(county_name) %>%
  relocate(matches("county"), .before = everything()) %>%
  filter(!county_row) %>%
  select(-county_row) %>%
  filter(!grepl("zus", municipality_name)) %>%
  # Clean numeric cols
  mutate(across(pop:gb_bhe_second_share, ~ str_remove_all(.x, "\\s")),
         across(c(pop:gb_bhe_second_share), ~ str_replace_multiple(.x, c("," = "\\.", "C" = "0", "-" = "0", "^\\(" = ""))),
         across(c(pop:gb_bhe_second_share), as.numeric)) %>%
  filter(if_any(pop:gb_bhe_second_share, ~ !is.na(.x))) %>%
  # Additional cleaning of numeric cols
  mutate(
    across(cdu_second_share:gb_bhe_second_share, ~ replace_na(.x, 0)),
  ) %>%
  select(-votes_valid_second_share) %>%
  select(-matches("share")) %>%
  mutate(state_name = "Saarland") %>% relocate(state_name, .before = everything()) %>%
  mutate(type = ifelse(municipality_name == "Saarbrücken", "city", "municipality"),
         municipality_name = ifelse(municipality_name == "Saarbrücken", county_name, municipality_name))

write_rds(sa57_municipalities_clean, "./output/cleaned/57_btw_sa_clean.rds")

## BTW 1961 ----
### NDS ----

clear_environment()

btw61_nds_cities <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1961_counties.rds") %>%
  filter(type == "city", key_state == "03") %>%
  select(county_name:dg, type, -csu, -votes_invalid) %>%
  rename_with(~ paste0(.x, "_second"), votes_valid:dg) %>%
  mutate(municipality_name = county_name)

xl_files_nds61 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "61_btw_nds", full.names = T)) %>%
  mutate(
    table = str_extract(path,"(?<=[0-9]_)[0-9]*(?=\\.xlsx)"),
    sheet = str_replace(path,"(?<=[0-9])_[0-9]*(?=\\.xlsx)", ""),
    sheet = str_extract(sheet,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet, table), as.numeric)) %>%
  arrange(sheet, table)

varnames <-
  c("municipality_name", "voters_eligible", "voters", "turnout", "votes_valid",
    "cdu", "spd", "fdp", "gdp", "dfu", "dg", "drp")

xl_files_nds61_clean <-
  future_imap(xl_files_nds61$path, function(xl_file, idx) {


    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    if (mean(file_df$`1` == "1" | file_df$`1` == "2", na.rm = T) > .9) {

      file_df %<>% unite("0", 1:2, sep = " ", remove = T, na.rm = T)

    }

    names(file_df) <- as.character(seq(1, ncol(file_df)))

    if (mean(is.na(file_df$`5`)) > .6) {

      file_df %<>% unite("4", 4:5, sep = "", remove = T, na.rm = T)

    }

    file_df %<>% mutate(sheet = basename(xl_file))

    names(file_df) <- c(varnames, "sheet")

    return(file_df)

  })

invalid_counties <-
  c("\" 2", "DALITZ2)", "DEPPOLDSHAUSEN 1)", "KUKATE 2)", "LANDWEHR1)", "LIEPEHOEFEN1",
    "NAUDEN3)", "NIENWEDEL )", "POTZWENDEN 1)", "PRABSTORF3", "SCHWEGEN1",
    "SELLIEN 3)", "Superscript(1)) einschl.", "TRIPKAU4)", "WALLEN 1)", "a",
    "HABERLOHY", "PLUMBOHM 2)", "GOVELIN", "HOHENKOERBEN v", "VORWERKY")


btw61_nds_municipalities_clean <-
  bind_rows(xl_files_nds61_clean) %>%
  # Counties
  mutate(turnout = ifelse(turnout == "", NA, turnout),
         county_row = if_all(voters_eligible:drp, is.na) & is.na(as.numeric(str_sub(municipality_name, 1, 1))),
         county_name = ifelse(county_row, municipality_name, NA),
         county_name = ifelse(county_name %in% invalid_counties, NA, county_name)) %>%
  fill(county_name, .direction = "down") %>%
  relocate(county_name, .before = everything()) %>%
  select(-county_row) %>%
  # Remove invalid rows
  rowwise() %>%
  mutate(missing_count = sum(is.na(c_across(voters_eligible:drp)))) %>%
  ungroup() %>%
  relocate(missing_count, .before = everything()) %>%
  filter(missing_count < 10) %>%
  select(-missing_count) %>%
  # Identify rows of same municipality
  mutate(
    municipality_id = (!is.na(voters_eligible) & is.na(lead(voters_eligible))) |
      (!is.na(voters) & is.na(lead(voters))) |
      (!is.na(turnout) & is.na(lead(turnout))),
    municipality_id = cumsum(municipality_id),
    municipality_id = paste0(municipality_id, str_extract(sheet, "[0-9]*(?=\\.xlsx)"))
  ) %>%
  relocate(municipality_id, .after = municipality_name) %>%
  group_by(municipality_id) %>% filter(n() == 2) %>%
  mutate(municipality_name = str_trim(str_remove_all(municipality_name, "[0-9]")),
         municipality_name = ifelse(municipality_name == "", NA, municipality_name),
         county_name = ifelse(row_number() == 2, NA, county_name),
         across(matches("name"), str_to_title),
         first_second_vote = ifelse(row_number() == 1, "first", "second")) %>%
  fill(county_name:turnout) %>%
  ungroup() %>%
  relocate(first_second_vote, .after = municipality_id) %>%
  # Clean numeric cols
  select(-turnout) %>%
  mutate(across(voters_eligible:drp, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:drp), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:drp), as.numeric),
         across(cdu:drp, ~ replace_na(.x, 0))) %>%
  # Pivot
  pivot_wider(names_from = first_second_vote, values_from = votes_valid:drp, id_cols = c(county_name:voters, sheet)) %>%
  select(-sheet) %>%
  mutate(type = "municipality") %>%
  bind_rows(btw61_nds_cities) %>%
  mutate(state_name = "Niedersachsen") %>% relocate(state_name, .before = everything())

write_rds(btw61_nds_municipalities_clean, "./output/cleaned/61_btw_nds_clean.rds")


### NRW ----

clear_environment()

xl_files_nrw61 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "61_btw_nrw", full.names = T)) %>%
  mutate(sheet = str_extract(path,"[0-9]*(?=\\.xlsx)"),
         sheet = as.numeric(sheet)) %>%
  arrange(sheet)

# Left pages

xl_files_nrw61_left <- xl_files_nrw61 %>%
  filter(sheet %in% seq(1, 92, 2)) %>%
  pull(path)

varnames_left <-
  c("row_id", "entity_name", "voters_eligible", "voters", "turnout",
    "votes_valid_second", "cdu_second", "cdu_second_share",
    "spd_second", "spd_second_share")

xl_files_nrw61_left_clean <-
  future_map(xl_files_nrw61_left, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1)

    # Remove missing leading columns

    # Check which columns have only missing values
    missing_cols <- sapply(file_df, function(x) all(is.na(x)))
    # Identify the first columns to remove
    first_col <- which(missing_cols)[1]
    # Remove columns with only missing values from the data frame
    if (!is.na(first_col) & first_col == 1) {
      file_df <- file_df %>%
        select(-all_of(first_col))
    }

    file_df %<>%
      select(everything(all_of(1:10))) %>%
      `colnames<-`(varnames_left) %>%
      mutate(sheet = file)

    return(file_df)


  })

xl_files_nrw61_left_clean_df <-
  xl_files_nrw61_left_clean  %>%
  bind_rows() %>%
  # Identify type of unit
  mutate(
    entity_name = str_replace(entity_name, "noch:", ""),
    entity_name = str_trim(entity_name),
    entity_name = ifelse(row_number() != 1 & lag(entity_name) == "Kempen-Krefeld", "Ldkr. Kempen-Krefeld", entity_name),
    type = case_when(
      str_detect(tolower(entity_name), "^krfr\\.st") ~ "city",
      str_detect(entity_name, "^kr\\.|Kreis|kreis$|kreis\\s|Ldkr|ldkr|^Idkr") ~ "county",
      str_detect(entity_name, "Amt") ~ "amt",
      T ~ ""
    )
  ) %>%
  mutate(sheet = as.numeric(str_extract(sheet, "[0-9]+(?=\\.xlsx)"))) %>%
  relocate(type, .after = row_id) %>%
  group_by(sheet) %>%
  mutate(cumsum_county = cumsum(type == "county")) %>%
  ungroup() %>%
  relocate(matches("cumsum"), sheet, .before = everything()) %>%
  mutate(type = case_when(
    type == "" & cumsum_county == 0 ~ "city",
    type == "" & cumsum_county != 0 ~ "municipality",
    T ~ type
  )) %>%
  select(-cumsum_county) %>%
  mutate(county_name = case_when(type == "county" | type == "city" ~ entity_name)) %>%
  relocate(county_name, .before = entity_name) %>%
  fill(county_name, .direction = "down") %>%
  mutate(across(c(county_name, entity_name), ~ str_remove(.x, "^[0-9]+|kr\\.|^Krfr\\.St|^Kreis|Amt|Ldkr\\.") %>% str_trim())) %>%
  mutate(across(c(county_name, entity_name), ~ str_remove(.x, "(^[[:punct:]]\\s)|(\\s[[:punct:]]$)|(\\s[[:punct:]]\\s)"))) %>%
  filter(!grepl("in.*gesa.*t", entity_name)) %>%
  filter(!if_all(voters_eligible:spd_second_share, is.na) & (type == "municipality" | type == "city")) %>%
  filter(!grepl("zus.*en", tolower(entity_name)))


# Right pages

xl_files_nrw61_right <- xl_files_nrw61 %>%
  filter(sheet %in% seq(2, 92, 2)) %>% pull(path)

varnames_right <- c("fdp_second", "fdp_second_share", "gdp_second", "gdp_second_share",
                    "dfu_second", "dfu_second_share", "dg_second", "dg_second_share",
                    "drp_second", "drp_second_share", "other_second", "other_second_share", "row_id")

xl_files_nrw61_right_clean <-
  future_map(xl_files_nrw61_right, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1) %>%
      rowwise() %>%
      mutate(numeric_share = sum(c_across(everything()) %>% str_count("[0-9]"), na.rm = t) / sum(c_across(everything()) %>% str_length(), na.rm = T)) %>%
      ungroup() %>%
      filter(numeric_share > .2) %>%
      select(-numeric_share) %>%
      select(where(function(x) any(!is.na(x)))) %>%
      `colnames<-`(varnames_right)


  }) %>%
  map_at(26, ~ .x %>% `colnames<-`(c(varnames_right, "row_id_2")) %>% unite(data = ., col = "row_id", matches("row_id"), sep = "", na.rm = T)) %>%
  imap(~ mutate(.x, sheet = xl_files_nrw61_right[.y]))


xl_files_nrw61_right_clean_df  <-
  xl_files_nrw61_right_clean  %>%
  bind_rows() %>%
  mutate(sheet = str_extract(sheet, "[0-9]+(?=\\.xlsx)"),
         sheet = as.numeric(sheet) - 1)

btw61_nrw_municipalities_clean <-
  left_join(xl_files_nrw61_left_clean_df, xl_files_nrw61_right_clean_df, by = c("row_id", "sheet")) %>%
  relocate(sheet, .before = everything()) %>%
  # Clean numeric cols
  mutate(across(c(voters_eligible:other_second_share), ~ str_replace_multiple(.x, c("," = "\\.", "-" = "0", "\\s" = ""))),
         across(c(voters_eligible:other_second_share),as.numeric),
         across(c(cdu_second:other_second_share), ~ replace_na(.x, 0)),
         across(matches("share"), ~ ifelse(between(.x, 0, 100), .x, NA))) %>%
  mutate(flag_share = abs(100- rowSums(select(., matches("second_share"))))) %>%
  relocate(matches("second_share"), .after = other_second) %>%
  mutate(across(cdu_second:other_second, ~ .x/votes_valid_second*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("second_share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  mutate(across(cdu_second_share:other_second_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid_second), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu_second = ifelse(flag_total>2, cdu_second_share_replace, cdu_second),
    spd_second = ifelse(flag_total>2, spd_second_share_replace, spd_second),
    fdp_second = ifelse(flag_total>2, fdp_second_share_replace, fdp_second),
    gdp_second = ifelse(flag_total>2, gdp_second_share_replace, gdp_second),
    dfu_second = ifelse(flag_total>2, dfu_second_share_replace, dfu_second),
    dg_second = ifelse(flag_total>2, dg_second_share_replace, dg_second),
    drp_second = ifelse(flag_total>2, drp_second_share_replace, drp_second),
    other_second = ifelse(flag_total>2, other_second_share_replace, other_second),
  ) %>%
  select(-matches("flag|share"), -row_id) %>%
  mutate(state_name = "Nordrhein-Westfalen") %>%
  relocate(state_name, .before = everything()) %>%
  rename(municipality_name = entity_name) %>%
  mutate(
    across(c(county_name, municipality_name), ~ ifelse(type == "city", paste0(.x, ", Stadt"), .x))
  ) %>%
  select(-sheet) %>%
  mutate(state_name = "Nordrhein-Westfalen") %>% relocate(state_name, .before = everything())

write_rds(btw61_nrw_municipalities_clean, "./output/cleaned/61_btw_nrw_clean.rds")

### SH ----

clear_environment()

xl_files_sh61 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "61_btw_sh", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]*(?=.xlsx)"))) %>%
  arrange(sheet)

varnames <-
  c("municipality_name", "voters_eligible", "voters",
    "votes_invalid", "votes_valid",
    "cdu", "spd", "fdp", "gdp",  "dfu", "dg", "drp", "ssw")

files_clean_sh61 <-
  future_imap(xl_files_sh61$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1) %>%
      # `colnames<-`(as.character(0:11)) %>%
      mutate(across(everything(), as.character))

  }) %>%
  imap(~ mutate(.x, sheet = xl_files_sh61$sheet[.y]))

sh61_municipalities_clean <-
  bind_rows(files_clean_sh61) %>%
  `colnames<-`(c(varnames, "sheet")) %>%
  # Identify county rows
  mutate(municipality_name = str_to_title(municipality_name),
         letter_first_ascii = str_extract(municipality_name, "^[:alpha:]")) %>%
  fill(letter_first_ascii, .direction = "down") %>%
  rowwise() %>%
  mutate(letter_first_ascii = as.integer(charToRaw(letter_first_ascii))) %>%
  ungroup() %>%
  fill(municipality_name, .direction = "down") %>%
  mutate(
    alphabet_start = letter_first_ascii < lag(letter_first_ascii) & lead(letter_first_ascii, 2) < lag(letter_first_ascii),
    county_row = case_when(
      if_all(voters_eligible:ssw, is.na) & alphabet_start & !grepl("[0-9]", municipality_name) ~ T,
      grepl("landkreis", tolower(municipality_name)) ~ T,
      grepl("^noch", tolower(municipality_name)) ~ T
    )) %>%
  relocate(county_row, letter_first_ascii, .before = everything()) %>%
  # Create col for county name
  mutate(county_name = ifelse(county_row, municipality_name, NA),
         county_name = str_trim(str_remove(county_name, "noch:|Noch:"))) %>%
  fill(county_name, .direction = "down") %>%
  relocate(county_name, .before = everything()) %>%
  filter(is.na(county_row), if_any(voters_eligible:ssw, ~ !is.na(.x))) %>%
  select(-c(county_row, letter_first_ascii, alphabet_start)) %>%
  # County var for cities
  mutate(type = ifelse(is.na(county_name), "city", "municipality"),
         county_name = ifelse(type == "city", municipality_name, county_name),
         county_name = str_remove(county_name, "landkreis|Lankreis")) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:ssw, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:ssw), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:ssw), as.numeric)) %>%
  mutate(voters_eligible = ifelse(is.na(voters_eligible), lag(voters_eligible), voters_eligible)) %>%
  filter(if_any(voters_eligible:ssw, ~ !is.na(.x))) %>%
  # Deal with wrong municipality names
  mutate(
    helper2 = voters_eligible != lag(voters_eligible),
    helper2 = replace_na(helper2, F),
    helper2 = ifelse(row_number() == 1, T, helper2),
    helper2_cum = cumsum(helper2)
  ) %>%
  group_by(helper2_cum) %>%
  mutate(across(matches("name"), ~ ifelse(row_number() != 1, NA, .x))) %>%
  fill(matches("name"), .direction = "downup") %>%
  ungroup() %>%
  mutate(
    helper = municipality_name != lag(municipality_name),
    helper = ifelse(row_number() == 1, T, helper),
    helper = cumsum(helper),
  ) %>%
  relocate(matches("helper"), .before = everything()) %>%
  group_by(helper) %>%
  mutate(across(matches("name"), ~ ifelse(row_number() > 2, NA, .x))) %>%
  filter(!is.na(municipality_name)) %>%
  fill(voters) %>%
  mutate(first_second_vote = case_when(row_number() == 1 ~ "first", row_number() == 2 ~ "second")) %>%
  ungroup() %>%
  select(-matches("helper|sheet")) %>%
  # Additional cleaning of numeric cols
  mutate(across(cdu:ssw, ~ replace_na(.x, 0))) %>%
  # Pivot
  pivot_wider(names_from = first_second_vote, values_from = votes_invalid:ssw,
              id_cols = c(county_name:voters, type)) %>%
  mutate(state_name = "Schleswig-Holstein") %>% relocate(state_name, .before = everything())

write_rds(sh61_municipalities_clean, "./output/cleaned/61_btw_sh_clean.rds")

### RLP ----

clear_environment()

xl_files_rlp61 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "61_btw_rlp", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]+(?=_[0-9]+\\.xlsx)")),
    table = as.numeric(str_extract(path, "[0-9]+(?=\\.xlsx)"))
  ) %>%
  arrange(sheet, table)

varnames <-
  c("key_municipality", "municipality_name", "election_year", "voters_eligible", "turnout",
    "first_second_vote", "votes_invalid",  "votes_valid",
    "cdu", "spd", "fdp", "drp", "dfu_bdd", "gdp", "dg")

files_clean_rlp61 <-
  future_imap(xl_files_rlp61$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

  })

files_clean_rlp61_df <-
  bind_rows(files_clean_rlp61) %>%
  `colnames<-`(varnames) %>%
  fill(key_municipality, municipality_name) %>%
  filter(!if_all(voters_eligible:dg, is.na)) %>%
  group_by(key_municipality, municipality_name) %>%
  filter(n() == 7) %>%
  mutate(
    election_year = case_when(
      row_number() %in% 1:3 ~ 1961,
      row_number() %in% 4:5 ~ 1957,
      row_number() %in% 6:7 ~ 1959
    ),
    election_type = case_when(
      row_number() %in% 1:5 ~ "federal",
      row_number() %in% 6:7 ~ "state"
    ),
    first_second_vote = case_when(
      row_number() == 1 ~ "first",
      row_number() %in% 2:5 ~ "second",
      row_number() %in% 6:7 ~ "first"
    ),
    unit = case_when(
      row_number() %in% c(1, 2, 4, 6) ~ "total",
      T ~ "share"
    ),
    voters_eligible = case_when(
      row_number() == 3 ~ NA,
      T ~ voters_eligible
    )
  ) %>%
  fill(voters_eligible, turnout) %>%
  ungroup() %>%
  relocate(election_type, unit, first_second_vote, .after = election_year) %>%
  filter(unit == "total") %>%
  select(-unit) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:dg, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:dg), ~ str_replace_multiple(.x, c("," = "\\.", "C" = "0", "-" = "0", "^\\(" = ""))),
         across(c(voters_eligible:dg), as.numeric)) %>%
  filter(if_any(voters_eligible:dg, ~ !is.na(.x))) %>%
  # Additional cleaning of numeric cols
  mutate(
    across(cdu:dg, ~ replace_na(.x, 0)),
    municipality_name = str_to_title(municipality_name),
    key_municipality = str_remove_all(key_municipality, "\\s"),
    key_district = str_sub(key_municipality, 1, 1),
    key_county = str_sub(key_municipality, 2, 3),
    key_municipality = str_sub(key_municipality, 4, 6)
  ) %>%
  relocate(matches("key"), .before = everything())

rlp61_cities <-
  read_csv("./additional_data/election_returns_cities/rlp_61_65_69_cities.csv",
           col_types = cols(key_state = col_character(), key_district = col_character(),
                            key_county = col_character(), key_municipality = col_character())) %>%
  filter(election_year == 1961) %>%
  mutate(type = "city", across(cdu:drp_npd, ~ replace_na(.x, 0))) %>%
  select(-c(aud, election_year, voters)) %>% rename(dfu = adf_dfu, drp = drp_npd)

rlp61_btw_municipalities_clean <-
  files_clean_rlp61_df %>%
  filter(election_year == 1961) %>%
  mutate(type = "municipality", key_state = "07") %>%
  select(-c(election_type, election_year)) %>%
  relocate(matches("key"), .before = everything()) %>%
  rename(dfu = dfu_bdd) %>%
  bind_rows(rlp61_cities) %>% relocate(type, county_name, .before = municipality_name) %>%
  arrange(key_district, key_county, key_municipality) %>%
  pivot_wider(names_from = first_second_vote, values_from = votes_invalid:dg) %>%
  mutate(state_name = "Rheinland-Pfalz") %>% relocate(state_name, .before = everything())

write_rds(rlp61_btw_municipalities_clean, "./output/cleaned/61_btw_rlp_clean.rds")

## BTW 1965 ----
### NDS ----

clear_environment()

btw65_nds_cities <-
  read_xlsx("./additional_data/election_returns_cities/nds_65_cities.xlsx",
            col_names = c("name", "election", "voters_eligible", "voters", "turnout",
                          "votes_valid_second",
                          "cdu", "spd", "fdp", "aud", "dfu", "fsu", "npd", "other",
                          "cdu_share", "spd_share", "fdp_share", "aud_share", "dfu_share",
                          "fsu_share", "npd_share", "other_share"),
            skip = 1) %>%
  fill(name, .direction = "down") %>%
  filter(election == "1965", grepl("Stadt", name)) %>%
  select(name, matches("vote"), cdu:other) %>%
  mutate(county_name = name, type = "city") %>%
  rename(municipality_name = name) %>%
  # Clean numeric cols
  mutate(across(voters_eligible:other, ~ str_remove_all(.x, "\\s")),
         across(c(voters_eligible:other), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0", "\\." = "", "o" = "0", "l" = "1"))),
         across(c(voters_eligible:other), as.numeric),
         across(cdu:other, ~ replace_na(.x, 0))) %>%
  mutate(other = rowSums(select(., aud, fsu))) %>%
  select(-aud, -fsu) %>%
  rename_with(~ paste0(.x, "_second"), cdu:other)


xl_files_nds65 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "65_btw_nds", full.names = T)) %>%
  mutate(
    table = str_extract(path,"(?<=[0-9]_)[0-9]*(?=\\.xlsx)"),
    sheet = str_replace(path,"(?<=[0-9])_[0-9]*(?=\\.xlsx)", ""),
    sheet = str_extract(sheet,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet, table), as.numeric)) %>%
  arrange(sheet, table)

varnames <-
  c("municipality_name", "voters_eligible", "voters", "turnout",
    "votes_valid", "cdu", "spd", "fdp", "dfu", "npd", "other", "cdu_share", "spd_share", "fdp_share")

# Read files and first cleaning
files_clean_nds65 <-
  future_imap(xl_files_nds65$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    if (ncol(file_df) == 2 | nrow(file_df) <= 6) {
      return(NULL)
    }

    file_df %<>%
      filter(if_any(2:11, ~ !is.na(.x)))

    if (ncol(file_df) == 14) {
      file_df %<>%
        `colnames<-`(varnames) %>%
        select(1:11)

    } else if (ncol(file_df) == 13) {
      file_df %<>%
        select(1:11) %>%
        `colnames<-`(varnames[1:11])
    } else if (ncol(file_df == 15)) {
      varnames <- c(varnames[1], "first_second_vote", varnames[2:14])
      file_df %<>%
        `colnames<-`(varnames) %>%
        select(1:12)
    }

    file_df %<>% mutate(sheet = basename(xl_file))

  }) %>%
  keep(~ !is.null(.x))


# Additional cleaning
files_clean_df <-
  files_clean_nds65 %>% bind_rows() %>%
  fill(municipality_name, .direction = "down") %>%
  mutate(county_row = if_any(voters_eligible:other, ~ grepl("landkreis", tolower(.x))),
         rowid = row_number())

county_names <-
  files_clean_df %>%
  filter(county_row) %>%
  unite(col = "county_name", voters_eligible:other, sep = " ", na.rm = T) %>%
  select(county_name, rowid)

counties_recode <-
  tibble::tribble(
    ~county_id, ~county_name,
    0L,           "Grafschaft Diepholz",
    3L,           "Hameln-Pyrmont",
    8L,           "Alfeld-Leine",
    11L,           "Göttingen",
    16L,           "Osterode am Harz",
    18L,           "Zellerfeld",
    19L,           "Burgdorf",
    25L,           "Gifhorn",
    28L,           "Lüneburg",
    31L,           "Bremervörde",
    38L,           "Aschendorf-Hümmling",
    40L,           "Grafschaft Bentheim",
    46L,           "Aurich",
    48L,           "Norden",
    52L,           "Blankenburg",
    53L,           "Braunschweig",
    54L,           "Gandersheim",
    55L,           "Goslar",
    57L,           "Wolfenbüttel",
    60L,           "Friesland",
    62L,           "Vechta",
    63L,           "Wesermarsch"
  )

btw65_nds_municipalities_clean <-
  left_join(files_clean_df, county_names, by = "rowid") %>%
  relocate(county_name, .before = everything()) %>%
  mutate(letter = tolower(str_sub(municipality_name, 1, 1))) %>%
  rowwise() %>%
  mutate(value = charToRaw(letter)) %>%
  ungroup() %>%
  mutate(is_after = value >= lag(value, 3),
         newcounty = is_after==F & lead(is_after) == F & lead(is_after, 2) == F,
         county_id = cumsum(newcounty),
         county_name = ifelse(lag(county_row), lag(county_name), NA)) %>%
  filter(!county_row) %>%
  select(-c(is_after, newcounty, letter, value, rowid, county_row)) %>%
  relocate(county_id, .before = everything()) %>%
  group_by(county_id) %>%
  fill(county_name, .direction = "down") %>%
  ungroup() %>%
  mutate(municipality_name = str_replace(municipality_name, "[0-9]$", "")) %>%
  group_by(municipality_name, sheet) %>%
  filter(n() == 2) %>%
  mutate(first_second_vote = ifelse(row_number() == 1, "first", "second")) %>%
  fill(voters_eligible:turnout, .direction = "down") %>%
  ungroup() %>%
  group_by(county_id, municipality_name) %>%
  mutate(across(voters_eligible:turnout, ~ ifelse(n() > 2, NA, .x))) %>%
  ungroup() %>%
  # Clean county name col
  mutate(county_id = ifelse(county_id %in% 21:23, 20, county_id),
         county_name = ifelse(county_id %in% 21:23, "Landkreis Celle", county_name)) %>%
  left_join(counties_recode, by = "county_id") %>%
  mutate(county_name = case_when(is.na(county_name.x) ~ county_name.y, T ~ county_name.x),
         county_name = str_replace(tolower(county_name), "landkreis", ""),
         across(c(county_name, municipality_name), ~ str_trim(str_to_title(.x)))) %>%
  select(-matches("county_name\\.")) %>% relocate(county_name, .before = municipality_name) %>%
  # Clean numeric cols
  mutate(
    across(c(voters_eligible:other), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0"))),
    across(c(voters_eligible:other), as.numeric),
    across(c(cdu:other), ~ replace_na(.x, 0)),
    other = NA
  ) %>%
  mutate(
    other = votes_valid - rowSums(select(., cdu:npd)),
    other = ifelse(other < 0, 0, other)
  ) %>%
  # Pivot
  select(-sheet) %>%
  group_by(pick(county_id:turnout)) %>% filter(n() == 2) %>% ungroup() %>%
  pivot_wider(names_from = first_second_vote, values_from = votes_valid:other, id_cols = county_id:turnout) %>%
  # Add cities
  mutate(type = "municipality") %>% relocate(type, .after = municipality_name) %>%
  bind_rows(btw65_nds_cities) %>%
  mutate(state_name = "Niedersachsen") %>% relocate(state_name, .before = everything()) %>% select(-county_id)

write_rds(btw65_nds_municipalities_clean, "./output/cleaned/65_btw_nds_clean.rds")

### NRW ----

clear_environment()

xl_files_nrw65 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "65_btw_nrw", full.names = T)) %>%
  mutate(sheet = str_extract(path,"[0-9]*(?=\\.xlsx)"),
         sheet = as.numeric(sheet)) %>%
  arrange(sheet)

# Left pages

xl_files_nrw65_left <- xl_files_nrw65 %>%
  filter(sheet %in% seq(1, 92, 2)) %>%
  pull(path)

varnames_left <-
  c("row_id", "entity_name", "pop", "pop_male", "catholic",
    "protestant", "pop_working", "voters_eligible", "voters", "turnout",
    "votes_valid_second", "cdu_second", "cdu_second_share")

xl_files_nrw_65_left_clean <-
  future_map(xl_files_nrw65_left, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1)

  }) %>%
  map_at(37, ~ .x %>% unite("3", 4:5, sep = "", na.rm = T) %>% mutate(`3` = ifelse(`3` == "", NA, `3`))) %>%
  future_map(function(file_df) {

    file_df_clean <-
      file_df %>%
      `colnames<-`(varnames_left) %>%
      # Filter missing data rows
      # Identify type of unit
      mutate(
        entity_name = str_replace(entity_name, "noch:", ""),
        entity_name = str_trim(entity_name),
        type = case_when(
          str_detect(tolower(entity_name), "^krfr\\.st") ~ "city",
          str_detect(entity_name, "Kreis|kreis$|kreis\\s|Ldkr|ldkr") ~ "county",
          str_detect(entity_name, "Amt") ~ "amt",
          T ~ ""
        )
      ) %>%
      relocate(type, .after = row_id) %>%
      mutate(cumsum_county = cumsum(type == "county")) %>%
      relocate(matches("cumsum"), .before = everything()) %>%
      mutate(type = case_when(
        type == "" & cumsum_county == 0 ~ "city",
        type == "" & cumsum_county != 0 ~ "municipality",
        T ~ type
      )) %>%
      select(-cumsum_county) %>%
      mutate(county_name = case_when(type == "county" | type == "city" ~ entity_name)) %>%
      relocate(county_name, .before = entity_name) %>%
      fill(county_name, .direction = "down")  %>%
      mutate(across(c(county_name, entity_name), ~ str_replace(.x, "^Krfr\\.St|^Kreis|Amt|Ldkr\\.", "") %>% str_trim())) %>%
      mutate(across(c(county_name, entity_name), ~ str_replace(.x, "(^[[:punct:]]\\s)|(\\s[[:punct:]]$)|(\\s[[:punct:]]\\s)", ""))) %>%
      filter(!if_all(pop:cdu_second_share, ~ is.na(.x))) %>%
      mutate(across(c(pop:cdu_second_share), ~ str_replace_multiple(.x, c("," = "\\.", "-" = "0", "\\s" = ""))),
             across(c(pop:cdu_second_share),as.numeric))


  })


# Right pages

xl_files_nrw65_right <- xl_files_nrw65 %>%
  filter(sheet %in% seq(2, 92, 2)) %>% pull(path)

varnames_right <- c("spd_second", "spd_second_share", "fdp_second",
                    "fdp_second_share", "aud_second", "aud_second_share",
                    "cvp_second", "cvp_second_share", "dfu_second", "dfu_second_share",
                    "fsu_second", "fsu_second_share", "npd_second", "npd_second_share",
                    "uap_second", "uap_second_share", "row_id")


xl_files_nrw65_right_clean <-
  future_map(xl_files_nrw65_right, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T) %>%
      select(-1)

  }) %>%
  map_at(20, ~ unite(.x, "X15", 16:18, sep = "", na.rm = T)) %>%
  map(function(file_df) {

    file_df_clean <-
      file_df %>%
      `colnames<-`(varnames_right) %>%
      mutate(across(matches("share|total"), ~ str_replace_multiple(.x, c("," = "\\.", "-" = "0", "\\s" = ""))),
             across(matches("share|total"), as.numeric)) %>%
      filter(!if_all(matches("share|total"), is.na))

  }) %>%
  map(~ drop_na(.x, row_id))

nrw65_clean_df <-
  map2(xl_files_nrw_65_left_clean, xl_files_nrw65_right_clean, ~ left_join(.x, .y  %>% mutate(joined = T), by = "row_id")) %>%
  bind_rows()

btw65_nrw_municipalities_clean <-
  nrw65_clean_df %>%
  filter(type == "municipality" | type == "city") %>%
  mutate(across(cdu_second:uap_second_share, as.numeric)) %>%
  # Caluclate flags based on reported totals and shares
  mutate(across(cdu_second:uap_second_share, ~ case_when(is.na(.x) & joined ~ 0, is.na(joined) ~ NA, T ~ .x))) %>%
  mutate(flag_share = abs(100- rowSums(select(., matches("second_share"))))) %>%
  relocate(matches("second_share"), .after = uap_second) %>%
  mutate(across(cdu_second:uap_second, ~ .x/votes_valid_second*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("second_share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  # If totals seem to contain errors, calculate totals based on shares
  mutate(across(cdu_second_share:uap_second_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid_second), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu_second = ifelse(flag_total>2, cdu_second_share_replace, cdu_second),
    spd_second = ifelse(flag_total>2, spd_second_share_replace, spd_second),
    fdp_second = ifelse(flag_total>2, fdp_second_share_replace, fdp_second),
    aud_second = ifelse(flag_total>2, aud_second_share_replace, aud_second),
    cvp_second = ifelse(flag_total>2, cvp_second_share_replace, cvp_second),
    dfu_second = ifelse(flag_total>2, dfu_second_share_replace, dfu_second),
    fsu_second = ifelse(flag_total>2, fsu_second_share_replace, fsu_second),
    npd_second = ifelse(flag_total>2, npd_second_share_replace, npd_second),
    uap_second = ifelse(flag_total>2, uap_second_share_replace, uap_second)
  ) %>%
  select(-matches("flag|share"), -row_id, -c(pop:pop_working)) %>%
  rename(municipality_name = entity_name) %>%
  mutate(state_name = "Nordrhein-Westfalen",
         across(c(county_name, municipality_name), ~ ifelse(type == "city", paste0(.x, ", Stadt"), .x))) %>% relocate(state_name, .before = everything()) %>%
  filter(joined) %>% select(-joined)

write_rds(btw65_nrw_municipalities_clean, "./output/cleaned/65_btw_nrw_clean.rds")

### SH ----

clear_environment()

xl_files_sh65 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "65_btw_sh", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]*(?=.xlsx)"))) %>%
  arrange(sheet)

varnames <-
  c("municipality_name", "voters_eligible", "voters",
    "votes_invalid", "votes_valid",
    "cdu", "spd", "fdp", "aud", "dfu", "npd", "fsu")

files_clean_sh65 <-
  future_imap(xl_files_sh65$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1) %>%
      select(1:12) %>%
      `colnames<-`(varnames) %>%
      mutate(sheet = basename(xl_file))

  })

sh65_municipalities_clean <-
  bind_rows(files_clean_sh65) %>%
  mutate(
    key_municipality = str_extract(municipality_name, "^[0-9]*"),
    municipality_name = str_replace(municipality_name, "^[0-9]*", ""),
    across(matches("municipality"), str_trim)
  ) %>%
  relocate(key_municipality, .before = everything()) %>%
  # Identify county rows
  rowwise() %>%
  mutate(
    county_row = case_when(
      if_all(c(voters_eligible:fsu), is.na) & if_all(c(matches("municipality")), ~ !is.na(.x)) & municipality_name != "" ~ T,
      T ~ F
    )
  ) %>%
  ungroup() %>%
  # Create county variables
  mutate(county_name = ifelse(county_row, municipality_name, NA),
         key_county = ifelse(county_row, key_municipality, NA),
         county_name = str_trim(str_replace(county_name, "NOCH:", ""))) %>%
  relocate(c(county_row, county_name, key_county), .before = everything()) %>%
  mutate(key_county = ifelse(key_county == "", NA, key_county)) %>%
  fill(county_name:municipality_name, .direction = "down") %>%
  filter((!county_row & !(municipality_name == county_name)) | is.na(county_name)) %>%
  select(-county_row) %>%
  # remove invalid rows
  group_by(key_municipality, municipality_name) %>%
  filter(n() == 3) %>%
  mutate(first_second_vote = case_when(row_number() == 1 ~ "first", row_number() == 2 ~ "second",
                                       row_number() == 3 ~ "second"),
         unit = case_when(row_number() %in% 1:2 ~ "total", T ~ "share")) %>%
  relocate(unit, first_second_vote, .after = municipality_name) %>%
  fill(voters_eligible, voters, .direction = "down") %>%
  ungroup() %>%
  # Clean numeric columns
  mutate(
    across(c(voters_eligible:fsu), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0"))),
    across(c(voters_eligible:fsu), as.numeric),
  ) %>%
  mutate(type = ifelse(municipality_name %in% c("FLENSBURG-STADT", "KIEL", "LUBECK"), "city", "municipality"),
         county_name = ifelse(type == "city", municipality_name, county_name),
         key_county = ifelse(type == "city", key_municipality, key_county),
         key_municipality = ifelse(type == "city", "000", key_municipality),
         key_district = "1"
  ) %>%
  relocate(key_district, .before = key_county) %>%
  pivot_wider(names_from = unit, values_from = c(voters:fsu)) %>%
  rename(turnout = voters_share) %>% select(-matches("valid_share"))

sh65_municipalities_clean_second <-
  sh65_municipalities_clean %>%
  filter(first_second_vote == "second") %>%
  rename_with(~ str_remove(.x, "_total")) %>%
  relocate(matches("share"), .after = fsu) %>%
  # Caluclate flags based on reported totals and shares
  mutate(flag_share = abs(100- rowSums(select(., matches("share"))))) %>%
  mutate(across(cdu:fsu, ~ .x/votes_valid*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  # If totals seem to contain errors, calculate totals based on shares
  mutate(across(cdu_share:fsu_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu = ifelse(flag_total>2 | is.na(flag_total), cdu_share_replace, cdu),
    spd = ifelse(flag_total>2 | is.na(flag_total), spd_share_replace, spd),
    fdp = ifelse(flag_total>2 | is.na(flag_total), fdp_share_replace, fdp),
    aud = ifelse(flag_total>2 | is.na(flag_total), aud_share_replace, aud),

    dfu = ifelse(flag_total>2 | is.na(flag_total), dfu_share_replace, dfu),
    fsu = ifelse(flag_total>2 | is.na(flag_total), fsu_share_replace, fsu),
    npd = ifelse(flag_total>2 | is.na(flag_total), npd_share_replace, npd),
  ) %>%
  select(-matches("flag|share"), -turnout)


sh65_municipalities_clean_comb <-
  filter(sh65_municipalities_clean, first_second_vote == "first") %>%
  select(-c(turnout, matches("share"))) %>%
  rename_with(~ str_remove(.x, "_total")) %>%
  bind_rows(sh65_municipalities_clean_second) %>%
  select(-sheet) %>%
  pivot_wider(names_from = first_second_vote, values_from = votes_invalid:fsu, id_cols = c(county_name:municipality_name, voters_eligible:voters)) %>%
  mutate(across(matches("name"), str_to_title),
         across(cdu_first:fsu_second, ~ replace_na(.x, 0))) %>%
  mutate(county_name = case_when(
    county_name == "Eckernföroe" ~ "Eckernforde",
    county_name == "Hzgt, Lauenburg" ~ "Hzgt. Lauenburg",
    county_name == "Norderoithmarschen" ~ "Norderdithmarschen",
    county_name == "Plon" ~ "Plön",
    county_name == "Suderdithmarschen" | county_name == "Uderdithmarschen" ~ "Süderdithmarschen",
    county_name == "Sudtondern" ~ "Südtondern",
    T ~ county_name
  )) %>%
  mutate(state_name = "Schleswig-Holstein") %>% relocate(state_name, .before = everything())

write_rds(sh65_municipalities_clean_comb, "./output/cleaned/65_btw_sh_clean.rds")

## BTW 1969 ----
### NDS ----

clear_environment()

btw69_nds_cities <- read_rds("./additional_data/county_data_bundeswahlleiter/btw_1969_counties.rds") %>%
  filter(type == "city", key_state == "03") %>%
  select(county_name:other, type, -csu, -votes_invalid) %>%
  rename_with(~ paste0(.x, "_second"), votes_valid:other) %>%
  mutate(municipality_name = county_name)

xl_files_nds69 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "69_btw_nds", full.names = T)) %>%
  mutate(
    table = str_extract(path,"(?<=[0-9]_)[0-9]*(?=\\.xlsx)"),
    sheet = str_replace(path,"(?<=[0-9])_[0-9]*(?=\\.xlsx)", ""),
    sheet = str_extract(sheet,"[0-9]*(?=\\.xlsx)")) %>%
  mutate(across(c(sheet, table), as.numeric)) %>%
  arrange(sheet, table)

varnames <-
  c("municipality_name", "voters_eligible", "voters", "turnout",
    "votes_valid", "cdu", "spd", "fdp", "adf", "ep", "fsu", "gpd", "npd", "other")

# Read files and first cleaning
files_clean_nds69 <-
  future_imap(xl_files_nds69$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T, col_types = "text") %>%
      select(-1)

    idx_placecol <-
      summarise(file_df, across(everything(), calculate_letter_share)) %>%
      which.max() %>% as.numeric()

    file_df %<>% select(-c(1:idx_placecol-1))

    if(ncol(file_df) == 13) {file_df %<>% mutate(other = NA)}

    colnames(file_df) <- varnames

    file_df %<>% mutate(sheet = basename(xl_file))

    if(ncol(file_df == 15)) {return(file_df)} else {return(NULL)}

  })

# Additional cleaning
btw69_nds_municipalities_clean <-
  bind_rows(files_clean_nds69) %>%
  # Create county identifier
  unite("county_name", municipality_name, voters_eligible, sep = " ", remove = F, na.rm = T) %>%
  mutate(county_name = ifelse(grepl("kreis", municipality_name), county_name, NA),
         county_name = str_trim(str_remove(county_name, "Landkreis"))) %>%
  fill(county_name, .direction = "down") %>%
  filter(!grepl("kreis", municipality_name), !if_all(cdu:gpd, is.na)) %>%
  # Identify rows from the same municipality
  mutate(help1 = voters_eligible == lead(voters_eligible),
         help2 = voters == lead(voters),
         help3 = turnout == lead(turnout),
         across(matches("help"), ~ replace_na(.x, F))) %>%
  mutate(helper = rowSums(select(., help1:help3)),
         helper = cumsum(helper >=2 & lead(helper < 3))) %>%
  group_by(sheet, helper) %>%
  filter(n() == 2) %>%
  mutate(municipality_name = paste(municipality_name, collapse = "")) %>%
  ungroup() %>%
  mutate(municipality_name = str_trim(str_to_title(str_remove_all(municipality_name, "[0-9]")))) %>%
  rename(municipality_id = helper) %>%
  relocate(municipality_id, .after = municipality_name) %>%
  select(-c(help1:help3)) %>%
  # Numeric columns
  group_by(municipality_id) %>%
  mutate(first_second_vote = ifelse(row_number() == 1, "first", "second")) %>%
  ungroup() %>%
  mutate(across(c(voters_eligible:other), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0"))),
         across(c(voters_eligible:other), as.numeric),
         across(c(cdu:other), ~ replace_na(.x, 0))) %>%
  mutate(other = rowSums(select(., ep, fsu, gpd))) %>%
  select(-c(ep, fsu, gpd)) %>%
  fill(voters_eligible, .direction = "downup") %>%
  # Pivot
  pivot_wider(values_from = votes_valid:other, names_from = first_second_vote) %>%
  # Add cities
  mutate(type = "municipality") %>%
  bind_rows(btw69_nds_cities) %>%
  select(-c(municipality_id, sheet)) %>%
  mutate(state_name = "Niedersachsen") %>%
  relocate(state_name, .before = everything()) %>%
  relocate(type, .after = municipality_name) %>%
  # Göttingen
  mutate(county_name = ifelse(municipality_name == "Goettingen Stadt", "Göttingen, Stadt", county_name))

write_rds(btw69_nds_municipalities_clean, "./output/cleaned/69_btw_nds_clean.rds")

### NRW ----

clear_environment()

xl_files_nrw69 <-
  tibble(path = list.files("./output/extracted_tables/", pattern = "69_btw_nrw", full.names = T)) %>%
  mutate(sheet = str_extract(path,"[0-9]*(?=\\.xlsx)"),
         sheet = as.numeric(sheet)) %>%
  arrange(sheet)

# Left pages

xl_files_nrw69_left <- xl_files_nrw69 %>%
  filter(sheet %in% seq(1, 58, 2)) %>%
  pull(path)

varnames_left <-
  c("row_id", "entity_name", "voters_eligible", "voters", "turnout",
    "votes_valid_second", "spd_second", "spd_second_share", "cdu_second", "cdu_second_share")

xl_files_nrw_69_left_clean <-
  future_map(xl_files_nrw69_left, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T, col_types = c("text")) %>%
      select(-1)

    file_df_clean <-
      file_df %>%
      `colnames<-`(varnames_left) %>%
      # Filter missing data rows
      # Identify type of unit
      mutate(
        entity_name = str_replace(entity_name, "noch:", ""),
        entity_name = str_trim(entity_name),
        type = case_when(
          str_detect(tolower(entity_name), "^krfr\\.st") ~ "city",
          str_detect(entity_name, "Kreis|kreis$|kreis\\s") ~ "county",
          str_detect(entity_name, "Amt") ~ "amt",
          T ~ ""
        )
      ) %>%
      relocate(type, .after = row_id) %>%
      mutate(cumsum_county = cumsum(type == "county")) %>%
      relocate(matches("cumsum"), .before = everything()) %>%
      mutate(type = case_when(
        type == "" & cumsum_county == 0 ~ "city",
        type == "" & cumsum_county != 0 ~ "municipality",
        T ~ type
      )) %>%
      select(-cumsum_county) %>%
      mutate(county_name = case_when(type == "county" | type == "city" ~ entity_name)) %>%
      relocate(county_name, .before = entity_name) %>%
      fill(county_name, .direction = "down") %>%
      mutate(across(c(county_name, entity_name), ~ str_replace(.x, "^Krfr\\.St|^Kreis|Amt", "") %>% str_trim())) %>%
      mutate(across(c(county_name, entity_name), ~ str_replace(.x, "(^[[:punct:]]\\s)|(\\s[[:punct:]]$)|(\\s[[:punct:]]\\s)", ""))) %>%
      filter(!if_all(voters_eligible:cdu_second_share, ~ is.na(.x))) %>%
      mutate(across(c(voters_eligible:cdu_second_share), ~ str_replace_multiple(.x, c("," = "\\.", "-" = "0", "\\s" = ""))),
             across(c(voters_eligible:cdu_second_share),as.numeric),
             sheet = basename(file))

    file_df_clean

  }) %>%
  future_map(~ drop_na(.x, row_id))

# Right pages

xl_files_nrw69_right <- xl_files_nrw69 %>%
  filter(sheet %in% seq(2, 58, 2)) %>% pull(path)

varnames_right <- c("fdp_second", "fdp_second_share", "adf_second", "adf_second_share",
                    "center_second", "center_second_share", "ep_second", "ep_second_share",
                    "fsu_second", "fsu_second_share", "gpd_second", "gpd_second_share",
                    "npd_second", "npd_second_share", "uap_second", "uap_second_share", "row_id")

xl_files_nrw69_right_clean <-
  future_map(xl_files_nrw69_right, function(file){

    file_df <-
      read_xlsx(file, sheet = 1, trim_ws = T, col_types = c("text")) %>%
      select(-1)

    file_df_clean <-
      file_df %>%
      `colnames<-`(varnames_right) %>%
      mutate(across(matches("share|total"), ~ str_replace_multiple(.x, c("," = "\\.", "-" = "0", "\\s" = ""))),
             across(matches("share|total"), as.numeric)) %>%
      filter(!if_all(matches("share|total"), is.na)) %>%
      mutate(sheet = basename(file))

    file_df_clean
  }) %>%
  map(~ drop_na(.x, row_id))

nrw69_clean_df <-
  map2(xl_files_nrw_69_left_clean, xl_files_nrw69_right_clean, ~ left_join(.x, .y, by = "row_id")) %>%
  bind_rows() %>%
  relocate(matches("sheet"), .after = everything()) %>%
  relocate(matches("second_share"), .after = uap_second) %>%
  mutate(
    across(c(spd_second:uap_second), ~ str_replace_multiple(.x, c("C" = "0", "-" = "0", "\\s" = ""))),
    across(c(spd_second:uap_second), as.numeric),
    across(spd_second:uap_second_share, ~ replace_na(.x, 0))
  ) %>%
  # Caluclate flags based on reported totals and shares
  mutate(flag_share = abs(100- rowSums(select(., matches("share"))))) %>%
  mutate(across(spd_second:uap_second, ~ .x/votes_valid_second*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  # If totals seem to contain errors, calculate totals based on shares
  mutate(across(spd_second_share:uap_second_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid_second), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu_second = ifelse(flag_total>2 | is.na(flag_total), cdu_second_share_replace, cdu_second),
    spd_second = ifelse(flag_total>2 | is.na(flag_total), spd_second_share_replace, spd_second),
    fdp_second = ifelse(flag_total>2 | is.na(flag_total), fdp_second_share_replace, fdp_second),
    adf_second = ifelse(flag_total>2 | is.na(flag_total), adf_second_share_replace, adf_second),
    center_second = ifelse(flag_total>2 | is.na(flag_total), center_second_share_replace, center_second),
    ep_second = ifelse(flag_total>2 | is.na(flag_total), ep_second_share_replace, ep_second),
    fsu_second = ifelse(flag_total>2 | is.na(flag_total), fsu_second_share_replace, fsu_second),
    gpd_second = ifelse(flag_total>2 | is.na(flag_total), gpd_second_share_replace, gpd_second),
    npd_second = ifelse(flag_total>2 | is.na(flag_total), npd_second_share_replace, npd_second),
    uap_second = ifelse(flag_total>2 | is.na(flag_total), uap_second_share_replace, uap_second),
  ) %>%
  select(-matches("flag|share|row_id"))

nrw69_municipalities_clean <-
  nrw69_clean_df %>%
  filter(type == "municipality" | type == "city") %>%
  select(-matches("sheet")) %>%
  rename(municipality_name = entity_name) %>%
  mutate(state_name = "Nordrhein-Westfalen",
         across(c(county_name, municipality_name), ~ ifelse(type == "city", paste0(.x, ", Stadt"), .x))) %>%
  relocate(state_name, .before = everything())

write_rds(nrw69_municipalities_clean, "./output/cleaned/69_btw_nrw_clean.rds")

### SH ----

clear_environment()

xl_files_sh69 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "69_btw_sh", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]*(?=.xlsx)"))) %>%
  arrange(sheet)

varnames <-
  c("municipality_name", "voters_eligible", "voters",
    "votes_invalid", "votes_valid",
    "cdu", "spd", "fdp", "adf", "ep", "npd", "fsu_gpd")

files_clean_sh69 <-
  future_imap(xl_files_sh69$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    file_df %>%
      `colnames<-`(varnames) %>%
      mutate(
        key_municipality = str_extract(municipality_name, "^[0-9]*"),
        municipality_name = str_replace(municipality_name, "^[0-9]*", ""),
        across(matches("municipality"), str_trim)
      ) %>%
      relocate(key_municipality, .before = everything()) %>%
      # Identify county rows
      rowwise() %>%
      mutate(
        county_row = case_when(
          if_all(c(voters_eligible:fsu_gpd), ~ is.na(.x)) & if_all(c(matches("municipality")), ~ !is.na(.x)) ~ T,
          T ~ F
        )
      ) %>%
      ungroup() %>%
      # Create county variables
      mutate(county_name = ifelse(county_row, municipality_name, NA),
             county_name = str_trim(str_replace(county_name, "NOCH:", ""))) %>%
      relocate(c(county_row, county_name), .before = everything()) %>%
      fill(county_name:municipality_name, .direction = "down") %>%
      filter((!county_row & !(municipality_name == county_name)) | is.na(county_name)) %>%
      select(-county_row) %>%
      # remove invalid rows
      group_by(key_municipality, municipality_name) %>%
      filter(n() == 3) %>%
      mutate(first_second_vote = case_when(row_number() == 1 ~ "first", row_number() == 2 ~ "second",
                                           row_number() == 3 ~ "second"),
             unit = case_when(row_number() %in% 1:2 ~ "total", T ~ "share")) %>%
      relocate(unit, first_second_vote, .after = municipality_name) %>%
      fill(voters_eligible, voters, .direction = "down") %>%
      ungroup() %>%
      # Clean numeric columns
      mutate(
        across(c(voters_eligible:fsu_gpd), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0"))),
        across(c(voters_eligible:fsu_gpd), as.numeric),
      ) %>%
      mutate(sheet = basename(xl_file))

  })


sh69_municipalities_clean <-
  bind_rows(files_clean_sh69) %>%
  mutate(county_name = case_when(
    municipality_name %in% c("FLENSBURG-STADT", "KIEL", "LOBECK", "NEUMONSTER") ~ municipality_name,
    T ~ county_name),
    type =  case_when(
      municipality_name %in% c("FLENSBURG-STADT", "KIEL", "LOBECK", "NEUMONSTER") ~ "city",
      T ~ "municipality")) %>%
  fill(county_name, .direction = "down") %>%
  pivot_wider(names_from = unit, values_from = voters:fsu_gpd) %>%
  rename(turnout = voters_share) %>% rename_with(~ str_remove(.x, "_total")) %>%
  relocate(matches("share"), .after = fsu_gpd) %>% select(-matches("valid_share")) %>%
  # Caluclate flags based on reported totals and shares
  mutate(flag_share = abs(100- rowSums(select(., matches("share"))))) %>%
  mutate(across(cdu:fsu_gpd, ~ .x/votes_valid*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  # If totals seem to contain errors, calculate totals based on shares
  mutate(across(cdu_share:fsu_gpd_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu = ifelse(flag_total>2 | is.na(flag_total), cdu_share_replace, cdu),
    spd = ifelse(flag_total>2 | is.na(flag_total), spd_share_replace, spd),
    fdp = ifelse(flag_total>2 | is.na(flag_total), fdp_share_replace, fdp),
    adf = ifelse(flag_total>2 | is.na(flag_total), adf_share_replace, adf),
    ep = ifelse(flag_total>2 | is.na(flag_total), ep_share_replace, ep),
    npd = ifelse(flag_total>2 | is.na(flag_total), npd_share_replace, npd),
    fsu_gpd = ifelse(flag_total>2 | is.na(flag_total), fsu_gpd_share_replace, fsu_gpd),
  ) %>%
  mutate(across(cdu:fsu_gpd, ~ replace_na(.x, 0))) %>%
  select(-matches("flag|share"), -c(sheet, turnout)) %>%
  mutate(across(matches("name"), str_to_title)) %>%
  pivot_wider(names_from = first_second_vote, values_from = votes_invalid:fsu_gpd) %>%
  rename(fsu_first = fsu_gpd_first, gpd_second = fsu_gpd_second) %>%
  mutate(state_name = "Schleswig-Holstein") %>% relocate(state_name, .before = everything())

write_rds(sh69_municipalities_clean, "./output/cleaned/69_btw_sh_clean.rds")

### SA ----

clear_environment()

xl_files_sa6169 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "69_btw_sa", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]*(?=.xlsx)"))) %>%
  arrange(sheet)

xl_files_sa6169_totals <-
  filter(xl_files_sa6169, sheet %% 2 != 0)

xl_files_sa6169_shares <-
  filter(xl_files_sa6169, sheet %% 2 == 0)

varnames <-
  c("municipality_name", "election_type", "election_year",
    "voters_eligible", "voters", "votes_valid_second",
    "cdu_second", "spd_second", "fdp_second", "adf_second", "center_second", "ep_second",
    "npd_second", "other_second")

counties <- c("H0mburg", "Merzig = Wadern", "0 T W E I L R", "Saarbruecken",
              "Saarlouis", "St. Ingbert", "St. Wendel") %>%
  map_vec(toupper)

files_clean_sa6169 <-
  map(list(xl_files_sa6169_totals$path, xl_files_sa6169_shares$path), function(x) {
    future_imap(x, function(xl_file, idx) {

      file_df <-
        read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
        select(-1) %>%
        filter(rowSums(!is.na(.)) > 2)

      if (all(is.na(file_df[[2]]))) {
        file_df %<>% select(-2)
      }

      # Identify B columns
      position_b_col <-
        file_df %>%
        summarise(across(everything(), ~ sum(grepl("^B", .x), na.rm = T))) %>%
        which.max() %>%
        as.numeric()

      # Paste all columns together before b col

      if (position_b_col > 2) {
        file_df %<>% unite(col = municipality_name, seq(1, position_b_col-1), sep = "")
      } else {
        file_df %<>% rename(municipality_name = 1)
      }

      # Check if B col is merged with year col

      b_col_merged <- file_df %>% summarise(across(all_of(2), calculate_letter_share)) < .8

      if (b_col_merged) {
        file_df %<>%
          mutate(election_type = str_extract(`2`, "^[:alpha:]"),
                 election_year = str_extract(`2`, "65|69|61")) %>%
          select(-2) %>%
          relocate(matches("election"), .after = municipality_name)
      } else{
        file_df %<>%
          rename(election_type = 2, election_year = 3)
      }

      file_df %<>%
        fill(municipality_name, .direction = "down") %>%
        group_by(municipality_name) %>%
        filter(!(municipality_name == "SAARBRUECKEN STADT" & row_number() > 3)) %>%
        filter(n() == 3 ) %>%
        mutate(election_type = "B",
               election_year = case_when(
                 row_number() == 1 ~ 1969,
                 row_number() == 2 ~ 1965,
                 row_number() == 3 ~ 1961
               )) %>%
        ungroup()

      if (ncol(file_df) == 13) {

        file_df %<>%
          `colnames<-`(varnames) %>%
          mutate(other_second = NA)

      } else{

        file_df %<>%
          select(all_of(1:14)) %>%
          `colnames<-`(varnames)
      }

      file_df %>%
        # Clean numeric columns
        mutate(
          across(c(election_year:other_second), ~ str_replace_multiple(.x, c("," = ".", "C" = "0", "-" = "0"))),
          across(c(election_year:other_second), as.numeric),
        ) %>%
        mutate(sheet = basename(xl_file))
    })

  })

sa6169_municipalities_totals_clean <-
  bind_rows(files_clean_sa6169[[1]]) %>%
  select(-c(election_type))

sa6169_municipalities_shares_clean <-
  bind_rows(files_clean_sa6169[[2]]) %>%
  select(-c(election_type))

sa6169_municipalities_comb_clean <-
  sa6169_municipalities_totals_clean %>%
  bind_cols(sa6169_municipalities_shares_clean %>% select(cdu_second:other_second) %>% rename_with(~ paste0(.x, "_share"))) %>%
  mutate(county_name = ifelse(municipality_name %in% counties | municipality_name == "SAARBRUECKEN STADT",
                              municipality_name, NA)) %>%
  fill(county_name, .direction = "up") %>%
  relocate(matches("share"), .after = other_second) %>%
  mutate(across(cdu_second:other_second_share, ~ replace_na(.x, 0))) %>%
  # Caluclate flags based on reported totals and shares
  mutate(flag_share = abs(100- rowSums(select(., matches("share"))))) %>%
  mutate(across(cdu_second:other_second, ~ .x/votes_valid_second*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  # If totals seem to contain errors, calculate totals based on shares
  mutate(across(cdu_second_share:other_second_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid_second), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu_second = ifelse(flag_total>2 | is.na(flag_total), cdu_second_share_replace, cdu_second),
    spd_second = ifelse(flag_total>2 | is.na(flag_total), spd_second_share_replace, spd_second),
    fdp_second = ifelse(flag_total>2 | is.na(flag_total), fdp_second_share_replace, fdp_second),
    adf_second = ifelse(flag_total>2 | is.na(flag_total), adf_second_share_replace, adf_second),
    ep_second = ifelse(flag_total>2 | is.na(flag_total), ep_second_share_replace, ep_second),
    npd_second = ifelse(flag_total>2 | is.na(flag_total), npd_second_share_replace, npd_second),
    center_second = ifelse(flag_total>2 | is.na(flag_total), center_second_share_replace, center_second),
    other_second = ifelse(flag_total>2 | is.na(flag_total), other_second_share_replace, other_second),
  ) %>%
  select(-matches("flag|share"), -sheet) %>%
  mutate(across(matches("name"), str_to_title), state_name = "Saarland",
         type = ifelse(municipality_name =="Saarbruecken Stadt", "city", "municipality"),
         county_name = case_when(county_name == "H0mburg" ~ "Homburg",
                                 county_name == "Merzig = Wadern" ~ "Merzig-Wadern",
                                 grepl("0 T W E I L R", county_name) ~ "Ottweiler",
                                 T ~ county_name)) %>%
  relocate(matches("name"), .before = everything())


sa61_municipalities_clean <- sa6169_municipalities_comb_clean %>%
  filter(election_year == 1961) %>%
  select(-c(election_year, adf_second:npd_second))

sa65_municipalities_clean <- sa6169_municipalities_comb_clean %>%
  filter(election_year == 1965) %>%
  select(-c(election_year, adf_second:ep_second))

sa69_municipalities_clean <- sa6169_municipalities_comb_clean %>%
  filter(election_year == 1969) %>%
  select(-election_year)

write_rds(sa61_municipalities_clean, "./output/cleaned/61_btw_sa_clean.rds")
write_rds(sa65_municipalities_clean, "./output/cleaned/65_btw_sa_clean.rds")
write_rds(sa69_municipalities_clean, "./output/cleaned/69_btw_sa_clean.rds")

### RLP ----

clear_environment()

xl_files_rlp69 <-
  tibble(
    path = list.files("./output/extracted_tables/", pattern = "69_btw_rlp", full.names = T) %>% sort(),
    sheet = as.numeric(str_extract(path, "[0-9]*(?=.xlsx)"))) %>%
  arrange(sheet)

varnames <-
  c("key_municipality", "municipality_name", "election_year", "voters_eligible", "voters", "turnout",
    "votes_invalid_share", "votes_valid",  "cdu", "cdu_share", "spd", "spd_share",
    "fdp", "fdp_share", "adf", "adf_share", "npd", "npd_share")

files_clean_rlp69 <-
  future_imap(xl_files_rlp69$path, function(xl_file, idx) {

    file_df <-
      read_xlsx(xl_file, sheet = 1, trim_ws = T) %>%
      select(-1)

    # Get position of placecol
    idx_placecol <-
      summarise(file_df, across(everything(), calculate_letter_share)) %>%
      which.max() %>% as.numeric()

    # Concatenate together id columns if separate
    if (idx_placecol > 1) {
      file_df %<>%
        unite("key_municipality", seq(1, idx_placecol-1), sep = "", remove = T)
    }

    if (ncol(file_df) == 18) {

      file_df %<>% `colnames<-`(varnames)

    } else {

      file_df %<>% `colnames<-`(varnames[2:18])

    }

    if (!"key_municipality" %in% names(file_df)) {

      file_df %<>%
        mutate(municipality_name = str_replace_all(municipality_name, "(^[[:punct:]]\\s)|(\\s[[:punct:]]$)|(\\s[[:punct:]]\\s)", ""),
               municipality_name = str_replace_all(municipality_name, "\\(|\\)", ""),
               municipality_name = str_trim(municipality_name),
               municipality_name = ifelse(str_length(municipality_name) == 1, NA, municipality_name),
               key_municipality = str_extract(municipality_name, "^[0-9]*"),
               municipality_name = str_replace(municipality_name, "^[0-9]*", ""),
               municipality_name = str_trim(municipality_name),
        ) %>%
        relocate(key_municipality, .before = everything())

    }


    file_df %>%
      mutate(sheet = basename(xl_file))

  })


county_names <-
  tibble(county_id = 0:27,
         county_name = c("Ahrweiler", "Altenkrichen (Westerwald)", "Bad Kreuznach", "Birkenfeld",
                         "Cochem-Zell", "Koblenz", "Mayen", "Neuwied", "Oberwesterwaldkreis",
                         "Rhein-Hunsrück-Kreis", "Rhein-Lahn-Kreis", "Unterwesterwaldkreis",
                         "Bernkastel-Wittlich", "Bitburg", "Daun", "Prüm", "Trier-Saarburg",
                         "Alzey-Worms", "Bad Dürkheim", "Donnersbergkreis", "Gemersheim",
                         "Kaiserslautern", "Kusel", "Landau-Bad Bergzabern", "Ludwigshafen",
                         "Mainz-Bingen", "Pirmasens", "Zweibrücken"))

rlp69_municipalities_clean <-
  bind_rows(files_clean_rlp69) %>%
  mutate(helper = cumsum(ifelse(is.na(election_year), 0, election_year) == "1969"),
         key_municipality = ifelse(key_municipality %in% c("NA", "NANA"), NA, key_municipality)) %>%
  relocate(helper, .before = everything()) %>%
  group_by(helper) %>%
  fill(key_municipality, municipality_name, .direction = "downup") %>%
  ungroup() %>%
  select(-helper) %>%
  filter(rowSums(is.na(.)) < 15) %>%
  group_by(key_municipality, municipality_name) %>%
  filter(n() == 4) %>%
  mutate(across(c(election_year:turnout), ~ ifelse(is.na(.x) & row_number() == 2, lag(.x), .x)),
         first_second_vote = case_when(
           row_number() == 1 & election_year == 1969 ~ "first",
           row_number() == 2 & election_year == 1969 ~ "second",
           row_number() == 3 & election_year == 1965 ~ "second",
           row_number() == 4 & election_year == 1967 ~ "second"
         )) %>%
  filter(election_year %in% c(1965, 1969)) %>%
  relocate(first_second_vote, .after = election_year) %>%
  ungroup() %>%
  # Clean numeric columns
  mutate(
    across(c(key_municipality, voters_eligible:npd_share), ~ str_replace_all(.x, "C", "0")),
    across(c(key_municipality, voters_eligible:npd_share), ~ str_replace_all(.x, ",", ".")),
    across(c(key_municipality, voters_eligible:npd_share), as.numeric),
    across(cdu:npd_share, ~ replace_na(.x, 0))
  ) %>%
  # pivot_wider(names_from = first_second_vote, values_from = votes_invalid_share:npd_share) %>%
  # Identify counties
  mutate( letter_first_ascii = str_extract(municipality_name, "^[:alpha:]")) %>%
  mutate(county_id = letter_first_ascii < lag(letter_first_ascii, 3) & !(municipality_name %in% c("GUTSBEZ.BAUMHOLDER", "HUERZWEILER",
                                                                                                  "SCHNEUDORF", "HALLENBORN", "STEINBACH AM GLAN",
                                                                                                  "HAHNWEGEN", "BERKOTH", "ELENBERG", "EPPENROD",
                                                                                                  "DELSBERG", "ALSENBACH", "NOLICHHOFEN"))) %>%
  group_by(sheet, municipality_name, key_municipality) %>%
  mutate(county_id = ifelse(row_number() == 1, county_id, NA),
         county_id = replace_na(county_id, F)) %>%
  ungroup() %>%
  mutate(county_id = cumsum(county_id)) %>%
  left_join(county_names, by = c("county_id")) %>%
  select(-letter_first_ascii) %>% relocate(county_name, county_id, .before = everything()) %>%
  mutate(municipality_name = str_to_title(municipality_name)) %>%
  # Caluclate flags based on reported totals and shares
  relocate(matches("share"), .after = npd) %>% select(-votes_invalid_share) %>%
  mutate(flag_share = abs(100- rowSums(select(., matches("share"))))) %>%
  mutate(across(cdu:npd, ~ .x/votes_valid*100, .names = "{.col}_share_man")) %>%
  mutate(flag_total = abs(100- rowSums(select(., matches("share_man"))))) %>%
  relocate(flag_share, flag_total, .after = everything()) %>%
  # If totals seem to contain errors, calculate totals based on shares
  mutate(across(cdu_share:npd_share, ~ ifelse(flag_total>2, round((.x/100)*votes_valid), NA),
                .names = "{.col}_replace")) %>%
  mutate(
    cdu = ifelse(flag_total>2 | is.na(flag_total), cdu_share_replace, cdu),
    spd = ifelse(flag_total>2 | is.na(flag_total), spd_share_replace, spd),
    fdp = ifelse(flag_total>2 | is.na(flag_total), fdp_share_replace, fdp),
    adf = ifelse(flag_total>2 | is.na(flag_total), adf_share_replace, adf),
    npd = ifelse(flag_total>2 | is.na(flag_total), npd_share_replace, npd),
  ) %>%
  select(-matches("flag|share")) %>%
  mutate(state_name = "Rheinland-Pfalz", key_state = "07") %>% relocate(state_name, .before = everything())

btw65_rlp_cities <-
  read_csv("./additional_data/election_returns_cities/rlp_61_65_69_cities.csv") %>%
  filter(election_year == 1965, first_second_vote == "second") %>%
  select(-c(election_year, first_second_vote, gdp, dg, key_district:key_municipality)) %>%
  rename(npd = drp_npd, dfu = adf_dfu) %>%
  mutate(county_name = paste0(county_name, ", Stadt")) %>%
  mutate(municipality_name = county_name, type = "city", state_name = "Rheinland-Pfalz",
         key_state = "07")

btw65_rlp_municipalities_clean <-
  rlp69_municipalities_clean %>%
  filter(election_year == 1965) %>%
  rename(dfu = adf) %>%
  select(-c(first_second_vote, election_year, sheet, key_municipality)) %>%
  mutate(aud = votes_valid - rowSums(select(., cdu:npd)),
         aud = ifelse(aud < 0, 0, aud),
         type = "municipality") %>%
  bind_rows(btw65_rlp_cities) %>%
  relocate(state_name, matches("key"), .before = everything()) %>%
  rename_with(~ paste0(.x, "_second"), votes_valid:aud)

write_rds(btw65_rlp_municipalities_clean, "./output/cleaned/65_btw_rlp_clean.rds")

btw69_rlp_cities <-
  read_csv("./additional_data/election_returns_cities/rlp_61_65_69_cities.csv") %>%
  filter(election_year == 1969) %>%
  select(-c(election_year, aud, gdp, dg)) %>%
  rename(npd = drp_npd, adf = adf_dfu) %>%
  mutate(county_name = paste0(county_name, ", Stadt")) %>%
  mutate(municipality_name = county_name, type = "city", state_name = "Rheinland-Pfalz") %>%
  pivot_wider(values_from = votes_valid:npd, names_from = first_second_vote, id_cols = c(county_name:voters, municipality_name:state_name))

btw69_rlp_municipalities_clean <-
  rlp69_municipalities_clean %>%
  filter(election_year == 1969) %>%
  mutate(type = "municipality") %>%
  pivot_wider(names_from = first_second_vote, values_from = votes_valid:npd) %>%
  select(-c(election_year, sheet, key_municipality)) %>%
  bind_rows(btw69_rlp_cities)

write_rds(btw69_rlp_municipalities_clean, "./output/cleaned/69_btw_rlp_clean.rds")

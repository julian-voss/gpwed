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
library(readxl)

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
overview_data_party_votes <-
  na_info %>%
  filter(vote_type == "party") %>%
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
overview_data_candidate_votes <-
  na_info %>%
  filter(vote_type == "candidate") %>%
  ggplot(., aes(x = year, y = state_lab, fill = non_missing)) +
  geom_tile(color = "white") +
  geom_text(aes(label = non_missing_lab), size = 3) +
  scale_x_continuous(breaks  = seq(1949, 1969, 4)) +
  labs(x = "Election", y = "State") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        text = element_text(size = 15))

ggsave("./output/figures/overview_data_party_votes.pdf",
       overview_data_party_votes, width = 6, height = 3.5)

ggsave("./output/figures/overview_data_candidate_votes.pdf",
       overview_data_candidate_votes, width = 6, height = 3.5)


# Figure 3  --------------------------------------------------------------------

## Define right-wing nationalist parties
rw_parties_list <- c("dp", "drp", "gb_bhe", "pdgd_dns", "dg", "npd", "uap", "gpd", "wav", "vu")

state_geometries <- st_read("./additional_data/shapefiles/vg5000/vg5000_ebenen_1231/VG5000_LAN.shp") %>%
  # Remove coastal areas
  filter(!OBJID %in% c("DEBKGVG500000CTC", "DEBKGVG500000CTD", "DEBKGVG500000CTA", "DEBKGVG500000CTG", "DEBKGVG500000CTB")) %>%
  mutate(AGS_num = as.numeric(AGS))

municipality_geometries <-
  st_read("./additional_data/shapefiles/vg5000/vg5000_ebenen_1231/VG5000_GEM.shp") %>%
  select(AGS, GEN)

gpwed_plot_dat <-
  gpwed %>%
  filter(vote_type == "party") %>%
  mutate(rw_parties = rowSums(select(., all_of(rw_parties_list)), na.rm = T),
         rw_parties = ifelse(is.na(share_imputed), NA, rw_parties),) %>%
  left_join(municipality_geometries, by = c("ags" = "AGS"))

party_plot_dat <-
  gpwed_plot_dat %>%
  select(ags, key_state, year, votes_valid, cdu_csu, spd, fdp, rw_parties, geometry) %>%
  mutate(across(cdu_csu:rw_parties, ~ (.x/votes_valid)*100),
         spd_quant = cut(spd, breaks = quantile(spd, na.rm = T), include.lowest = T),
         cdu_quant = cut(cdu_csu, breaks = quantile(cdu_csu, na.rm = T), include.lowest = T),
         fdp_quant = cut(fdp, breaks = quantile(fdp, na.rm = T), include.lowest = T),
         rw_quant = cut(rw_parties, breaks = quantile(rw_parties, na.rm = T), include.lowest = T)) %>%
  st_as_sf()

# Theme
party_plot_theme <- theme(
  legend.position = "bottom", panel.grid = element_blank(), panel.background = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(), text = element_text(size = 15),
  strip.background.x = element_blank(),
  legend.box.spacing = unit(0, "cm")
)

# Plot helper function
make_party_plot <- function(data, party_var, quant_var, base_color, party_lab) {
  quantiles <- round(quantile(data[[party_var]], na.rm = TRUE))
  breaks <- c(
    paste0("[", quantiles[1], ",", quantiles[2], "]"),
    paste0("(", quantiles[2], ",", quantiles[3], "]"),
    paste0("(", quantiles[3], ",", quantiles[4], "]"),
    paste0("(", quantiles[4], ",", 100, "]")
  )
  alphas <- seq(0.3, 1, length.out = length(levels(data[[quant_var]])))
  fill_colors <- scales::alpha(base_color, alphas)

  data %>%
    mutate(party_lab = party_lab) %>%
    ggplot(data = .) +
    geom_sf(aes(fill = .data[[quant_var]]), color = NA) +
    geom_sf(data = state_geometries, fill = NA, color = "black", lwd = 0.15) +
    scale_fill_manual("", values = fill_colors, na.translate = FALSE, labels = breaks) +
    facet_grid(party_lab~year) +
    party_plot_theme +
    guides(fill = guide_legend(label.position = "bottom"))
}

# Generate individual plots
plot_cdu <- make_party_plot(party_plot_dat, "cdu_csu", "cdu_quant", "black", "CDU/CSU")
plot_spd <- make_party_plot(party_plot_dat, "spd", "spd_quant", "#E3000F", "SPD")
plot_fdp <- make_party_plot(party_plot_dat, "fdp", "fdp_quant", "#ffa900", "FDP")
plot_rw  <- make_party_plot(party_plot_dat, "rw_parties", "rw_quant", "#0a2258", "Right-Wing")

plot_cdu

# Combine plots
plot <- plot_cdu / plot_spd / plot_fdp / plot_rw

ggsave("./output/figures/descriptive_maps.png", plot,
       width = 8, height = 11, dpi = 600)

# Technical validation ----------------------------------------------
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



# Missing party candidates -----------------------------------------------------

read_btw_district <- function(file, year, cdu_col, spd_col, fdp_col, csu_col) {
  read_delim(file,
             col_names = TRUE,
             skip = 5,
             delim = ";",
             locale = locale(encoding = "latin1")) %>%
    select(Wahlkreis,
           Wahlkreis_name = `...2`,
           valid_votes = `Gültige...8`,
           cdu = all_of(cdu_col),
           spd = all_of(spd_col),
           fdp = all_of(fdp_col),
           csu = all_of(csu_col)) %>%
    mutate(across(valid_votes:csu, as.numeric)) %>%
    drop_na(valid_votes) %>%
    mutate(cdu_csu = rowSums(select(., cdu, csu))) %>%
    select(-c(cdu, csu)) %>%
    relocate(cdu_csu, .before = spd) %>%
    mutate(year = year)
}

btw53_district <- read_btw_district(
  "./additional_data/district_data_bundeswahlleiter/btw53_kerg.csv",
  year = 1953,
  cdu_col = "CDU...10",
  spd_col = "SPD...12",
  fdp_col = "FDP...14",
  csu_col = "CSU...16") %>%
  filter(!str_sub(Wahlkreis, 1, 1) == 9) %>%
  mutate(Wahlkreis = as.numeric(Wahlkreis)) %>%
  filter(Wahlkreis > 22, !between(Wahlkreis, 60, 125), !between(Wahlkreis, 57, 59))


btw57_district <- read_btw_district(
  "./additional_data/district_data_bundeswahlleiter/btw57_kerg.csv",
  year = 1957,
  cdu_col = "CDU...10",
  spd_col = "SPD...12",
  fdp_col = "FDP...16",
  csu_col = "CSU...14") %>%
  filter(!str_sub(Wahlkreis, 1, 1) == 9) %>%
  mutate(Wahlkreis = as.numeric(Wahlkreis)) %>%
  filter(Wahlkreis > 22,
         !between(Wahlkreis, 60, 125),
         !between(Wahlkreis, 57, 59)
  )

btw_all_years <- bind_rows(btw53_district, btw57_district)

missing_candidates_info <- btw_all_years %>%
  pivot_longer(cdu_csu:fdp, names_to = "party", values_to = "candidate_votes") %>%
  mutate(missing_candidate = candidate_votes == 0) %>%
  group_by(year, party) %>%
  summarise(missing_candidate = mean(missing_candidate), .groups = "drop") %>%
  mutate(missing_candidate = missing_candidate, 2,
         party = case_match(party, "cdu_csu" ~ "CDU/CSU", "spd" ~ "SPD", "fdp" ~ "FDP")) %>%
  rename(Election = year)

zero_info_by_party <-
  gpwed %>%
  filter(!(key_state == "01" & year < 1961),
         !key_state %in% c("02", "04", "10"),
         !(key_state %in% c("05", "07") & year == 1965),
         year %in% c(1953, 1957)) %>%
  filter(vote_type == "candidate") %>%
  pivot_longer(cols = c(cdu_csu, spd, fdp), names_to = "party") %>%
  mutate(missing = as.integer(value == 0),
         party = case_match(party, "cdu_csu" ~ "CDU/CSU", "spd" ~ "SPD", "fdp" ~ "FDP")) %>%
  group_by(year, vote_type, party) %>%
  rename(Election = year) %>%
  summarise(zero_values = mean(missing, na.rm = T), .groups = "drop")

plot_dat <-
  left_join(missing_candidates_info, zero_info_by_party)

# Figure 2

missing_candidate_votes <-
  plot_dat %>%
  pivot_longer(c(missing_candidate, zero_values)) %>%
  mutate(name = case_match(
    name, "missing_candidate" ~ "Non-contested\nconstituencies",
    "zero_values" ~ "Municipalities with\nzero candidate votes"
  ),
  name = factor(name, levels = c("Non-contested\nconstituencies", "Municipalities with\nzero candidate votes")),
  Election = factor(Election)) %>%
  ggplot(data = ., aes(x = party, y = value, fill = name)) +
  geom_col(color = "black", width = .5, position = position_dodge(width = .6)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("", values = c("grey90", "grey50")) +
  facet_wrap(~Election) +
  labs(y = "", x = "Party") +
  haschaR::theme_hanno() +
  theme(legend.position = "bottom")

ggsave("./output/figures/missing_candidate_votes.pdf",
       missing_candidate_votes, width = 6, height = 4)

# Section C.2: Missing mail-in votes -------------------------------------------

he_57 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1957.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_name = 5,
         voters_eligible = 10, voters = 11,
         votes_invalid_candidate = 13, votes_valid_candidate = 14,
         votes_invalid_party = 23, votes_valid_party = 24
  ) %>%
  select(-all_of(c(6:9, 12))) %>%
  rename_with(tolower) %>%
  filter(!is.na(ags) | municipality_name == "Briefwahl") %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_candidate"), `spd...15`:dg) %>%
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_party"), `spd...25`:`drp...31`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  mutate(county_key = str_sub(ags, 1, 3)) %>%
  relocate(county_key, .after = county_name) %>%
  select(-matches("candidate")) %>%
  rename_with(~str_remove(.x, "_party")) %>%
  mutate(mail_urn = ifelse(municipality_name == "Briefwahl", "mail", "urn"))


he_61 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1961.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_key = 5, county_name = 6,
         voters_eligible = 11, voters = 12,
         votes_invalid_candidate = 14, votes_valid_candidate = 15,
         votes_invalid_party = 24, votes_valid_party = 25
  ) %>%
  select(-all_of(c(7:10, 13))) %>%
  rename_with(tolower) %>%
  filter(!is.na(ags) | municipality_name == "Briefwahl") %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_candidate"), `cdu...16`:dvg) %>%
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_party"), `cdu...26`:`drp...31`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  mutate(county_key = str_sub(ags, 1, 3)) %>%
  relocate(county_key, .after = county_name) %>%
  select(-matches("candidate")) %>%
  rename_with(~str_remove(.x, "_party")) %>%
  mutate(mail_urn = ifelse(municipality_name == "Briefwahl", "mail", "urn"))

he_65 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1965.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 1, district = 2, stat_key = 3, municipality_name = 4, county_name = 5,
         voters_eligible = 10, voters = 11,
         votes_invalid_candidate = 13, votes_valid_candidate = 14,
         votes_invalid_party = 22, votes_valid_party = 23
  ) %>%
  select(-all_of(c(6:9, 12))) %>%
  rename_with(tolower) %>%
  filter(!is.na(ags) | municipality_name == "Briefwahl") %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_candidate"), `spd...15`:pöhn) %>%
  rename_with(~ str_remove_all(.x, "\\.{3}[0-9]*") %>% paste0(., "_party"), `spd...24`:`npd...29`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  mutate(county_key = str_sub(ags, 1, 3)) %>%
  relocate(county_key, .after = county_name) %>%
  select(-matches("candidate")) %>%
  rename_with(~str_remove(.x, "_party")) %>%
  mutate(mail_urn = ifelse(municipality_name == "Briefwahl", "mail", "urn"))


he_69 <-
  read_xlsx("./additional_data/state_provided_files/BW_Hessen-1969.xlsx", sheet = 1, skip = 3, col_types = "text") %>%
  # Variable management
  rename(ags = 2, district = 1, stat_key = 3, municipality_name = 4, county_name = 5,
         voters_eligible = 9, voters = 10,
         votes_invalid_candidate = 12, votes_valid_candidate = 13,
         votes_invalid_party = 21, votes_valid_party = 22
  ) %>%
  select(-all_of(c(6:8, 11))) %>%
  rename_with(tolower) %>%
  filter(!is.na(ags) | municipality_name == "Briefwahl") %>%
  # Proper names
  rename_with(~ str_remove_all(.x, "[0-9]|\\.") %>% paste0(., "_candidate"), `spd...14`:uap) %>%
  rename_with(~ str_remove_all(.x, "[0-9]|\\.") %>% paste0(., "_party"), `spd...23`:`ep...29`) %>%
  rename_with(~ str_replace_all(.x, "/", "_")) %>%
  mutate(county_key = str_sub(ags, 1, 3)) %>%
  relocate(county_key, .after = county_name) %>%
  select(-matches("candidate")) %>%
  rename_with(~str_remove(.x, "_party")) %>%
  mutate(mail_urn = ifelse(municipality_name == "Briefwahl", "mail", "urn"))

calculate_mail_urn_error <- function(df, year) {
  # Identify party columns
  party_vars <- df %>%
    select(-c(ags, district, stat_key, municipality_name, county_name, county_key,
              voters_eligible, voters, votes_invalid, votes_valid, mail_urn)) %>%
    names()

  df_long <- df %>%
    mutate(voters = as.numeric(voters),
           votes_valid = as.numeric(votes_valid)) %>%
    mutate(across(all_of(party_vars), ~ as.numeric(.x))) %>%
    group_by(county_name, mail_urn) %>%
    summarise(
      voters = sum(voters, na.rm = TRUE),
      across(all_of(party_vars), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = all_of(party_vars), names_to = "party", values_to = "votes_party") %>%

    # # Group other parties
    # mutate(party = if_else(tolower(party) %in% c("cdu", "spd", "fdp"), tolower(party), "other")) %>%

    group_by(county_name, mail_urn, party) %>%
    summarise(
      votes_party = sum(votes_party, na.rm = TRUE),
      voters = first(voters),
      .groups = "drop"
    ) %>%

    mutate(vote_share = votes_party / voters) %>%
    select(county_name, mail_urn, party, voters, vote_share) %>%
    pivot_wider(names_from = mail_urn, values_from = c(vote_share, voters)) %>%
    filter(!is.na(vote_share_mail) & !is.na(vote_share_urn)) %>%
    mutate(
      mail_share = voters_mail / (voters_mail + voters_urn),
      mail_urn_diff = vote_share_mail - vote_share_urn,
      error = mail_share * mail_urn_diff,
      year = year
    ) %>%
    select(year, county_name, party, vote_share_mail, vote_share_urn, mail_urn_diff, mail_share, error)

  return(df_long)
}

error_57 <- calculate_mail_urn_error(he_57, 1957)
error_61 <- calculate_mail_urn_error(he_61, 1961)
error_65 <- calculate_mail_urn_error(he_65, 1965)
error_69 <- calculate_mail_urn_error(he_69, 1969)

error_all <- bind_rows(error_57, error_61, error_65, error_69)


# Plot distribution of error
mail_error_distribution <- error_all %>%
  filter(party %in% c("cdu", "spd", "fdp")) %>%
  ggplot(., aes(x = error)) +
  geom_histogram(color = "black") +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~year) +
  labs(x = "Error", y = "Observations") +
  haschaR::theme_hanno()

mail_error_distribution

ggsave("./output/figures/mail_error_distribution.pdf", mail_error_distribution,
       width = 6, height = 4)

# Plot summary statistics for error by party
mail_error_table_dat <- error_all %>%
  rename(Party = party) %>%
  mutate(Party = case_when(!Party %in% c("cdu", "spd", "fdp") ~ "Other", T ~ Party)) %>%
  group_by(Party) %>%
  # filter(Party %in% c("cdu", "spd", "fdp")) %>%
  summarise(
    Mean = mean(error, na.rm = T),
    SD = sd(error, na.rm = T),
    Min = min(error, na.rm = T),
    p25 = quantile(error, probs = .25, na.rm = T),
    Median = quantile(error, probs = .50, na.rm = T),
    p75 = quantile(error, probs = .75, na.rm = T),
    Max = max(error, na.rm = T)) %>%
  mutate(across(Mean:Max, ~ round(.x , 4) * 100)) %>%
  rename(`$P_{25}$` = p25,
         `$P_{75}$` = p75) %>%
  mutate(Party = toupper(Party),
         Party = case_when(Party == "CDU" ~ "CDU", Party == "OTHER" ~ "Other", T ~ Party),
         Party = factor(Party, levels = c("CDU", "SPD", "FDP", "Other"))) %>%
  arrange(Party)


#Save as a LaTeX table
mail_error_table <-
  kable(mail_error_table_dat, format = "latex", booktabs = TRUE, caption = "County-level deviations by Party", escape = F) %>%
  as.character() %>%
  str_remove_all("\\\\begin\\{table\\}|\\\\end\\{table\\}")

# Write to a .tex file
cat(mail_error_table, file = "./output/figures/mail_error_table.tex")

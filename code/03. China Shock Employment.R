# Project: Trump Trade Policy Targets Dashboard
# File description: Aggregating manufacturing employment in counties most affected by the China shock
# last update: 4/28/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(tidycensus)
library(scales)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/trade-policy-targets-tracker",
  "sarah" = "/Users/sarah/Documents/GitHub/trade-policy-targets-tracker"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_cbp <- file.path(path_data, "CBP")
path_app <- file.path(path_project, "trade-target-tracker")
path_appdata <- file.path(path_app, "cleaned_data")

##################
### Data build ###
##################

china_shock <- read.csv(file.path(path_data, "china_shock_estimates_2.csv"))
china_most_hit_czones <- china_shock %>% select(2:11) %>% filter(CA.level.category == "Most hit") %>%
  mutate(CZ90 = as.numeric(czone_1990)) %>% .$CZ90

# Commuting zones to counties crosswalk
czone_to_counties <- read_xls(file.path(path_cbp, "czlma903.xls"), sheet = "CZLMA903")
china_most_hit_counties <- czone_to_counties %>% mutate(CZ90 = as.numeric(CZ90),
                                                  county = as.numeric(`County FIPS Code`),
                                                  county_name = `County Name`) %>%
  filter(CZ90 %in% china_most_hit_czones) %>% select(county, county_name, CZ90)

# Add VA cities that got subsumed into counties
va_cities <- data.frame(county = c(51560),
                        county_name = c("Clifton Forge City"),
                        CZ90 = c(602))
china_most_hit_counties <- bind_rows(china_most_hit_counties, va_cities)

# SIC to NAICS crosswalk from Eckert, Fort, Schott, and Yang (2021)
sic_naics_crosswalk <- read.csv(file.path(path_cbp, "full_sic87_naics97.csv")) %>%
  filter(naics97 == "31----") %>% mutate(sic = sic87) %>%
  select(sic, weight_emp) %>% na.omit()

# Generate establishment size bins
estab_bins <- c(
  n1_4 = (1+4)/2,
  n5_9 = (5+9)/2,
  n10_19 = (10+19)/2,
  n20_49 = (20+49)/2,
  n50_99 = (50+99)/2,
  n100_249 = (100+249)/2,
  n250_499 = (250+499)/2,
  n500_999 = (500+999)/2,
  n1000 = 0,
  n1000_1 = (1000+1499)/2,
  n1000_2 = (1500+2499)/2,
  n1000_3 = (2500+4999)/2,
  n1000_4 = 5000
)

for(year in 90:97){
  emp_var <- paste0("man_emp_", as.character(year))
  cbp_year <- read.table(file.path(path_cbp, paste0("cbp", as.character(year), "co.txt")),
                                   header=TRUE, sep = ',')
  cbp_year <- cbp_year %>% filter(sic %in% sic_naics_crosswalk$sic) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    left_join(sic_naics_crosswalk, by = "sic") %>%
    mutate(across(starts_with("n"), ~.*estab_bins[cur_column()]),
           emp = case_when(empflag == "" ~ floor(as.numeric(emp)*weight_emp), # if no suppression, employment is total employment
                           empflag != "" ~ floor((n1_4 + n5_9 + n10_19 + n20_49 + n50_99 + # if suppression, employment is mean employment in supressed bins.
                             n100_249 + n250_499 + n500_999 + n1000_1 +
                             n1000_2 + n1000_3 + n1000_4)*weight_emp)),
           county = fipstate * 1000 + fipscty) %>%
    group_by(county) %>% summarise(!!emp_var := sum(emp))
  china_most_hit_counties <- china_most_hit_counties %>% left_join(cbp_year, by = "county")
}

for(year in c(as.character(98:99), paste0("0", 0:9), as.character(10:22))){
  emp_var <- paste0("man_emp_", year)
  cbp_year <- read.table(file.path(path_cbp, paste0("cbp", year, "co.txt")),
                         header=TRUE, sep = ',')
  
  cbp_year <- cbp_year %>% rename_with(tolower) %>% filter(naics == "31----") %>% select(-naics) %>%
    mutate_all(~replace(., is.na(.), 0))
  if(year == "17"){
    cbp_year <- cbp_year %>% mutate(emp = case_when(empflag == "" ~ as.numeric(emp),
                                                    empflag == "A" ~ 19/2,
                                                    empflag == "B" ~ (20+99)/2,
                                                    empflag == "C" ~ (100+249)/2,
                                                    empflag == "E" ~ (250+499)/2,
                                                    empflag == "F" ~ (500+999)/2,
                                                    empflag == "G" ~ (1000+2499)/2,
                                                    empflag == "H" ~ (2500+4999)/2,
                                                    empflag == "I" ~ (5000+9999)/2,
                                                    empflag == "J" ~ (10000+24999)/2,
                                                    empflag == "K" ~ (25000+49999)/2,
                                                    empflag == "L" ~ (50000+99999)/2,
                                                    empflag == "M" ~ 100000,
                                                    TRUE ~ 0),
                                    county = fipstate * 1000 + fipscty)
  }else if(year %in% c(as.character(18:22))){
    cbp_year <- cbp_year %>% mutate(emp = as.numeric(emp),
                                    county = fipstate * 1000 + fipscty)
  }else{
    cbp_year <- cbp_year %>% mutate(across(starts_with("n"), ~as.numeric(.)*estab_bins[cur_column()]),
                                    emp = case_when(empflag == "" ~ as.numeric(emp),
                                                    empflag != "" ~ n1_4 + n5_9 + n10_19 + n20_49 +
                                                      n50_99 + n100_249 + n250_499 + n500_999 +
                                                      n1000_1 + n1000_2 + n1000_3 + n1000_4),
                                    county = fipstate * 1000 + fipscty)
  }
  cbp_year <- cbp_year %>% ungroup() %>% group_by(county) %>% summarise(!!emp_var := sum(emp))
  china_most_hit_counties <- china_most_hit_counties %>% left_join(cbp_year, by = "county")
}

manu_emp_cn_most_hit <- china_most_hit_counties %>% ungroup() %>% mutate_all(~replace(., is.na(.), 0)) %>%
  select(-1:-3) %>% mutate_all(~round(.)) %>% summarise_all(sum)

# save as time series
  china_shock = manu_emp_cn_most_hit %>%
    pivot_longer(cols = names(manu_emp_cn_most_hit)) %>%
    mutate(year = as.numeric(gsub("man_emp_", "", name)),
           year = case_when(
             year >= 60 ~ 1900 + year,
             TRUE ~ 2000 + year
           ), value = as.numeric(value)/1000000) %>% select(year, value)
  
  # convert to time series
  china_shock_yr = ts(china_shock$value, start = c(1990), frequency = 1)

save(china_shock_yr, file = file.path(path_appdata, "china_shock.RData"))


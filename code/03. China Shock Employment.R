# Project: Trump Trade Policy Targets Dashboard
# File description: Aggregating manufacturing employment in counties most affected by the China shock
# last update: 4/15/2025 by Jiaxin He

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

china_shock_counties <- read.csv(file.path(path_data, "china_shock_estimates_2.csv"))
china_most_hit <- china_shock_counties %>% select(2:11) %>% filter(CA.level.category == "Most hit") %>%
  mutate(county = cnty) %>% select(county, czone_1990, Full.County.name)

for(year in 90:97){
  emp_var <- paste0("man_emp_", as.character(year))
  cbp_year <- read.table(file.path(path_cbp, paste0("cbp", as.character(year), "co.txt")),
                                   header=TRUE, sep = ',')
  cbp_year <- cbp_year %>% filter(sic == "20--") %>%
    mutate(emp = as.numeric(emp), county = fipstate * 1000 + fipscty) %>%
    group_by(county) %>% summarise(!!emp_var := sum(emp))
  china_most_hit <- china_most_hit %>% left_join(cbp_year, by = "county")
}

for(year in c(as.character(98:99), paste0("0", 0:9), as.character(10:22))){
  emp_var <- paste0("man_emp_", year)
  cbp_year <- read.table(file.path(path_cbp, paste0("cbp", year, "co.txt")),
                         header=TRUE, sep = ',')
  if(year == "15"){
    cbp_year <- cbp_year %>% rename(naics = NAICS, emp = EMP, fipstate = FIPSTATE, fipscty = FIPSCTY)
  }
  
  cbp_year <- cbp_year %>% filter(naics == "31----") %>%
    mutate(emp = as.numeric(emp), county = fipstate * 1000 + fipscty) %>%
    group_by(county) %>% summarise(!!emp_var := sum(emp))
  china_most_hit <- china_most_hit %>% left_join(cbp_year, by = "county")
}

manu_emp_cn_most_hit <- china_most_hit %>% ungroup() %>% mutate_all(~replace(., is.na(.), 0)) %>%
  select(-1:-3) %>% summarise_all(sum)

# save as time series
  china_shock = manu_emp_cn_most_hit %>%
    pivot_longer(cols = names(manu_emp_cn_most_hit)) %>%
    mutate(year = as.numeric(gsub("man_emp_", "", name)),
           year = case_when(
             year >= 60 ~ 1900 + year,
             TRUE ~ 2000 + year
           ), value = as.numeric(value)/1000) %>% select(year, value)
  
  # convert to time series
  china_shock_yr = ts(china_shock$value, start = c(1990), frequency = 1)

save(china_shock_yr, file = file.path(path_appdata, "china_shock.RData"))

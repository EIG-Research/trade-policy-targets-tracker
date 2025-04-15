
# clean output of stata employment data -- time series conversion

rm(list = ls())
###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(readxl)

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
path_data <- file.path(path_project, "trade-target-tracker/cleaned_data")
setwd(path_data)

# read in data
employment_pop_ratio = read_excel( "employment_pop_ratio_native_men.xlsx") %>%
  mutate(employment_rate = employment_rate*100) %>% filter(year < 2025) # don't have all months there yet.
  
employment_prime = read_excel("employment_prime_age_native_men.xlsx") %>%
  mutate(employment_level_millions = employment_level/1000000) %>%
  select(-c(employment_level, employment_level_in_thousands))

# save as time series
emp_pop_ratio <- ts(employment_pop_ratio$employment_rate, start = c(1994, 1), frequency = 4)
emp_lvl_prime_age <- ts(employment_prime$employment_level_millions, start = c(1994, 1), frequency = 4)


save(emp_pop_ratio, emp_lvl_prime_age, file = "cps_employment.RData")


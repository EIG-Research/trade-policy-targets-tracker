# Project: Trump Trade Policy Targets Dashboard
# File description: Clean output of stata employment data -- time series conversion
# last update: 4/15/2025 by Jiaxin He

rm(list = ls())
###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(readxl)
library(seasonal)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO"
  )

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_cps <- file.path(path_data, "CPS")
path_app <- file.path(path_project, "trade-target-tracker")
path_appdata <- file.path(path_app, "cleaned_data")

# import monthly data
employment_pop_ratio_m <- read_excel(file.path(path_cps, "employment_rate_native.xlsx")) %>%
  mutate(employment_rate = employment_rate) %>%
  select(year, month, employment_rate)

employment_prime_m <- read_excel(file.path(path_cps, "employment_level_native.xlsx")) %>%
  mutate(employment_level_millions = employment_level/1000000) %>%
  select(-c(employment_level))

# save as time series, adjust seasonally, and aggregate into quarters
start_month <- c(employment_prime_m$year[1], match(tools::toTitleCase(employment_prime_m$month[1]), month.name))
quarterly <- function(df, start_month, func, seasonal = FALSE){
  df_ts <- df %>% ts(., start = start_month, frequency = 12)
  if(seasonal){
    df_ts <- final(seas(df_ts)) # Seasonally adjust FRED budget and construction spending data
  }
  df_ts %>% aggregate(., nfrequency = 4, FUN = func)
}

emp_lvl_prime_age_m <- quarterly(employment_prime_m$employment_level_millions, start_month, mean, seasonal = TRUE)
emp_pop_ratio_m <- quarterly(employment_pop_ratio_m$employment_rate, start_month, mean, seasonal = TRUE)

save(emp_lvl_prime_age_m, emp_pop_ratio_m, file = file.path(path_appdata, "cps_employment.RData"))

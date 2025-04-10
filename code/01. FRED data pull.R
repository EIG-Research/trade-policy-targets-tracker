# Project: Trump Trade Policy Targets Dashboard
# File description: FRED data pull for:
#   1. Federal budget balance
#   2. Total private construction spending, manufacturing
#   3. Employment, Manufacturing
#   4. Employment, Motor Vehicles and Parts
#   5. Employment, Total Private
# last update: 4/10/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(tidycensus)
library(scales)

# Install and load FRED API
# Link: https://fredblog.stlouisfed.org/2024/12/leveraging-r-for-powerful-data-analysis/
# install.packages("devtools")
# devtools::install_github("manutzn/fredo")
library(fredo)
library(seasonal) # Use X-13 to seasonally adjust monthly data

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/trade-policy-targets-tracker"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_app <- file.path(path_project, "trade-target-tracker")
path_appdata <- file.path(path_app, "cleaned_data")

##################
### Data build ###
##################

FRED_API_KEY <- your_api_key_here
api_key <- Sys.getenv("FRED_API_KEY")

# Query monthly federal budget balance
budget_id <- "MTSDS133FMS"
start_date <- "1990-01-01"
end_date <- "2025-03-01"
budget_month <- fredo(budget_id, start_date, end_date)

# Aggregate by quarter and adjust seasonally

# Query private construction spending, manufacturing

# Query all employees, manufacturing

# Query all employees, motor vehicles and parts

# Query all employees, private

# Adjust to 2017 dollars

# Calculate manufacturing and automotive shares of private employment

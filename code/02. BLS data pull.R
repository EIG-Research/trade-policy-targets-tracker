# Project: Trump Trade Policy Targets Dashboard
# File description: BLS data pull for: County-level QCEW for usage in China Shock analysis
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

# Install and load BLS API
# Link: https://github.com/keberwein/blscrapeR
# devtools::install_github("keberwein/blscrapeR")
library(blscrapeR)

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


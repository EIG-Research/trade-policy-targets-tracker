# Project: Trump Trade Policy Targets Dashboard
# File description: FRED data pull for:
#   0. CPI-U
#   1. Federal budget balance
#   2. Total private construction spending, manufacturing
#   3. Employment, Manufacturing
#   4. Employment, Motor Vehicles and Parts
#   5. Employment, Total Private
# last update: 4/22/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

###########################
###   Load Packages     ###
###########################
library(curl)
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
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/trade-policy-targets-tracker",
  "sarah" = "/Users/sarah/Documents/GitHub/trade-policy-targets-tracker"
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

# Insert your FRED API key here
FRED_API_KEY <- "ac17e5b7bc9c9e7c03f3f452bcb3051e"
api_key <- Sys.getenv("FRED_API_KEY")

start_date <- "1990-01-01"
end_date <- "2025-03-01"

# Query quarterly chained PCE (2017 basis)
PCE_id <- "PCECTPI"
PCE_qt <- fredo(FRED_API_KEY, PCE_id, "1989-01-01", end_date) %>% select(value) %>%
  ts(., start = c(1989,1), frequency = 4)

# Query monthly CPI-U
CPI_id <- "CPIAUCSL"
CPI_month <- fredo(FRED_API_KEY, CPI_id, "1989-01-01", end_date)

# Query monthly federal budget balance
budget_id <- "MTSDS133FMS"
budget_month <- fredo(FRED_API_KEY, budget_id, start_date, end_date)

# Query private construction spending, manufacturing
construction_id <- "PRMFGCON"
contruction_month <- fredo(FRED_API_KEY, construction_id, start_date, end_date)

# Query all employees, manufacturing
emp_manu <- "MANEMP"
manu_month <- fredo(FRED_API_KEY, emp_manu, start_date, end_date)

# Query all employees, motor vehicles and parts
emp_motor <- "CES3133600101"
motor_month <- fredo(FRED_API_KEY, emp_motor, start_date, end_date)

# Query all employees, private
emp_priv <- "USPRIV"
priv_month <- fredo(FRED_API_KEY, emp_priv, start_date, end_date)

# Tabulate by quarters, seasonally adjust the unadjusted ones
quarterly <- function(df, start_month, func, seasonal = FALSE){
  df_ts <- df %>% select(value) %>% ts(., start = start_month, frequency = 12)
  if(seasonal){
    df_ts <- final(seas(df_ts)) # Seasonally adjust FRED budget and construction spending data
  }
  df_ts %>% aggregate(., nfrequency = 4, FUN = func)
}

cpi_qt <- quarterly(CPI_month, c(1989,1), mean, FALSE)
budget_qt <- quarterly(budget_month, c(1990,1), sum, TRUE)
construction_qt <- quarterly(contruction_month, c(1993,1), sum, TRUE)
manu_qt <- quarterly(manu_month, c(1990,1), mean, FALSE)
motor_qt <- quarterly(motor_month, c(1990,1), mean, FALSE)
priv_qt <- quarterly(priv_month, c(1990,1), mean, FALSE)

# Calculate quarterly inflation
pce_inflation <- (PCE_qt[5:length(PCE_qt)] / PCE_qt[1:(length(PCE_qt)-4)] - 1) %>%
  ts(., start = c(1990,1), frequency = 4)
pce_adj <- PCE_qt[5:length(PCE_qt)]/100

# Adjust budegt to billions of 2017 dollars, using CPI-U
cpi_adj <- cpi_qt[5:length(cpi_qt)] / mean(cpi_qt[((2017-1989)*4 + 1):((2017-1989)*4 + 4)])
budget_real <- budget_qt / (cpi_adj*1000)

# Adjust construction spending to billions of 2017 dollars
path_bea <- file.path(path_project, "data/BEA")
manu_const_price_index <- read_xlsx(file.path(path_bea, "bea_const_price_indices.xlsx"),
                                    sheet = "Table",
                                    skip = 5) %>% filter(`...2` == "Manufacturing") %>% select(-1:-2)
manu_const_adj <- as.numeric(unlist(manu_const_price_index))/100
construction_real <- construction_qt / (manu_const_adj*1000)

# Calculate manufacturing and automotive shares of private employment
manu_share <- manu_qt / priv_qt
motor_share <- motor_qt / priv_qt
manu_qt <- manu_qt / 1000
motor_qt <- motor_qt / 1000

# Export data
save(pce_adj, pce_inflation, budget_real, construction_real,
     manu_qt, motor_qt, manu_share, motor_share, file = file.path(path_appdata, "fred_data.RData"))

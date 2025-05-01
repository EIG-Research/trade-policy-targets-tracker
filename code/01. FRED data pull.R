# Project: Trump Trade Policy Targets Dashboard
# File description: FRED data pull for:
#   0. PCE and Chained CPI-U
#   1. Federal budget balance
#   2. Total private construction spending, manufacturing
#   3. Employment, Manufacturing
#   4. Employment, Motor Vehicles and Parts
#   5. Employment, Total Private
#   6. Median household income
#   7. Industrial Production
#   8. Customs duties
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
library(zoo)
library(readxl)

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

# Query monthly C-CPI-U
CPI_id <- "SUUR0000SA0"
CPI_month <- fredo(FRED_API_KEY, CPI_id, "1999-12-01", end_date)

# Import monthly R-CPI-U-RS from BLS and combine with C-CPI-U, replicating Census' approach
path_bls <- file.path(path_project, "data/BLS")
rcpi_rs <- read_xlsx(file.path(path_bls, "r-cpi-u-rs-allitems.xlsx"), sheet = "All items", skip = 5)
rcpi_rs <- rcpi_rs %>% filter(YEAR >= 1989 & YEAR <= 1999) %>% select(-AVG) %>%
  pivot_longer(cols = -YEAR, names_to = "month", values_to = "value") %>%
  mutate(date = as.Date(as.yearmon(paste(month, YEAR, sep = " ")))) %>%
  select(date, value)
rcpi_rs$value <- rcpi_rs$value / tail(rcpi_rs$value, 1) * 100
CPI_month <- bind_rows(rcpi_rs, CPI_month %>% select(date, value)) %>% distinct(date, .keep_all = TRUE)

# Query monthly federal budget balance
budget_id <- "MTSDS133FMS"
budget_month <- fredo(FRED_API_KEY, budget_id, start_date, end_date)

# Query private construction spending, manufacturing
construction_id <- "PRMFGCON"
contruction_month <- fredo(FRED_API_KEY, construction_id, start_date, end_date)

# Query real annual median household income
income_id <- "MEHOINUSA672N"
income_yr <- fredo(FRED_API_KEY, income_id, start_date, end_date) %>% select(value) %>%
  ts(., start = c(1990,1), frequency = 1)

# Query all employees, manufacturing
emp_manu <- "MANEMP"
manu_month <- fredo(FRED_API_KEY, emp_manu, start_date, end_date)

# Query all employees, motor vehicles and parts
emp_motor <- "CES3133600101"
motor_month <- fredo(FRED_API_KEY, emp_motor, start_date, end_date)

# Query all employees, private
emp_priv <- "USPRIV"
priv_month <- fredo(FRED_API_KEY, emp_priv, start_date, end_date)

# Query industrial production, manufacturing (seasonally adjusted)
ip_manu <- "IPGMFSQ"
ipman_qt <- fredo(FRED_API_KEY, ip_manu, start_date, end_date)
ipman_qt <- ipman_qt %>% select(value) %>% ts(., start = c(1990,1), frequency = 4)

# Query customs duties revenue data
duties_rev <- "B235RC1Q027SBEA"
duties_rev_qt <- fredo(FRED_API_KEY, duties_rev, start_date, end_date) %>% select(value) %>%
  mutate(value = value/4) %>% ts(., start = c(1990,1), frequency = 4)

# Query real YoY GDP growth
gdp_growth <- "A191RO1Q156NBEA"
gdp_growth_qt <- fredo(FRED_API_KEY, gdp_growth, start_date, end_date) %>% select(value) %>%
  mutate(value = value) %>% ts(., start = c(1990,1), frequency = 4)

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

# Adjust median household income to 2017 PCE dollars
cpi_yr <- cpi_qt %>% aggregate(., nfrequency = 1, FUN = mean)
cpi_adj_yr <- cpi_yr[2:(length(cpi_yr)-1)]/cpi_yr[2017-1989+1]
pce_adj_yr <- PCE_qt[5:(length(PCE_qt)-4)] %>% ts(., start = c(1990,1), frequency = 4) %>%
  aggregate(., nfrequency = 1, FUN = mean) / 100
income_yr <- income_yr * cpi_adj_yr / pce_adj_yr

# Calculate manufacturing and automotive shares of private employment
manu_share <- manu_qt / priv_qt
motor_share <- motor_qt / priv_qt
manu_qt <- manu_qt / 1000
motor_qt <- motor_qt / 1000

# Export data
save(pce_adj, pce_inflation, ipman_qt, duties_rev_qt, gdp_growth_qt, income_yr, budget_real, construction_real,
     manu_qt, motor_qt, manu_share, motor_share, file = file.path(path_appdata, "fred_data.RData"))

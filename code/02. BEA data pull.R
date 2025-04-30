# Project: Trump Trade Policy Targets Dashboard
# File description: BEA data pull for:
#   1. Aggregate trade balance
#   2. Trade balance with China
#   3. Total real value added, manufacturing
# Additional historical data downloaded manually from Census and OECD
# last update: 4/14/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(tidycensus)
library(scales)
library(readxl)
library(seasonal)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "sarah" = "/Users/sarah/Documents/GitHub/trade-policy-targets-tracker",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/trade-policy-targets-tracker"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_bea <- file.path(path_data, "BEA")
path_app <- file.path(path_project, "trade-target-tracker")
path_appdata <- file.path(path_app, "cleaned_data")

##################
### Data build ###
##################

# Load 2017 real dollar adjustments from cleaned FRED data
load(file.path(path_appdata, "fred_data.RData"))

# Import trade balance data downloaded from BEA
# Link: https://www.bea.gov/data/intl-trade-investment/international-trade-goods-and-services
# Monthly aggregate: U.S. Trade in Goods and Services, 1960-present, Table 1, column 3 "Goods"
# Quarterly with China: U.S. Trade in Goods and Services by Selected Countries and Areas, 1999-present, Table 6
trade_agg_month <- read_xlsx(file.path(path_bea, "trad-time-series-0225.xlsx"),
                             sheet = "Table 1",
                             skip = 74) %>%
  select(1,3) %>%
  na.omit() %>% rename(month = Monthly, balance = `...3`)


trade_china_qt <- read_xlsx(file.path(path_bea, "trad-geo-time-series-0125.xlsx"),
                            sheet = "Table 6",
                            skip = 5) %>% slice(29:n()) %>% select(Period, China) %>%
  na.omit() %>% rename(quarter = Period, balance = China)

# Monthly goods trade with China from 1992 to 1998 are obtained from Census
# Link: https://www.census.gov/foreign-trade/balance/c5700.html
trade_china_hist <- read.csv(file.path(path_bea, "trad-china-hist-92-98.csv")) %>%
  select(Month, Balance) %>% rename(month = Month, balance = Balance)
trade_china_hist$balance <- round(as.numeric(sub(",", "", trade_china_hist$balance)), digits = 0)

# Real value added by industry
va_manu_q <- read.csv(file.path(path_bea, "real_VA_manu_2005_2024_Q.csv"), skip = 3, header = TRUE)
va_manu_q <- head(va_manu_q, -5) %>% select(-1:-2)
va_manu_2005_2024_qt <- as.numeric(unlist(va_manu_q[2,])) %>%
  ts(., start = c(2005, 1), frequency = 4) / 1000

va_manu_a <- read.csv(file.path(path_bea, "real_VA_manu_1997_2004_A.csv"), skip = 3, header = TRUE)
va_manu_a <- head(va_manu_a, -5) %>% select(-1:-2) %>% na.omit()
va_manu_1997_2004_year <- as.numeric(unlist(va_manu_a[1,])) %>%
  ts(., start = c(1997, 1), frequency = 1) / 1000

# Manufacturing value added share of gdp
share_va_manu_q <- read.csv(file.path(path_bea, "manu_share_gdp_2005_2024_Q.csv"), skip = 3, header = TRUE)
share_va_manu_q <- head(share_va_manu_q, -5) %>% select(-1:-2)
share_va_manu_2005_2024_qt <- as.numeric(unlist(share_va_manu_q[2,])) %>%
  ts(., start = c(2005, 1), frequency = 4)

share_va_manu_a <- read.csv(file.path(path_bea, "manu_share_gdp_1997_2004_A.csv"), skip = 3, header = TRUE)
share_va_manu_a <- head(share_va_manu_a, -5) %>% select(-1:-2) %>% na.omit()
share_va_manu_1997_2004_year <- as.numeric(unlist(share_va_manu_a[1,])) %>%
  ts(., start = c(1997, 1), frequency = 1)

# Aggregate quarterly total trade balance
trade_agg_qt <- trade_agg_month$balance %>% ts(., start = c(1992,1), frequency = 12) %>%
  aggregate(., nfrequency = 4, FUN = sum)

# Aggregate quarterly China trade balance
# Seasonally adjust historical data
trade_china_qt_92_98 <- final(seas(trade_china_hist$balance %>%
                                    ts(., start = c(1992,1), frequency = 12))) %>%
  aggregate(., nfrequency = 4, FUN = sum)
trade_china_qt_99_24 <- trade_china_qt$balance %>% ts(., start = c(1999,1), frequency = 4)
trade_china_qt <- round(ts(c(trade_china_qt_92_98, trade_china_qt_99_24), start = start(trade_china_qt_92_98),
                     frequency = 4), digits = 0)

# Adjust to billions of 2017 dollars
# Remove the -1 when trade data comes out
trade_agg_qt <- trade_agg_qt / (pce_adj[9:(length(pce_adj)-1)]*1000)
trade_china_qt <- trade_china_qt / (pce_adj[9:(length(pce_adj)-1)]*1000)

# Export data
save(trade_agg_qt, trade_china_qt, va_manu_1997_2004_year, va_manu_2005_2024_qt,
     share_va_manu_1997_2004_year, share_va_manu_2005_2024_qt, file = file.path(path_appdata, "bea_data.RData"))

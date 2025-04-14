# Project: Trump Trade Policy Targets Dashboard
# File Description: R Shiny application

# last update: 4/14/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

# restart R session, run if shinyapps.io deployment gives an error
# .rs.restartR()

###########################
###   Load Packages     ###
###########################
library(scales)
library(zoo)
library(tidyr)
library(dplyr)
library(Hmisc)
library(grattan)
library(bslib)
library(ggplot2)
library(ggfortify)
library(dichromat)
library(cowplot)

# R Shiny
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='economicinnovationgroup',
                          token='1E424A49864E72123BE5CAA19E6D2274',
                          secret='/OJ/Oy/GW2sk6ibHJt4JgoqzB80U03mcEyFJn0ev')

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
setwd(path_app)

#################
### Load Data ###
#################
# Change to just file.path("cleaned_data", "fred_data.RData") when deploying online
load(file.path("./cleaned_data", "fred_data.RData"))
load(file.path("./cleaned_data", "bea_data.RData"))

######################
### Build Shiny UI ###
######################

# Define EIG color palette
eig_colors <- c("#1a654d", "#5e9c86", "#008080", "#044140")	  # EIG green colors

ui <- page_fillable(
  titlePanel("Welcome to the Trade Policy Dashboard", windowTitle = "Welcome to the Trade Policy Dashboard"),
  textOutput("description"),
  navset_card_tab(
    ### Inflation ###
    nav_panel("Inflation", plotOutput("plot_inflation")),
    
    ### Federal Budget Balance ###
    nav_panel("Federal Budget Balance", plotOutput("plot_budget")),
    
    ### Trade Balance ###
    nav_panel("Trade Balance", plotOutput("plot_trade")),
    
  )
)

##########################
### Build Shiny Server ###
##########################

server <- function(input, output) {
  output$description <- renderText("This tool tracks quarterly trends in key economic indicators identified by
  the Trump administration’s trade agenda as gauges of success. Each indicator provides insight into
  how policy actions align with stated goals, offering an up-to-date look at economic outcomes. Use
  the buttons below to explore individual indicators, view their associated targets, and assess progress
  across different areas of trade policy.")
  
  output$plot_inflation <- renderPlot(
    autoplot(cpi_inflation, ts.colour = eig_colors[1]) +
      geom_hline(yintercept = 0.005, color = eig_colors[4]) +
      geom_text(aes(x = as.Date(as.yearmon(1992)), y = 0.003, label = "Long-run Fed target"),
                stat = "unique", color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(breaks = seq(-0.02, 0.02, 0.005), labels = scales::percent) +
      ylab("Inflation (%)") +
      xlab("Time (Quarter)")
  )
  
  output$plot_budget <- renderPlot(
    autoplot(budget_real, ts.colour = eig_colors[1]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      ylab("Federal Budget Balance (Millions of Dollars)") +
      xlab("Time (Quarter)")
  )
  
  output$plot_trade <- renderPlot(
    autoplot(trade_agg_qt, ts.colour = eig_colors[1]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      ylab("Trade Balance (Millions of Dollars)") +
      xlab("Time (Quarter)")
  )
}

shinyApp(ui = ui, server = server)

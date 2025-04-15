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
  
  ## Pull in EIG theme font and set the look of the header ##
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Source+Serif+Pro:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      h1 {
        font-family: 'Source Serif Pro', serif;
        font-weight: bold;
        margin: 0;
        font-size: 2.5em;
      }
      .header-container {
        display: flex;
        align-items: center;
        background-color: #044140;
        padding: 20px;
        color: white;
        justify-content: center;
      }
    "))
  ),
  
  
  ## Title ##
  div(
    class = "header-container",
    
    # Title with bold font
    h1("Welcome to the Trade Policy Dashboard")
  ),
  
  ## Tracker description ##
  textOutput("description"),
  
  navset_card_tab(
    
    ### Inflation ###
    nav_panel("Inflation", 
              fluidRow(
                column(8, plotOutput("plot_inflation")),  # Plot on the left
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_inflation"))
                ))
    ),
    
    ### Federal Budget Balance ###
    nav_panel("Federal Budget Balance", 
              fluidRow(
                column(8, plotOutput("plot_budget")),  # Plot on the left
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_budget"))
                ))
    ),
    
    ### Trade Balance ###
    nav_panel("Trade Balance", 
              fluidRow(
                column(8, plotOutput("plot_trade")),  # Plot on the left
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_trade"))
                ))
    ),
    
    ## Trade deficit with China ##
    
    ## Employment rate, native born men 16+ ##
    
    ## Employment, native born men prime age ##
    
    ## Total Private Construction Spending in Manufacturing ##
    
    ## Real value added, manufacturing ##
    
    ## Manufacturing share of private employment ##
    
    ## Employment, manufacturing ##
    
    ## Motor vehicles and parts share of private employment ##
    
    ## Employment, motor vehicles and parts ## 
    
    ## Employment in manufacturing, counties most affected by the "China shock"  ##
    
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
  
  # Baseline year breaks
  year_breaks <- seq(as.Date(as.yearqtr("1990 Q1")), as.Date(as.yearqtr("2025 Q1")), by = "5 years")
  
  # Convert date into year-quarter
  date2qt <- function(x) {
    y <- format(x, "%Y")
    q <- paste0("Q", as.numeric(format(x, "%m")) %/% 3 + 1)
    paste(y, q)
  }
  
  cpi_end <- as.Date(as.yearmon(end(cpi_inflation)[1] + (end(cpi_inflation)[2] - 1)/4))
  budget_end <- as.Date(as.yearmon(end(budget_real)[1] + (end(budget_real)[2] - 1)/4))
  trade_end <- as.Date(as.yearmon(end(trade_agg_qt)[1] + (end(trade_agg_qt)[2] - 1)/4))
  df_trade <- data.frame(quarter = as.Date(time(trade_agg_qt)), agg_balance = as.matrix(trade_agg_qt),
                         china_balance = as.matrix(trade_china_qt))
  
  output$plot_inflation <- renderPlot(
    autoplot(cpi_inflation, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = cpi_end, y = tail(cpi_inflation, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = cpi_end, y = tail(cpi_inflation, 1),
               label = paste0(as.character(round(tail(cpi_inflation, 1)*100, digits = 2)), "%"),
               vjust = -1, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = 0.005, color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("1990-01-01"), y = 0.005, label = "Long-run Fed target",
                hjust = 0, vjust = 2, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(breaks = seq(-0.02, 0.02, 0.005), labels = scales::percent) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = year_breaks, labels = date2qt, expand = c(0,0)) +
      ylab("Inflation (%)") +
      xlab("Time (Quarter)")
  )
  
  output$text_inflation <- renderText({
    "Bringing down inflation is a major goal for the current administration. During a campaign speech, Donald Trump vowed that “starting on Day 1, we will end inflation and make America affordable again.” The federal reserve’s inflation target is 2%."
  })
  
  output$plot_budget <- renderPlot(
    autoplot(budget_real, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = budget_end, y = tail(budget_real, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = budget_end, y = tail(budget_real, 1),
               label = paste0(as.character(round(tail(budget_real, 1), digits = 1)), "B"),
               vjust = -1, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = 0, color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("1990-01-01"), y = 0, label = "Target: Balanced Budget",
               hjust = 0, vjust = -1, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(breaks = seq(-2250, 250, 250)) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = year_breaks, labels = date2qt, expand = c(0,0)) +
      ylab("Fiscal Balance (Billions of Dollars)") +
      xlab("Time (Quarter)")
  )
  
  output$text_budget <- renderText({
    "The Trump administration has called for a balanced budget, to be achieved through spending reductions that offset planned tax cuts. The budget deficit currently stands at $1.8 trillion."
  })
  
  output$plot_trade <- renderPlot(
    ggplot(df_trade, aes(x = quarter)) +
      geom_line(aes(y = agg_balance), color = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = trade_end, y = tail(trade_agg_qt, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = trade_end, y = tail(trade_agg_qt, 1),
               label = paste0(as.character(round(tail(trade_agg_qt, 1), digits = 1)), "B"),
               vjust = 2, color = eig_colors[1]) +
      geom_line(aes(y = china_balance), color = eig_colors[2]) +
      # Add current level
      geom_point(aes(x = trade_end, y = tail(trade_china_qt, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = trade_end, y = tail(trade_china_qt, 1),
               label = paste0(as.character(round(tail(trade_china_qt, 1), digits = 1)), "B"),
               vjust = 2, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = 0, color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("1990-01-01"), y = 0, label = "Target: Eliminate Trade Deficit",
               hjust = 0, vjust = -1, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = year_breaks, labels = date2qt, expand = c(0,0)) +
      ylab("Trade Balance (Billions of Dollars)") +
      xlab("Time (Quarter)")
  )
  
  output$text_trade <- renderText({
    "The administration advocates for an “America First Trade Policy,” aimed at reducing the trade deficit in goods by raising tariffs on U.S. trading partners. The deficit currently stands at $917.8 billion."
  })
}

shinyApp(ui = ui, server = server)

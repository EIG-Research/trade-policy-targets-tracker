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
library(readxl)

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
setwd(path_app)

#################
### Load Data ###
#################
# Change to just file.path("cleaned_data", "fred_data.RData") when deploying online
load(file.path("./cleaned_data", "fred_data.RData"))
load(file.path("./cleaned_data", "bea_data.RData"))
load(file.path("./cleaned_data", "cps_employment.RData"))

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
    nav_panel("Native Male Employment Rate", 
              fluidRow(
                column(8, plotOutput("plot_employment_pop_native")),  # Plot on the left
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_employment_pop_native"))
                ))
    ),
    
    ## Employment, native born men prime age ##
    nav_panel("Prime-Age Native Male Employment Level", 
              fluidRow(
                column(8, plotOutput("plot_employment_lvl_native_prime")),  # Plot on the left
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_employment_lvl_native_prime"))
                ))
    ),
    
    ## Total Private Construction Spending in Manufacturing ##
    
    ## Real value added, manufacturing ##
  
    ## Manufacturing share of private employment ##

    ## Employment, manufacturing ##
    
    ## Motor vehicles and parts share of private employment ##
    nav_panel("Vehicle Employment Share", 
              fluidRow(
                column(8, plotOutput("plot_motor_share")),  # Plot on the left
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_motor_share"))
                ))
    ),

    ## Employment, motor vehicles and parts ## 
    nav_panel("Vehicle Employment Level", 
              fluidRow(
                column(8, plotOutput("plot_motor_qt")),  # Plot on the left
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_motor_qt"))
                ))
    )
    
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
  
  
  ## Inflation ##
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
  
  output$text_inflation <- renderText({
    "Bringing down inflation is a major goal for the current administration. During a campaign speech, Donald Trump vowed that “starting on Day 1, we will end inflation and make America affordable again.” The federal reserve’s inflation target is 2%."
  })
  
  
  ### Federal Budget Balance ###
  output$plot_budget <- renderPlot(
    autoplot(budget_real, ts.colour = eig_colors[1]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      ylab("Federal Budget Balance (Millions of Dollars)") +
      xlab("Time (Quarter)")
  )
  
  output$text_budget <- renderText({
    "The Trump administration has called for a balanced budget, to be achieved through spending reductions that offset planned tax cuts. The budget deficit currently stands at $1.8 trillion."
  })
  
  
  ### Trade Balance ###
  output$plot_trade <- renderPlot(
    autoplot(trade_agg_qt, ts.colour = eig_colors[1]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      ylab("Trade Balance (Millions of Dollars)") +
      xlab("Time (Quarter)")
  )
  
  output$text_trade <- renderText({
    "The administration advocates for an “America First Trade Policy,” aimed at reducing the trade deficit in goods by raising tariffs on U.S. trading partners. The deficit currently stands at $917.8 billion."
  })
  
  ## Employment rate, native born men 16+ ##
  output$plot_employment_pop_native <- renderPlot(
    autoplot(emp_pop_ratio, ts.colour = eig_colors[1]) +
      geom_hline(yintercept = 69.7, color = eig_colors[4]) +
      geom_text(aes(x = as.Date(as.yearmon(2011)), y = 69.4, label = "2000 level"),
                stat = "unique", color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      ylab("Native Men Employment-to-Population Ratio (%)") +
      xlab("Time (Quarter)")
  )
  
  output$text_employment_pop_native <- renderText({
    "Administration officials hope to raise native-born employment in part by imposing more severe immigration restrictions and creating new jobs by restricting trade. The native-born male employment rate currently stands at 65.5%. We set the target to be 69.7%, which is the level before China joined the WTO in 2001."
  })
  
  
  ## Employment, native born men prime age ##
  output$plot_employment_lvl_native_prime <- renderPlot(
    autoplot(emp_lvl_prime_age, ts.colour = eig_colors[1]) +
      geom_hline(yintercept = 43.3, color = eig_colors[4]) +
      geom_text(aes(x = as.Date(as.yearmon(2011)), y = 43.1, label = "2000 level"),
                stat = "unique", color = eig_colors[4]) +
      
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      ylab("Prime-Age Native Men Employment (Millions of Workers)") +
      xlab("Time (Quarter)")
  )
  
  output$text_employment_lvl_native_prime <- renderText({
    "Administration officials hope to raise native-born employment in part by imposing more severe immigration restrictions and creating new jobs by restricting trade. The prime age employment rate for native-born men is 41.4 million. We set the target to be 43.3, which is the level before China joined the WTO in 2001."
  })
  
  
  ## Motor vehicles and parts share of private employment ##
  output$plot_motor_share <- renderPlot(
    autoplot(motor_share, ts.colour = eig_colors[1]) +
      geom_hline(yintercept = 0.0118, color = eig_colors[4]) +
      geom_text(aes(x = as.Date(as.yearmon(2010)), y = 0.0116, label = "2000 level"),
                stat = "unique", color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(breaks = seq(0.00, 0.02, 0.0025), labels = scales::percent) +
      ylab("Share of Private Employment (%)") +
      xlab("Time (Quarter)")
  )
  
  output$text_motor_share <- renderText({
    "With the introduction of reciprocal tariffs on April 2nd, the president said that “jobs and factories will come roaring back.” Vehicle-related manufacturing jobs make up less than 1% of total U.S. jobs, down from 1.18% in 2000,  the level before China joined the WTO in 2001."
  })
  

  ## Employment, motor vehicles and parts ## 
  output$plot_motor_qt <- renderPlot(
    autoplot(motor_qt, ts.colour = eig_colors[1]) +
      geom_hline(yintercept = 1305.3333, color = eig_colors[4]) +
      geom_text(aes(x = as.Date(as.yearmon(1992)), y = 1295, label = "2000 level"),
                stat = "unique", color = eig_colors[4]) +
      
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      ylab("Motor Vehicle Employment (Thousands of Workers)") +
      xlab("Time (Quarter)")
  )
  
  output$text_motor_qt <- renderText({
    "With the introduction of reciprocal tariffs on April 2nd, the president said that “jobs and factories will come roaring back.” There are 1.0  million vehicle-related manufacturing jobs, down from 1.3 million in 2000, the level before China joined the WTO in 2001."
  })
  
  
  ## Employment in manufacturing, counties most affected by the "China shock"  ##
  
  
}

shinyApp(ui = ui, server = server)


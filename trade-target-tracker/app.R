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
library(tidyr)
library(dplyr)
library(Hmisc)
library(grattan)
library(tidycensus)
library(bslib)
library(ggplot2)
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
eig_colors <- c("#b3d6dd", "#79c5fd", "#176F96", "#234f8b", 	# EIG blue colors
                "#008080", "#5e9c86", "#1a654d", "#044140",	  # EIG green colors
                "#feecd6", "#f0b799", "#da9969", "#e1ad28")		# EIG beige, red, and yellow

# EIG palette selector
eig_palette <- function(n_colors, input_palette){
  if(n_colors <= length(input_palette)){
    return(input_palette[floor(seq(from = 1, to = length(input_palette), length.out = n_colors))])
  }else{
    return(colorRampPalette(input_palette)(n_colors))
  }
}

ui <- page_fillable(
  navset_card_tab(
    ### Inflation ###
    nav_panel("Inflation", plotOutput("plot_inflation")),
    
    ### Federal Budget Balance ###
    nav_panel("Federal Budget Balance", plotOutput("plot_budget")),
    
    ### Aggregate Trade Balance ###
    nav_panel("Aggregate Trade Balance", plotOutput("plot_agg_trade"))
  )
)

##########################
### Build Shiny Server ###
##########################

server <- function(input, output) {
  output$plot_inflation <- renderPlot(
    ggplot()
  )
  
  output$plot_budget <- renderPlot(
    ggplot()
  )
  
  output$plot_agg_trade <- renderPlot(
    ggplot()
  )
}

shinyApp(ui = ui, server = server)

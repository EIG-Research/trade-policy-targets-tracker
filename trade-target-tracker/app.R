# Project: Analyzing the effect of income cutoffs in noncompete bans
# File Description: R Shiny income breakdown dashboard

# last update: 4/3/2025 by Jiaxin He

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
### Load Data ###
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

load(file.path(path_appdata, "fred_data.RData"))

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
    ### Plot 1 ###
    nav_panel("Plot 1", plotOutput("plot_1")),
    
    ### Plot 2 ###
    nav_panel("Plot 2", plotOutput("plot_2")),
    
    ### Plot 3 ###
    nav_panel("Plot 3", plotOutput("plot_3"))
  )
)

##########################
### Build Shiny Server ###
##########################

server <- function(input, output) {
  output$plot_1 <- renderPlot(
    ggplot()
  )
  
  output$plot_2 <- renderPlot(
    ggplot()
  )
  
  output$plot_3 <- renderPlot(
    ggplot()
  )
}

shinyApp(ui = ui, server = server)

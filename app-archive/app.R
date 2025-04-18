# Project: Trump Trade Policy Targets Dashboard
# File Description: R Shiny application

# last update: 4/15/2025 by Jiaxin He

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
### Load Data ###
#################
# Change to just file.path("cleaned_data", "fred_data.RData") when deploying online
load(file.path("cleaned_data", "fred_data.RData"))
load(file.path("cleaned_data", "bea_data.RData"))
load(file.path("cleaned_data", "cps_employment.RData"))
load(file.path("cleaned_data", "china_shock.RData"))

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
        justify-content: center; /* aligns items to the left */
      }
      .header-logo {
        height: 60px;
        margin-right: 20px;
      }
      .card-header-tabs>li>a{
        color: #1a654d;
      }
      .card-header-tabs>li>a:hover{
        color: #5e9c86;
      }
      .nav-tabs>li>a{
        color: #1a654d;
      }
      .nav-tabs>li>a:hover{
        color: #5e9c86;
      }
    "))
  ),
  
  
  ## Title ##
  div(
    class = "header-container",
    
    # Logo to left of the title
    img(src = "EIG_reverse.png", alt = "EIG Logo", class = "header-logo"),
    
    # Title with bold font
    h1("Welcome to the Trade Policy Dashboard")
  ),
  
  ## Tracker description ##
  textOutput("description"),
  
  navset_card_tab(
    
    ### Inflation ###
    nav_panel("Inflation", 
              fluidRow(
                column(8, plotOutput("plot_inflation"),
                       div(
                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                         HTML('Source: <a href="https://fred.stlouisfed.org/series/CPIAUCSL" target="_blank">Bureau of Labor Statistics, CPI-U,</a> seasonally adjusted')
                       ) 
                  ),  # Plot on the left
                 
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_inflation"))
                ))
    ),
    
    ### Federal Budget Balance ###
    nav_panel("Budget Balance", 
              fluidRow(
                column(8, plotOutput("plot_budget"),
                       div(
                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                         HTML('Source: <a href="https://fred.stlouisfed.org/series/MTSDS133FMS" target="_blank">Department of the Treasury, Fiscal Service,</a> seasonally adjusted, in 2017 dollars')
                       )
                ),  # Plot on the left
                
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_budget"))
                ))
    ),
    
    ### Trade Balance ###
    nav_panel("Trade Balance", 
              fluidRow(
                column(8, plotOutput("plot_trade"),
                       div(
                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                         HTML('Source: <a href="https://www.bea.gov/data/intl-trade-investment/international-trade-goods-and-services" target="_blank">Bureau of Economic Analysis,</a> seasonally adjusted, in 2017 dollars. Available beginning in Q1 1992.')
                       )
                ),  # Plot on the left
                
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  textOutput("text_trade"))
                ))
    ),

    ## Native Employment ##
    nav_panel("Native Employment",
      navset_tab(
        ## Employment rate, native born men 16+ ##
        nav_panel("Native Male Employment Rate", 
                  fluidRow(
                    column(8, plotOutput("plot_employment_pop_native"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://cps.ipums.org/cps/index.shtml" target="_blank">Current Population Survey,</a> Quarterly averages of seasonally adjusted monthly rates.')
                           )
                     ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_employment_pop_native"))
                    ))
        ),
        
        ## Employment, native born men prime age ##
        nav_panel("Prime-Age Native Male Employment Level", 
                  fluidRow(
                    column(8, plotOutput("plot_employment_lvl_native_prime"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://cps.ipums.org/cps/index.shtml" target="_blank">Current Population Survey,</a> Quarterly averages of seasonally adjusted monthly rates.')
                           )
                    ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_employment_lvl_native_prime"))
                    ))
        ),
      )
    ),
    
    ## Manufacturing Employment ##
    nav_panel("Manufacturing Employment",
      navset_tab(
        ## Employment, manufacturing ##
        nav_panel("Manufacturing Employment Level", 
                  fluidRow(
                    column(8, plotOutput("plot_emp_manu"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://fred.stlouisfed.org/series/MANEMP" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                           )
                    ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_emp_manu"))
                    ))
        ),
        
        ## Manufacturing share of private employment ##
        nav_panel("Manufacturing Share", 
                  fluidRow(
                    column(8, plotOutput("plot_share_manu"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://fred.stlouisfed.org/series/MANEMP" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                           )
                    ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_share_manu"))
                    ))
        ),
        
        ## Employment, motor vehicles and parts ## 
        nav_panel("Automotive Employment Level", 
                  fluidRow(
                    column(8, plotOutput("plot_motor_qt"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://fred.stlouisfed.org/series/CES3133600101" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                           )
                    ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_motor_qt"))
                    ))
        ),
        
        ## Motor vehicles and parts share of private employment ##
        nav_panel("Automotive Share", 
                  fluidRow(
                    column(8, plotOutput("plot_motor_share"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://fred.stlouisfed.org/series/CES3133600101" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                           )
                    ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_motor_share"))
                    ))
        ),
        
        ## Employment in manufacturing, counties most affected by the "China shock"  ##
        nav_panel("Manufacturing Employment - China Shock", 
                  fluidRow(
                    column(8, plotOutput("plot_china_shock"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://www.census.gov/programs-surveys/cbp.html" target="_blank"> Census Bureau County Business Patterns 1990-2022,</a> quarterly data is not available.')
                           )
                    ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_china_shock"))
                    ))
        )
      )
    ),
    
    ## Manufacturing Output ##
    nav_panel("Manufacturing Output",
      navset_tab(
        ## Total Private Construction Spending in Manufacturing ##
        nav_panel("Construction Spending", 
                  fluidRow(
                    column(8, plotOutput("plot_const"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://fred.stlouisfed.org/series/PRMFGCON" target="_blank"> Census Bureau,</a>  seasonally adjusted, in 2017 dollars. Available beginning Q1 1993.')
                           )
                    ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_const"))
                    ))
        ),
        
        ## Real value added, manufacturing ##
        nav_panel("Value Added", 
                  fluidRow(
                    column(8, plotOutput("plot_va"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://www.bea.gov/itable/gdp-by-industry" target="_blank"> Bureau of Economic Analysis,</a>seasonally adjusted, in 2017 dollars. Available beginning in 1997; 1997 to 2004 data are annual.')
                           )
                    ),  # Plot on the left
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      textOutput("text_va"))
                    ))
        )
      )
    )
    
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
  across different areas of trade policy. All figures are in 2017 dollars for consistency")
  
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
  const_end <- as.Date(as.yearmon(end(construction_real)[1] + (end(construction_real)[2] - 1)/4))
  va_end <- as.Date(as.yearmon(end(va_manu_2005_2024_qt)[1] + (end(va_manu_2005_2024_qt)[2] - 1)/4))
  manu_end <- as.Date(as.yearmon(end(manu_qt)[1] + (end(manu_qt)[2] - 1)/4))
  native_end <- as.Date(as.yearmon(end(emp_lvl_prime_age_m)[1] + (end(emp_lvl_prime_age_m)[2] - 1)/4))

  df_trade <- data.frame(quarter = as.Date(time(trade_agg_qt)), agg_balance = as.matrix(trade_agg_qt),
                         china_balance = as.matrix(trade_china_qt)) %>%
    pivot_longer(cols = c(agg_balance, china_balance),
                 names_to = "series", values_to = "value")
  df_va <- data.frame(quarter = c(as.Date(time(va_manu_1997_2004_year)), as.Date(time(va_manu_2005_2024_qt))),
                      value_added = c(as.matrix(va_manu_1997_2004_year), as.matrix(va_manu_2005_2024_qt)))
  
  output$plot_inflation <- renderPlot(
    autoplot(cpi_inflation, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = cpi_end, y = tail(cpi_inflation, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = cpi_end, y = tail(cpi_inflation, 1),
               label = paste0(as.character(round(tail(cpi_inflation, 1)*100, digits = 1)), "%"),
               vjust = 2, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = 0.02, color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("1990-01-01"), y = 0.02, label = "Long-run Fed target",
                hjust = 0, vjust = 2, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(breaks = seq(-0.02,0.1,0.02), labels = scales::percent) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), cpi_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Inflation (%)",
        x = "Time (Quarterly)"
      ))
  
  output$text_inflation <- renderText({
    "Bringing down inflation was a key issue during the 2024 presidential election, and is a major goal for the administration. During a campaign speech, Donald Trump vowed that “starting on Day 1, we will end inflation and make America affordable again.” In Quarter 1, 2025, inflation stood at 2.7% The federal reserve’s inflation target is 2%."
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
                   breaks = c(head(year_breaks, -1), budget_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Fiscal Balance (Billions of Dollars)",
        x = "Time (Quarterly)"
      ))
  
  output$text_budget <- renderText({
    "During his first joint-address to congress, the president said that “in the near future, I want to do what has not been done in 24 years: balance the federal budget.” The administration aims to achieve this through a series of spending reductions that offset planned tax cuts. The budget deficit was $400 billion for Q1 2025."
  })
  
  output$plot_trade <- renderPlot(
    ggplot(df_trade, aes(x = quarter, y = value, color = series)) +
      geom_line() + scale_color_manual(name = "",
                                       values = c("agg_balance" = eig_colors[1], "china_balance" = eig_colors[2]),
                                       labels = c("All Foreign Trade", "Trade with Mainland China")) +
      # Add current level of aggregate trade
      geom_point(aes(x = trade_end, y = tail(trade_agg_qt, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = trade_end, y = tail(trade_agg_qt, 1),
               label = paste0(as.character(round(tail(trade_agg_qt, 1), digits = 1)), "B"),
               vjust = 2, color = eig_colors[1]) +
      
      # Add current level of China trade
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
                   breaks = c(head(year_breaks, -1), trade_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Trade Balance (Billions of Dollars)",
        x = "Time (Quarterly)"
      ))
  
  output$text_trade <- renderText({
    "The administration advocates for an “America First Trade Policy,” aimed at eliminating the trade deficit by raising tariffs on U.S. trading partners. As of Q4 2024, the aggregate US trade deficit stood at $193.6 billion. As of publication, China has the highest planned tariff rate of 125%. The trade deficit with China stands at $53.3 billion as of Quarter 4 2024, which the administration aims to bring to zero."
  })
  
  output$plot_const <- renderPlot(
    autoplot(construction_real, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = const_end, y = tail(construction_real, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = const_end, y = tail(construction_real, 1),
               label = paste0(as.character(round(tail(construction_real, 1), digits = 1)), "B"),
               vjust = -1, color = eig_colors[1]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), const_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Construction Spending (Billions of Dollars)",
        x = "Time (Quarterly)"
      ))
  
  output$text_const <- renderText({
    "The Trump administration aims to re-shore factories, with an emphasis on shipbuilding. Construction spending on manufacturing facilities was $45.8 billion in Q4 2024, which rose markedly during the Biden administration."
  })
  
  output$plot_va <- renderPlot(
    ggplot(df_va, aes(x = quarter, y = value_added)) +
      geom_line(color = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = va_end, y = tail(va_manu_2005_2024_qt, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = va_end, y = tail(va_manu_2005_2024_qt, 1),
               label = paste0(as.character(round(tail(va_manu_2005_2024_qt, 1), digits = 1)), "B"),
               vjust = -1, color = eig_colors[1]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), va_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Value Added (Billions of Dollars)",
        x = "Time (Quarterly)"
      ))
  
  output$text_va <- renderText({
    "White House trade policy aims to reverse the \"hollowing out of our manufacturing base\" and strengthen domestic manufacturing capacity by increasing the cost of foreign-manufactured goods. Real value added in manufacturing $2.4 trillion in Quarter 4, 2024, and has risen steadily over the past few decades."
  })

  ## Employment, native born men prime age ##
  output$plot_employment_lvl_native_prime <- renderPlot(
    autoplot(emp_lvl_prime_age_m, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = native_end, y = tail(emp_lvl_prime_age_m, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = native_end, y = tail(emp_lvl_prime_age_m, 1),
               label = paste0(as.character(round(tail(emp_lvl_prime_age_m, 1), digits = 1)), "M"),
               vjust = -1, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = mean(emp_lvl_prime_age_m[25:28]), color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("2001-01-01"), y = mean(emp_lvl_prime_age_m[25:28]),
               label = paste0("2000 level, before China joined the WTO", " = ", round(mean(emp_lvl_prime_age_m[25:28]), digits = 1), "M"),
               hjust = 0, vjust = -1, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), native_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Employment (Millions of Workers)",
        x = "Time (Quarterly)"
      ))
  
  output$text_employment_lvl_native_prime <- renderText({
    "During the election, JD Vance argued that \"we have seven million — just men, not even women, just men — who have completely dropped out of the labor force….we cannot have an entire American business community that is giving up on American workers and then importing millions of illegal laborers.\" The prime age employment rate for native-born men in Quarter 1 2025 was 42.4 million. We set the target to be 44 million, which is the level in 2000 before China joined the WTO."
  })
  
  ## Employment rate, native born men 16+ ##
  output$plot_employment_pop_native <- renderPlot(
    autoplot(emp_pop_ratio_m, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = native_end, y = tail(emp_pop_ratio_m, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = native_end, y = tail(emp_pop_ratio_m, 1),
               label = paste0(as.character(round(tail(emp_pop_ratio_m, 1)*100, digits = 1)), "%"),
               vjust = 2, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = mean(emp_pop_ratio_m[25:28]), color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("2001-01-01"), y = mean(emp_pop_ratio_m[25:28]),
               label = paste0("2000 level, before China joined the WTO", " = ", round(mean(emp_pop_ratio_m[25:28])*100, digits = 1), "%"),
               hjust = 0, vjust = -1, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), native_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Employment-to-Population Ratio (%)",
        x = "Time (Quarterly)"
      ))
  
  output$text_employment_pop_native <- renderText({
    "The Administration hopes to raise native-born employment in part by imposing more severe immigration restrictions and creating new jobs by restricting trade. The native-born male employment rate currently stands at 63.3%. We set the target to be 71.1%, which is the 2000 level before China joined the WTO."
  })
  
  
  output$plot_emp_manu <- renderPlot(
    autoplot(manu_qt, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = manu_end, y = tail(manu_qt, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = manu_end, y = tail(manu_qt, 1),
               label = paste0(as.character(round(tail(manu_qt, 1), digits = 1)), "M"),
               vjust = -1, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = mean(manu_qt[41:44]), color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("2000-01-01"), y = mean(manu_qt[41:44]),
               label = paste0("2000 level, before China joined the WTO", " = ", round(mean(manu_qt[41:44]), digits = 1), "M"),
               hjust = 0, vjust = -1, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), manu_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Employment (Millions of Workers)",
        x = "Time (Quarterly)"
        ))
  
  output$text_emp_manu <- renderText({
    "With the introduction of reciprocal tariffs on April 2nd, the president said that \"jobs and factories will come roaring back.\" Manufacturing employment stands at 12.8 million in Quarter 1 2025, down from the chosen target of 17.3 in 2000, the level before China joined the WTO in 2001."
  })
  
  output$plot_share_manu <- renderPlot(
    autoplot(manu_share, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = manu_end, y = tail(manu_share, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = manu_end, y = tail(manu_share, 1),
               label = paste0(as.character(round(tail(manu_share, 1)*100, digits = 1)), "%"),
               vjust = -1, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = mean(manu_share[41:44]), color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("2000-01-01"), y = mean(manu_share[41:44]),
               label = paste0("2000 level, before China joined the WTO", " = ", round(mean(manu_share[41:44])*100, digits = 1), "%"),
               hjust = 0, vjust = -1, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), manu_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Share of Private-Sector Workers (%)",
        x = "Time (Quarterly)"
        ))
  
  output$text_share_manu <- renderText({
    "With the introduction of reciprocal tariffs on April 2nd, the president said that \"jobs and factories will come roaring back.\" In  Quarter 1 2025 Manufacturing jobs made up 9.4% of employment, down from the chosen target of 15.5%, the level before China joined the WTO in 2001."
  })
  
  ## Employment, motor vehicles and parts ## 
  output$plot_motor_qt <- renderPlot(
    autoplot(motor_qt, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = manu_end, y = tail(motor_qt, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = manu_end, y = tail(motor_qt, 1),
               label = paste0(as.character(round(tail(motor_qt, 1), digits = 1)), "M"),
               vjust = 2, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = mean(motor_qt[41:44]), color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("2001-01-01"), y = mean(motor_qt[41:44]),
               label = paste0("2000 level, before China joined the WTO", " = ", round(mean(motor_qt[41:44]), digits = 1), "M"),
               hjust = 0, vjust = 2, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), native_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Employment (Millions of Workers)",
        x = "Time (Quarterly)"
      ))
  
  output$text_motor_qt <- renderText({
    "With the introduction of reciprocal tariffs on April 2nd, the president said that “jobs and factories will come roaring back.” There are 1.0 million vehicle-related manufacturing jobs, down from 1.3 million in 2000, the level before China joined the WTO in 2001."
  })
  
  ## Motor vehicles and parts share of private employment ##
  output$plot_motor_share <- renderPlot(
    autoplot(motor_share, ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = manu_end, y = tail(motor_share, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = manu_end, y = tail(motor_share, 1),
               label = paste0(as.character(round(tail(motor_share, 1)*100, digits = 1)), "%"),
               vjust = 2, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = mean(motor_share[41:44]), color = eig_colors[4]) +
      annotate(geom = "text", x = as.Date("2001-01-01"), y = mean(motor_share[41:44]),
               label = paste0("2000 level, before China joined the WTO", " = ", round(mean(motor_share[41:44])*100, digits = 1), "%"),
               hjust = 0, vjust = 2, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(limits = c(as.Date(as.yearqtr("1989 Q1")), as.Date(as.yearqtr("2026 Q2"))),
                   breaks = c(head(year_breaks, -1), native_end), labels = date2qt, expand = c(0,0)) +
      labs(
        y = "Share of Private-Sector Workers (%)",
        x = "Time (Quarterly)"
      ))
  
  output$text_motor_share <- renderText({
    "With the introduction of reciprocal tariffs on April 2nd, the president said that \"jobs and factories will come roaring back.\" Vehicle-related manufacturing jobs made up 0.7% of total U.S. jobs in Quarter 1 2025, down from 1.2% in 2000, the level before China joined the WTO in 2001."
  })
  
  ## Employment in manufacturing, counties most affected by the "China shock"  ##
  output$plot_china_shock <- renderPlot(
    autoplot(china_shock_yr , ts.colour = eig_colors[1]) +
      # Add current level
      geom_point(aes(x = 2022, y = tail(china_shock_yr, 1)), color = eig_colors[1], size = 1.5) +
      annotate(geom = "text", x = 2022, y = tail(china_shock_yr, 1),
               label = paste0(as.character(round(tail(china_shock_yr, 1), digits = 1)), "K"),
               vjust = 2, color = eig_colors[1]) +
      # Add policy target
      geom_hline(yintercept = china_shock_yr[11], color = eig_colors[4]) +
      annotate(geom = "text", x = 2001, y = china_shock_yr[11],
               label = paste0("2000 level, before China joined the WTO", " = ", round(china_shock_yr[11], digits = 1), "K"),
               hjust = 0, vjust = 2, color = eig_colors[4]) +
      theme_half_open() + background_grid(major = c("y"), minor = c("none")) +
      scale_x_continuous(limits = c(1989, 2023), breaks = c(seq(1990,2020,5), 2022)) +
      labs(
        y = "Employment (Thousands of Workers)",
        x = "Time (Annual)"
      ))
  
  output$text_china_shock <- renderText({
    "Identified by Autor et al. (2016), manufacturing employment in the 145 counties most impacted by trade with China are 0.47 million (2022). The target is 0.65 million, total employment in these counties before China joined the WTO in 2001."
  })
}

shinyApp(ui = ui, server = server)

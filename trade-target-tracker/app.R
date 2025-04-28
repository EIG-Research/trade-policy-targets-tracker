# Project: Trump Trade Policy Targets Dashboard
# File Description: R Shiny application

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
library(dichromat)
library(cowplot)

# plotly
library(plotly)

# R Shiny
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='economicinnovationgroup',
                          token='1E424A49864E72123BE5CAA19E6D2274',
                          secret='/OJ/Oy/GW2sk6ibHJt4JgoqzB80U03mcEyFJn0ev')

#################
### Load Data ###
#################
setwd("/Users/sarah/Documents/GitHub/trade-policy-targets-tracker/trade-target-tracker")
# Change to just file.path("cleaned_data", "fred_data.RData") when deploying online
load(file.path("cleaned_data", "fred_data.RData"))
load(file.path("cleaned_data", "bea_data.RData"))
load(file.path("cleaned_data", "cps_employment.RData"))
load(file.path("cleaned_data", "china_shock.RData"))

######################
### Build Shiny UI ###
######################

# Define EIG color palette
eig_colors <- c("#1a654d", "#5e9c86", "#008080", "#044140", "#e1ad28")	  # EIG theme colors

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
    h1("Trade Policy Dashboard")
  ),
  
  navset_card_tab(
    ### Trade Balance ###
    nav_panel("Goods Trade Balance", 
              fluidRow(
                column(8, plotlyOutput("plotly_trade"),
                       div(
                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                         HTML('Source: <a href="https://www.bea.gov/data/intl-trade-investment/international-trade-goods-and-services" target="_blank">Bureau of Economic Analysis,</a> seasonally adjusted, in 2017 dollars (adjusted using <a href="https://fred.stlouisfed.org/series/PCECTPI" target="_blank">PCE</a>). Aggregate data is available beginning in Q1 1992. China-specific data is  available beginning in Q1 1992; 1992 to 1998 data only includes goods but not services, from the Census Bureau.')
                       )
                ),  # Plot on the left
                
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  uiOutput("text_trade"))
                ))
    ),
    
    ## Manufacturing Employment ##
    nav_panel("Manufacturing Employment",
              navset_tab(
                ## Employment, manufacturing ##
                nav_panel("Manufacturing Employment Level", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_emp_manu"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://fred.stlouisfed.org/series/MANEMP" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                                   )
                            ),  # Plot on the left
                            
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_emp_manu"))
                            ))
                ),
                
                ## Manufacturing share of private employment ##
                nav_panel("Manufacturing Employment Share", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_share_manu"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://fred.stlouisfed.org/series/MANEMP" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                                   )
                            ),  # Plot on the left
                            
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_share_manu"))
                            ))
                ),
                
                ## Employment, motor vehicles and parts ## 
                nav_panel("Automotive Employment Level", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_motor_emp"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://fred.stlouisfed.org/series/CES3133600101" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                                   )
                            ),  # Plot on the left
                            
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_motor_emp"))
                            ))
                ),
                
                ## Motor vehicles and parts share of private employment ##
                nav_panel("Automotive Share", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_motor_share"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://fred.stlouisfed.org/series/CES3133600101" target="_blank">Bureau of Labor Statistics,</a> seasonally adjusted.')
                                   )
                            ),  # Plot on the left
                            
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_motor_share"))
                            ))
                ),
                
                ## Employment in manufacturing, counties most affected by the "China shock"  ##
                nav_panel("China Shock Jobs", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_china_shock"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://www.census.gov/programs-surveys/cbp.html" target="_blank"> Census Bureau County Business Patterns 1990-2022,</a> quarterly data is not available.')
                                   )
                            ),  # Plot on the left
                            
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_china_shock"))
                            ))
                )
              )
    ),
    
    ## Manufacturing Capacity ##
    nav_panel("Manufacturing Capacity",
              navset_tab(
                
                ## Real value added, manufacturing ##
                nav_panel("Value Added", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_va"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://www.bea.gov/itable/gdp-by-industry" target="_blank"> Bureau of Economic Analysis,</a>seasonally adjusted, in 2017 dollars. Available beginning in 1997; 1997 to 2004 data are annual.')
                                   )
                            ),  # Plot on the left
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_va"))
                            ))
                ),
                
                ## Manufacturing share of GDP##
                nav_panel("Value Added Share of GDP", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_va_share"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://www.bea.gov/itable/gdp-by-industry" target="_blank"> Bureau of Economic Analysis</a>. Available beginning in 1997; 1997 to 2004 data are annual.')
                                   )
                            ),  # Plot on the left
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_va_share"))
                            ))
                ),
                
                ## Total Private Construction Spending in Manufacturing ##
                nav_panel("Manufacturing Construction Spending", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_const"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://fred.stlouisfed.org/series/PRMFGCON" target="_blank"> Census Bureau,</a>  seasonally adjusted, in 2017 dollars (adjusted using <a href="https://apps.bea.gov/iTable/?reqid=19&step=2&isuri=1&categories=survey&_gl=1*885yn5*_ga*MTc5MDExNjA3OS4xNzQ0NzQxMTkx*_ga_J4698JNNFT*MTc0NTMzNTgyOS44LjEuMTc0NTMzNjQ4Mi41NS4wLjA.#eyJhcHBpZCI6MTksInN0ZXBzIjpbMSwyLDNdLCJkYXRhIjpbWyJjYXRlZ29yaWVzIiwiU3VydmV5Il0sWyJOSVBBX1RhYmxlX0xpc3QiLCIxNDQiXV19" target="_blank">Price Index for Private Fixed Investment in Manufacturing Structures</a> to be consistent with the Federal Reserve and the BEA). Available beginning Q1 1993.')
                                   )
                            ),  # Plot on the left
                            
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_const"))
                            ))
                ),
                
                ## Industrial Production, Manufacturing ##
                nav_panel("Industrial Production", 
                          fluidRow(
                            column(8, plotlyOutput("plotly_ind_prod"),
                                   div(
                                     style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                                     HTML('Source: <a href="https://fred.stlouisfed.org/series/IPGMFSQ" target="_blank"> Federal Reserve</a>, seasonally adjusted, 2017 basis = 100.')
                                   )
                            ),  # Plot on the left
                            column(4, div(
                              style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                              uiOutput("text_ind_prod"))
                            ))
                ),
              )
    ),
    
    
    ## Native Employment ##
#    nav_panel("Native Employment",
#              fluidRow(
#                column(8, plotlyOutput("plotly_employment_pop_native"),
#                       div(
#                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
#                         HTML('Source: <a href="https://cps.ipums.org/cps/index.shtml" target="_blank">Current Population Survey,</a> Quarterly averages of seasonally adjusted monthly rates.')
#                       )
#                ),  # Plot on the left
                
#                column(4, div(
#                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
#                  uiOutput("text_employment_pop_native"))
#                ))
              
#    ),
    
    ## Household Income ##
    nav_panel("Income",
              fluidRow(
                column(8, plotlyOutput("plotly_hh_income", height = "500px"),
                       div(
                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                         HTML('Source: <a href="https://fred.stlouisfed.org/series/MEHOINUSA672N" target="_blank" > Census Bureau,</a> in 2017 dollars (adjusted using the PCE), seasonally adjusted.')
                       )
                ),
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  uiOutput("text_hh_income")
                ))
              )
    ),
    
    ### Inflation ###
    nav_panel("Inflation",
              fluidRow(
                column(8,  plotlyOutput("plotly_inflation"),
                       div(
                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                         HTML('Source: <a href="https://fred.stlouisfed.org/series/PCECTPI" target="_blank">Bureau of Economic Analysis, Personal Consumption Expenditures: Chain-type Price Index</a>, 2017 basis, seasonally adjusted.')
                       )
                ),
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  uiOutput("text_inflation"))
                ))
    ),
    
    ### Federal Budget Balance ###
    nav_panel("Budget Balance", 
              fluidRow(
                column(8, plotlyOutput("plotly_budget"),
                       div(
                         style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                         HTML('Source: <a href="https://fred.stlouisfed.org/series/MTSDS133FMS" target="_blank">Department of the Treasury, Fiscal Service,</a> seasonally adjusted, in 2017 dollars (adjusted using <a href="https://fred.stlouisfed.org/series/CPIAUCSL" target="_blank">CPI-U</a>, following the Treasury Department method).')
                       )
                ),  # Plot on the left
                
                column(4, div(
                  style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                  uiOutput("text_budget"))
                ))
    )
    
  )
)

##########################
### Build Shiny Server ###
##########################

server <- function(input, output) {
  
  ## Inflation ##
  # convert inflation timeseries to dataframe
  inflation_df <- tibble(
    quarter = as.Date(as.yearqtr(time(pce_inflation))),
    inflation = as.numeric(pce_inflation)*100,
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_inflation <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(inflation_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    plot_ly(
      data = inflation_df,
      x = ~quarter,
      y = ~inflation,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Inflation rate (%)",
                     tickformat = ".0f",
                     ticksuffix = "%",
                     rangemode = "tozero"),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1]),
        
        # add horizontal line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = 2, y1 = 2,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # label for the 2% 
        annotations = list(
          list(
            xref = "paper",
            x = 0,
            y = 1.5,
            text = "Long-run Fed target (2%)",
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_inflation <- renderUI({
    HTML('<p>During a <a href="https://www.google.com/search?sca_esv=cc91aa7b516a412e&q=%22Starting+on+Day+1,+we+will+end+inflation+and+make+America+affordable+again%22+speech+august+2024&udm=39&fbs=ABzOT_CWdhQLP1FcmU5B0fn3xuWpmDtIGL1r84kuKz6yAcD_insVp1f9hFz8mUUtzTwQJFouCD7u3pHL14acV3Obfjf5O4Vw3Yj1b1LJCToA-0AtYv29Z1Q7pD9J5KIFLPeTfdotEyfFQrOPYEM53beMeRzUDW_IJGxB1vzIh9GVyeV_othw6NQyUH8FMOgZFA9tvALg9l3F7Mdscc9bI995RPinlUWBbQ&sa=X&ved=2ahUKEwi_h-7kw-yMAxXHCTQIHV3UKgQQs6gLegQIERAB&biw=1458&bih=909&dpr=1#fpstate=ive&ip=1&vld=cid:87eca06d,vid:A6ziIkgI6ao,st:0" target="_blank" >campaign speech</a> in August 2024, then-candidate Donald Trump vowed that “starting on Day 1 [of my administration], we will end inflation and make America affordable again,” though he also later <a href="https://apnews.com/article/trump-immigration-tariffs-pardons-abortion-prosecutions-riot-72c08269f0a870d20e29033319201e22" target="_blank">said</a> that he “can’t guarantee” that prices would not rise because of tariffs.<br><br>Heading into the Trump presidency, inflation was running at roughly 2.5 percent year-over-year, above the Federal Reserve’s target of 2 percent.</p>'
    )})
  
  ## Real Median Household Income ##
  output$plotly_hh_income <- renderPlotly({
    
    income_df <- tibble(
      quarter = as.Date(time(income_yr)),
      income = as.numeric(income_yr),
      hover_label = format(as.yearqtr(quarter), "%Y")
    )
    
    # grow at a rate of 3.4% a year.
     growth_rate <-0.034
    
    start_val <- income_df %>% filter(quarter == as.Date("2023-01-01")) %>% pull(income)
    start_year = 2023
    years <- 2024:2028
    
    future_df <- tibble(
      year = years,
      quarter = as.Date(paste0(years, "-01-01")),
      income = NA_real_,  # placeholder for missing actual values
      trend = start_val + start_val * growth_rate * (years - start_year),
      hover_label = as.character(year)
    ) %>% select(-c(year))
    
    income_df = income_df %>% mutate(trend = NA_real_)
    income_df = bind_rows(income_df, future_df)
    
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(income_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_texts <- as.character((tick_years))
    
    plot_ly(
      data = income_df,
      x = ~quarter,
      y = ~income,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      name = 'Actual',
      hovertemplate = "%{x}: %{y:$.0f}<extra></extra>",
      hoverlabel = list(bgcolor = eig_colors[1])
    ) %>%  
      add_lines(
        data = income_df,
        x = ~quarter,
        y = ~trend,
        name = 'Projection at prior Trump Administration rate',
        line = list(color = eig_colors[5], dash = 'dash'),
        hoverlabel = list(bgcolor = eig_colors[5])
      ) %>%
      layout(
        xaxis = list(title = "Time (Annual)",
                     tickvals = tick_years,
                     ticktext = tick_texts,
                     hoverformat = "%Y"),
        
        yaxis = list(title = "Household Median Income (Dollars)",
                     tickformat = ",.0f",
                     ticksuffix = ""),
        
        hovermode = "closest",
        
        legend = list(
          x = 0,          # 0 = left side
          y = 1,          # 1 = top side
          xanchor = "left",
          yanchor = "top",
          title = list(text = ""))
      )
  })
  
  output$text_hh_income <- renderUI({
    HTML('<p>“An increase in real median household income” was one of the explicit goals of the Trump administration’s <a href="https://ustr.gov/sites/default/files/files/reports/2025/President%20Trump%27s%202025%20Trade%20Policy%20Agenda.pdf" target="_blank">trade policy agenda.</a> The administration also celebrated the 3.4 percent annual growth rate of real median household income from 2016 to 2019, coinciding with President Trump’s first term.<br><br>We have thus set the target at 3.4 percent annual growth. Real median household income was $81,779 in 2023, the most recent year for which the data is available, and we will update this figure for 2024 once it is released by the Census Bureau.</p>')
  })
  
  
  ## Budget ##
  budget_df <- tibble(
    quarter = as.Date(as.yearqtr(time(budget_real))),
    budget = as.numeric(budget_real),
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_budget <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(budget_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    plot_ly(
      data = budget_df,
      x = ~quarter,
      y = ~budget,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:.1f}B<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Fiscal Balance (Billions of Dollars)",
                     tickformat = ",.0f",
                     ticksuffix = "",
                     rangemode = "tozero"),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1]),
        
        # add target
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = 0, y1 = 0,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # label for target 
        annotations = list(
          list(
            xref = "paper",
            x = 0.5,
            y = 75,
            text = "Target: Balanced Budget",
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_budget <- renderUI({
    HTML('<p>The administration has repeatedly argued that tariffs will be a significant source of revenue. On April 8th, 2025, six days after “Liberation Day,” President Trump <a href="https://www.reuters.com/world/us/trump-says-us-taking-2-billion-day-tariffs-2025-04-08/?utm_source=chatgpt.com" target="_blank">claimed<a/> that the United States was already bringing in $2 billion in tariff revenues daily. Separately, trade advisor Peter Navarro <a href="https://www.foxnews.com/video/6370789893112" target="_blank">argued</a> just prior to the enactment of the new tariffs that they would raise $600 billion. With a combination of tariff revenues and spending reductions to offset tax cuts, <a href="https://www.whitehouse.gov/remarks/2025/03/remarks-by-president-trump-in-joint-address-to-congress/" target="_blank">Trump hopes to balance the federal budget</a> for the first time in 24 years. The budget deficit was $400 billion for Q1 2025.</p>'
    )})
  
  
  ## Trade ##
  trade_agg_df <- tibble(
    quarter = as.Date(as.yearqtr(time(trade_agg_qt))),
    Total = as.numeric(trade_agg_qt)
  )
  
  trade_china_df <- tibble(
    quarter = as.Date(as.yearqtr(time(trade_china_qt))),
    China = as.numeric(trade_china_qt)
  )
  
  trade_df <- left_join(trade_agg_df, trade_china_df, by = "quarter") %>%
    pivot_longer(cols = c(Total, China),
                 names_to = "type",
                 values_to = "deficit") %>%
    mutate(hover_label = format(as.yearqtr(quarter), "%Y Q%q"))
  
  output$plotly_trade <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(trade_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    plot_ly(
      data = trade_df,
      x = ~quarter,
      y = ~deficit,
      color = ~type,
      colors = c("Total" =eig_colors[1], "China" = eig_colors[5]),
      type = 'scatter',
      mode = 'lines',
      text = ~type,
      hovertemplate = "%{x}: %{fullData.name}: %{y:$,.1f}B<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Trade Balance (Billions of Dollars)",
                     tickformat = ",.0f",
                     ticksuffix = "",
                     rangemode = "tozero"),
        
        legend = list(title = list(text = "Deficit Type")),
        
        hovermode = "closest",
        
        # add horizontal line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = 0, y1 = 0,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # label for balance 
        annotations = list(
          list(
            xref = "paper",
            x = 0.03,
            y = 10,
            text = "Target: Eliminate Goods Deficit",
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
    
  })
  
  output$text_trade <- renderUI({
    HTML('<p>The Trump administration has <a href = "https://ustr.gov/sites/default/files/files/reports/2025/President%20Trump%27s%202025%20Trade%20Policy%20Agenda.pdf" target="_blank">cited</a> as one of its goals a “decrease in the size of the trade in goods deficit,” adding that “reversing the flow of American wealth to foreign countries in the form of the trade deficit” would allow the U.S. to reclaim its technological, economic, and military edge. Such a reversal of capital flows would require running an American trade surplus. And in its <a href="https://web.archive.org/web/20250403054403/https://ustr.gov/issue-areas/reciprocal-tariff-calculations" target="_blanl">original calculation</a> of reciprocal tariffs, the Trump administration explicitly says that the tariffs were set at the level that would eliminate its trade deficit with each country, which if achieved would collectively also imply that its aggregate deficit with the world was also eliminated.<br><br> We have thus set the target deficit at zero, both for the bilateral deficit with China and the aggregate deficit with the rest of the world. We will update this target if the administration clarifies.</p>'
    )})
  
  ## Value Added ##
  va_df <- tibble(
    quarter = c(as.Date(time(va_manu_1997_2004_year)), as.Date(time(va_manu_2005_2024_qt))),
    value_added = c(as.matrix(va_manu_1997_2004_year), as.matrix(va_manu_2005_2024_qt)),
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  va_df_trend <- va_df %>%
    filter(quarter >= as.Date("2010-01-01"),
           quarter != as.Date("2020-04-01"))
  trend_model <- lm(value_added ~ as.numeric(quarter), data = va_df_trend)
  va_df <- va_df %>%
    mutate(trend = predict(trend_model, newdata = data.frame(quarter = as.numeric(quarter))))
  
  output$plotly_va <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(va_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1))  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    plot_ly(
      data = va_df,
      x = ~quarter,
      y = ~value_added,
      type = 'scatter',
      mode = 'lines',
      name = "Real Value Added Levels",
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:$,.0f}B<extra></extra>"
    ) %>%
      add_trace(
        data = va_df_trend,
        x = ~quarter,
        y = ~predict(trend_model),
        name = "Trendline after the Great Financial Crisis",
        line = list(color = eig_colors[4], width = 2, dash = "dash"),
        hoverinfo = "none",
        hovertemplate = NULL,
        showlegend = TRUE
      ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Value Added (Billions of Dollars)",
                     tickformat = ",.0f",
                     ticksuffix = ""),
        
        legend = list(
          x = 0,          # 0 = left side
          y = 1,          # 1 = top side
          xanchor = "left",
          yanchor = "top",
          title = list(text = "")
        ),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1])
      )
  })
  
  output$text_va <- renderUI({
    HTML('<p>The Trump administration <a href="https://www.whitehouse.gov/fact-sheets/2025/04/fact-sheet-president-donald-j-trump-declares-national-emergency-to-increase-our-competitive-edge-protect-our-sovereignty-and-strengthen-our-national-and-economic-security/#:~:text=Large%20and%20persistent%20annual%20U.S.,base%20dependent%20on%20foreign%20adversaries" target="_blank">argues</a> that the “decline in manufacturing output has reduced U.S. manufacturing capacity.” The White House also <a href="https://www.whitehouse.gov/fact-sheets/2025/04/fact-sheet-president-donald-j-trump-declares-national-emergency-to-increase-our-competitive-edge-protect-our-sovereignty-and-strengthen-our-national-and-economic-security/#:~:text=Large%20and%20persistent%20annual%20U.S.,base%20dependent%20on%20foreign%20adversaries" target="_blank">says</a> that goods trade deficits have led to the “hollowing out” of the manufacturing base and “resulted in a lack of incentive to increase advanced domestic manufacturing capacity.”<br><br>We have set the target for manufacturing value added, a measure of the sector’s contribution to the overall economy, at the growth rate since after the Great Financial Crisis.</p>'
    )})
  
  ## Share of Value Added as a Percentage of GDP##
  share_va_df <- tibble(
    quarter = c(as.Date(time(share_va_manu_1997_2004_year)), as.Date(time(share_va_manu_2005_2024_qt))),
    share_gdp = c(as.matrix(share_va_manu_1997_2004_year), as.matrix(share_va_manu_2005_2024_qt)),
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_va_share <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(share_va_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1))  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    y_lvl <- share_va_df %>% filter(quarter == "2000-01-01") %>% .$share_gdp
    
    plot_ly(
      data = share_va_df,
      x = ~quarter,
      y = ~share_gdp,
      type = 'scatter',
      mode = 'lines',
      name = "Value Added Share of GDP",
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:,.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Value Added Share of GDP (%)",
                     tickformat = ",.0f",
                     ticksuffix = "%"),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1])
        
        
        # add horizontal line
  #      shapes = list(
   #       list(
  #          type = "line",
  #          xref = "paper",
   #         x0 = 0, x1 = 1,
  #          y0 = y_lvl, y1 = y_lvl,
   #         line = list(color = eig_colors[4], width = 2, dash = "dash")
  #        )
  #      ),
        
        # label for balance 
   #     annotations = list(
  #        list(
  #          xref = "paper",
   #         x = 0.12,
  #          y = y_lvl+0.25,
  #          text = paste0("2000 level, before China joined the WTO = ",round(y_lvl,1),"%"),
  #          showarrow = FALSE,
  #          font = list(color = eig_colors[4], size = 14),
  #          xanchor = "left",
  #          yanchor = "middle"
  #        )
  #      )
      )
  })
  
  output$text_va_share <- renderUI({
    HTML('<p>The Trump administration has included “an increase in the manufacturing sector’s share of gross domestic product” as one of the specific goals of its <a href="https://www.wita.org/atp-research/trade-policy-agenda-report/" target="_blank">trade agenda.</a><br><br>We have not set a precise target for this indicator, but we will be monitoring it for a sustained reversal of the long-term downward trend, in accordance with the stated goal of the administration.</p>'
    )})
  
  ## Construction Spending ##
  const_df <- tibble(
    quarter = as.Date(time(construction_real)),
    const_spending = as.numeric(construction_real),
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_const <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(const_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    
    y_lvl <- const_df %>% filter(quarter == "2024-10-01")
    y_lvl <- as.numeric(y_lvl[1,2])
    
    plot_ly(
      data = const_df,
      x = ~quarter,
      y = ~const_spending,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:$,.1f}B<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Construction Spending (Billions of Dollars)",
                     tickformat = ",.0f",
                     ticksuffix = ""),
        
        # add target line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = y_lvl, y1 = y_lvl,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # label for target 
        annotations = list(
          list(
            xref = "paper",
            x = 0.4,
            y = y_lvl+1,
            text = paste0("Q4 2024 level: ",round(y_lvl,1),"B"),
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        ),
        
        legend = list(title = list(text = "Manufacturing Value Added")),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1])
        
      )
  })
  
  output$text_const <- renderUI({
    HTML('<p>The Trump administration has <href ="https://ustr.gov/sites/default/files/files/reports/2025/President%20Trump%27s%202025%20Trade%20Policy%20Agenda.pdf" target="_blank">bemoaned</a> that “over 100,000 factories closed between 1997 and 2016.” President Trump has also <a href="https://www.nytimes.com/2025/04/03/business/economy/trump-tariffs-us-manufacturing-economy.html" target="_blank">said</a> that owing to his new trade policy, “Jobs and factories will come roaring back into our country.”

Spending on factory construction had already climbed steeply in the years before the Trump presidency, so we have set the target at maintaining those high levels.
</p>'
    )})
  
  ## Industrial Production ##
  ind_prod_df <- tibble(
    quarter = as.Date(as.yearqtr(time(ipman_qt))),
    ip = as.numeric(ipman_qt),
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_ind_prod <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(ind_prod_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    y_lvl <- ind_prod_df %>% filter(quarter == "2007-10-01") %>% .$ip
    
    plot_ly(
      data = ind_prod_df,
      x = ~quarter,
      y = ~ip,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:.1f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Industrial Production Index",
                     tickformat = ".0f",
                     ticksuffix = ""),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1]),
        
        # add target line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = y_lvl, y1 = y_lvl,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # add label for target
        annotations = list(
          list(
            xref = "paper",
            x = 0.02,
            y = y_lvl + 2,
            text = paste0("2007 Q4 peak, before the Great Financial Crisis = " , round(y_lvl, 1)),
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_ind_prod <- renderUI({
    HTML('<p>The Trump administration, in declaring a <a href="https://www.whitehouse.gov/fact-sheets/2025/04/fact-sheet-president-donald-j-trump-declares-national-emergency-to-increase-our-competitive-edge-protect-our-sovereignty-and-strengthen-our-national-and-economic-security/#:~:text=Large%20and%20persistent%20annual%20U.S.,base%20dependent%20on%20foreign%20adversaries." target="_blank">national emergency</a> to justify the use of tariffs, cited the decline in manufacturing output as one of the reasons tariffs are needed. Goods trade deficits “have led to the hollowing out of our manufacturing base; resulted in a lack of incentive to increase advanced domestic manufacturing capacity; undermined critical supply chains; and rendered our defense-industrial base dependent on foreign adversaries.“<br><br>We set the target to be a return to the pre-financial crisis peak of 106.1, which had been reached after a long upward trajectory.</p>'
    )})
  
  ## Employment rate, native born 16+ ##
  emp_pop_ratio_df = tibble(
    quarter = as.Date(as.yearqtr(time(emp_pop_ratio_m))),
    emp_pop = as.numeric(emp_pop_ratio_m)*100,
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_employment_pop_native <- renderPlotly({
    
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(emp_pop_ratio_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 2)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    y_lvl = emp_pop_ratio_df %>% mutate(year = lubridate::year(quarter)) %>%
      filter(year == 2000) %>% summarise(mean(emp_pop))
    y_lvl = as.numeric(y_lvl[1,1])
    
    plot_ly(
      data = emp_pop_ratio_df,
      x = ~quarter,
      y = ~emp_pop,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:,.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Employment-to-Population Ratio (%)",
                     tickformat = ".0f",
                     ticksuffix = "%"),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1]),
        
        # add horizontal line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = y_lvl, y1 = y_lvl,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # label for balance 
        annotations = list(
          list(
            xref = "paper",
            x = 0.25,
            y = y_lvl+0.4,
            text = paste0("2000 level, before China joined the WTO = ",round(y_lvl,1),"%"),
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
    
  })
  
  output$text_employment_pop_native <- renderUI({
    HTML('<p>According to Vice President J.D. Vance, the Trump administration <a href="https://www.nytimes.com/2024/10/12/magazine/jd-vance-interview.html" target="_blank"> hopes</a> to raise native-born employment in part by imposing more severe immigration restrictions and creating new jobs by restricting trade. NEC director <a href = "https://www.whitehouse.gov/articles/2025/04/sunday-shows-president-trumps-bold-vision-for-economic-prosperity/" target  "_blank"> has also said: </a>"China entered the WTO in 2000. In the 15 years that followed, real incomes declined about $1,200 cumulatively over that time … We got the cheap goods at the grocery store, but then we had fewer jobs.” The native-born employment rate currently stands at 59.4 percent. We set the target to be 64.5 percent, which is the 2000 level before China joined the WTO.</p>'
    )})
  
  ## Employment Manufacturing ##
  manu_df = tibble(
    quarter = as.Date(as.yearqtr(time(manu_qt))),
    manufacturing = as.numeric(manu_qt),
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_emp_manu <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(manu_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    # Add ticks
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    y_lvl = manu_df %>% mutate(year = lubridate::year(quarter)) %>%
      filter(year == 2000) %>% summarise(y = mean(manufacturing))
    y_lvl = as.numeric(y_lvl[1,1])
    
    plot_ly(
      data = manu_df,
      x = ~quarter,
      y = ~manufacturing,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:,.1f}M<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Employment (Millions of Workers)",
                     tickformat = ",.0f",
                     ticksuffix = ""),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1]),
        
        # add target line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = y_lvl, y1 = y_lvl,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # add label for target
        annotations = list(
          list(
            xref = "paper",
            x = 0.27,
            y = y_lvl + 0.2,
            text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"M"),
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_emp_manu <- renderUI({
    HTML('<p>“Using trade policy to increase the number of manufacturing jobs in our Country… will help raise wages and return our country to one with a more vibrant and secure middle class,” according to the Trump administration’s <a href="https://ustr.gov/sites/default/files/files/reports/2025/President%20Trump%27s%202025%20Trade%20Policy%20Agenda.pdf" target="_blank">2025 Trade Policy Agenda,</a> which further laments that manufacturing jobs “declined from 17 million in 1993 to 12 million in 2016.”<br><br>We have set the target at 17.3 million manufacturing jobs, a level slightly below the mid-1990s peak but from just before China’s entry into the WTO — which the administration has repeatedly blamed for <a href="https://www.whitehouse.gov/fact-sheets/2025/04/fact-sheet-president-donald-j-trump-declares-national-emergency-to-increase-our-competitive-edge-protect-our-sovereignty-and-strengthen-our-national-and-economic-security/#:~:text=Large%20and%20persistent%20annual%20U.S.,base%20dependent%20on%20foreign%20adversaries." target="_blank">lower incomes</a> and <a href="https://www.whitehouse.gov/articles/2025/04/sunday-shows-president-trumps-bold-vision-for-economic-prosperity/" target="_blank">lost jobs.</a></p>'
    )})
  
  ## Manufacturing Share ##
  
  manu_share_df = tibble(
    quarter = as.Date(as.yearqtr(time(manu_share))),
    manufacturing_share = as.numeric(manu_share) *100,
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_share_manu <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(manu_share_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    y_lvl = manu_share_df %>% mutate(year = lubridate::year(quarter)) %>%
      filter(year == 2000) %>% summarise(y = mean(manufacturing_share))
    y_lvl = as.numeric(y_lvl[1,1])
    
    plot_ly(
      data = manu_share_df,
      x = ~quarter,
      y = ~manufacturing_share,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:,.1f}%<extra></extra>") %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Share of Private-Sector Workers (%)",
                     tickformat = ".0f",
                     ticksuffix = "%"),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1])
        
        # add target line
       # shapes = list(
      #    list(
      #      type = "line",
      #      xref = "paper",
      #      x0 = 0, x1 = 1,
      #      y0 = y_lvl, y1 = y_lvl,
      #      line = list(color = eig_colors[4], width = 2, dash = "dash")
      #    )
      #  ),
        
        # add label for target
       # annotations = list(
      #    list(
      #      xref = "paper",
      #      x = 0.29,
      #      y = y_lvl + 0.3,
      #      text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"%"),
      #      showarrow = FALSE,
      #      font = list(color = eig_colors[4], size = 14),
      #      xanchor = "left",
      #      yanchor = "middle"
      #    )
      #  )
      )
  })
  
  output$text_share_manu <- renderUI({
    HTML('<p>“Using trade policy to increase the number of manufacturing jobs in our Country… will help raise wages and return our country to one with a more vibrant and secure middle class,” according to the Trump administration’s <href="https://ustr.gov/sites/default/files/files/reports/2025/President%20Trump%27s%202025%20Trade%20Policy%20Agenda.pdf" target="_blank">2025 Trade Policy Agenda.</a><br><br>The administration has not set an explicit target for the manufacturing share of total jobs in the labor force, but we include the indicator because we think it will be worth monitoring for a sustained reversal of its decades-long decline.</p>'
    )})
  
  ## Employment, motor vehicles and parts ## 
  
  motor_df = tibble(
    quarter = as.Date(as.yearqtr(time(motor_qt))),
    motor_level = as.numeric(motor_qt),
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_motor_emp <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(motor_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    y_lvl = motor_df %>% mutate(year = lubridate::year(quarter)) %>%
      filter(year == 2000) %>% summarise(y = mean(motor_level))
    y_lvl = as.numeric(y_lvl[1,1])
    
    plot_ly(
      data = motor_df,
      x = ~quarter,
      y = ~motor_level,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:,.1f}M<extra></extra>") %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Employment (Millions of Workers)",
                     tickformat = ".1f",
                     ticksuffix = ""),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1]),
        
        # add target line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = y_lvl, y1 = y_lvl,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # add label for target
        annotations = list(
          list(
            xref = "paper",
            x = 0.33,
            y = y_lvl + 0.025,
            text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"M"),
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_motor_emp <- renderUI({
    HTML('<p>The auto sector is a particular focus of President Trump’s trade agenda, which <a href="https://www.whitehouse.gov/fact-sheets/2025/04/fact-sheet-president-donald-j-trump-declares-national-emergency-to-increase-our-competitive-edge-protect-our-sovereignty-and-strengthen-our-national-and-economic-security/#:~:text=Large%20and%20persistent%20annual%20U.S.,base%20dependent%20on%20foreign%20adversaries." target="_blank">seeks</a> “better-paying American jobs making beautiful American-made cars.” And, for example, among the reasons given for deploying tariffs is to counter the “variety of non-tariff barriers that impede access to the Japanese and Korean automotive markets, including non-acceptance of certain U.S. standards, duplicative testing and certification requirements, and transparency issues.”<br><br>To be consistent with total manufacturing employment, we have set the target for auto employment at the level from just before China’s entry into the WTO, or 1.3 million auto manufacturing jobs.

         
         </p>'
    )})
  
  ## Motor vehicles and parts share of private employment ##
  
  motor_share_df = tibble(
    quarter = as.Date(as.yearqtr(time(motor_share))),
    motor_share = as.numeric(motor_share)*100,
    hover_label = format(as.yearqtr(quarter), "%Y Q%q")
  )
  
  output$plotly_motor_share <- renderPlotly({
    # Dynamically generate tick dates: Q1 every 5 years
    date_range <- range(motor_share_df$quarter)
    start_year <- lubridate::year(date_range[1])
    end_year   <- lubridate::year(date_range[2])
    
    tick_years <- c(start_year,
                    seq((start_year %/% 5 + 1)*5, end_year %/% 5*5, by = 5))
    tick_dates <- c(as.Date(paste0(tick_years, "-01-01")),
                    tail(date_range, 1)) %>% unique()  # Q1 of each year
    tick_texts <- as.character(as.yearqtr(tick_dates))
    
    y_lvl = motor_share_df %>% mutate(year = lubridate::year(quarter)) %>%
      filter(year == 2000) %>% summarise(y = mean(motor_share))
    y_lvl = as.numeric(y_lvl[1,1])
    
    plot_ly(
      data = motor_share_df,
      x = ~quarter,
      y = ~motor_share,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:,.1f}%<extra></extra>") %>%
      layout(
        xaxis = list(title = "Time (Quarterly)",
                     tickvals = tick_dates,
                     ticktext = tick_texts,
                     hoverformat = "%Y Q%q",
                     range = c(tick_dates[1], tick_dates[length(tick_dates)])),
        
        yaxis = list(title = "Share of Private-Sector Workers (%)",
                     tickformat = ".1f",
                     ticksuffix = "%"),
        
        hovermode = "closest",
        hoverlabel = list(bgcolor = eig_colors[1])
        
        # add target line
  #      shapes = list(
  #        list(
  #          type = "line",
  #          xref = "paper",
  #          x0 = 0, x1 = 1,
  #          y0 = y_lvl, y1 = y_lvl,
  #          line = list(color = eig_colors[4], width = 2, dash = "dash")
  #        )
  #      ),
        
        # add label for target
   #     annotations = list(
  #        list(
  #          xref = "paper",
  #          x = 0.31,
  #          y = y_lvl + 0.02,
  #          text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"%"),
  #          showarrow = FALSE,
  #          font = list(color = eig_colors[4], size = 14),
  #          xanchor = "left",
  #          yanchor = "middle"
  #        )
  #      )
      )
  })
  
  output$text_motor_share <- renderUI({
    HTML('<p>The auto sector is a particular focus of President Trump’s trade agenda, which <a href="https://www.whitehouse.gov/fact-sheets/2025/04/fact-sheet-president-donald-j-trump-declares-national-emergency-to-increase-our-competitive-edge-protect-our-sovereignty-and-strengthen-our-national-and-economic-security/#:~:text=Large%20and%20persistent%20annual%20U.S.,base%20dependent%20on%20foreign%20adversaries." target="_blank">seeks</a> “better-paying American jobs making beautiful American-made cars.” And, for example, among the reasons given for deploying tariffs is to counter the “variety of non-tariff barriers that impede access to the Japanese and Korean automotive markets, including non-acceptance of certain U.S. standards, duplicative testing and certification requirements, and transparency issues.”<br><br>The administration has not established a set target for either the total manufacturing share of U.S. jobs or the equivalent share for auto jobs, so we have not added one. But we will monitor this indicator for any meaningful increase.</p>'
    )})
  
  ## Employment in manufacturing, counties most affected by the "China shock"  ##
  china_shock_df <- tibble(year = time(china_shock_yr),
                           cs_manu_emp = as.numeric(china_shock_yr),
                           hover_label = as.character(year))
  
  output$plotly_china_shock <- renderPlotly({
    
    # Dynamically generate tick dates: Q1 every 5 years
    start_year <- china_shock_df$year[1]
    end_year   <- china_shock_df$year[nrow(china_shock_df)]
    
    tick_years <- c(seq(start_year, end_year, by = 5), end_year)
    tick_texts <- as.character(tick_years)
    
    y_lvl <- china_shock_df %>% filter(year == 2000) %>% .$cs_manu_emp
    
    plot_ly(
      data = china_shock_df,
      x = ~year,
      y = ~cs_manu_emp,
      type = 'scatter',
      mode = 'lines',
      line = list(color = eig_colors[1], width = 2),
      text = ~hover_label,
      hovertemplate = "%{x}: %{y:,.1f}K<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Time (Annual)",
                     tickvals = tick_years,
                     hoverformat = "%Y Q%q",
                     range = c(tick_years[1], tick_years[length(tick_years)])),
        
        yaxis = list(title = "Employment (Thousands of Workers)",
                     tickformat = ".0f",
                     ticksuffix = ""),
        
        hovermode = "closest",
        
        # add target line
        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0, x1 = 1,
            y0 = y_lvl, y1 = y_lvl,
            line = list(color = eig_colors[4], width = 2, dash = "dash")
          )
        ),
        
        # add label for target
        annotations = list(
          list(
            xref = "paper",
            x = 0.29,
            y = y_lvl + 8.5,
            text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"K"),
            showarrow = FALSE,
            font = list(color = eig_colors[4], size = 14),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_china_shock <- renderUI({
    HTML('<p>As <a href="https://www.nber.org/papers/w21906" target="_blank">estimated</a> by economists David Autor, David Dorn, and Gordon H. Hanson, manufacturing employment in the 145 counties most impacted by trade with China was 473 thousand jobs as of 2022, the most recent available data. 

We have set the target at 649 thousand jobs, the total employment in these counties just before China joined the WTO in 2001. The administration has repeatedly blamed China’s entry into the WTO for <a href="https://www.whitehouse.gov/fact-sheets/2025/04/fact-sheet-president-donald-j-trump-declares-national-emergency-to-increase-our-competitive-edge-protect-our-sovereignty-and-strengthen-our-national-and-economic-security/#:~:text=Large%20and%20persistent%20annual%20U.S.,base%20dependent%20on%20foreign%20adversaries." target="_blank">lower incomes</a> and <a href="https://www.whitehouse.gov/articles/2025/04/sunday-shows-president-trumps-bold-vision-for-economic-prosperity/" target="_blank">lost jobs</a> in the United States.</p>'
    )})
}

shinyApp(ui = ui, server = server)


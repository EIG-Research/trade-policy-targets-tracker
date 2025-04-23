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
setwd("/Users/jiaxinhe/Documents/projects/trade-policy-targets-tracker/trade-target-tracker")

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
    
    ## Household Income ##
    nav_panel("Income",
              fluidRow(
                column(8, plotlyOutput("plotly_hh_income"),
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
    

    ## Native Employment ##
    nav_panel("Native Employment",
                  fluidRow(
                    column(8, plotlyOutput("plotly_employment_pop_native"),
                           div(
                             style = "padding-top: 8px; text-align: left; font-size: 12px; color: #555;",
                             HTML('Source: <a href="https://cps.ipums.org/cps/index.shtml" target="_blank">Current Population Survey,</a> Quarterly averages of seasonally adjusted monthly rates.')
                           )
                     ),  # Plot on the left
                    
                    column(4, div(
                      style = "display: flex; justify-content: center; align-items: center; height: 400px;",
                      uiOutput("text_employment_pop_native"))
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
        nav_panel("Manufacturing Share", 
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
        nav_panel("Manufacturing Employment - China Shock", 
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
    
    ## Manufacturing Output ##
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
        nav_panel("Construction Spending", 
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
                         HTML('Source: <a href="https://fred.stlouisfed.org/series/MTSDS133FMS" target="_blank">Department of the Treasury, Fiscal Service,</a> seasonally adjusted, in 2017 dollars (adjusted using <a href="https://fred.stlouisfed.org/series/CPIAUCSL" target="_blank">CPI-U</a>, following the Treasury Department method)')
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
            y = 1.75,
            text = "Long-run Fed target (2%)",
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_inflation <- renderUI({
    HTML('<p>During a <a href="https://www.google.com/search?sca_esv=cc91aa7b516a412e&q=%22Starting+on+Day+1,+we+will+end+inflation+and+make+America+affordable+again%22+speech+august+2024&udm=39&fbs=ABzOT_CWdhQLP1FcmU5B0fn3xuWpmDtIGL1r84kuKz6yAcD_insVp1f9hFz8mUUtzTwQJFouCD7u3pHL14acV3Obfjf5O4Vw3Yj1b1LJCToA-0AtYv29Z1Q7pD9J5KIFLPeTfdotEyfFQrOPYEM53beMeRzUDW_IJGxB1vzIh9GVyeV_othw6NQyUH8FMOgZFA9tvALg9l3F7Mdscc9bI995RPinlUWBbQ&sa=X&ved=2ahUKEwi_h-7kw-yMAxXHCTQIHV3UKgQQs6gLegQIERAB&biw=1458&bih=909&dpr=1#fpstate=ive&ip=1&vld=cid:87eca06d,vid:A6ziIkgI6ao,st:0" target="_blank"> campaign speech</a> in August 2024, Donald Trump vowed that “starting on Day 1, we will end inflation and make America affordable again.” In Q4 2024, inflation stood at 2.5 percent. The Federal Reserve’s inflation target is 2 percent.</p>'
  )})
  
  ## Real Median Household Income ##
  
  income_df <- tibble(
    quarter = as.Date(time(income_yr)),
    income = as.numeric(income_yr),
    hover_label = format(as.yearqtr(quarter), "%Y")
  )
  
  income_df_trend <- income_df %>%
    # get relevant years - trump up until break.
    filter(quarter >= as.Date("2017-01-01"),
           quarter < as.Date("2020-01-01"))
  
  trend_model <- lm(income ~ as.numeric(quarter), data = income_df_trend)
  
  # project 2024 and 2025
  growth_rate <- coef(trend_model)["as.numeric(quarter)"]*365
  
  start_val <- income_df %>% filter(quarter == as.Date("2023-01-01")) %>% pull(income)
  
  start_year = 2023
  years <- 2024:2028
  
  future_df <- tibble(
    year = years,
    quarter = as.Date(paste0(years, "-01-01")),
    trend = start_val + growth_rate * (years - start_year)
  )
  
  income_df = bind_rows(income_df, future_df)
  
  output$plotly_hh_income <- renderPlotly({
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
      hovertemplate = "%{x}: %{y:.1f}B<extra></extra>"
    ) %>%  
      add_lines(
        data = income_df,
        x = ~quarter,
        y = ~trend,
        name = 'Projection at prior Trump Administration rate',
        line = list(color = eig_colors[5], dash = 'dash')
      ) %>%
      add_trace(
      data = income_df_trend,
      x = ~quarter,
      y = ~predict(trend_model),
      name = "Income Trendline during Trump's first Administration",
      line = list(color = eig_colors[4], width = 2, dash = "dash"),
      hoverinfo = "none",
      hovertemplate = NULL,
      showlegend = TRUE
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
        hoverlabel = list(bgcolor = eig_colors[1])
      )
  })
  
  output$text_hh_income <- renderUI({
    HTML('<p>As part of its <a href = "https://www.wita.org/atp-research/trade-policy-agenda-report/" target = "_blank" > trade policy agenda,</a> the administration aims to boost real median household income, attributing sluggish growth in the early 2000s to China’s accession to the WTO and asserting that strengthening Trump’s first-term trade restrictions will accelerate income growth. Real median household income in was $81,779 in 2023. We set the benchmark to be the pre-COVID growth rate during Trump’s first term.</p>')
    
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
            x = 0.75,
            y = 45,
            text = "Target: Balanced Budget",
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_budget <- renderUI({
    HTML('<p><a href="https://www.whitehouse.gov/remarks/2025/03/remarks-by-president-trump-in-joint-address-to-congress/" target="_blank"> During</a> his first address to a joint session of Congress on March 4, 2025, the president said, “In the near future, I want to do what has not been done in 24 years: balance the federal budget.” The administration aims to achieve this through a series of spending reductions that more than offset planned tax cuts. The budget deficit was $400 billion for Q1 2025.</p>'
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
            x = 0.01,
            y = 5,
            text = "Target: Eliminate Deficit",
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
    
  })
  
  output$text_trade <- renderUI({
    HTML('<p>The administration <a href="https://ustr.gov/sites/default/files/files/reports/2025/President%20Trump%27s%202025%20Trade%20Policy%20Agenda.pdf?utm_source=chatgpt.com" taraget="_blank"> advocates</a> for an “America First Trade Policy,” aimed at eliminating the trade deficit by raising tariffs on U.S. trading partners. As of Q4 2024, the aggregate U.S. trade deficit stood at $201 billion, while the bilateral trade deficit with China stands at $53 billion. The administration aims to bring both down to zero.</p>'
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
    HTML('<p>White House <a href = "https://www.whitehouse.gov/fact-sheets/2025/04/report-to-the-president-on-the-america-first-trade-policy-executive-summary/?utm_source=chatgpt.com" target="_blank"> trade</a> policy aims to reverse the “hollowing out of our manufacturing base” and strengthen domestic manufacturing capacity by increasing the cost of foreign-manufactured goods. Real value added in manufacturing was $2.4 trillion in Q4, 2024, and has risen steadily over the past few decades. We set the target to be the growth rate since Q1 2010.</p>'
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
            x = 0.12,
            y = y_lvl+0.15,
            text = paste0("2000 level, before China joined the WTO = ",round(y_lvl,1),"%"),
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_va_share <- renderUI({
    HTML('<p>To be filled</p>'
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
        
        # label for balance 
        annotations = list(
          list(
            xref = "paper",
            x = 0.4,
            y = y_lvl+0.8,
            text = paste0("Q4 2024 level: ",round(y_lvl,1),"B"),
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
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
    HTML('<p>The Trump administration aims to <a href="https://www.whitehouse.gov/presidential-actions/2025/04/restoring-americas-maritime-dominance/" target="_blank"> re-shore factories,</a> with an emphasis on shipbuilding. Construction spending on manufacturing facilities was $37.7 billion in Q4 2024. As construction spending rose markedly during the Biden administration, it is difficult to set a target.</p>'
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
                     ticksuffix = "",
                     rangemode = "tozero"),
        
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
            x = 0.4,
            y = y_lvl + 2,
            text = paste0("2007 Q4 peak, before the Great Financial Crisis = " , round(y_lvl, 1)),
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_ind_prod <- renderUI({
    HTML('<p>To be filled</p>'
    )})
  
  ## Employment rate, native born men 18+ ##
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
            y = y_lvl+0.3,
            text = paste0("2000 level, before China joined the WTO = ",round(y_lvl,1),"%"),
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
    
  })
  
  output$text_employment_pop_native <- renderUI({
    HTML('<p>According to Vice President J.D. Vance, the Trump Administration <a href="https://www.nytimes.com/2024/10/12/magazine/jd-vance-interview.html" target="_blank"> hopes</a> to raise native-born employment in part by imposing more severe immigration restrictions and creating new jobs by restricting trade. The native-born employment rate currently stands at 59.4 percent. We set the target to be 64.5 percent, which is the 2000 level before China joined the WTO.</p>'
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
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_emp_manu <- renderUI({
    HTML('<p>With the introduction of reciprocal tariffs on April 2nd, President Trump <a href = "https://www.nytimes.com/2025/04/03/business/economy/trump-tariffs-us-manufacturing-economy.html" target="_blank"> said</a> that “jobs and factories will come roaring back.” Manufacturing employment stands at 12.8 million in Q1 2025, down from the chosen target of 17.3 in 2000, the level from just before China joined the WTO in 2001.</p>'
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
            x = 0.29,
            y = y_lvl + 0.3,
            text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"%"),
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_share_manu <- renderUI({
    HTML('<p>With the introduction of reciprocal tariffs on April 2nd, President Trump <a href = "https://www.nytimes.com/2025/04/03/business/economy/trump-tariffs-us-manufacturing-economy.html" target="_blank"> said</a> that “jobs and factories will come roaring back.” In Q1 2025, manufacturing jobs made up 9.4 percent of employment, down from the chosen target of 15.5 percent, the level from just before China joined the WTO in 2001.</p>'
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
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_motor_emp <- renderUI({
    HTML('<p>With the introduction of reciprocal tariffs on April 2nd, President Trump <a href = "https://www.nytimes.com/2025/04/03/business/economy/trump-tariffs-us-manufacturing-economy.html" target="_blank"> said</a> that “jobs and factories will come roaring back.” There are 1.0 million vehicle-related manufacturing jobs, down from 1.3 million in 2000, the level from just before China joined the WTO in 2001.</p>'
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
            x = 0.31,
            y = y_lvl + 0.015,
            text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"%"),
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_motor_share <- renderUI({
    HTML('<p>With the introduction of reciprocal tariffs on April 2nd, President Trump <a href = "https://www.nytimes.com/2025/04/03/business/economy/trump-tariffs-us-manufacturing-economy.html" target="_blank"> said</a> that “jobs and factories will come roaring back.” Vehicle-related manufacturing jobs made up 0.7 percent of total U.S. jobs in Q1 2025, down from 1.2 percent in 2000, the level from just before China joined the WTO in 2001.</p>'
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
            x = 0.28,
            y = y_lvl + 8,
            text = paste0("2000 level, before China joined the WTO = " , round(y_lvl, 1),"K"),
            showarrow = FALSE,
            font = list(color = eig_colors[2], size = 12),
            xanchor = "left",
            yanchor = "middle"
          )
        )
      )
  })
  
  output$text_china_shock <- renderUI({
    HTML('<p>Identified by <a href = "https://www.nber.org/papers/w21906" target="_blank"> Autor et al. (2016),</a> manufacturing employment in the 145 counties most impacted by trade with China is 473 thousand jobs (2022). The target is set at 649 thousand, the total employment in these counties just before China joined the WTO in 2001.</p>'
      )})
}

shinyApp(ui = ui, server = server)

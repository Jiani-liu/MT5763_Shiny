library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(jsonlite)
library(httr)
library(htmltools)
library(htmlwidgets)
library(geojsonio)
library(furrr)

# data wrangling
library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)

# visualization
library(ggplot2)
library(plotly)
library(scales)
library(glue)
library(leaflet)
library(DT)

ui<-dashboardPage(
  dashboardHeader(title = "COVID-19 Real Time Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Mapper", icon = icon("globe-africa"), tabName = "map"),  
      menuItem(text = "Search by country", icon = icon("search-location"), tabName = "bycountry"),
      menuItem(text = "Trend", icon = icon("chart-line"), tabName = "trend"),
      menuItem(text = "Comparison", icon = icon("chart-bar"), tabName = "stat"),
      menuItem(text = "Data", icon = icon("table"), tabName = "data"))),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    shinyDashboardThemes(
      theme = "blue_gradient"),
    tabItems(
      
      # Map
      tabItem(tabName = "map", align = "center",
              
              fluidRow(
                column(width = 3, 
                       valueBoxOutput("world_confirmed",width = 12)),
                
                column(width = 3, 
                       valueBoxOutput("world_death",width = 12)),
                
                column(width = 3, 
                       valueBoxOutput("world_recover",width = 12)),
                
                column(width = 3, 
                       valueBoxOutput("world_affect",width = 12))),
              
              fluidRow( 
                column(10,leafletOutput("map_leaflet", width = "100%",height = "600px"),
                       br(),
                       h6("Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", align = "left"),
                       br()),
                column(2, box(width = 20,
                              h3(paste("Global Cases")),
                              hr(),
                              tags$head(
                                tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                              h5(textOutput("clean_date_reactive"), align = "left"),
                              
                              br(),
                              plotOutput("epi_curve", height="130px", width="100%"),
                              br(),
                              plotOutput("cumulative_plot", height="130px", width="100%"),
                              br(),
                              span(tags$i(h6("Noted:Reported cases are subject to significant variation in testing policy and capacity between countries.",align = "left")), 
                                   style="color:#045a8d"),
                              br(),
                              actionButton(inputId = 'refresh', label = "Refresh now"),
                              p(class = "text-muted",
                                br(), align = "left", 
                                "Source data updates every hour.")
                ))
              )
              
      ),
      # search by country
      tabItem(tabName = "bycountry",
              fluidPage(
                box(title = "Country Data",width = 12,
                    div(style="text-align: center;",
                        div(
                          style="display: inline-block;",
                          tags$span(htmlOutput("ranking"), style="position:relative; top:-1em")),
                        div(
                          style="padding: 15px;width: 50%; display: inline-block;",
                          uiOutput("countries_list",width = "100%"))),
                    
                    tags$br(),
                    infoBoxOutput("country_confirmed"),
                    infoBoxOutput("country_deaths"),
                    infoBoxOutput("country_recovered"),
                    tags$br(),
                    
                    div(id="country_dt",
                        DT::dataTableOutput("country_data") %>% 
                          withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                    # Download csv file Button
                    downloadButton("downloadData", "Download Data",style="float:right;")),
                h6("Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University"))),
      # Statistics
      tabItem(tabName = "stat",
              
              h2("Covid-19 Comparative Case", align = "center"),
              
              uiOutput("date_list", align = "center"),
              
              fluidRow(
                column(6, plotlyOutput("stat_death")),
                column(6, plotlyOutput("stat_recover"))),
              h6("Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University")),
      # Trend
      tabItem(tabName = "trend", 
              
              h2("COVID-19 Region Plots", align = "center"),
              uiOutput("countries_list2",width="100%", align = "center"),
              
              plotlyOutput("trend_line", height = "500px"),
              span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
              span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
              h6("Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University")),
      
      
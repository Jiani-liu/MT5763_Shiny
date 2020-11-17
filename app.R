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
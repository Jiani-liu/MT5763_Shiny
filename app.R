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
  
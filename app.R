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
      # Data
      tabItem(tabName = "data", 
              
              h2("COVID-19 Data"),
              
              br(),
              
              DT::dataTableOutput("countries_in_date_table"),
              
              br(),
              
              downloadButton(outputId = "download", label = "Download",style="float:right;"),
              
              br(), 
              
              h6("Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University"))
      
      
    )
  )
)


server <- (function(input, output,session) {
  
  #Refresh data for every hour
  autoInvalidate <- reactiveTimer(1000*60*60)
  observe({
    autoInvalidate()
    print("Refreshing...")
    
    ## Confirmed
    case_confirmed <- readFunc("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv","case")
    
    ## Recovery
    case_recover <- readFunc("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv","recover")
    
    ## Death
    case_death <- readFunc("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv","death")
    
    ## Merge/Join data.frame
    covid <- case_confirmed %>% 
      left_join(case_recover) %>% 
      left_join(case_death) %>% 
      mutate(cfr = death/case)  %>% 
      mutate(current_confirmed = case  - death - recover) %>% 
      rename(state = `Province/State`,
             country = `Country/Region`) %>% 
      filter(!(country %in% c("Diamond Princess", "MS Zaandam")))  
    
    # Date min
    min_date <- min(covid$date)
    
    # Date max
    max_date <- max(covid$date)
    
    # Replace NA with 0
    covid <- replace_NA(covid)
    
    # Data from the latest date only
    
    covid_update <- covid %>% 
      filter(date == max_date) 
    
    
    covid_update %<>% select(c(date,state,country, case, recover, death, cfr, current_confirmed,
                               Lat,Long)) %>%
      mutate(cfr_percent = percent(cfr,accuracy = 0.01)) %>%
      mutate(txt=paste0('<b>',country,' ','</span>','</b>',state,
                        '<hr style = "margin-top: 5px;margin-bottom: 5px">',
                        'Total cases: ','<b>', format(case, big.mark=" "),'</b>','</span>',
                        '<br>',
                        'Active cases: ','<b>', format(current_confirmed, big.mark=" "),'</b>','</span>',
                        '<br>',
                        'Recovered: ','<b>', format(recover, big.mark=" "),'</b>','</span>',
                        '<br>',
                        'Deaths: ','<b>', format(death, big.mark=" "),'</b>','</span>',
                        '<br>',
                        'CFR: ','<b>', format(cfr_percent, big.mark=" "),'</b>','</span>'
      ))
    
    map <- leaflet(width="100%") %>% 
      addTiles() %>% 
      setView(lng = 0, 
              lat = 25,
              zoom = 2) %>% 
      addCircleMarkers(covid_update$Long,
                       covid_update$Lat,
                       radius=(covid_update$case)^(1/5)+2, stroke=F,
                       color='red', fillOpacity=0.3,
                       popup=covid_update$txt)
   
     # The last cases in the world
    world <- covid_update %>% summarise(case = sum(case, na.rm = TRUE),
                                        current_confirmed = sum(current_confirmed, na.rm = TRUE),
                                        death = sum(death, na.rm = TRUE),
                                        recover = sum(recover, na.rm = TRUE))
    # Cases around the world group by date
    world_per_date <- covid %>% group_by(date) %>% summarise(case = sum(case, na.rm = TRUE),
                                                             current_confirmed = sum(current_confirmed, na.rm = TRUE),
                                                             death = sum(death, na.rm = TRUE),
                                                             recover = sum(recover, na.rm = TRUE))
    country_per_date <- covid %>% group_by(date,country) %>% summarise(case = sum(case, na.rm = TRUE),
                                                                       current_confirmed = sum(current_confirmed, na.rm = TRUE),
                                                                       death = sum(death, na.rm = TRUE),
                                                                       recover = sum(recover, na.rm = TRUE))
    ## Daily increase in deaths and recovered cases
    # Set NA on increases on day 1
    data <- country_per_date
    data %<>% arrange(country, date)
    n <- nrow(data)
    day1 <<- min(data$date)
    data %<>% mutate(new.confirmed = ifelse(date == day1, NA, case - lag(case, n=1)),
                     new.deaths = ifelse(date == day1, NA, death - lag(death, n=1)),
                     new.recoverd = ifelse(date == day1, NA, recover - lag(recover, n=1)))
    # Change negative numbers to 0
    data %<>% mutate(new.confirmed = ifelse(new.confirmed < 0, 0, new.confirmed),
                     new.deaths = ifelse(new.deaths < 0, 0, new.deaths),
                     new.recoverd = ifelse(new.recoverd < 0, 0, new.recoverd))
    # Increase in deaths based on total deaths and confirmed cases
    data %<>% mutate(rate.upper = (100 * death / (death + recover)) %>% round(1))
    # Lower bound for deaths based on confirmed cases
    data %<>% mutate(rate.lower = (100 * death / case) %>% round(1))
    # Mortality rate per day
    data %<>% mutate(rate.daily = (100 * new.deaths / (new.deaths + new.recoverd)) %>% round(1))
    
    data[is.na(data)] <- 0
    
    # Prepare data for making the plots
    data.long <- data %>%
      select(c(country, date, case, current_confirmed, recover, death)) %>%
      gather(key=type, value=count, -c(country, date))
    
    data.long %<>% mutate(type=recode_factor(type, Case='Total Confirmed',
                                             current.confirmed='Current Confirmed',
                                             recover='Recovered',
                                             death='Deaths'))
    
    # Data frame for the rates of each country
    rates.long <- data %>%
      select(c(country, date, rate.upper, rate.lower, rate.daily)) %>%
      gather(key=type, value=count, -c(country, date))
    
    rates.long %<>% mutate(type=recode_factor(type, rate.daily='Daily',
                                              rate.lower='Lower bound',
                                              rate.upper='Upper bound'))
    
    
      
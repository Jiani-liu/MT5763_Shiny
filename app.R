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
    
    options(scipen = 999)
    
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
    
    ## Top 10 countries
    # Ranking by confirmed cases after selecting the current date
    data.latest.all <- data %>% filter(date == max_date) %>%
      select(country, date,
             case, new.confirmed, current_confirmed,
             recover, death, new.deaths, death.rate=rate.lower) %>% 
      mutate(ranking = dense_rank(desc(case)))
    # dataTable
    dt <- data.latest.all %>% filter(country!='World')
    dt$ranking <- NULL
    dt <- dt %>% mutate(ranking = dense_rank(desc(case)))
    dt$death.rate <- paste(dt$death.rate,"%" , sep=" ")
    
    dt <- dt[order(dt$ranking),]
    country_list <- dt$country %>% as.character()
    # Selecting top 10 countries including World (11)
    k <- 10
    top.countries <- data.latest.all %>% filter(ranking <= k+1) %>%
      arrange(ranking) %>% pull(country) %>% as.character()
    
    
    # put all other countries in a single group of others
    data.latest <- data.latest.all %>% filter(!is.na(country)) %>%
      mutate(country=ifelse(ranking <= k + 1, as.character(country), 'Others')) %>%
      mutate(country=country %>% factor(levels=c(top.countries)))
    data.latest %<>% group_by(country) %>%
      summarise(case=sum(case), new.confirmed=sum(new.confirmed),
                current_confirmed=sum(current_confirmed),
                recovere=sum(recover), death=sum(death), new.deaths=sum(new.deaths)) %>%
      mutate(death.rate=(100 * death/case) %>% round(1))
    data.latest %<>% select(c(country, case, death, death.rate,
                              new.confirmed, new.deaths, current_confirmed))
    
    # Preparing data for plots
    data.latest.long <- data.latest  %>%
      gather(key=type, value=count, -country)
    data.latest.long %<>% mutate(type=recode_factor(type,
                                                    case='Total Confirmed',
                                                    death='Total Deaths',
                                                    death.rate='Death Rate (%)',
                                                    new.confirmed='New Confirmed (compared with one day before)',
                                                    new.deaths='New Deaths (compared with one day before)',
                                                    current_confirmed='Current Confirmed'))
  })
  
  ## GLOBAL
  
  #valueboxoutputs
  output$world_confirmed <- renderValueBox({
    autoInvalidate()
    valueBox(subtitle = "Number of Cases", 
             color = "red",
             value = number(sum(covid_update$case, na.rm = T), accuracy = 1, big.mark = ","),
             icon = icon("virus"))
  })
  
  output$world_death <- renderValueBox({
    autoInvalidate()
    valueBox(subtitle = "Number of Death", 
             color = "black",
             value = number(sum(covid_update$death, na.rm = T), accuracy = 1, big.mark = ","),
             icon = icon("book-dead"))
  })
  
  output$world_recover <- renderValueBox({
    autoInvalidate()
    valueBox(subtitle = "Number of Recovery", 
             color = "green",
             value = number(sum(covid_update$recover, na.rm = T), accuracy = 1, big.mark = ","),
             icon = icon("heartbeat"))  
    
  })
  output$world_affect <- renderValueBox({
    autoInvalidate()
    valueBox(subtitle = "Number of Countries/regions affected", 
             color = "yellow",
             value = number(nrow(subset(covid_update, country!="Diamond Princess Cruise Ship"))),
             icon = icon("globe"))
  })
  
  # map
  output$map_leaflet <- renderLeaflet({
    autoInvalidate()
    map})
  
  # date
  output$clean_date_reactive <- renderText({
    paste("Last Updated:",covid_update$date[1])
  })
  
  # plot 1
  output$cumulative_plot <- renderPlot({
    autoInvalidate()
    ggplot(world_per_date,aes(date, case/1000000, color="brown")) + 
      geom_line() + geom_point(size = 1, alpha = 0.8)+
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      scale_x_date(labels = date_format(format = "%b")
      ) + theme_bw() +
      ylab("cumulative cases (million)") + theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
                                                 plot.margin = margin(5, 12, 5, 5))
  })
  
  # plot 2
  output$epi_curve <- renderPlot({
    autoInvalidate()
    ggplot(world_per_date,aes(date, current_confirmed/1000000, fill="brown")) + 
      geom_bar(position="stack", stat="identity") + 
      ylab("new cases (million)")+
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      scale_x_date(labels = date_format(format = "%b")
      ) + theme_bw() +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
            plot.margin = margin(5, 12, 5, 5))
    
  })
  
  # Trend Line
  output$trend_line <- renderPlotly({
    autoInvalidate()
    p1 <- country_per_date %>% 
      filter(country %in% input$trend_country) %>% 
      ggplot(aes(date, case, color = country, group = country,
                 
                 text = glue("Country : {paste0(country)}
                     Date : {date}
                     Case : {number(case, big.mark = ',')}
                     Current confirmed : {number(current_confirmed,big.mark = ',')}
                     Recovery : {number(recover, big.mark = ',')}
                     Death : {number(death, big.mark = ',')}")
      )) +
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      scale_x_date(date_breaks = "1 month",
                   labels = date_format(format = "%b")
      ) +
      geom_line() +
      labs(title = "COVID-19 Case by Country",
           x = NULL,
           y = "Number of Cases",
           color = "country"
      ) +
      theme_bw() +
      theme_algo
    
    ggplotly(p1, tooltip = "text")  
  })
  
  # selection UI
  output$countries_list2<-renderUI({
    autoInvalidate()
    pickerInput("trend_country", "Country/Region:",   
                choices = country_list, 
                options = list('actions-box' = TRUE, 'none-selected-text'= "Please make a selection!"),
                selected = country_list[1:3],
                multiple = TRUE)
    
  })
  
  ##Comparison
  
  #date
  output$date_list<-renderUI({
    autoInvalidate()
    dateInput("date", label = "Select Date", value = as.Date(max_date))
  })
  
  # Bar Chart
  output$stat_death <- renderPlotly({
    autoInvalidate()
    p1 <- covid %>% 
      filter(date == (input$date)) %>% 
      drop_na(case, recover, death) %>% 
      group_by(country) %>% 
      summarise(death = sum(death)) %>% 
      arrange(desc(death)) %>% 
      slice(1:10) %>% 
      ggplot(aes(death, reorder(country, death), fill = death,
                 
                 text = glue("Country : {country}
                         Death : {number(death, big.mark = ',')}")
      )) +
      geom_col(color = "firebrick") +
      theme_minimal() +
      labs(x = "Number of Death",
           y = NULL,
           title = "Top Countries Based on Number of Death"
      ) +
      scale_fill_gradient(low = "lightyellow", high = "firebrick") +
      scale_x_continuous(labels = number_format(big.mark = ",")) +
      theme(legend.position = "none")
    
    ggplotly(p1, tooltip = "text")
  })
  
  output$stat_recover <- renderPlotly({
    autoInvalidate()
    p1 <- covid %>% 
      filter(date == (input$date)) %>% 
      drop_na(case, recover, death) %>%
      group_by(country) %>% 
      summarise(recover = sum(recover)) %>% 
      arrange(desc(recover)) %>% 
      slice(1:10) %>% 
      ggplot(aes(recover, reorder(country, recover), fill = recover,
                 
                 text = glue("Country : {country}
                                               Recover : {number(recover, big.mark = ',')}")
      )) +
      geom_col(color = "#3bac01") +
      theme_minimal() +
      labs(x = "Number of Recovery",
           y = NULL,
           title = "Top Countries Based on Number of Recovery"
      ) +
      scale_fill_gradient(low = "lightyellow", high = "#3bac01") +
      scale_x_continuous(labels = number_format(big.mark = ",")) +
      theme(legend.position = "none")
    
    ggplotly(p1, tooltip = "text")
  })
  
  
  ## Search by country
  output$countries_list<-renderUI({
    autoInvalidate()
    hide(id = "loading-content", anim = TRUE, animType = "fade") 
    selectInput("country","Choose a country", "Select a country:", choices= country_list)
  })
  
  # diplay ranking
  output$ranking <- renderUI({
    autoInvalidate()
    if(!is.null(input$country)){
      country <- dt %>% filter(country == input$country)
      HTML(paste('<b style = "font-size: 20px">Ranking: ',country$ranking,'</b>'))
    }
  })
  
  # InfoBoxOutputs
  output$country_confirmed <- renderInfoBox({
    autoInvalidate()
    if(!is.null(input$country))
    {
      country <- countryCurrentData()
      infoBox(
        tags$b("Confirmed"),
        country$case,
        tags$span("Current cases: ",tags$b(country$current_confirmed),style = "font-size: 14px"),
        icon = icon("procedures"),
        color = "red", fill = TRUE
      )
    }
    else
    {
      infoBox(
        "Confirmed", "Loading...", icon = icon("procedures"),
        color = "red", fill = TRUE
      )
    }
  })
  
  output$country_deaths <- renderInfoBox({
    autoInvalidate()
    if(!is.null(input$country))
    {
      country <- dt %>% filter(country == input$country)
      infoBox(
        tags$b("Deaths"),
        country$death,
        tags$span("Death rate: ",tags$b(country$death.rate),style = "font-size: 14px"),
        icon = icon("bed"),
        color = "black", fill = TRUE
      )
    }
    else
    {
      infoBox(
        "Deaths", "Loading...", icon = icon("procedures"),
        color = "black", fill = TRUE
      )
    }
  })
  
  output$country_recovered <- renderInfoBox({
    autoInvalidate()
    if(!is.null(input$country))
    {
      country <- countryCurrentData()
      infoBox(
        tags$b("Recovered"),
        country$recover,
        icon = icon("heartbeat"),
        color = "green", fill = TRUE
      )
    }
    else
    {
      infoBox(
        "Recovered", "Loading...", icon = icon("heartbeat"),
        color = "green", fill = TRUE
      )
    }
  })
  
  
  output$confirmed <- renderValueBox({
    autoInvalidate()
    valueBox(
      format(world$case, big.mark=" "),icon = icon("procedures"), tags$span(
        tags$b("Confirmed", style = "font-size: 20px"),
        tags$br(),
        tags$span("New Confirmed: ", style = "font-size: 13px",tags$b(format(data.world.date.max$new.confirmed, big.mark=" "))),
        tags$br(),
        tags$span("Current Confirmed: ", style = "font-size: 13px",tags$b(format(data.world.date.max$current_confirmed, big.mark=" ")))
      ),
      
      color = "orange"
    )
  })
  
  output$deaths <- renderValueBox({
    autoInvalidate()
    valueBox(
      format(world$death, big.mark=" "), tags$span(
        tags$b("Deaths", style = "font-size: 20px"),
        tags$br(),
        tags$span("New Deaths: ", style = "font-size: 13px",tags$b(format(data.world.date.max$new.deaths, big.mark=" "))),
        tags$br(),
        tags$span("Daily Rate: ", style = "font-size: 13px",tags$b(data.world.date.max$rate.daily," %"))
      ),
      icon = icon("bed"),
      color = "red"
    )
  })
  
  output$recovered <- renderValueBox({
    autoInvalidate()
    valueBox(
      format(world$recover, big.mark=" "), tags$span(
        tags$b("Recovered", style = "font-size: 20px"),
        tags$br(),
        tags$br(),
        tags$span("New Recovered: ", style = "font-size: 13px",tags$b(format(data.world.date.max$new.recoverd, big.mark=" "))),
        tags$br()
      ), 
      icon = icon("heartbeat"),
      color = "lime"
    )
  })
  
  
  countryData <- reactive({
    autoInvalidate()
    country <- getSelectedCountryData(input$country)
    country$case <- format(country$case, big.mark=" ")
    country$new.confirmed <- format(country$new.confirmed, big.mark=" ")
    country$current_confirmed <- format(country$current_confirmed, big.mark=" ")
    country$recover <- format(country$recover, big.mark=" ")
    country$death <- format(country$death, big.mark=" ")
    country$new.deaths <- format(country$new.deaths, big.mark=" ")
    country
    
  })
  
  countryCurrentData <- reactive({
    autoInvalidate()
    country <- countryData() %>% filter(date == max_date)
    country
  })
  
  output$country_data <- DT::renderDataTable({
    autoInvalidate()
    if(!is.null(input$country)){
      country <- countryData()
      DT::datatable(country[, c("date","new.confirmed","new.deaths","new.recoverd")],
                    rownames = FALSE, options = list(pageLength = 10, order = list(list(0, 'desc')))
      )
    }
    
  })
  
  ## data
  output$countries_in_date_table <- DT::renderDataTable({
    autoInvalidate()
    dt$case <- format(dt$case, big.mark=" ")
    dt$new.confirmed <- format(dt$new.confirmed, big.mark=" ")
    dt$current_confirmed <- format(dt$current_confirmed, big.mark=" ")
    dt$recover <- format(dt$recover, big.mark=" ")
    dt$death <- format(dt$death, big.mark=" ")
    dt$new.deaths <- format(dt$new.deaths, big.mark=" ")
    DT::datatable(dt[, c("ranking","country", "case","new.confirmed","current_confirmed","recover", "death","new.deaths","death.rate")],
                  rownames = FALSE, options = list(pageLength = 50, order = list(list(2, 'desc')))
    )
  })
  
  # Download csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$country, ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(countryData(), file, row.names = TRUE)
      contentType = "text/csv"
    }
  )  
  
  # Download
  output$download <- downloadHandler( 
    filename = function() {paste("data-", Sys.Date(), ".csv", sep="")},
    content = function(file) {
      write.csv(covid, file, row.names = T)
    })
  
  # Refresh
  observe({
    if(input$refresh){session$reload()} }) 
})

shinyApp(ui, server)
  
      
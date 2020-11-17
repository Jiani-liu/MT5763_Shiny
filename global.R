
# function to read data and rename columns
readFunc <- function(filePath,label) { 
  read.csv(url(filePath),check.names = F) %>%
    pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), 
                 names_to = "date", values_to = label) %>% 
    mutate(date = mdy(date))
}

# Replace NA with 0
replace_NA <- function(filename){
  # Identify numeric columns
  i <- sapply(filename, is.numeric)
  # Creation of a temporary dataframe
  filenameNA <- filename[i]
  # Replace NA with 0
  filenameNA %<>% mutate_all(~replace(., is.na(.), 0))
  # Modifier countries dataframe
  filename$case <- filenameNA$case
  filename$death <- filenameNA$death
  filename$recover <- filenameNA$recover
  filename$cfr <- filenameNA$cfr
  filename$current_confirmed <- filenameNA$current_confirmed
  return(filename)
}

# Get data by selected country
dataLong <- function(countryName){
  long <- data.long %>% filter(country == countryName)
  return(long)
}

# Get country data by name
selectCountryData <- function(CountryName){
  data.country <- data %>% filter(country == CountryName)
  data.country[is.na(data.country)] <- 0
  return(data.country)
}

# Get all data for a selected country
getSelectedCountryData <- function(CountryName){
  country <- data %>% filter(country == CountryName)
  country$country <- NULL
  return(country)
}


# Current confirmed cases and new daily confirmed cases
currentConfirmedCases <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  # n <- nrow(selected.data)
  plot <- ggplot(selected.data, aes(x=date, y=current_confirmed)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('Current Confirmed Cases from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# Select daily new confirmed cases by country and return the plot
DailyNewConfirmedCases <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  # n <- nrow(selected.data)
  plot <- ggplot(selected.data, aes(x=date, y=new.confirmed)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('Daily New confirmed Cases from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# Select new deaths by country and return the plot
newDeaths <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  plot <- ggplot(selected.data, aes(x=date, y=new.deaths)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('New Deaths from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# Select new recovered cases by country and return the plot
newRecoveredCases <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  plot <- ggplot(selected.data, aes(x=date, y=new.recoverd)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('New Recovered Cases from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# Theme for Visualization

theme_algo <- theme(
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = "gray80"),
  panel.grid.minor = element_blank(),
  plot.title = element_text(family = "serif", size = 18)
)


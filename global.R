
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

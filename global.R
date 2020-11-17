
# function to read data and rename columns
readFunc <- function(filePath,label) { 
  read.csv(url(filePath),check.names = F) %>%
    pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), 
                 names_to = "date", values_to = label) %>% 
    mutate(date = mdy(date))
}
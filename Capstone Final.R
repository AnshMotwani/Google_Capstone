library(tidyverse)
library(data.table)
library(viridis)
library(ggpubr)
install.packages("leaflet")
library(leaflet)
install.packages("htmlwidgets")
library(htmlwidgets)
library(htmltools)
library(janitor)
library(lubridate)
library(scales)
library(knitr)


df_oneyear_cleaned <- fread("df_oneyear_cleaned.csv")

# Create a data frame which groups number of trips by station name and includes latitude and longitude coordinates for each station
map_data <- df_oneyear_cleaned %>%
  select(start_station_name, start_lat, start_lng ) %>% 
  group_by(start_station_name) %>%
  mutate(numtrips = n()) %>% 
  distinct( start_station_name, .keep_all = TRUE)

map_data1 <- df_oneyear_cleaned %>%
  select(start_station_name, start_lat, start_lng ) %>% 
  group_by(start_station_name)

view(map_data1)

# Create a sequence of values which will act as the key shown on the leaflet map to group stations which have a similar number of trips occurring together
mybins <- seq(0, 70000, by = 10000)

# Assign the viridis colour palette to visually show how popular a station is 
mypalette <- colorBin( palette ="viridis",domain = map_data$numtrips,na.color = "transparent", bins = mybins )

# Prepare text to be used in a tooltip so that users can interact with the coloured markers on the map
mytext <- paste("Station name: ", map_data$start_station_name, "<br/>", "Number of trips: ", map_data$numtrips, sep = "" ) %>% lapply(htmltools::HTML)

# Create an interactive html leaflet widget to show the most popular stations

p1 <- leaflet(map_data) %>% addTiles() %>%    
  setView( lng = -87.6298, lat = 41.8781, zoom = 11.5 ) %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addCircleMarkers(
    ~ start_lng, ~ start_lat, 
    fillColor = ~ mypalette(numtrips), 
    fillOpacity = 0.7, 
    color = "white", 
    radius = 8, 
    stroke = FALSE,
    label = mytext,
    labelOptions = labelOptions(
      style = list( 
        "font-weight" = "normal", 
        padding = "3px 8px"
      ), 
      textsize = "13px", 
      direction = "auto"
    ) 
  ) %>%
  addLegend( 
    pal = mypalette, 
    values = ~ numtrips, 
    opacity = 0.9,
    title = "Number of trips", 
    position = "bottomright"
  )

p1

### 3.2 Most popular time of year 

# Arrange weekdays in order 
df_oneyear_cleaned$day_of_week <- ordered(
  df_oneyear_cleaned$day_of_week, 
  levels = c(
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
  )
)
# Create data frame that summarises the number of trips by date 
heat_map_data <- df_oneyear_cleaned %>%
  select(
    YMD, 
    day_of_week, 
    week, 
    year
  ) %>%
  group_by(
    YMD
  ) %>%
  mutate(
    numtrips = n()
  ) %>%
  distinct(
    YMD, 
    .keep_all = TRUE
  )






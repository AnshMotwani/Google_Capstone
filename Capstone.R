### 1.Loading the libraries and data

##loading the libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(knitr)
library(data.table)

##loading the CSV files into dataframes
df_aug21 <- read.csv("202108-divvy-tripdata.csv")
df_sep21 <- read.csv("202109-divvy-tripdata.csv")
df_oct21 <- read.csv("202110-divvy-tripdata.csv")
df_nov21 <- read.csv("202111-divvy-tripdata.csv")
df_dec21 <- read.csv("202112-divvy-tripdata.csv")
df_jan22 <- read.csv("202201-divvy-tripdata.csv")
df_feb22 <- read.csv("202202-divvy-tripdata.csv")
df_mar22 <- read.csv("202203-divvy-tripdata.csv")
df_apr22 <- read.csv("202204-divvy-tripdata.csv")
df_may22 <- read.csv("202205-divvy-tripdata.csv")
df_jun22 <- read.csv("202206-divvy-tripdata.csv")
df_jul22 <- read.csv("202207-divvy-tripdata.csv")

### 2.Combining and Altering Datasets

##combing the dataframes into a single dataframe
df_oneyear <- rbind(df_aug21,df_sep21,df_oct21,df_nov21,df_dec21,df_jan22,df_feb22,df_mar22,df_apr22,df_may22,df_jun22,df_jul22)
dim(df_oneyear)


##converting the dates from char to date format
df_oneyear$started_at <- lubridate::ymd_hms(df_oneyear$started_at)
df_oneyear$ended_at <- lubridate::ymd_hms(df_oneyear$ended_at)

##converting station_id to char from int
df_oneyear <- mutate( df_oneyear, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
str(df_oneyear)


## 3.Preparing the Data set

##created new columns for calculations
df_oneyear$start_time_h <- lubridate::hour(df_oneyear$started_at)
df_oneyear$end_time_h <- lubridate::hour(df_oneyear$ended_at)

##created new column for total ride time taken
df_oneyear$time_taken <- difftime(df_oneyear$ended_at , df_oneyear$started_at ,  units = "mins")
df_oneyear$rounded_time_taken <- round(df_oneyear$time_taken , digits = 0)

df_oneyear$start_date <- as.Date(df_oneyear$started_at)
df_oneyear$end_date <- as.Date(df_oneyear$ended_at)

##ordering by date
df_oneyear <- df_oneyear %>% arrange(started_at)

##3.1 summarizing by date

# Year 
df_oneyear$year <- format(df_oneyear$started_at, "%Y")

# Month 
df_oneyear$month <- format(df_oneyear$started_at, "%m" )

# Week 
df_oneyear$week <- format(df_oneyear$started_at, "%W" )

# Day
df_oneyear$day <- format(df_oneyear$started_at, "%d" )

# Day of week 
df_oneyear$day_of_week <- format( df_oneyear$started_at, "%A" )

# Date, YYYY-MM-DD
df_oneyear$YMD <- format( df_oneyear$started_at, "%Y-%m-%d" )

# Time of Day, HH:MM:SS
df_oneyear$ToD <- format( df_oneyear$started_at, "%H:%M:%S" )


# 4.Cleaning the data set


##removing empty rows and columns
df_oneyear <- janitor::remove_empty(df_oneyear, which = c("cols"))
df_oneyear <- janitor::remove_empty(df_oneyear, which = c("rows"))
dim(df_oneyear)
str(df_oneyear)

#Adding the required columns in the clean data frame
df_oneyear_cleaned <- df_oneyear[c("ride_id","rideable_type","started_at","ended_at","start_station_name","start_station_id","end_station_name","end_station_id","start_lat","start_lng","end_lat","end_lng","member_casual","time_taken","start_date","end_date","rounded_time_taken","year","month","week","day","day_of_week","YMD","ToD")]

# Remove ride lengths < 0
df_oneyear_cleaned <- df_oneyear_cleaned %>% filter(!(rounded_time_taken < 0))

#removing station names having null start and end station names
df_oneyear_cleaned <- df_oneyear_cleaned %>% filter(!(is.na(start_station_name) | start_station_name == "")) %>% filter(!(is.na(end_station_name) | end_station_name == ""))

#writing the records to CSV to analyze further in SQL
write.csv(df_oneyear_cleaned , "oneyear_cleaned.csv") 

##checking for duplicate records
ride_id_check <- df_oneyear_cleaned %>% count(ride_id) %>% filter(n > 1)
view(ride_id_check) ##no data

## 5.Analysing the data
##unique Ride Types
unique(df_oneyear_cleaned$rideable_type)

rideable_type_check <-df_oneyear_cleaned %>% mutate(year = year(started_at), month = month(started_at)) %>% group_by(month, year) %>% select(rideable_type, month, year) %>% count(rideable_type)
view(rideable_type_check)

##Unique Station Names
station_name_check <- df_oneyear_cleaned %>%
  group_by(start_station_name) %>%
  count(start_station_name) 
view(station_name_check)

##Adding unique station data for each month
Aug_2021 <- df_oneyear_cleaned %>%
  filter(month == "08") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Sep_2021 <- df_oneyear_cleaned %>%
  filter(month == "09") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Oct_2021 <- df_oneyear_cleaned %>%
  filter(month == "10") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Nov_2021 <- df_oneyear_cleaned %>%
  filter(month == "11") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Dec_2021 <- df_oneyear_cleaned %>%
  filter(month == "12") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Jan_2022 <- df_oneyear_cleaned %>%
  filter(month == "01") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Feb_2022 <- df_oneyear_cleaned %>%
  filter(month == "02") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Mar_2022 <- df_oneyear_cleaned %>%
  filter(month == "03") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Apr_2022 <- df_oneyear_cleaned %>%
  filter(month == "04") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

May_2022 <- df_oneyear_cleaned %>%
  filter(month == "05") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Jun_2022 <- df_oneyear_cleaned %>%
  filter(month == "06") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

Jul_2022 <- df_oneyear_cleaned %>%
  filter(month == "07") %>%
  group_by(start_station_name) %>%
  count(start_station_name)

##checking if the station names are present in the given month
station_name_check$Aug_2021 <- as.integer(station_name_check$start_station_name %in% Aug_2021$start_station_name)
station_name_check$Sep_2021 <- as.integer(station_name_check$start_station_name %in% Sep_2021$start_station_name)
station_name_check$Oct_2021 <- as.integer(station_name_check$start_station_name %in% Oct_2021$start_station_name)
station_name_check$Nov_2021 <- as.integer(station_name_check$start_station_name %in% Nov_2021$start_station_name)
station_name_check$Dec_2021 <- as.integer(station_name_check$start_station_name %in% Dec_2021$start_station_name)
station_name_check$Jan_2022 <- as.integer(station_name_check$start_station_name %in% Jan_2022$start_station_name)
station_name_check$Feb_2022 <- as.integer(station_name_check$start_station_name %in% Feb_2022$start_station_name)
station_name_check$Mar_2022 <- as.integer(station_name_check$start_station_name %in% Mar_2022$start_station_name)
station_name_check$Apr_2022 <- as.integer(station_name_check$start_station_name %in% Apr_2022$start_station_name)
station_name_check$May_2022 <- as.integer(station_name_check$start_station_name %in% May_2022$start_station_name)
station_name_check$Jun_2022 <- as.integer(station_name_check$start_station_name %in% Jun_2022$start_station_name)
station_name_check$Jul_2022 <- as.integer(station_name_check$start_station_name %in% Jul_2022$start_station_name)
view(station_name_check)

## Saving the data frames to csv file
fwrite(
  df_oneyear_cleaned, 
  "df_oneyear_cleaned.csv", 
  col.names = TRUE,
  row.names = FALSE
)

fwrite(
  df_oneyear, 
  "df_oneyear.csv", 
  col.names = TRUE,
  row.names = FALSE
)

fwrite(
  rideable_type_check, 
  "rideable_type_check.csv",
  col.names = TRUE, 
  row.names = FALSE
)

fwrite(
  station_name_check, 
  "station_name_check.csv",
  col.names = TRUE, 
  row.names = FALSE
)


view(df_oneyear_cleaned)


  


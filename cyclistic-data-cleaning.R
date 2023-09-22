# Load packages
library(tidyverse) # for many data analysis tools
library(scales) # for modifying units of measure
library(sf) # for working with geospatial data & maps
library(ggmap) # for more detailed background maps
library(plotly) # for better visualization of maps
library(osmdata) # for working with openstreetmap.org
library(janitor) # for cleaning data
library(ggthemes) # for more themes (including theme_map())


# set color variables
color_member <- "#00204D"
color_casual <- "#6699FF"


# Load the past 12 months of Cyclistic's historical data into data frames
july <- read.csv("data/202207-divvy-tripdata.csv")
august <- read.csv("data/202208-divvy-tripdata.csv")
september <- read.csv("data/202209-divvy-publictripdata.csv")
october <- read.csv("data/202210-divvy-tripdata.csv")
november <- read.csv("data/202211-divvy-tripdata.csv")
december <- read.csv("data/202212-divvy-tripdata.csv")
january <- read.csv("data/202301-divvy-tripdata.csv")
february <- read.csv("data/202302-divvy-tripdata.csv")
march <- read.csv("data/202303-divvy-tripdata.csv")
april <- read.csv("data/202304-divvy-tripdata.csv")
may <- read.csv("data/202305-divvy-tripdata.csv")
june <- read.csv("data/202306-divvy-tripdata.csv")


# Combine the 12 historical data frames (months) into a single data frame (year)
data <- rbind(july, august, september, october, november, december, january, february, march, april, may, june)


# Create a dataframe of the unmodified data set
data_unclean <- data


# Remove month variables from environment now that bind is complete
rm(january, february, march, april, may, june, july, august, september, october, november, december)


# Check for duplicate ride_id's
has_duplicates <- length(data$ride_id) != length(unique(data$ride_id))
print(has_duplicates)


# Check for NA values
na_counts <- colSums(is.na(data))
print(na_counts)


# Drop rows with missing ending coordinates (these show excessive ride time)
data <- data %>% drop_na(c(end_lat, end_lng))


# Check for empty string values
char_vars <- data %>% 
select_if(is.character)

empty_strings <- function(vec) {
  sum(vec == "", na.rm = TRUE)
}
sum_empty_strings <- sapply(char_vars, empty_strings)
print(sum_empty_strings)


# Convert empty strings and zero values to NA
data$start_station_name[data$start_station_name == ""] = NA
data$end_station_name[data$end_station_name == ""] = NA
data$start_station_id[data$start_station_id == ""] = NA
data$end_station_id[data$end_station_id == ""] = NA
data$end_lat[data$end_lat == 0] = NA
data$end_lng[data$end_lng == 0] = NA


# Trim excess whitespace from strings
data$start_station_name <- trimws(gsub("\\s+", " ", data$start_station_name))
data$end_station_name <- trimws(gsub("\\s+", " ", data$end_station_name))
data$start_station_id <- trimws(gsub("\\s+", " ", data$start_station_id))
data$end_station_id <- trimws(gsub("\\s+", " ", data$end_station_id))
data$ride_id <- trimws(gsub("\\s+", " ", data$ride_id))
data$rideable_type <- trimws(gsub("\\s+", " ", data$rideable_type))


# Drop rows if start time is later than end time (impossible scenario)
data <- data %>% filter(!started_at >= ended_at)


# Convert started_at and ended_at columns to datetime format
data <- mutate(data, started_at = ymd_hms(started_at))
data <- mutate(data, ended_at = ymd_hms(ended_at))


# Remove underscore in rideable_type column values and convert to title case
data$rideable_type <- str_to_title(str_replace(data$rideable_type, "_", " "))


# Convert member_casual column to title case
data$member_casual <- str_to_title(data$member_casual)


# Create ride_length column and convert to minutes
data <- mutate(data, ride_length = round(difftime(ended_at, started_at, units  = "mins"), digits = 2))


# Remove rows with ride_length less than 1 minute
# This helps exclude observations of "false rides" (i.e. someone is un-docking and immediately re-docking the bike)
data <- filter(data, ride_length >= 1.00)


# Create a column for day of week that the ride started
data <- mutate(data, day_of_week = weekdays(started_at, abbreviate = F))


# Create column for month that the ride started
data <- mutate(data, month = month(started_at, label = T))


# Create a function to determine the phase of day
day_phase <- function(time) {
  hr <- lubridate::hour(time)
  dplyr::case_when(hr > 5 & hr < 12 ~ 'Morning',
                   hr >= 12 & hr < 18 ~ 'Afternoon',
                   hr >=  18 & hr <= 23 ~ 'Evening',
                   TRUE ~ 'Night')
}

# Create phase_of_day column
data <- mutate(data, phase_of_day = day_phase(started_at))


# import Divvy's complete station list
# source: https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations-All-Map/bk89-9dk7
stations_divvy <- read.csv("data/bike-stations_city-of-chicago/Divvy_Bicycle_Stations_-_All_-_Map.csv")
stations_divvy <- stations_divvy %>% select(Station.Name, Latitude, Longitude)


# import additional stations not included in City of Chicago Divvy station listing
# These locations were individually collected by analyzing Google Maps and the Divvy station list
stations_additional <- read.csv("data/stations_additional/stations_additional.csv")


# combine station lists
stations <- rbind(stations_divvy, stations_additional)


# Trim white space
stations$Station.Name <- trimws(gsub("\\s+", " ", stations$Station.Name))


# Create start_stations data frame & rename variables (for future join)
start_stations <- stations %>%
  rename(start_station_name = Station.Name,
         start_station_lat = Latitude,
         start_station_lng = Longitude)


# Create end_stations data frame & rename variables (for future join)
end_stations <- stations %>%
  rename(end_station_name = Station.Name,
         end_station_lat = Latitude,
         end_station_lng = Longitude)


# Join station data with main data set
data <- left_join(data, start_stations)
data <- left_join(data, end_stations)
  

# Remove end_stations and start_stations data sets from environment now that join is complete
rm(start_stations, end_stations)


#Import data and load data

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

trip_data_2022_01 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202101-divvy-tripdata.csv")
trip_data_2022_02 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202102-divvy-tripdata.csv")
trip_data_2022_03 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202103-divvy-tripdata.csv")
trip_data_2022_04 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202104-divvy-tripdata.csv")
trip_data_2022_05 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202105-divvy-tripdata.csv")
trip_data_2022_06 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202106-divvy-tripdata.csv")
trip_data_2022_07 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202107-divvy-tripdata.csv")
trip_data_2022_08 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202108-divvy-tripdata.csv")
trip_data_2022_09 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202109-divvy-tripdata.csv")
trip_data_2022_10 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202110-divvy-tripdata.csv")
trip_data_2022_11 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202111-divvy-tripdata.csv")
trip_data_2022_12 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202112-divvy-tripdata.csv")


# Combine data into 1 data frame
trip_data_2022 <- rbind(trip_data_2022_01, trip_data_2022_02, trip_data_2022_03, trip_data_2022_04, trip_data_2022_05, trip_data_2022_06, trip_data_2022_07, trip_data_2022_08, trip_data_2022_09, trip_data_2022_10, trip_data_2022_11, trip_data_2022_12)


## Clean data
# Remove NA row
trip_data_2022_clean <- na.omit(trip_data_2022)

# Rename Member type
trip_data_2022_clean <- rename(trip_data_2022_clean, member_type = member_casual)



## Processing data
# Add duration column
trip_data_process <- mutate(trip_data_2022_clean, duration = round((as.numeric(ymd_hms(ended_at) - ymd_hms(started_at)))/60))

# Add day of the week column
trip_data_process <- mutate(trip_data_process, day=weekdays(ymd_hms(started_at)))

# Add start hour column
trip_data_process <- mutate(trip_data_process, started_hour=format((ymd_hms(started_at)), format = "%H"))


## Analyze
# Plot start_hour 
ggplot(data = trip_data_process) + geom_bar(mapping = aes(x = started_hour)) + facet_wrap(~member_type)

# Plot day of the week 
ggplot(data = trip_data_process) + geom_bar(mapping = aes(x = day)) + facet_wrap(~member_type)

# Summaries cycle duration for annual member during 8am and 5pm
work_duration_summary <- trip_data_process %>%
  group_by(member_type) %>%
  filter(started_hour == 8 | 17) %>%
  summarise(mean(duration))

# Plot duration frequency for each member type
work_duration_summary <- trip_data_process %>%
  group_by(member_type) %>%
  summarise(count = n() )
ggplot(data = trip_data_process) + geom_point(mapping = aes(x = member_type, y = duration))

# Plot bike type popularity
ggplot(data = trip_data_process) + geom_bar(mapping = aes(x = rideable_type, fill = member_type))

# Plot location popularity
top_location_summary <- trip_data_process %>%
  group_by(start_station_name) %>%
  filter(start_station_name != "")  %>%
  filter(member_type == "member")  %>%
  summarise(count = n()) %>%
  top_n(n = 10, wt = count) %>%
  arrange(desc(count))

ggplot(data = top_location_summary) + geom_bar(stat='identity', mapping = aes(x =    start_station_name, y = count))


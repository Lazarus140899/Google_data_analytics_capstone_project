# Google Data Analytics Professional Certificate Capstone Project: Bike Sharing data


# First we install all the required packages: 

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("readr")
install.packages("janitor")
install.packages("data.table")
install.packages("tidyr")


# Now we activate the installed packages:

library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)




# Importing the data sets:


data_sep2020 <-  read.csv("D:\\google project data\\dataset_csv_format\\Sep2020-tripdata.csv")
data_oct2020 <-  read.csv("D:\\google project data\\dataset_csv_format\\Oct2020-tripdata.csv")
data_nov2020 <-  read.csv("D:\\google project data\\dataset_csv_format\\Nov2020-tripdata.csv")
data_dec2020 <-  read.csv("D:\\google project data\\dataset_csv_format\\Dec2020-tripdata.csv")
data_jan2021 <-  read.csv("D:\\google project data\\dataset_csv_format\\Jan2021-tripdata.csv")
data_feb2021 <-  read.csv("D:\\google project data\\dataset_csv_format\\Feb2021-tripdata.csv")
data_mar2021 <-  read.csv("D:\\google project data\\dataset_csv_format\\Mar2021-tripdata.csv")
data_apr2021 <-  read.csv("D:\\google project data\\dataset_csv_format\\Apr2021-tripdata.csv")
data_may2021 <-  read.csv("D:\\google project data\\dataset_csv_format\\May2021-tripdata.csv")
data_june2021 <- read.csv("D:\\google project data\\dataset_csv_format\\June2021-tripdata.csv")
data_july2021 <- read.csv("D:\\google project data\\dataset_csv_format\\July2021-tripdata.csv")
data_aug2021 <-  read.csv("D:\\google project data\\dataset_csv_format\\Aug2021-tripdata.csv")


# The column names of all the dataset is checked for confirming consistency:

colnames(data_sep2020)
colnames(data_oct2020)
colnames(data_nov2020)
colnames(data_dec2020)
colnames(data_jan2021)
colnames(data_feb2021)
colnames(data_mar2021)
colnames(data_apr2021)
colnames(data_may2021)
colnames(data_june2021)
colnames(data_july2021)
colnames(data_aug2021)



# To check the structure of all the column mentioned in the datasets:

str(data_sep2020)
str(data_oct2020)
str(data_nov2020)
str(data_dec2020)
str(data_jan2021)
str(data_feb2021)
str(data_mar2021)
str(data_apr2021)
str(data_may2021)
str(data_june2021)
str(data_july2021)
str(data_aug2021)



# While checking the datatypes of the columns mentioned in the datasets , we come across columns named "start_station_id", "end_station_id", which are either present in character format
# or in integer format. So to convert the column in a common dataset. we convert the column in character format


# For start station id:

data_sep2020 <-  mutate(data_sep2020,start_station_id = as.character(start_station_id))
data_oct2020 <-  mutate(data_oct2020,start_station_id = as.character(start_station_id))
data_nov2020 <-  mutate(data_nov2020,start_station_id = as.character(start_station_id))
data_dec2020 <-  mutate(data_dec2020,start_station_id = as.character(start_station_id))
data_jan2021 <-  mutate(data_jan2021,start_station_id = as.character(start_station_id))
data_feb2021 <-  mutate(data_feb2021,start_station_id = as.character(start_station_id))
data_mar2021 <-  mutate(data_mar2021,start_station_id = as.character(start_station_id))
data_apr2021 <-  mutate(data_apr2021,start_station_id = as.character(start_station_id))
data_may2021 <-  mutate(data_may2021,start_station_id = as.character(start_station_id))
data_june2021 <- mutate(data_june2021,start_station_id = as.character(start_station_id))
data_july2021 <- mutate(data_july2021,start_station_id = as.character(start_station_id))
data_aug2021 <-  mutate(data_aug2021,start_station_id = as.character(start_station_id))


# For end station id:

data_sep2020 <-  mutate(data_sep2020,end_station_id = as.character(end_station_id))
data_oct2020 <-  mutate(data_oct2020,end_station_id = as.character(end_station_id))
data_nov2020 <-  mutate(data_nov2020,end_station_id = as.character(end_station_id))
data_dec2020 <-  mutate(data_dec2020,end_station_id = as.character(end_station_id))
data_jan2021 <-  mutate(data_jan2021,end_station_id = as.character(end_station_id))
data_feb2021 <-  mutate(data_feb2021,end_station_id = as.character(end_station_id))
data_mar2021 <-  mutate(data_mar2021,end_station_id = as.character(end_station_id))
data_apr2021 <-  mutate(data_apr2021,end_station_id = as.character(end_station_id))
data_may2021 <-  mutate(data_may2021,end_station_id = as.character(end_station_id))
data_june2021 <- mutate(data_june2021,end_station_id = as.character(end_station_id))
data_july2021 <- mutate(data_july2021,end_station_id = as.character(end_station_id))
data_aug2021 <-  mutate(data_aug2021,end_station_id = as.character(end_station_id))


# Now combining all the data for the last 12 months into a single dataframe named "trip_data" :
trip_data <- rbind(data_sep2020,data_oct2020, data_nov2020,data_dec2020, data_jan2021, data_feb2021, data_mar2021, 
                   data_apr2021, data_may2021, data_june2021, data_july2021, data_aug2021)



# To check the value in the rideable_type column:
trip_data$rideable_type



# Now we need to separately create columns for months,year,day and day of the week:
trip_data$months <- format(as.Date(trip_data$started_at), "%m")
trip_data$year <- format(as.Date(trip_data$started_at), "%Y")
trip_data$day <- format(as.Date(trip_data$started_at), "%d")
trip_data$day_of_the_week <- format(as.Date(trip_data$started_at),"%a")



# Converting time into the format of hours and minutes:
trip_data$time <- format(trip_data$started_at, format = "%H:%M")
trip_data$time <- as.POSIXct(trip_data$time, format = "%H:%M")



# Calculating the length of the ride(in mins) taken by the user:
trip_data$ride_length <- (as.double(difftime(trip_data$ended_at,trip_data$started_at)))/60



# To convert ride_length column from "factor" to "numeric" so that we can use it 
as.factor(trip_data$ride_length)
trip_data$ride_length <- as.numeric(as.character(trip_data$ride_length))
is.numeric(trip_data$ride_length)



# Now we need to check the minimum value of ride_length and need to see whether it has any negative values or not :
min(trip_data$ride_length)



# Checking for trip lengths less than 0
nrow(subset(trip_data,trip_data$ride_length < 0))



# Checking for testrides that were made by company for quality checks
nrow(subset(trip_data, start_station_name %like% "TEST"))
nrow(subset(trip_data, start_station_name %like% "test"))
nrow(subset(trip_data, start_station_name %like% "Test"))



# We create a new dataframe named "trip_data_pos" which will only have data containing positive ride_length values and are not observations of tests undertaken by the company 
trip_data_pos <- trip_data[!(trip_data$ride_length<0),]



# Removing the test rides
trip_data_pos<- trip_data_pos[!((trip_data_pos$start_station_name %like% "TEST" | trip_data_pos$start_station_name %like% "test")),]



# Checking the dataframe:
glimpse(trip_data_pos)



# Checking count of distinct values
table(trip_data_pos$member_casual)



# Aggregating total trip duration by customer type
setNames(aggregate(ride_length ~ member_casual, trip_data_pos, sum), c("member_casual", "total_ride_length"))



# To find the mean, median, maximum and minimum values of ride length for casual and regular members:
aggregate(trip_data_pos$ride_length ~ trip_data_pos$member_casual, FUN= mean)
aggregate(trip_data_pos$ride_length ~ trip_data_pos$member_casual, FUN= median)
aggregate(trip_data_pos$ride_length ~ trip_data_pos$member_casual, FUN= max)
aggregate(trip_data_pos$ride_length ~ trip_data_pos$member_casual, FUN= min)



# Checking the dataframe trip_data_pos:
glimpse(trip_data_pos)



# Creating a summary table for number of rides with respect to customer types and day of the week:
trip_data_pos_summary <- trip_data_pos %>%
  group_by(member_casual, day_of_the_week) %>%
  summarise(no_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, desc(no_of_rides))

# To check the new summary dataframe: 
glimpse(trip_data_pos_summary)





# VISUALIZATIONS:

# Bar graph representing the relation between customer_type and day of the week:
trip_data_pos %>%  
  group_by(member_casual, day_of_the_week) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(member_casual, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = no_of_rides, fill = member_casual)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



# Average number of trips by customer types and month:
trip_data_pos %>% 
  group_by(member_casual, months) %>%  
  summarise(no_of_rides = n(),`average_duration_(mins)`= mean(ride_length)) %>% 
  arrange(member_casual,desc(no_of_rides))



trip_data_pos %>%  
  group_by(member_casual, months) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(member_casual, months)  %>% 
  ggplot(aes(x = months, y = no_of_rides, fill = member_casual)) +
  labs(title ="Total trips by customer type Vs. Months") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



# Average trip duration by customer type on each day of the week:
trip_data_pos %>%  
  group_by(member_casual, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(ride_length)) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")



# Average trip duration by customer type in each month:
trip_data_pos %>%
  group_by(member_casual, months) %>% 
  summarise(average_trip_duration = mean(ride_length)) %>%
  ggplot(aes(x = months, y = average_trip_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))



# Demand of bikes over a period of 24 hours ( 1 day):
trip_data_pos %>%  
  group_by(member_casual, time) %>% 
  summarise(no_of_trips = n()) %>%
  ggplot(aes(x = time, y = no_of_trips, color = member_casual, group = member_casual)) +
  geom_line() + scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) + labs(title ="Demand over 24 hours of a day", x = "Time of the day")



# Type of rides VS no of trips with respect to the customer types:
trip_data_pos %>%
  group_by(rideable_type, member_casual) %>%
  summarise(no_of_trips = n()) %>%  
  ggplot(aes(x= rideable_type, y=no_of_trips, fill= member_casual))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")



# Now at the end, we create a csv file consisting of the clean data which can be used for further analysis and visualizations:
clean_data <- aggregate(trip_data_pos$ride_length ~ trip_data_pos$member_casual + trip_data_pos$day_of_the_week, FUN = mean)
write.csv(clean_data, "Clean Data.csv", row.names = F)




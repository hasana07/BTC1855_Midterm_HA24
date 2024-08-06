#Coding in R Midterm - Hasan Abdo
#Bay Area Bike Rental Operation Research

#Reading data sets
#Used function read.csv to load csv file. Note to reviewer, make sure the file is in your own working directory and that you change the file path. 
#Argument header= TRUE indicate there is a header.  

station_data <- read.csv("~/Documents/BTC1855 Coding in R/Midterm_Hasan/station.csv", header = TRUE)
trip_data <- read.csv("~/Documents/BTC1855 Coding in R/Midterm_Hasan/trip.csv", header = TRUE)
weather_data <- read.csv("~/Documents/BTC1855 Coding in R/Midterm_Hasan/weather.csv", header = TRUE)

#starting EDA (Exploratory Data Analysis)
#roughly using the steps outlined in the recommended link: https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/

#Please note any inferences/conclusions will be included in the written report. 
#Installing and loading appropriate packages:
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")

library(tidyverse) 
library(funModeling) 
library(Hmisc)

#EDA on the stations data set
#overview of the data set
glimpse(station_data)

#Creates a frequency table along with plot for categorical variables within the data
#For this data set, I think this would be most beneficial for the "city" variable, to see what city has most stations. 
#Installation date may be added as a piece of information, don't see much utility in it however. 
freq(station_data)

#great way of taking a final look at the data. Shows each variable separate, shows missing, and I can use it to identify any outliers
describe(station_data)


#EDA on the trip data set
#overview of the data set
glimpse(trip_data)

#although this shows 70 unique start/end station IDs, we can see that there's 74 unique start and ending station names, which may need me to look further into the discrepancy. Some stations could be using the same ID. 
print(status(trip_data))

#Can show the most frequently used start and end stations, also the frequency of subscribers vs non-subscribers. 
freq(trip_data)
#Shows a table of unique zip codes and their frequencies (recommend by the website), I can already see a couple of missing values and possible outliers from this
freq((trip_data$zip_code))
#Only useful for mean of trip duration
print(profiling_num(trip_data))

#final look at the data, also helps see outliers
describe(trip_data)



#EDA on the weather data set
#overview of the data set
glimpse(weather_data)

print(status(weather_data))

#may need to add a "No event" label for no events. Also, there's 2 different "rain" values for the variable events. 
freq(weather_data)

#mean of different values is probably the most important output this function gives
print(profiling_num(weather_data))
#summarizing data and taking final looks 
describe(weather_data)


##Data cleaning##
#The following will contain steps I take in order to clean the data, will also remove any outliers here
#Loading tidyr and dplyr packages for later use 
library("dplyr")
library("tidyr")

#Data cleaning-Station Data 
#Data set seems to be clean, nothing to really change here. There are no missing values or any evident outliers. 

#Data cleaning-Trip Data 
#creating a new data frame to do cleaning and conserve "raw" data 
clean_trip <- trip_data

#turning missing and non-sensical values to NAs, before removing them. 
#these are supposed home zip codes of subscribers, however customers manually input these and it was noted could be unreliable. 
clean_trip$zip_code[clean_trip$zip_code == ""] <- NA
clean_trip$zip_code[clean_trip$zip_code == "v6z2x"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "nil"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "M4S1P"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "99999"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "9990540"] <- NA
#now on the lower end..
clean_trip$zip_code[clean_trip$zip_code == "0"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "1"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "100"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "1000"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "10000"] <- NA
clean_trip$zip_code[clean_trip$zip_code == "100004"] <- NA

#Finding trips that start and end at the same station, with duration less than 3 minutes (since these are "cancelled trips")
cancelled_trips <- trip_data %>% 
  filter(start_station_id == end_station_id & duration < 180)

#finding the number of these trips 
nrow(cancelled_trips)

#Recording trip TDs for those "cancelled trips"
cancelled_trips_ID <- cancelled_trips %>% 
  select(id)
#Displaying IDs
print(cancelled_trips_ID)

#removing those trips from the clean dataset 
clean_trip <- clean_trip %>% 
  filter(!(start_station_id == end_station_id & duration < 180))

#removing outliers
#in the duration column, the longest bike trip recorded was 17270400 seconds, which roughly translates to 199 days. 
#I will remove this data point since the next longest bike trip was recorded to be only 8 days, which is a large difference in comparison
clean_trip <- clean_trip %>% 
  filter(!(duration == 17270400))

#getting the ID of this outlier
duration_outlier <- trip_data %>% 
  filter(duration == 17270400) %>% 
  select(id)
#displaying the ID
print(duration_outlier)


#Data cleaning-Weather Data 
#creating a new data frame to do cleaning and conserve "raw" data 
clean_weather <- weather_data

#Changing the "" in the events variable to "No Event".
clean_weather$events[clean_weather$events == ""] <- "No Event"
#There is 1 "rain" and 280 "Rain", which need to be combined. I'll change rain to Rain
clean_weather$events[clean_weather$events == "rain"] <- "Rain"

#Changing the "T" or Trace values in the precipitation_inches column to 0.09 to then convert the column as numeric. 
clean_weather$precipitation_inches[clean_weather$precipitation_inches == "T"] <- "0.009"
clean_weather$precipitation_inches <- as.numeric(as.character(clean_weather$precipitation_inches))


##--RUSH HOURS--##

#loading lubridate library to convert date values to POSIXct
library(lubridate)
#convert start and end dates to to POSIXct in order in order to manipulate the numbers 
clean_trip$start_date <- mdy_hm(clean_trip$start_date)
clean_trip$end_date <- mdy_hm(clean_trip$end_date)
#making a copy of the clean trip data set to add the rush hour columns/analysis to add rush hour date/time columns
clean_trip1 <- clean_trip

#filtering out all the trips that take longer than 5 hours for rush hour analysis
#assuming each person is taking their bike from around where they live to their work (counting this as a trip)
#The furthest 2 Bay Area cities (from each other) included in this data are San Francisco and San Jose. According to Google Maps, a bike ride from those cities should take around 5 hours. While this can probably be accomplished in a shorter time if using an electric bike, I want to conserve as much data as possible. 
clean_trip1 <- clean_trip1 %>% 
  filter(!(duration > 18000))

#identifying those "outliers" I took out
duration_outlier2 <- clean_trip %>% 
  filter(duration > 18000) %>% 
  select(id)

#displaying the IDs of those whose trips were longer than 5 hours
print(duration_outlier2)

#creating a new column for a mid-point between the start and end dates/times (serves as the mean time) I can use this average time to determine the rush hours.
clean_trip1 <- clean_trip1 %>%
  mutate(midpoint_time = as.POSIXct((as.numeric(start_date) + as.numeric(end_date)) / 2, origin = "1970-01-01", tz = "UTC"))

#creating a new column to extract the weekday, and the "hour" from the mid-point time. 
clean_trip1 <- clean_trip1 %>%
  mutate(weekday = wday(midpoint_time, label = TRUE, abbr = FALSE),
  hour = hour(midpoint_time))
#filtering for weekdays only 
clean_trip_wday <- clean_trip1 %>%
  filter(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
#creating a histogram that shows the most common hours - will help determine rush hours 
hist(clean_trip_wday$hour, 
     breaks = 24,         
     main = "Histogram identifying bike trip rush hours",
     xlab = "Hour of the Day (24-hr)",
     ylab = "Frequency",
     col = "lightblue",
     xaxt = "n")
#adding x-axis that has more ticks than default for better readability 
axis(1, at = seq(0, 23, by = 1))



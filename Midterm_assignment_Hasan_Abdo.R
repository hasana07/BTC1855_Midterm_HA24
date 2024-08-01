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

freq(station_data)
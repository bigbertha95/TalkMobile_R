#Read in events - includes event id, timestamp, longitude, and latitude
events.df <- read.csv("C:/Users/lonit/OneDrive/Documents/498/Original_Data/events.csv")

#Create a column for the day of week that the app event occurred 
events.df$day <- weekdays(as.Date(events.df$timestamp))

#Create a column with the hour the app event occurred
events.df$hour <- format(as.POSIXct(events.df$timestamp, format="%Y-%m-%d %H:%M:%S"), format="%H")

#convert the hour variable from character variable to factor
events.df$day <- as.factor(events.df$day)
events.df$hour <- as.factor(events.df$hour)

#Create new dataframe with count of events by hour and by day
library(dplyr)
HoursbyDay.df <- events.df %>% count(hour, day)

levels(HoursbyDay.df$hour)

#Order the day of the week factor variable by levels in the correct order   
HoursbyDay.df$day <- factor(HoursbyDay.df$day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Reverse the hours so that it displays on the heatmap in chronological order top to bottom 
HoursbyDay.df$hour <- factor(HoursbyDay.df$hour, levels=rev(levels(HoursbyDay.df$hour)))

#Create a quick heatmap
library(ggplot2)
ggplot(data = HoursbyDay.df, aes(x = day, y = hour)) +
  geom_tile(aes(fill = n)) 

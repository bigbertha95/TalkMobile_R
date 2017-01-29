#Read in the separate data files
gender_age_train <- read.csv("C:/Users/lonit/OneDrive/Documents/498/Original_Data/gender_age_train.csv")
app_labels <- read.csv("C:/Users/lonit/OneDrive/Documents/498/Original_Data/app_labels.csv")
events <- read.csv("C:/Users/lonit/OneDrive/Documents/498/Original_Data/events.csv")
label_categories <- read.csv("C:/Users/lonit/OneDrive/Documents/498/Original_Data/label_categories.csv")
phone_brand_device_model <- read.csv("C:/Users/lonit/OneDrive/Documents/498/Original_Data/phone_brand_device_model.csv") 
app_events <- read.csv("C:/Users/lonit/OneDrive/Documents/498/Original_Data/app_events.csv")


#################################
##EDA ON GENDER_AGE_TRAIN FILE ##
#Check structure of variables
str(gender_age_train)

#Check for any missing values in file - none are missing
sapply(gender_age_train, function(x) sum(is.na(x)))

#Get table of gender values/counts
table(gender_age_train$gender)

#Get table of age group levels/counts
table(gender_age_train$group)

#Get summary statistics for age values
summary(gender_age_train$age)

#Create dataframe for easier view of age counts
AgeCounts <- as.data.frame(table(gender_age_train$age))

#Get occurrences of unique device IDs in age gender file - 74,645 and all are unique 
uniqueIDs_AgeGender <- aggregate(data.frame(count = gender_age_train$device_id), list(value = gender_age_train$device_id), length)



#################################
##EDA ON APP_LABELS FILE ##
#Check structure of variables
str(app_labels)

#Check for any missing values in file - none are missing
sapply(app_labels, function(x) sum(is.na(x)))

#Get occurrences of unique app IDs in app_labels file - there are 108,149 app IDS
uniqueAppIDs_AppLabels <- aggregate(data.frame(count = app_labels$app_id), list(value = app_labels$app_id), length)

#Most app IDs occur only a few times, but max is 245
summary(uniqueAppIDs_AppLabels$count)

#Get occurrences of unique app labels in app_labels file
uniqueAppLabels_AppLabels <- aggregate(data.frame(count = app_labels$label_id), list(value = app_labels$label_id), length)

#Get summary of values - app labels occur anywhere from 1 to 56,902 times in the file
summary(uniqueAppLabels_AppLabels)



#################################
##EDA ON PHONE_BRAND_DEVICE_MODEL FILE ##
#Check structure of variables
str(phone_brand_device_model)

#Check for missing values- none are missing
sapply(phone_brand_device_model, function(x) sum(is.na(x)))

#Check levels of brands and device models
levels(phone_brand_device_model$phone_brand)
levels(phone_brand_device_model$device_model)

#Get occurrences of unique device IDs in phone_brand_device_model file - 186,715 
ModelBranduniqueIDs <- aggregate(data.frame(count = phone_brand_device_model$device_id), list(value = phone_brand_device_model$device_id), length)

#Get frequency count for how often each device ID occurse - 186,185 appear once, 530 appear twice
table(ModelBranduniqueIDs$count)

#table of counts of unique values in the phone_brand column - 131 unique brands; the top one appears 43,210 times, a number appear only once
uniqueBrands <- aggregate(data.frame(count = phone_brand_device_model$phone_brand), list(value = phone_brand_device_model$phone_brand), length)

#table of counts of unique values in the device_model column -1,599 unique values; the top one appears 7,358 times, a number appear only once
uniqueDeviceModels <- aggregate(data.frame(count = phone_brand_device_model$device_model), list(value = phone_brand_device_model$device_model), length)


#################################
##EDA ON EVENTS FILE ##
#Check structure of variables
str(events)

#Check for missing values- none are missing
sapply(events, function(x) sum(is.na(x)))

#Get occurrences of unique device IDs in phone_brand_device_model file - 60,864 
uniqueDeviceEvents <- aggregate(data.frame(count = events$device_id), list(value = events$device_id), length)

#Get summary statistics for number of times the device occurs in the file - min of 1 to max of 33,450
summary(uniqueDeviceEvents$count)

#Get occurrences of unique event IDs - there are 3,252,950, all occur only once
uniqueEventIDs <- aggregate(data.frame(count = events$event_id), list(value = events$event_id), length)

#Summary oflabeling for the unique event IDs  
summary(uniqueEventIDs$value)

#Create a column with the hour the app event occurred
events.df$hour <- format(as.POSIXct(events.df$timestamp, format="%Y-%m-%d %H:%M:%S"), format="%H")

#Create a column for the date the app event occurred
events.df$date <- format(as.POSIXct(events.df$timestamp, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")

#Change date to factor
events.df$date <- as.factor(events.df$date)

#Get table of occurrences by date
levels(events.df$date)

#Create a column for the day of week that the app event occurred 
events.df$day <- weekdays(as.Date(events.df$timestamp))

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

#Get occurrences of unique latitudes - 3,086 total, highest value are at zero - 968,955
uniqueLatitudes <- aggregate(data.frame(count = events$latitude), list(value = events$latitude), length)

#Get occurrences of unique longitudes -3,588 total, highest value are at zero, slight mismatch from zero values of latitudes - 968,711
uniqueLongitudes <- aggregate(data.frame(count = events$longitude), list(value = events$longitude), length)


#################################
##EDA ON LABEL_CATEGORIES FILE ##
#Check structure of variables
str(label_categories)

#Check for missing values- none are missing (although on further investigation, it appears there are a few missing values for categories that may be empty strings)
sapply(label_categories, function(x) sum(is.na(x)))

#Assign the empty strings to NA, now shows 3 values missing
label_categories$category[label_categories$category==""] <- NA

#Get occurrences of unique label categories - there are 835, with most occurring only once, but some 2 or 3 times, and 26 are "unknown"
uniqueLabelCategories <- aggregate(data.frame(count = label_categories$category), list(value = label_categories$category), length)

#Get occurrences of unique label IDs - there are 930, each occurring only once
uniqueLabelIDs_LabelCat <- aggregate(data.frame(count = label_categories$label_id), list(value = label_categories$label_id), length)
 

#################################
##EDA ON APP_EVENTS FILE ##
#Check structure of variables - 32,473,067 observations
str(app_events)

#Check for missing values- none are missing
sapply(app_events, function(x) sum(is.na(x)))

#Check values of is_active, all 0 or 1, 19,740,071 are 0 and 12,732,996 are 1
table(app_events$is_active)

#Check values of is_installed - everything is a 1
table(app_events$is_installed)

#Get occurrences of unique app IDs - there are 19,044, with counts ranging from 1 to 1,164,000
uniqueAppIDs_AppEvents <- aggregate(data.frame(count = app_events$app_id), list(value = app_events$app_id), length)

#Get summary statistics of app ID occurrences
summary(uniqueAppIDs_AppEvents$count)

#Get occurrences of unique event IDs - there are 1,488,096, with frequency counts ranging from 1 to 320 
uniqueEventIDs_AppEvents <- aggregate(data.frame(count = app_events$event_id), list(value = app_events$event_id), length)

#Get summary statistics of unique event ID occurrences
summary(uniqueEventIDs_AppEvents$count)

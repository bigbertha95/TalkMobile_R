####Loading needed packages####
rm(list=ls())

library(mice)
library(lubridate)
library(Hmisc)
library(stringr)
library(readr)
library(plyr)
library(dplyr)
#Create a function for dimension reduction
collapsecategory <- function(x,p){
  levels_len = length(levels(x))
  levels(x)[levels_len+1] = 'Other'
  y= table(x)/length(x)
  y1 = as.vector(y)
  y2 = names(y)
  y2_len = length(y2)
  for(i in 1:y2_len){
    if(y1[i]<=p){
      x[x == y2[i]] = 'Other'
    }
  }
  x <- droplevels(x)
  x
}
####Reading in original data files####
device_ids_matched <- read_csv("~/List of 23038 device ids that match.csv")
#Note this file was added to google drive PREDICT 498/ Additional Data Files
device_ids_train <- read_csv("~/device ids for training set")
#Note this file was added to google drive PREDICT 498/ Additional Data Files
device_ids_test <- read_csv("~/device ids for test set")
#Note this file was added to google drive PREDICT 498/ Additional Data Files
gender_age_train <- read_csv("~/gender_age_train/gender_age_train.csv")
app_labels <- read_csv("~/app_labels.csv/app_labels.csv")

events <- read_csv("~/events.csv/events.csv")
label_categories <- read_csv("~/label_categories.csv/label_categories_big2.csv")
#Note this file was modified by CJ and pulled from google drive
phone_brand_device_model <- read_csv("~/phone_brand_device_model.csv/Brand_Find_Replace2.csv") 
#Note this file was modififed by CJ and pulled from google drive
phone_brand_device_model$phone_brand <- as.factor(phone_brand_device_model$phone_brand)
phone_brand_device_model$device_model <- as.factor(phone_brand_device_model$device_model)
phone_brand_device_model$phone_brand <- collapsecategory(phone_brand_device_model$phone_brand, p = .01)
phone_brand_device_model$device_model <- collapsecategory(phone_brand_device_model$device_model, p = .01)
app_events <- read_csv("~/app_events.csv/app_events.csv")


#label_categories <- read_csv("~/label_categories.csv/label_categories_big2.csv")
#phone_brand_device_model <- read_csv("~/phone_brand_device_model.csv") 
#NOTE: Reading in the labels and brands modefied by CJ, so the raw data is not needed

####Merging the data into one file####
#     File to predict age/gender     #
####Merge gender_age_train and phone_brand_device_model into new df named 'merge_data_1'####
merged_data_1 <-
  gender_age_train %>%
  left_join(phone_brand_device_model, by = "device_id")%>% #Merging the data
  filter(device_id %in% device_ids_matched$device_id) #Removing unnecessary device_ids
#NOTE: The merged file returns more observations than the original gender_age_train set indicating duplicates

#Identifying duplicates where the entire row is a duplicate (every column matches)
merged_data_1$duplicate <- duplicated(merged_data_1) | duplicated(merged_data_1, fromLast = TRUE)

#Counting the number of duplicate rows
plyr::count(merged_data_1, "duplicate")
#There are 386 duplicate rows

#Creating a flag variable to identify the cases that were duplicates after deduping
merged_data_1$device_model_duplicate_flag <- 0
merged_data_1$device_model_duplicate_flag[merged_data_1$duplicate == "TRUE"] <- 1
plyr::count(merged_data_1, "device_model_duplicate_flag")

#Removing duplicates where the entire row is a duplicate (every column matches)
#Also dropping the duplicate column as it is no longer needed
#By remove, I mean leaving only 1 of the duplicated rows in the dataset
merged_data_1 <- merged_data_1[!duplicated(merged_data_1), ] %>%
  select(-duplicate)
#NOTE: There is one duplicate observation where the entire row isn't duplicated

#Identifying the duplicate where only device_id is a duplicated
merged_data_1$duplicate_device_id <- duplicated(merged_data_1$device_id) | duplicated(merged_data_1$device_id, fromLast = TRUE)

#Counting the number of duplicate rows for just device_id
plyr::count(merged_data_1, "duplicate_device_id")
#Confirms 2 rows with same device_id
#NOTE: This obervation has the same device_id, but different device_model and phone_brand

#Entirely removing the device_id that has inconsistent data
#By remove, I mean removing both observations and eliminating this device_id entirely
merged_data_1 <- merged_data_1 %>%
  filter(duplicate_device_id == "FALSE") %>%
  select(-duplicate_device_id)

#Visually inspecting observations
head(merged_data_1)
#summarizing each variable in the df
summary(merged_data_1)
#NOTE: gender, group, phone_brand, and device_model are being read as large character fields when in reality these could be factors

#Changing gender, group, phone_brand, and device_model into factor types
merged_data_1$gender <- as.factor(merged_data_1$gender)
merged_data_1$group <- as.factor(merged_data_1$group)
merged_data_1$phone_brand <- as.factor(merged_data_1$phone_brand)
merged_data_1$device_model <- as.factor(merged_data_1$device_model)
merged_data_1$device_model_duplicate_flag <- as.factor(merged_data_1$device_model_duplicate_flag)

#Confirming types and verified that there are no NA values in the merged set
summary(merged_data_1)
merged_data_1_missing_pattern <- as.data.frame(md.pattern(merged_data_1))

#Removing the phone_brand_device_model df and the gender_age_train df from the environment as they are now merged into the new df
rm(phone_brand_device_model)
rm(gender_age_train)
rm(merged_data_1_missing_pattern)


####Merge events and app_events into new df named 'merged_data_2'####
#Because of the size of these files it is easier to check or duplicates prior to merge
#Checking if there are any complete (full row) duplicates in the events data
events$duplicate <- duplicated(events) | duplicated(events, fromLast = TRUE)
plyr::count(events, "duplicate")
#There are no complete duplicates

#Checking if there are any event_id only duplicates in the events data
events$duplicate_event_id <- duplicated(events$event_id) | duplicated(events$event_id, fromLast = TRUE)
plyr::count(events, "duplicate_event_id")
#There are no duplicate event_ids in the events data

#Filtering events to only the necessary device_ids
events <- events %>%
  filter(device_id %in% device_ids_matched$device_id)

#Removing the variables (duplicate and duplicate_event_id) used to identify duplicates from events data
events <- events %>%
  select(-duplicate)%>%
  select(-duplicate_event_id)

head(events)

#Checking if there are any complete (full row) duplicates in the app_events data
app_events$duplicate <- duplicated(app_events) | duplicated(app_events, fromLast = TRUE)
plyr::count(app_events, "duplicate")
#Indicates that there are 23,820 rows that have complete duplicates

#Creating a new dataset of only duplicates to visually verify the duplicates
app_events_duplicates <- app_events %>%
  filter(duplicate == "TRUE")
#Quick visual look and it does appear that these are all full duplicates (across all rows)

#Deleting the df of only duplicates that was used to visually check for full duplicates
rm(app_events_duplicates) 

#Creating a flag variable to identify which obervations were duplicates after deduping
app_events$app_event_full_duplicate_flag <- 0
app_events$app_event_full_duplicate_flag[app_events$duplicate == "TRUE"] <- 1
plyr::count(app_events, "app_event_full_duplicate_flag")

#Removing the complete (full row) duplicates
#By remove, I mean that I leave only 1 of the matching observations the df
#Also dropping the duplicate column as it is no longer needed
app_events <- app_events[!duplicated(app_events), ] %>%
  select(-duplicate)

#Visual observation to assess the first few rows of data
head(app_events)

#For the app_events data it would make sense to have duplicate event_ids and duplicate app_ids, but duplicate combinations shouldn't exist
#Checking if there are any event_id and app_id combination duplicates in the cleaner app_events data
app_events$duplicate <- duplicated(app_events[,1:2]) | duplicated(app_events[,1:2], fromLast = TRUE)
plyr::count(app_events, "duplicate")
#There are 2,946 duplicates rows of event_id and app_id

#Creating a new dataset of only duplicates to visually verify the duplicates
app_events_duplicates <- app_events %>%
  filter(duplicate == "TRUE")
#Quick visual look and it does appear that these are duplicates with multiple rows with thes same event_id and app_id 
#Duplicates have the same event_id and app_id, but each has one row where is_active is 1 and one row where is_active is 0

#Deleting the df of only duplicates that was used to visually check for full duplicates
rm(app_events_duplicates)

#Creating a flag variable so that these duplicates can be identified after deduping
app_events$is_active_duplicate_flag <- 0
app_events$is_active_duplicate_flag[app_events$duplicate == "TRUE"] <- 1
head(app_events)

#Removing the partial (app_id and event_id combination) duplicates
#By remove, I mean that I leave only 1 of the matching observations the df
#I am forcing it to choose the observation that has is_active as 1
app_events <- app_events%>%
  filter(duplicate == "FALSE" | duplicate == "TRUE" & is_active == 1)

#Confirming only half of the duplicates remain
plyr::count(app_events, "duplicate")

#Dropping the duplicate column as it is no longer needed
app_events <- app_events%>%
  select(-duplicate)

#Merging the events and app_events dfs into a new df named merged_data_2
#Using a full_join, so all observations should remain
merged_data_2 <-
  events %>%
  left_join(app_events, by = "event_id") 

#Getting summary of the new combined dataset
summary(merged_data_2)


n_distinct(merged_data_2$event_id)
#1,212,306 unique event_id as expected
summary(plyr::count(merged_data_2$event_id))

n_distinct(merged_data_2$device_id)
#22,402 unique device_ids - means that there will be missing when merged to merged_data_1
summary(plyr::count(merged_data_2$device_id))
summary(merged_data_2)

#Removing the app_events and events dfs from the environment as they are now merged
rm(app_events)
rm(events)

###Merge app_labels and label_categories into new df named 'merge_data_3'###
#Checking to see if there are full duplicates in the label_categories df
label_categories$duplicate <- duplicated(label_categories) | duplicated(label_categories, fromLast = TRUE)
plyr::count(label_categories, "duplicate")
#No duplicates

#Checking to see if label_id within label_categories alone is duplicated
label_categories$duplicate <- duplicated(label_categories$label_id) | duplicated(label_categories$label_id, fromLast = TRUE)
plyr::count(label_categories, "duplicate")
#No duplicates

#Removing duplicate column as it is no longer needed
label_categories <- label_categories%>%
  select(-duplicate)

#Checking to see if there are any full duplicates in the app_labels df
app_labels$duplicate <- duplicated(app_labels) | duplicated(app_labels, fromLast = TRUE)
plyr::count(app_labels, "duplicate")
#There are 32,134 rows that are complete matches with other rows

#Removing the duplicate rows in the app_labels df
#By remove, I mean keeping only 1 row when there are multiple rows that are identical
app_labels <- app_labels[!duplicated(app_labels), ] %>%
  select(-duplicate)

#Checking to see if app_id within app_labels along is duplicated
app_labels$duplicate <- duplicated(app_labels$app_id) | duplicated(app_labels$app_id, fromLast = TRUE)
plyr::count(app_labels, "duplicate")
#Yes, most are duplicated which indicates that most app_ids have multiple label_ids (meaning apps have multple category assignments)

app_labels <- app_labels%>%
  select(-duplicate)

merged_data_3 <-
  app_labels %>%
  inner_join(label_categories, by = "label_id") 
merged_data_3 $flag_1=ifelse(merged_data_3$category== "High Flow", 1, 0)#Creates a flag for 773,High Flow,flag_1
#NOTE: there are 423 more observations post merge compared to the n from app_labels prior to merge
#This was found by doing a full_join
#This indicates that there are 423 labels that didn't match to an app
#We don't care about labels that aren't associated with an app, so a left_join was used

summary(merged_data_3)

#the category variable is being stored as a large character, but should be a factor
merged_data_3$category <- as.factor(merged_data_3$category)

#removing the app_labels and label_categories dfs from the environment because they are now merged
rm(app_labels)
rm(label_categories)

###Merging merged_data_1 and merged_data_2 into a new df named 'merged_data_4###
#left_join was used because we only want more information on devices also found in the training set
merged_data_4 <- merged_data_1%>%
  left_join(merged_data_2, by = "device_id")

summary(merged_data_4)
n_distinct(merged_data_4$device_id) #23,038 as expected

###Merging merged_data_4 and merged_data_3 into new df named 'final_merged_data;###
final_merged_data <- merged_data_4 %>%
  left_join(merged_data_3, by = "app_id")



#Combining labels that have same breakdown by age/gender group to reduce dimensionality
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 732, 731, final_merged_data$label_id) 
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 279, 222, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 747, 255, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 749, 255, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 937, 933, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 141, 246, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 152, 246, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 818, 246, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 820, 246, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 738, 737, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 205, 204, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 206, 204, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 843, 169, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 271, 169, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 840, 169, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 558, 551, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 758, 254, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 795,  27, final_merged_data$label_id)
final_merged_data$flag_3=ifelse(final_merged_data$label_id== 188, 1, 0)#Creates a flag for 188, Flight, which has high number of females 24-26 years old,flag_3
final_merged_data$flag_4=ifelse(final_merged_data$label_id== 211, 1, 0)#Creates a flag for 211, Take-Away ordering, which has high number of females 24-26 years old,flag_4
final_merged_data$video_tag=ifelse(final_merged_data$label_id== 179, 1, 0)#Creates a flag for video, higher with young people
final_merged_data$im_tag=ifelse(final_merged_data$label_id== 172, 1, 0)#Creates a flag for IM, higher with females 
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 188, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 211, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 179, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 172, NULL, final_merged_data$label_id)
summary(final_merged_data)
summary(final_merged_data)

#Creating flags for labels that have different breakdown by age/gender group
final_merged_data$flag_2=ifelse(final_merged_data$label_id== 774, 1, 0)#Creates a flag for 774,High Mobility,flag_2
final_merged_data$flag_5=ifelse(final_merged_data$label_id== 775, 1, 0)#Creates a flag for 775, Liquid Medium, which has high number of older people,flag_5
final_merged_data$lowest_risk=ifelse(final_merged_data$label_id== 787, 1, 0)#Creates a flag for 787, Lowest Risk, lowest_risk
final_merged_data$low_risk=ifelse(final_merged_data$label_id== 786, 1, 0)#Creates a flag for 786, Low Risk,low_risk
final_merged_data$medium_risk=ifelse(final_merged_data$label_id== 785, 1, 0)#Creates a flag for 785, Medium Risk,medium_risk
final_merged_data$high_risk=ifelse(final_merged_data$label_id== 783, 1, 0)#Creates a flag for 783, High Risk, high_risk
final_merged_data$higher_income=ifelse(final_merged_data$label_id== 779, 1, 0)#Creates a flag for 779, Higher Income,high_income 
final_merged_data$low_income=ifelse(final_merged_data$label_id== 782, 1, 0)#Creates a flag for 782, Low Income,low_income 
final_merged_data$fixed_income=ifelse(final_merged_data$label_id== 752, 1, 0)#Creates a flag for 752, Fixed Income,fixed_income
final_merged_data$low_profit=ifelse(final_merged_data$label_id== 781, 1, 0)#Creates a flag for low profitability, Low Profit,low_profit
final_merged_data$high_profit=ifelse(final_merged_data$label_id== 778, 1, 0)#Creates a flag for low profitability, High Profit,high_profit
final_merged_data$moderate_profit=ifelse(final_merged_data$label_id== 780, 1, 0)#Creates a flag for moderate profitability, Moderate Profit,moderate_profit
final_merged_data$industry_tag=ifelse(final_merged_data$label_id== 548, 1, 0)#Creates a flag for industry activity more youth
final_merged_data$class_elim=ifelse(final_merged_data$label_id== 813, 1, 0)#Creates a flag for Elimination of Class, higher with young females 
final_merged_data$p2p=ifelse(final_merged_data$label_id== 757, 1, 0)#Creates a flag for peer-to-peer, higher with older men
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 773, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 774, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 775, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 787, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 786, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 785, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 783, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 779, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 782, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 752, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 781, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 778, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 780, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 548, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 172, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 813, NULL, final_merged_data$label_id)
final_merged_data$label_id <- ifelse(final_merged_data$label_id == 757, NULL, final_merged_data$label_id)
final_merged_data$tech=ifelse(final_merged_data$big_category == 'Technology', 1, 0)#Creates a flag for technology category, higher with men

## Create flags for specific phone brands
final_merged_data$meilu=ifelse(final_merged_data$phone_brand == 'Meilu', 1, 0)#Creates a flag for Meilu, higher with men 23-26
final_merged_data$vivo=ifelse(final_merged_data$phone_brand == 'vivo', 1, 0)#Creates a flag for vivo, higher with women under 23
final_merged_data$Jinli=ifelse(final_merged_data$phone_brand == 'Jinli', 1, 0)#Creates a flag for Jinli, higher with men over 39
final_merged_data$CNMO=ifelse(final_merged_data$phone_brand == 'CNMO', 1, 0)#Creates a flag for CNMO, higher with men
final_merged_data$Proso=ifelse(final_merged_data$phone_brand == 'Proso', 1, 0)#Creates a flag for Proso, higher with women over 43

summary(final_merged_data)
n_distinct(final_merged_data$device_id) #23,038 as expected
head(final_merged_data)

rm(merged_data_2)
rm(merged_data_3)
rm(merged_data_4)
rm(device_ids_matched)

###Writing to csv file (note hashed out because I don't want to rewrite)
#write_csv(final_merged_data, "merged_data_Jordan.csv")

#Reducing final_merged_data to only device ids in the training set#
final_merged_data_train <- final_merged_data %>%
  filter(device_id %in% device_ids_train$device_id)
n_distinct(final_merged_data_train$device_id) #17,278 as expected

summary(final_merged_data_train)

#Creating each column for the final training set
by_device_id_train_counts_1 <- final_merged_data_train %>%
  group_by(
    device_id
  ) %>%
  dplyr::summarize(
    event_count = n_distinct(event_id, na.rm = TRUE), 
    app_id_unique_count = n_distinct(app_id, na.rm = TRUE), 
    is_active_sum = sum(is_active, na.rm = TRUE),
    label_id_unique_count = n_distinct(label_id, na.rm = TRUE),
    timestamp_unique_count = n_distinct(timestamp, na.rm = TRUE),
    earilest_timestamp = min(timestamp),
    latest_timestamp = max(timestamp),
    days_between_first_and_last_timestamp = difftime(max(timestamp, na.rm = TRUE), min(timestamp, na.rm = TRUE), units = "days"),
    hours_between_first_and_last_timestamp = difftime(max(timestamp, na.rm = TRUE), min(timestamp, na.rm = TRUE), units = "hours"),
    big_category_unique_count = n_distinct(big_category, na.rm = TRUE)
  )

by_device_id_train_counts_2 <- final_merged_data_train %>%
  group_by(
    device_id,
    big_category
  ) %>%
  dplyr::summarize(
    category_count = n()
  ) %>%
  filter(
    !is.na(big_category)
  ) %>%
  spread(
    big_category,
    category_count,
    fill = 0
  )

names(by_device_id_train_counts_2)[2:ncol(by_device_id_train_counts_2)] <- str_c(
  'bc_',
  str_replace_all(
    names(by_device_id_train_counts_2)[2:ncol(by_device_id_train_counts_2 )],
    ' ',
    ''
  )
)

#merging counts together and adding demographics back into training set
by_device_id_final_train <- left_join(by_device_id_train_counts_1, by_device_id_train_counts_2, by = 'device_id') %>%
  left_join(merged_data_1, by = "device_id")

rm(by_device_id_train_counts_1)
rm(by_device_id_train_counts_2)
rm(final_merged_data_train)

#writing the final train dataset to a csv
write_csv(by_device_id_final_train, "by_device_id_final_train.csv")

#Reducing final_merged_data to only device ids in the test set#
final_merged_data_test <- final_merged_data %>%
  filter(!device_id %in% device_ids_train$device_id)
n_distinct(final_merged_data_test$device_id) #5,760 as expected

#Creating each column for the final test set
by_device_id_test_counts_1 <- final_merged_data_test %>%
  group_by(
    device_id
  ) %>%
  dplyr::summarize(
    event_count = n_distinct(event_id, na.rm = TRUE), 
    app_id_unique_count = n_distinct(app_id, na.rm = TRUE), 
    is_active_sum = sum(is_active, na.rm = TRUE),
    label_id_unique_count = n_distinct(label_id, na.rm = TRUE),
    timestamp_unique_count = n_distinct(timestamp, na.rm = TRUE),
    earilest_timestamp = min(timestamp),
    latest_timestamp = max(timestamp),
    days_between_first_and_last_timestamp = difftime(max(timestamp, na.rm = TRUE), min(timestamp, na.rm = TRUE), units = "days"),
    hours_between_first_and_last_timestamp = difftime(max(timestamp, na.rm = TRUE), min(timestamp, na.rm = TRUE), units = "hours"),
    big_category_unique_count = n_distinct(big_category, na.rm = TRUE)
  )

by_device_id_test_counts_2 <- final_merged_data_test %>%
  group_by(
    device_id,
    big_category
  ) %>%
  dplyr::summarize(
    category_count = n()
  ) %>%
  filter(
    !is.na(big_category)
  ) %>%
  spread(
    big_category,
    category_count,
    fill = 0
  )

names(by_device_id_test_counts_2)[2:ncol(by_device_id_test_counts_2)] <- str_c(
  'bc_',
  str_replace_all(
    names(by_device_id_test_counts_2)[2:ncol(by_device_id_test_counts_2 )],
    ' ',
    ''
  )
)

#merging counts together and adding demographics back into training set
by_device_id_final_test <- left_join(by_device_id_test_counts_1, by_device_id_test_counts_2, by = 'device_id') %>%
  left_join(merged_data_1, by = "device_id")

#writing the final train dataset to a csv
write_csv(by_device_id_final_test, "by_device_id_final_test.csv")

rm(by_device_id_test_counts_1)
rm(by_device_id_test_counts_2)
rm(final_merged_data_test)
rm(device_ids_test)
rm(device_ids_train)

#write_csv(test, "sample_demographic_file.csv")
#device_id_matched <- as.data.frame(test$device_id)
#write_csv(device_id_matched, "List of 23038 device ids that match")

#set.seed(1)
#device_ids_train <- sample_frac(device_ids_matched, size = 0.75)
#device_ids_test <- device_ids_matched %>%
#  filter(!device_id %in% device_ids_train$device_id)

#write_csv(device_ids_train, "device ids for training set")
#write.csv(device_ids_test, "device ids for test set")


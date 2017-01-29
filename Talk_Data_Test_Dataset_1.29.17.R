#R Code modified from publicly available Kaggle sources
#https://www.kaggle.com/rissonyao/talkingdata-mobile-user-demographics/talkingdata-eda-benchmarking
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("pryr")
#install.packages("devtools")
rm(list=ls())
library(DataExplorer); library(lars); library(caret); library(e1071); 
library(class);library(leaps); library(Hmisc); library(plyr); library(devtools); library(ggplot2); # Data visualization
library(readr); # CSV file I/O, e.g. the read_csv function; 
library(lubridate); library(date) # date functions
library(data.table); # Using data.table to speed up runtime
library(repr); # Use repr to adjust plot size
library(profvis); library(dplyr); library(ggmap); library(knitr);library(scales) #for scaling dates
library(reshape2); library(bit64); library(showtext);library(extrafont); library(zoo); library(dummy);
library(doParallel)
registerDoParallel(cores = 4)
require(data.table)


# load the data
gat.tr <- fread("C:\\Users\\user1\\Documents\\Northwestern University\\PREDICT 498\\DATA\\gender_age_train\\gender_age_train.csv", head=T, colClasses = c("character", "character", "integer", "character"))# load the "gender, age training data" file
names(gat.tr) #"device_id" "gender"    "age"       "group" 

setkeyv(gat.tr, "device_id")

dim(gat.tr) # 74645     4
summary(gat.tr) #device_id  gender  age  group

set.seed(1306) # set random number generator seed to enable
# repeatability of results
n <- dim(gat.tr)[1]
tr.val.test <- sample(n, round(n/3)) # randomly sample 33% train, validate, and test
ga.train <- gat.tr[tr.val.test,]
ga.train.val <- gat.tr[-tr.val.test,]
vt <- dim(ga.train.val)[1]
val.train <- sample(vt, round(vt/2)) # randomly sample remaining 66% into validate, and test
ga.val <- ga.train.val [-val.train,]
ga.test <- ga.train.val [val.train,]

dim(ga.train) #[1] 24882     4
summary(ga.train)
dim(ga.val) #[1] 24881     4
summary(ga.val)
dim(ga.test) #[1] 24882     4
summary(ga.test)
rm(ga.train.val, ga.val, ga.train, gat.tr)
#Reloading third file brand Device Type data and setting device ID as the key to create test dataset
brand <- fread("C:\\Users\\user1\\Documents\\Northwestern University\\PREDICT 498\\DATA\\phone_brand_device_model.csv\\Brand_Find_Replace.txt", head= T, colClasses = c("character", "character", "character")) # load the "brand models" file
names(brand) #"device_id"    "phone_brand"  "device_model"
dim(brand) #187245      3

#Remove duplicates
setkey(brand,NULL)
brand <-unique(brand)
dim(brand) #186722      3
setkey(brand,device_id)

brand.test <- merge(ga.test,brand,by="device_id")


#Remove old object to save memory **Very important with dataset this large**
rm(brand,ga.test)
head(brand.test)
dim(brand.test) # 37323     6
head(brand.test)
summary(brand.test)

#Add event data using device_id as key
events <- read.csv("C:\\Users\\user1\\Documents\\Northwestern University\\PREDICT 498\\Data\\events.csv\\events.csv", header=T, numerals='no.loss') # load the "mobile events" file 
names(events) # "event_id"  "device_id" "timestamp" "longitude" "latitude" 
dim(events) #3252950       5
#Remove duplicates: None found
#setkey(events,NULL)
#events <-unique(events)
#dim(events)
events$timestamp <- as.Date(events$timestamp, format=c('%Y-%m-%d %H:%M:%S'))
events$event_id <- as.character(events$event_id)
events$device_id <- as.character(events$device_id)
events <- data.table(events)
summary(events)
setkey(events,device_id)

dim(events) #3252950       5


gat_br_events <- merge(events, brand.test, by="device_id")

#Remove old objects
rm(brand.test,events)
dim(gat_br_events) #410927     10
summary(gat_br_events)
head(gat_br_events) # device_id event_id timestamp longitude latitude gender age  group phone_brand device_model

#Change key to event ID to merge with app events
setkey(gat_br_events,event_id)


#Add app events and merge to new dataset
app_events <- read.csv("C:\\Users\\user1\\Documents\\Northwestern University\\PREDICT 498\\Data\\app_events.csv\\app_events.csv", colClasses = c("character", "character","numeric", "numeric"))
# load the "application events" file
app_events<-app_events[,c(1,2,4)]
names(app_events) #"event_id"     "app_id"   "is_active" 
app_events<- data.table(app_events)
setkey(app_events,event_id) #

dim(app_events) # 32473067        3

mobile2 <- merge(app_events,gat_br_events,by="event_id")

rm(gat_br_events, app_events);gc()
mobile2 <- data.table(mobile2)
summary(mobile2)
dim(mobile2) # 4078104      12
head(mobile2)

#Change key to app_id to merge app_label data
setkey(mobile2,app_id)
head(mobile2)

##Load app label data and merge with label categories before merging with mobile2
app_labels <- fread("C:\\Users\\user1\\Documents\\Northwestern University\\PREDICT 498\\Data\\app_labels.csv\\app_labels.csv", colClasses = c("character", "character")) # load the "app labels" file
names(app_labels) #"app_id"   "label_id"
setkey(app_labels,label_id) #
dim(app_labels) #459943      2
summary(app_labels)
app_labels <- data.table(app_labels)
#Load Label Cateogries
label_cat <- read.csv("C:\\Users\\user1\\Documents\\Northwestern University\\PREDICT 498\\Data\\label_categories.csv\\label_categories.csv", colClasses = c("character", "character")) # load the "label categories" file
names(label_cat) #"label_id" "category" "big_category"
label_cat <- data.table(label_cat)
setkey(label_cat,label_id)
dim(label_cat) #930   3
summary(label_cat) 
label_cat <- data.table(label_cat)
labels <- merge(label_cat,app_labels,by="label_id", allow.cartesian=TRUE)
rm(label_cat,app_labels)
dim(labels) #459943      4
setkey(labels, NULL)
labels <- unique(labels)
dim(labels) #459452      4
names(labels) #"label_id"     "category"     "big_category" "app_id"   
setkey(labels, "app_id")
summary(labels)
labels <- data.table(labels)
labels$flag_1=ifelse(labels$category== "High Flow", 1, 0)#Creates a flag for 773,High Flow,flag_1
summary(labels)

## Merge large mobile data table with labels
mobile.test <- merge(labels, mobile2, by = "app_id", allow.cartesian=TRUE)
mobile.test <- data.table(mobile.test)
dim(mobile.test) #26,241,748       16
#Remove old object and check memory allocation
rm(mobile2, labels);gc()
summary(mobile.test) 
head(mobile.test)
names(mobile.test) # [1] "app_id"       "label_id"     "category"     "big_category" "event_id"     "is_installed" "is_active"   
#[8] "device_id"    "timestamp"    "longitude"    "latitude"     "gender"       "age"          "group"       
#[15] "phone_brand"  "device_model"
setkey(mobile.test, device_id)
unique_devices <- unique(mobile.test$device_id)
summary(unique_devices) #Length 7797 devices

write.csv(mobile.test, file = "C:\\Users\\user1\\Documents\\Northwestern University\\PREDICT 498\\Data\\mobile.test.csv")

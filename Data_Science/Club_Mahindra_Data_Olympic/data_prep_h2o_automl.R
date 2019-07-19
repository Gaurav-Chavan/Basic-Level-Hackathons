#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxx Import Libraries and set path xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Install Missing Packages Autoamtically.
list.of.packages <- c("lubridate", "readxl","h2o","mice","VIM","Hmisc","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)




# Load Libraries
library(h2o) # for data modelling
library(readxl) # for excel import
library(lubridate) # for date operations
library(mice)  #Missing Values Imputation 
library(VIM) #plotting
library(Hmisc) # Imputing Missing Values
library(dummies) # For one-hot encoding




options(scipen = 999999) # dealing with reciprocals



setwd("~/Club Mahindra")

#--Import Dataset
train <- read.csv("train.csv",header = T,sep = ",",na.strings = "",stringsAsFactors = F)
test <- read.csv("test.csv",header = T,sep = ",",na.strings = "",stringsAsFactors = F)


#---Data Checkpoints ----------------#
summary(train)
str(train)



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxxxxxxxxxxx Data Preparation  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#--  Remove identifier Variables as model build should be dependant upon features 
#--  & independant of Identifiers
 
train$reservation_id <-NULL
test$reservation_id <-NULL


#---member id
#check for uniqueness 
unique(unique(train$memberid) %in% unique(test$memberid))
# No match found hence can be removed

train$memberid <- NULL
test$memberid <- NULL


#--persontravellingid                
#check for uniqueness 
unique(unique(train$persontravellingid) %in% unique(test$persontravellingid))
#  match found hence cant be removed, but in private set there can be outlier category hence removing it.
train$persontravellingid <- NULL
test$persontravellingid <- NULL


#--resort_id
unique(unique(train$resort_id) %in% unique(test$resort_id))
#  match found hence cant be removed, but in private set there can be outlier category hence removing it.
train$resort_id <- NULL
test$resort_id <- NULL


#xxxxxxxxxxxxx Dealing with Dates  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxx For train set

#-----------  booking_date ----------------------#

# Converting character Date into Actual Date
train$booking_date <- dmy(train$booking_date)

# Get Year
train$booking_Year <- year(train$booking_date)

# Get month
train$booking_month <- month(train$booking_date)

# Get day of month
train$booking_day <- day(train$booking_date)

# Get day of week
train$booking_wday <- wday(train$booking_date)

# Get day of quarter
train$booking_qday <- qday(train$booking_date)

# Get week
train$booking_week <- week(train$booking_date)

# Get Quarter
train$booking_quarter <- quarter(train$booking_date)


#-----------  checkin_date  ----------------------#


# Converting character Date into Actual Date
train$checkin_date <- dmy(train$checkin_date)

# Get Year
train$checkin_Year <- year(train$checkin_date)

# Get month
train$checkin_month <- month(train$checkin_date)

# Get day of month
train$checkin_day <- day(train$checkin_date)

# Get day of week
train$checkin_wday <- wday(train$checkin_date)

# Get day of quarter
train$checkin_qday <- qday(train$checkin_date)

# Get week
train$checkin_week <- week(train$checkin_date)

# Get Quarter
train$checkin_quarter <- quarter(train$checkin_date)


#-----------  checkin_date  ----------------------#


# Converting character Date into Actual Date
train$checkout_date <- dmy(train$checkout_date)

# Get Year
train$checkout_Year <- year(train$checkout_date)

# Get month
train$checkout_month <- month(train$checkout_date)

# Get day of month
train$checkout_day <- day(train$checkout_date)

# Get day of week
train$checkout_wday <- wday(train$checkout_date)

# Get day of quarter
train$checkout_qday <- qday(train$checkout_date)

# Get week
train$checkout_week <- week(train$checkout_date)

# Get Quarter
train$checkout_quarter <- quarter(train$checkout_date)


#xxxxxx For test set

#-----------  booking_date ----------------------#

# Converting character Date into Actual Date
test$booking_date <- dmy(test$booking_date)

# Get Year
test$booking_Year <- year(test$booking_date)

# Get month
test$booking_month <- month(test$booking_date)

# Get day of month
test$booking_day <- day(test$booking_date)

# Get day of week
test$booking_wday <- wday(test$booking_date)

# Get day of quarter
test$booking_qday <- qday(test$booking_date)

# Get week
test$booking_week <- week(test$booking_date)

# Get Quarter
test$booking_quarter <- quarter(test$booking_date)


#-----------  checkin_date  ----------------------#


# Converting character Date into Actual Date
test$checkin_date <- dmy(test$checkin_date)

# Get Year
test$checkin_Year <- year(test$checkin_date)

# Get month
test$checkin_month <- month(test$checkin_date)

# Get day of month
test$checkin_day <- day(test$checkin_date)

# Get day of week
test$checkin_wday <- wday(test$checkin_date)

# Get day of quarter
test$checkin_qday <- qday(test$checkin_date)

# Get week
test$checkin_week <- week(test$checkin_date)

# Get Quarter
test$checkin_quarter <- quarter(test$checkin_date)


#-----------  checkout_date  ----------------------#


# Converting character Date into Actual Date
test$checkout_date <- dmy(test$checkout_date)

# Get Year
test$checkout_Year <- year(test$checkout_date)

# Get month
test$checkout_month <- month(test$checkout_date)

# Get day of month
test$checkout_day <- day(test$checkout_date)

# Get day of week
test$checkout_wday <- wday(test$checkout_date)

# Get day of quarter
test$checkout_qday <- qday(test$checkout_date)

# Get week
test$checkout_week <- week(test$checkout_date)

# Get Quarter
test$checkout_quarter <- quarter(test$checkout_date)







#xxxxxxxxxxxxx Dealing with Missing Values  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#

colSums(is.na(train))
colSums(is.na(test))

summary(train)
summary(test)

# season_holidayed_code               
# state_code_residence 
# roomnights -> negative values


# season_holidayed_code impute with  median value
train$season_holidayed_code <- with(train, impute(season_holidayed_code,median))
test$season_holidayed_code <- with(test, impute(season_holidayed_code,median))


# state_code_residence impute with  median value
train$state_code_residence <- with(train, impute(state_code_residence,median))
test$state_code_residence <- with(test, impute(state_code_residence,median))

# roomnights
train$roomnights[train$roomnights < 0] <- NA
train$roomnights <- with(train, impute(roomnights,median))

#write the data and import it freshly as imputation leads to change in data structure.
write.csv(train,"train_complete.csv",row.names = F)

write.csv(test,"test_complete.csv",row.names = F)

rm(train,test)


train <- read.csv("train_complete.csv",header = T,stringsAsFactors = F)

test <- read.csv("test_complete.csv",header = T,stringsAsFactors = F)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxx Categorizing Features Properly into Continuous and Categorical 


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


str(train)

# channel_code                      
unique(train$channel_code)
unique(test$channel_code)

# converting channel_code into categorical
train$channel_code <- factor(train$channel_code,levels = c(1,2,3),labels = c(1,2,3) )
test$channel_code <- factor(test$channel_code,levels = c(1,2,3),labels = c(1,2,3) )



# main_product_code                      
unique(train$main_product_code)
unique(test$main_product_code)

# converting main_product_code into categorical
train$main_product_code <- factor(train$main_product_code,levels = c(1,2,3,4,7),labels = c(1,2,3,4,7) )
test$main_product_code <- factor(test$main_product_code,levels = c(1,2,3,4,7),labels = c(1,2,3,4,7) )


# resort_region_code
unique(train$resort_region_code)
unique(test$resort_region_code)

train$resort_region_code <- factor(train$resort_region_code,levels = c(1,2,3),labels = c(1,2,3) )
test$resort_region_code <- factor(test$resort_region_code,levels = c(1,2,3),labels = c(1,2,3) )


# resort_type_code
unique(train$resort_type_code)
unique(test$resort_type_code)

# Encode 0 to 6 for better categorization
train$resort_type_code[train$resort_type_code==0] <- 6
test$resort_type_code[test$resort_type_code==0] <- 6


train$resort_type_code <- factor(train$resort_type_code,levels = c(1,2,3,4,5,6,7),labels = c(1,2,3,4,5,6,7) )
test$resort_type_code <- factor(test$resort_type_code,levels = c(1,2,3,4,5,6,7),labels = c(1,2,3,4,5,6,7) )


# room_type_booked_code
unique(train$room_type_booked_code)
unique(test$room_type_booked_code)

train$room_type_booked_code <- factor(train$room_type_booked_code,levels = c(1,2,3,4,5,6),labels = c(1,2,3,4,5,6) )
test$room_type_booked_code <- factor(test$room_type_booked_code,levels = c(1,2,3,4,5,6),labels = c(1,2,3,4,5,6) )

# season_holidayed_code
unique(train$season_holidayed_code)
unique(test$season_holidayed_code)

train$season_holidayed_code <- factor(train$season_holidayed_code,levels = c(1,2,3,4),labels = c(1,2,3,4) )
test$season_holidayed_code <- factor(test$season_holidayed_code,levels = c(1,2,3,4),labels = c(1,2,3,4) )

# season_holidayed_code
unique(train$season_holidayed_code)
unique(test$season_holidayed_code)

train$season_holidayed_code <- factor(train$season_holidayed_code,levels = c(1,2,3,4),labels = c(1,2,3,4) )
test$season_holidayed_code <- factor(test$season_holidayed_code,levels = c(1,2,3,4),labels = c(1,2,3,4) )

# state_code_residence
sort(unique(train$state_code_residence))
sort(unique(test$state_code_residence))

train$state_code_residence <- factor(train$state_code_residence,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)
,labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)
 )
test$state_code_residence <- factor(test$state_code_residence,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)
,labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)
 )


# state_code_resort
sort(unique(train$state_code_resort))
sort(unique(test$state_code_resort))
#--- Its actually categorical data, but as there are many less leads pertaining to
#--- how the state code will be on out of box test set, its better to treat it as continuous-###



# member_age_buckets  
sort(unique(train$member_age_buckets))
sort(unique(test$member_age_buckets))

train$member_age_buckets <- factor(train$member_age_buckets,levels = c("A","B","C","D","E","F","G","H","I","J")
                                     ,labels = c("A","B","C","D","E","F","G","H","I","J")
)

test$member_age_buckets <- factor(test$member_age_buckets,levels = c("A","B","C","D","E","F","G","H","I","J")
                                  ,labels = c("A","B","C","D","E","F","G","H","I","J")
)

# booking_type_code
sort(unique(train$booking_type_code))
sort(unique(test$booking_type_code))

# converting booking_type_code into categorical
train$booking_type_code <- factor(train$booking_type_code,levels = c(1,2),labels = c(1,2) )
test$booking_type_code <- factor(test$booking_type_code,levels = c(1,2),labels = c(1,2) )


# cluster_code
sort(unique(train$cluster_code))
sort(unique(test$cluster_code))

train$cluster_code <- factor(train$cluster_code,levels = c("A","B","C","D","E","F")
                                   ,labels = c("A","B","C","D","E","F")
)

test$cluster_code <- factor(test$cluster_code,levels = c("A","B","C","D","E","F")
                                  ,labels = c("A","B","C","D","E","F")
)

# reservationstatusid_code
sort(unique(train$reservationstatusid_code))
sort(unique(test$reservationstatusid_code))


train$reservationstatusid_code <- factor(train$reservationstatusid_code,levels = c("A","B","C","D")
                             ,labels = c("A","B","C","D")
)

test$reservationstatusid_code <- factor(test$reservationstatusid_code,levels = c("A","B","C","D")
                            ,labels = c("A","B","C","D")
)



#---------------------- Categorizing Date Features  --------------------#


# booking_month, checkin_month,checkout_month  Convert into factors
train$booking_month <- factor(train$booking_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
test$booking_month <-  factor(test$booking_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))

train$checkin_month <- factor(train$checkin_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
test$checkin_month <-  factor(test$checkin_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))

train$checkout_month <- factor(train$checkout_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
test$checkout_month <-  factor(test$checkout_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))


# booking_day , checkin_day ,checkout_day   Convert into factors
train$booking_day <- factor(train$booking_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)

test$booking_day <- factor(test$booking_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)

train$checkin_day <- factor(train$checkin_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)

test$checkin_day <- factor(test$checkin_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                           labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)

train$checkout_day <- factor(train$checkout_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)

test$checkout_day <- factor(test$checkout_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                           labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)


# booking_qday  , checkin_qday  ,checkout_qday    Convert into factors
train$booking_qday <- factor(train$booking_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                               41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                               81,82,83,84,85,86,87,88,89,90,91,92),
                               
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                          81,82,83,84,85,86,87,88,89,90,91,92)
)


test$booking_qday <- factor(test$booking_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                             41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                             81,82,83,84,85,86,87,88,89,90,91,92),
                              
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                         81,82,83,84,85,86,87,88,89,90,91,92)
)


train$checkin_qday <- factor(train$checkin_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                           41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                           81,82,83,84,85,86,87,88,89,90,91,92),
                             
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                        41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                        81,82,83,84,85,86,87,88,89,90,91,92)
)


test$checkin_qday <- factor(test$checkin_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                         81,82,83,84,85,86,87,88,89,90,91,92),
                            
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                       41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                       81,82,83,84,85,86,87,88,89,90,91,92)
)

train$checkout_qday <- factor(train$checkout_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                           41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                           81,82,83,84,85,86,87,88,89,90,91,92),
                             
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                        41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                        81,82,83,84,85,86,87,88,89,90,91,92)
)


test$checkout_qday <- factor(test$checkout_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                         81,82,83,84,85,86,87,88,89,90,91,92),
                            
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                       41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                       81,82,83,84,85,86,87,88,89,90,91,92)
)


# booking_week  , checkin_week  ,checkout_week    Convert into factors
train$booking_week <- factor(train$booking_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                             41,42,43,44,45,46,47,48,49,50,51,52,53),
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53))

test$booking_week <- factor(test$booking_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                           41,42,43,44,45,46,47,48,49,50,51,52,53),
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                         41,42,43,44,45,46,47,48,49,50,51,52,53))

train$checkin_week <- factor(train$checkin_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                         41,42,43,44,45,46,47,48,49,50,51,52,53),
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                        41,42,43,44,45,46,47,48,49,50,51,52,53))

test$checkin_week <- factor(test$checkin_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                       41,42,43,44,45,46,47,48,49,50,51,52,53),
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                       41,42,43,44,45,46,47,48,49,50,51,52,53))

train$checkout_week <- factor(train$checkout_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                         41,42,43,44,45,46,47,48,49,50,51,52,53),
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                        41,42,43,44,45,46,47,48,49,50,51,52,53))

test$checkout_week <- factor(test$checkout_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                       41,42,43,44,45,46,47,48,49,50,51,52,53),
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                       41,42,43,44,45,46,47,48,49,50,51,52,53))


# booking_quarter  , checkin_quarter  ,checkout_quarter    Convert into factors
train$booking_quarter <- factor(train$booking_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

test$booking_quarter <- factor(test$booking_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

train$checkin_quarter <- factor(train$checkin_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

test$checkin_quarter <- factor(test$checkin_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

train$checkout_quarter <- factor(train$checkout_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

test$checkout_quarter <- factor(test$checkout_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))



#booking_wday  , checkin_wday  ,checkout_wday Convert into factors

train$booking_wday <- factor(train$booking_wday,levels=c(1,2,3,4,5,6,7),labels=c(1,2,3,4,5,6,7))

test$booking_wday <- factor(test$booking_wday,levels=c(1,2,3,4,5,6,7),labels=c(1,2,3,4,5,6,7))


train$checkin_wday <- factor(train$checkin_wday,levels=c(1,2,3,4,5,6,7),labels=c(1,2,3,4,5,6,7))

test$checkin_wday <- factor(test$checkin_wday,levels=c(1,2,3,4,5,6,7),labels=c(1,2,3,4,5,6,7))

train$checkout_wday <- factor(train$checkout_wday,levels=c(1,2,3,4,5,6,7),labels=c(1,2,3,4,5,6,7))

test$checkout_wday <- factor(test$checkout_wday,levels=c(1,2,3,4,5,6,7),labels=c(1,2,3,4,5,6,7))


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#---------------------- Feature Engineering ---------------------------------#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


train$booking_date <- ymd(train$booking_date)
test$booking_date <- ymd(test$booking_date)

train$checkin_date <- ymd(train$checkin_date)
test$checkin_date <- ymd(test$checkin_date)

train$checkout_date <- ymd(train$checkout_date)
test$checkout_date <- ymd(test$checkout_date)



# booking_date - checkin_date = time_gap
train$time_gap_days <- as.numeric(difftime(train$checkin_date,train$booking_date,units = "days"))
train$time_gap_weeks <- as.numeric(difftime(train$checkin_date,train$booking_date,units = "weeks"))

test$time_gap_days <- as.numeric(difftime(test$checkin_date,test$booking_date,units = "days"))
test$time_gap_weeks <- as.numeric(difftime(test$checkin_date,test$booking_date,units = "weeks"))


# checkin_date - checkout_date = duration_stay
train$duration_stay_days <- as.numeric(difftime(train$checkout_date,train$checkin_date,units = "days"))
train$duration_stay_weeks <- as.numeric(difftime(train$checkout_date,train$checkin_date,units = "weeks"))

test$duration_stay_days <- as.numeric(difftime(test$checkout_date,test$checkin_date,units = "days"))
test$duration_stay_weeks <- as.numeric(difftime(test$checkout_date,test$checkin_date,units = "weeks"))


# numberofadults + numberofchildren = family_size
train$family_size <- as.numeric(train$numberofadults + train$numberofchildren) 
test$family_size <- as.numeric(test$numberofadults + test$numberofchildren)

# numberofadults/family_size = adult_ratio
train$adult_ratio <- train$numberofadults/ train$family_size 

# Dealing with NA
train$adult_ratio[is.na(train$adult_ratio)] <- 0

test$adult_ratio <- test$numberofadults/test$family_size

# Dealing with NA
test$adult_ratio[is.na(test$adult_ratio)] <- 0



# numberofchildrens/family_size = kids_ratio

train$kids_ratio <- train$numberofchildren/train$family_size 

# Dealing with NA
train$kids_ratio[is.na(train$kids_ratio)] <- 0


test$kids_ratio <- test$numberofchildren/test$family_size
# Dealing with NA
test$kids_ratio[is.na(test$kids_ratio)] <- 0


# Remove Dates 

train$booking_date <- NULL
train$checkin_date <- NULL
train$checkout_date <- NULL


test$booking_date <- NULL
test$checkin_date <- NULL
test$checkout_date <- NULL



# Type Casting  


train$numberofadults  <- as.numeric( train$numberofadults)
train$numberofchildren  <- as.numeric( train$numberofchildren)
train$roomnights <- as.numeric( train$roomnights)
train$state_code_resort <- as.numeric(train$state_code_resort) 
train$total_pax <- as.numeric(train$total_pax)
train$booking_Year <- as.numeric(train$booking_Year)
train$checkin_Year <- as.numeric(train$checkin_Year)
train$checkout_Year <- as.numeric(train$checkout_Year)

test$numberofadults  <- as.numeric( test$numberofadults)
test$numberofchildren  <- as.numeric( test$numberofchildren)
test$roomnights <- as.numeric( test$roomnights)
test$state_code_resort <- as.numeric(test$state_code_resort) 
test$total_pax <- as.numeric(test$total_pax)
test$booking_Year <- as.numeric(test$booking_Year)
test$checkin_Year <- as.numeric(test$checkin_Year)
test$checkout_Year <- as.numeric(test$checkout_Year)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#---------------------- Feature Normalization ---------------------------------#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Tree based models are not distance based models and can handle varying ranges of features. 
# Hence, Scaling is not required while modelling trees.



# arrange columns
train <- train[,c(1:16,18:45,17)]






#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#---------------- Training using H2o ------------------#

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# Running on 6GB RAm

h2o.init()


output <- "amount_spent_per_room_night_scaled"
input  <- setdiff( names(train), output )

train = as.h2o(train)


# Predictive Modelling for 1 and 1/2 hour removed Random Forest and Deeplearning

aml <- h2o.automl(y = output,x = input,training_frame = train,nfolds = 5,seed = 123,max_runtime_secs = 5400,stopping_metric = "RMSE",exclude_algos = c("DeepLearning","DRF"))




# check the leaderboard
lb <- aml@leaderboard
lb

# get model ids  for all models 
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]

# Get all model stacked ensembled model
se <- h2o.getModel(grep("StackedEnsemble_AllModels",model_ids,value = TRUE)[1])

metalearner <- h2o.getModel(se@model$metalearner$name)

h2o.varimp(metalearner)

gbm <- h2o.getModel(grep("GBM",model_ids,value=TRUE)[1])

imp  <- as.data.frame(h2o.varimp(gbm))

h2o.varimp_plot(gbm)



#-------------- Make Predictions -------------------------#
test = as.h2o(test)


pred <- h2o.predict(gbm, test)

pred = as.data.frame(pred)


sample_submission <- read.csv("sample_submission.csv", na.strings="", stringsAsFactors=FALSE)

sample_submission$amount_spent_per_room_night_scaled <- pred$predict

write.csv(sample_submission,"h2o_Automl.csv",row.names = F)





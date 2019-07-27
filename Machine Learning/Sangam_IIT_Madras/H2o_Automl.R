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

options(scipen = 999999) # dealing with reciprocals


# Import Train,Test & Sample_Submission  files
Train <- read.csv("Train.csv", na.strings="", stringsAsFactors=FALSE)
Test <-  read.csv("Test.csv", na.strings="", stringsAsFactors=FALSE)
sample_submission <- read.csv("sample_submission.csv", na.strings="", stringsAsFactors=FALSE)



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#---------------------- Data Preparation Phase. ---------------------------------#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# Check structure of data & Missingness if any
str(Train)
str(Test)

colSums(is.na(Train)) # no missing values found
colSums(is.na(Test)) # no missing values found



# ---------------- Converting character Date into Actual Date ----------------------#

# For train & test set

Train$date_time <- ymd_hms(Train$date_time)

# Get Year
Train$Year <- year(Train$date_time)

# Get month
Train$month <- month(Train$date_time)

# Get day of month
Train$day <- day(Train$date_time)

# Get day of week
Train$wday <- wday(Train$date_time)

# Get day of quarter
Train$qday <- qday(Train$date_time)

# Get week
Train$week <- week(Train$date_time)


# Get Quarter
Train$quarter <- quarter(Train$date_time)

# Get Hour
Train$hour <- lubridate::hour(Train$date_time)

#Get Minutes 
Train$minutes <- lubridate::minute(Train$date_time)

#Get Seconds
Train$seconds <- lubridate::second(Train$date_time)

# Check if seconds  & minutes contain one constant value, if so remove it as it has zero variance.
unique(Train$seconds)
unique(Train$minutes)

Train$seconds <- NULL
Train$minutes <- NULL



# Similar operation goes to test dataset
Test$date_time <- ymd_hms(Test$date_time)

# Get Year
Test$Year <- year(Test$date_time)

# Get month
Test$month <- month(Test$date_time)

# Get day of month
Test$day <- day(Test$date_time)

# Get day of week
Test$wday <- wday(Test$date_time)

# Get day of quarter
Test$qday <- qday(Test$date_time)

# Get week
Test$week <- week(Test$date_time)


# Get Quarter
Test$quarter <- quarter(Test$date_time)

# Get Hour
Test$hour <- lubridate::hour(Test$date_time)


# Remove unwanted features
Train$date_time <- NULL
Test$date_time <- NULL


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#---------------------- Feature Engineering ---------------------------------#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# xxxxxxx   Categorical Variables xxxxxxxxxxxx#
str(Train)

# is_holiday
table(Train$is_holiday)

table(Test$is_holiday)

# check for uniqueness, if same holidays are present in test or not. If not removing it would be essential 
unique(unique(Train$is_holiday) %in% unique(Test$is_holiday)) # its unique

# convert into categories
Train$is_holiday <- as.factor(Train$is_holiday)

Test$is_holiday <- as.factor(Test$is_holiday)



# weather_type  
# check for uniqueness, if same weather_type are present in test or not. If not removing it would be essential 
unique(unique(Train$weather_type) %in% unique(Test$weather_type)) # its not unique but test contains all wather type present in train


# convert into categories
Train$weather_type <- as.factor(Train$weather_type)

Test$weather_type <- as.factor(Test$weather_type)


# weather_description
# check for uniqueness, if same weather_description are present in test or not. If not removing it would be essential 
unique(unique(Test$weather_description) %in% unique(Train$weather_description))# its unique


# convert into categories
Train$weather_description <- as.factor(Train$weather_description)

Test$weather_description <- as.factor(Test$weather_description)


#------------------------ Dealing with Dates ---------------------------------------------#
#Year is continuous in nature


# month Convert into factors
Train$month <- factor(Train$month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
Test$month <-  factor(Test$month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))


# Disbursal_day
Train$day <- factor(Train$day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)

Test$day <- factor(Test$day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)


# Disbursal_qday
Train$qday <- factor(Train$qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                             41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                             81,82,83,84,85,86,87,88,89,90,91,92),
                      
                      labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                 41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                 81,82,83,84,85,86,87,88,89,90,91,92)
)


Test$qday <- factor(Test$qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                           41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                           81,82,83,84,85,86,87,88,89,90,91,92),
                     
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                81,82,83,84,85,86,87,88,89,90,91,92)
)

# Disbursal_week
Train$week <- factor(Train$week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                           41,42,43,44,45,46,47,48,49,50,51,52,53),
                      labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                 41,42,43,44,45,46,47,48,49,50,51,52,53))

Test$week <- factor(Test$week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                         41,42,43,44,45,46,47,48,49,50,51,52,53),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                41,42,43,44,45,46,47,48,49,50,51,52,53))

#weekday
Train$wday <- factor(Train$wday,levels=c(1,2,3,4,5,6,7),labels = c(1,2,3,4,5,6,7))
Test$wday <- factor(Test$wday,levels=c(1,2,3,4,5,6,7),labels = c(1,2,3,4,5,6,7))



# Disbursal_quarter
Train$quarter <- factor(Train$quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

Test$quarter <- factor(Test$quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))


# Hour
# Dealing with 0
Train$hour[Train$hour==0] <- 24
Test$hour[Test$hour==0] <- 24

Train$hour <- factor(Train$hour,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                                labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
                    )

Test$hour <- factor(Test$hour,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
)






#---------------------- Correlation Analysis ---------------------------#

# load required libraries
library(caret)
library(corrplot)
library(plyr)



# Identifying numeric variables
numericData <- Train[sapply(Train, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
Train1 <- Train[, -which(colnames(Train) %in% highlyCorCol)]
dim(Train1)

Test1 <- Test[, -which(colnames(Test) %in% highlyCorCol)]


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#---------------------- Predictive Modelling ---------------------------------#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

library(h2o)

# Running on 6GB RAm

h2o.init(max_mem_size = '6G')


output <- "traffic_volume"
input  <- setdiff( names(Train1), output )

train = as.h2o(Train1)


# Predictive Modelling
aml <- h2o.automl(y = output,x = input,training_frame = train,nfolds = 10,seed = 123,exclude_algos = c("DeepLearning","DRF"),stopping_metric = "MSE")


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

h2o.varimp(gbm)
h2o.varimp_plot(gbm)

#-------------- Make Predictions -------------------------#
test = as.h2o(Test1)

pred <- h2o.predict(gbm, test)

pred = as.data.frame(pred)

pred$predict <- round(pred$predict)

Test <-  read.csv("Test.csv", na.strings="", stringsAsFactors=FALSE)

Test$traffic_volume <- pred$predict


write.csv(Test[,c(1,15)],"h2o_Automl_1.csv",row.names = F)


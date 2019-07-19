#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxx Import Libraries and set path xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Install Missing Packages Autoamtically.
list.of.packages <- c("lubridate", "readxl","h2o","mice","VIM","Hmisc","dplyr","caret","tidyr","stringr","corrplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)




# Load Libraries
library(h2o) # for data modelling
library(readxl) # for excel import
library(lubridate) # for date operations
library(mice)  #Missing Values Imputation 
library(VIM) #plotting
library(Hmisc) # Imputing Missing Values
library(caret)  # For ML operations
library(tidyr) # for cleaning data
library(stringr) # for cleaning text data
library(corrplot) # for correlation plotting
library(reshape)

setwd("~/New folder/LFTS")



# Import Train, Test, Sample SUbmission  files
train <- read.csv("train.csv", na.strings="")

test <- read.csv("test_bqCt9Pv.csv", na.strings="")

sample_submission <- read.csv("sample_submission_24jSKY6.csv", na.strings="")



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxxxxxxxxxxx Data Preparation  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Get summary of training data
summary(train)
str(train)



# Remove UniqueID from train as well as test as ML model generated should be 
# expalainable via features and not ID's

train$UniqueID <- NULL
test$UniqueID <- NULL


#-----------  LocationDesc  ----------------------------------------------------#
unique(train$LocationDesc)

train$LocationDesc <- as.character(train$LocationDesc)
test$LocationDesc <- as.character(test$LocationDesc)


# Get State COde form Location Desc


train = transform(train, LocationDesc = colsplit(LocationDesc, split = ",", names = c('City', 'Code')))



#-- Feature Preparation ---------------------------------#

# 1. Identifying and converting Categorical variables

# branch_id

#== check for uniqueness
unique(unique(train$branch_id) %in% unique(test$branch_id))

#== There are ID's which are present in  both train and test set. Hence treated as Categorical
train$branch_id <- as.factor(train$branch_id)
test$branch_id <- as.factor(test$branch_id)


# supplier_id

#== check for uniqueness
unique(unique(train$supplier_id) %in% unique(test$supplier_id))

#== There are certain ID;s which are not in train/test set. Hence removing it.
#== **** Making supplier_id into continuous leads to Unbiased Data.
train$supplier_id <- NULL
test$supplier_id <- NULL


# State_ID
#== check for uniqueness
unique(unique(train$State_ID) %in% unique(test$State_ID))

#== There are ID's which are present in  both train and test set. Hence treated as Categorical
train$State_ID <- as.factor(train$State_ID)
test$State_ID <- as.factor(test$State_ID)




# manufacturer_id

#== check for uniqueness
unique(unique(train$manufacturer_id) %in% unique(test$manufacturer_id))

#== There are certain ID;s which are not in train/test set. Hence removing it.
#== **** Making manufacturer_id into continuous leads to Unbiased Data.
train$manufacturer_id <- NULL
test$manufacturer_id <- NULL



# Current_pincode_ID

#== check for uniqueness
unique(unique(train$Current_pincode_ID) %in% unique(test$Current_pincode_ID))

#== There are certain ID;s which are not in train/test set. Hence removing it.
#== **** Making manufacturer_id into continuous leads to Unbiased Data.
train$Current_pincode_ID <- NULL
test$Current_pincode_ID <- NULL


# Employee_code_ID

#== check for uniqueness
unique(unique(train$Employee_code_ID) %in% unique(test$Employee_code_ID))


#== There are certain ID;s which are not in train/test set. Hence removing it.
#== **** Making manufacturer_id into continuous leads to Unbiased Data.
train$Employee_code_ID <- NULL
test$Employee_code_ID <- NULL



#== Converting Flags into Categorical Type

#  MobileNo_Avl_Flag"  
#= Convert 1 and 0 to YES - NO accordingly (Factor with level as 0 leads to irregularity)

#= Check for distribution
table(train$MobileNo_Avl_Flag)
table(test$MobileNo_Avl_Flag)

#= Converting Numbers into yes/no
train$MobileNo_Avl_Flag[train$MobileNo_Avl_Flag==1] <- "YES"
train$MobileNo_Avl_Flag[train$MobileNo_Avl_Flag==0] <- "NO"

test$MobileNo_Avl_Flag[test$MobileNo_Avl_Flag==1] <- "YES"
test$MobileNo_Avl_Flag[test$MobileNo_Avl_Flag==0] <- "NO"

#= Cnverting Data into factors
train$MobileNo_Avl_Flag <- factor(train$MobileNo_Avl_Flag,levels = c("YES","NO"),labels =  c("YES","NO"))
test$MobileNo_Avl_Flag <- factor(test$MobileNo_Avl_Flag,levels = c("YES","NO"),labels =  c("YES","NO"))




#  Aadhar_flag
table(train$Aadhar_flag)
table(test$Aadhar_flag)



#= Converting Numbers into yes/no
train$Aadhar_flag[train$Aadhar_flag==1] <- "YES"
train$Aadhar_flag[train$Aadhar_flag==0] <- "NO"

test$Aadhar_flag[test$Aadhar_flag==1] <- "YES"
test$Aadhar_flag[test$Aadhar_flag==0] <- "NO"

#= Cnverting Data into factors
train$Aadhar_flag <- factor(train$Aadhar_flag,levels = c("YES","NO"),labels =  c("YES","NO"))
test$Aadhar_flag <- factor(test$Aadhar_flag,levels = c("YES","NO"),labels =  c("YES","NO"))



#  PAN_flag
table(train$PAN_flag)
table(test$PAN_flag)

#= Converting Numbers into yes/no
train$PAN_flag[train$PAN_flag==1] <- "YES"
train$PAN_flag[train$PAN_flag==0] <- "NO"

test$PAN_flag[test$PAN_flag==1] <- "YES"
test$PAN_flag[test$PAN_flag==0] <- "NO"

#= Cnverting Data into factors
train$PAN_flag <- factor(train$PAN_flag,levels = c("YES","NO"),labels =  c("YES","NO"))
test$PAN_flag <- factor(test$PAN_flag,levels = c("YES","NO"),labels =  c("YES","NO"))


#= VoterID_flag                       
table(train$VoterID_flag)
table(test$VoterID_flag)


#= Converting Numbers into yes/no
train$VoterID_flag[train$VoterID_flag==1] <- "YES"
train$VoterID_flag[train$VoterID_flag==0] <- "NO"

test$VoterID_flag[test$VoterID_flag==1] <- "YES"
test$VoterID_flag[test$VoterID_flag==0] <- "NO"

#= Cnverting Data into factors
train$VoterID_flag <- factor(train$VoterID_flag,levels = c("YES","NO"),labels =  c("YES","NO"))
test$VoterID_flag <- factor(test$VoterID_flag,levels = c("YES","NO"),labels =  c("YES","NO"))



#  "Driving_flag"         
table(train$Driving_flag)
table(test$Driving_flag)


#= Converting Numbers into yes/no
train$Driving_flag[train$Driving_flag==1] <- "YES"
train$Driving_flag[train$Driving_flag==0] <- "NO"

test$Driving_flag[test$Driving_flag==1] <- "YES"
test$Driving_flag[test$Driving_flag==0] <- "NO"


#= Cnverting Data into factors
train$Driving_flag <- factor(train$Driving_flag,levels = c("YES","NO"),labels =  c("YES","NO"))
test$Driving_flag <- factor(test$Driving_flag,levels = c("YES","NO"),labels =  c("YES","NO"))



# "Passport_flag" 
table(train$Passport_flag)
table(test$Passport_flag)


#= Converting Numbers into yes/no
train$Passport_flag[train$Passport_flag==1] <- "YES"
train$Passport_flag[train$Passport_flag==0] <- "NO"

test$Passport_flag[test$Passport_flag==1] <- "YES"
test$Passport_flag[test$Passport_flag==0] <- "NO"


#= Cnverting Data into factors
train$Passport_flag <- factor(train$Passport_flag,levels = c("YES","NO"),labels =  c("YES","NO"))
test$Passport_flag <- factor(test$Passport_flag,levels = c("YES","NO"),labels =  c("YES","NO"))




#--------------------- Dealing with Dates --------------------------#

#= Birth Date

#------------Train Set ----------------#

# Converting character Date into Actual Date
train$Date.of.Birth <- dmy(as.character(train$Date.of.Birth))


# Get Year
train$Birth_Year <- year(train$Date.of.Birth)

# Get month
train$Birth_month <- month(train$Date.of.Birth)

# Get day of month
train$Birth_day <- day(train$Date.of.Birth)

# Get day of week
train$Birth_wday <- wday(train$Date.of.Birth)

# Get day of quarter
train$Birth_qday <- qday(train$Date.of.Birth)

# Get week
train$Birth_week <- week(train$Date.of.Birth)

# Get Quarter
train$Birth_quarter <- quarter(train$Date.of.Birth)


#----For test set ---#
# Converting character Date into Actual Date
test$Date.of.Birth <- dmy(as.character(test$Date.of.Birth))


# Get Year
test$Birth_Year <- year(test$Date.of.Birth)

# Get month
test$Birth_month <- month(test$Date.of.Birth)

# Get day of month
test$Birth_day <- day(test$Date.of.Birth)

# Get day of week
test$Birth_wday <- wday(test$Date.of.Birth)

# Get day of quarter
test$Birth_qday <- qday(test$Date.of.Birth)

# Get week
test$Birth_week <- week(test$Date.of.Birth)

# Get Quarter
test$Birth_quarter <- quarter(test$Date.of.Birth)



# Remove Date as we have extracter a lot more features
train$Date.of.Birth <- NULL
test$Date.of.Birth <- NULL



#== check for uniqueness Birth Month
unique(unique(train$Birth_month) %in% unique(test$Birth_month))

train$Birth_month <- as.factor(train$Birth_month)
test$Birth_month <-  as.factor(test$Birth_month)

#== check for uniqueness Birth Day
unique(unique(train$Birth_day) %in% unique(test$Birth_day))

train$Birth_day <- as.factor(train$Birth_day)
test$Birth_day <-  as.factor(test$Birth_day)


#== check for uniqueness Birth wDay
unique(unique(train$Birth_wday) %in% unique(test$Birth_wday))
train$Birth_wday <- as.factor(train$Birth_wday)
test$Birth_wday <-  as.factor(test$Birth_wday)


#== check for uniqueness Birth qDay
unique(unique(train$Birth_qday) %in% unique(test$Birth_qday))
train$Birth_qday <- as.factor(train$Birth_qday)
test$Birth_qday <-  as.factor(test$Birth_qday)


#== check for uniqueness Birth week
unique(unique(train$Birth_week) %in% unique(test$Birth_week))
train$Birth_week <- as.factor(train$Birth_week)
test$Birth_week <-  as.factor(test$Birth_week)



#== check for uniqueness Birth week
unique(unique(train$Birth_quarter) %in% unique(test$Birth_quarter))
train$Birth_quarter <- as.factor(train$Birth_quarter)
test$Birth_quarter <-  as.factor(test$Birth_quarter)


#  DisbursalDate

#------------Train Set ----------------#

# Converting character Date into Actual Date
train$DisbursalDate <- dmy(as.character(train$DisbursalDate))


# Get Year
train$Disbursal_Year <- year(train$DisbursalDate)

# Get month
train$Disbursal_month <- month(train$DisbursalDate)

# Get day of month
train$Disbursal_day <- day(train$DisbursalDate)

# Get day of week
train$Disbursal_wday <- wday(train$DisbursalDate)

# Get day of quarter
train$Disbursal_qday <- qday(train$DisbursalDate)

# Get week
train$Disbursal_week <- week(train$DisbursalDate)

# Get Quarter
train$Disbursal_quarter <- quarter(train$DisbursalDate)


#----For test set ---#
# Converting character Date into Actual Date
test$DisbursalDate <- dmy(as.character(test$DisbursalDate))


# Get Year
test$Disbursal_Year <- year(test$DisbursalDate)

# Get month
test$Disbursal_month <- month(test$DisbursalDate)

# Get day of month
test$Disbursal_day <- day(test$DisbursalDate)

# Get day of week
test$Disbursal_wday <- wday(test$DisbursalDate)

# Get day of quarter
test$Disbursal_qday <- qday(test$DisbursalDate)

# Get week
test$Disbursal_week <- week(test$DisbursalDate)

# Get Quarter
test$Disbursal_quarter <- quarter(test$DisbursalDate)



# Remove Date as we have extracter a lot more features
train$DisbursalDate <- NULL
test$DisbursalDate <- NULL


#== check for uniqueness Disbursal_wday
unique(unique(train$Disbursal_wday) %in% unique(test$Disbursal_wday))

train$Disbursal_wday <- as.factor(train$Disbursal_wday)
test$Disbursal_wday <-  as.factor(test$Disbursal_wday)

#== check for uniqueness Disbursal_month
unique(unique(train$Disbursal_month) %in% unique(test$Disbursal_month))

# Convert into factors
train$Disbursal_month <- factor(train$Disbursal_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
test$Disbursal_month <-  factor(test$Disbursal_month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))


# Disbursal_day
train$Disbursal_day <- factor(train$Disbursal_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
                              )

test$Disbursal_day <- factor(test$Disbursal_day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)


# Disbursal_qday
train$Disbursal_qday <- factor(train$Disbursal_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                               41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                               81,82,83,84,85,86,87,88,89,90,91,92),
                                                    
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                          81,82,83,84,85,86,87,88,89,90,91,92)
                               )


test$Disbursal_qday <- factor(test$Disbursal_qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                               41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                               81,82,83,84,85,86,87,88,89,90,91,92),
                               
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                          81,82,83,84,85,86,87,88,89,90,91,92)
)

# Disbursal_week
train$Disbursal_week <- factor(train$Disbursal_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                             41,42,43,44,45,46,47,48,49,50,51,52,53),
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53))

test$Disbursal_week <- factor(test$Disbursal_week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                             41,42,43,44,45,46,47,48,49,50,51,52,53),
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53))
# Disbursal_quarter
train$Disbursal_quarter <- factor(train$Disbursal_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

test$Disbursal_quarter <- factor(test$Disbursal_quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))



#-------------- Checking Categorical Variables -------------------------#

# == PERFORM_CNS.SCORE.DESCRIPTION 

unique(train$PERFORM_CNS.SCORE.DESCRIPTION)
unique(test$PERFORM_CNS.SCORE.DESCRIPTION)


'%!in%' <- function(x,y)!('%in%'(x,y))

# Get Non- Matching Category in train set
# as train set has more categories
unique(train$PERFORM_CNS.SCORE.DESCRIPTION[train$PERFORM_CNS.SCORE.DESCRIPTION %!in% test$PERFORM_CNS.SCORE.DESCRIPTION])



#= --------------- Feature Labelling ------------------------ #

# Convert "Not Scored "as one single category
# as there are splits in levels for train and test set

train$PERFORM_CNS.SCORE.DESCRIPTION <- as.character(train$PERFORM_CNS.SCORE.DESCRIPTION)
test$PERFORM_CNS.SCORE.DESCRIPTION <- as.character(test$PERFORM_CNS.SCORE.DESCRIPTION)


# Splitting the string
train$PERFORM_CNS.SCORE.DESCRIPTION_New <-  as.data.frame(stringr::str_split_fixed(train$PERFORM_CNS.SCORE.DESCRIPTION, ":",2))$V1
test$PERFORM_CNS.SCORE.DESCRIPTION_New <-  as.data.frame(stringr::str_split_fixed(test$PERFORM_CNS.SCORE.DESCRIPTION, ":",2))$V1

# Removal of original feature
train$PERFORM_CNS.SCORE.DESCRIPTION <- NULL
test$PERFORM_CNS.SCORE.DESCRIPTION <- NULL


# CREDIT.HISTORY.LENGTH 
train$CREDIT.HISTORY.LENGTH_Year <-  as.data.frame(stringr::str_split_fixed(train$CREDIT.HISTORY.LENGTH, " ",2))$V1
train$CREDIT.HISTORY.LENGTH_Month <-  as.data.frame(stringr::str_split_fixed(train$CREDIT.HISTORY.LENGTH, " ",2))$V2


train$CREDIT.HISTORY.LENGTH_Year <- as.character(train$CREDIT.HISTORY.LENGTH_Year)
train$CREDIT.HISTORY.LENGTH_Month <- as.character(train$CREDIT.HISTORY.LENGTH_Month)


train$CREDIT.HISTORY.LENGTH_Year <-  as.data.frame(stringr::str_split_fixed(train$CREDIT.HISTORY.LENGTH_Year, "yrs",2))$V1
train$CREDIT.HISTORY.LENGTH_Month <-  as.data.frame(stringr::str_split_fixed(train$CREDIT.HISTORY.LENGTH_Month, "mon",2))$V1


train$CREDIT.HISTORY.LENGTH_Year <- as.numeric(as.character(train$CREDIT.HISTORY.LENGTH_Year))
train$CREDIT.HISTORY.LENGTH_Month <- as.numeric(as.character(train$CREDIT.HISTORY.LENGTH_Month))


# test
test$CREDIT.HISTORY.LENGTH_Year <-  as.data.frame(stringr::str_split_fixed(test$CREDIT.HISTORY.LENGTH, " ",2))$V1
test$CREDIT.HISTORY.LENGTH_Month <-  as.data.frame(stringr::str_split_fixed(test$CREDIT.HISTORY.LENGTH, " ",2))$V2


test$CREDIT.HISTORY.LENGTH_Year <- as.character(test$CREDIT.HISTORY.LENGTH_Year)
test$CREDIT.HISTORY.LENGTH_Month <- as.character(test$CREDIT.HISTORY.LENGTH_Month)


test$CREDIT.HISTORY.LENGTH_Year <-  as.data.frame(stringr::str_split_fixed(test$CREDIT.HISTORY.LENGTH_Year, "yrs",2))$V1
test$CREDIT.HISTORY.LENGTH_Month <-  as.data.frame(stringr::str_split_fixed(test$CREDIT.HISTORY.LENGTH_Month, "mon",2))$V1


test$CREDIT.HISTORY.LENGTH_Year <- as.numeric(as.character(test$CREDIT.HISTORY.LENGTH_Year))
test$CREDIT.HISTORY.LENGTH_Month <- as.numeric(as.character(test$CREDIT.HISTORY.LENGTH_Month))


# COnvert Month in Factor
train$CREDIT.HISTORY.LENGTH_Month <- as.factor(train$CREDIT.HISTORY.LENGTH_Month)

test$CREDIT.HISTORY.LENGTH_Month <- as.factor(test$CREDIT.HISTORY.LENGTH_Month)




train$CREDIT.HISTORY.LENGTH <- NULL
test$CREDIT.HISTORY.LENGTH <- NULL




# AVERAGE.ACCT.AGE 
train$AVERAGE.ACCT.AGE_Year <-  as.data.frame(stringr::str_split_fixed(train$AVERAGE.ACCT.AGE, " ",2))$V1
train$AVERAGE.ACCT.AGE_Month <-  as.data.frame(stringr::str_split_fixed(train$AVERAGE.ACCT.AGE, " ",2))$V2


train$AVERAGE.ACCT.AGE_Year <- as.character(train$AVERAGE.ACCT.AGE_Year)
train$AVERAGE.ACCT.AGE_Month <- as.character(train$AVERAGE.ACCT.AGE_Month)


train$AVERAGE.ACCT.AGE_Year <-  as.data.frame(stringr::str_split_fixed(train$AVERAGE.ACCT.AGE_Year, "yrs",2))$V1
train$AVERAGE.ACCT.AGE_Month <-  as.data.frame(stringr::str_split_fixed(train$AVERAGE.ACCT.AGE_Month, "mon",2))$V1


train$AVERAGE.ACCT.AGE_Year <- as.numeric(as.character(train$AVERAGE.ACCT.AGE_Year))
train$AVERAGE.ACCT.AGE_Month <- as.numeric(as.character(train$AVERAGE.ACCT.AGE_Month))



test$AVERAGE.ACCT.AGE_Year <-  as.data.frame(stringr::str_split_fixed(test$AVERAGE.ACCT.AGE, " ",2))$V1
test$AVERAGE.ACCT.AGE_Month <-  as.data.frame(stringr::str_split_fixed(test$AVERAGE.ACCT.AGE, " ",2))$V2


test$AVERAGE.ACCT.AGE_Year <- as.character(test$AVERAGE.ACCT.AGE_Year)
test$AVERAGE.ACCT.AGE_Month <- as.character(test$AVERAGE.ACCT.AGE_Month)


test$AVERAGE.ACCT.AGE_Year <-  as.data.frame(stringr::str_split_fixed(test$AVERAGE.ACCT.AGE_Year, "yrs",2))$V1
test$AVERAGE.ACCT.AGE_Month <-  as.data.frame(stringr::str_split_fixed(test$AVERAGE.ACCT.AGE_Month, "mon",2))$V1


test$AVERAGE.ACCT.AGE_Year <- as.numeric(as.character(test$AVERAGE.ACCT.AGE_Year))
test$AVERAGE.ACCT.AGE_Month <- as.numeric(as.character(test$AVERAGE.ACCT.AGE_Month))


# COnvert Month in Factor
train$AVERAGE.ACCT.AGE_Month <- as.factor(train$AVERAGE.ACCT.AGE_Month)

test$AVERAGE.ACCT.AGE_Month <- as.factor(test$AVERAGE.ACCT.AGE_Month)



train$AVERAGE.ACCT.AGE <- NULL
test$AVERAGE.ACCT.AGE <- NULL




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxxxxxxxxxxx Missing Value Imputation   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

colSums(is.na(train))

# Employment.Type   is missing

colSums(is.na(test))

# Employment.Type


summary(train)


# Current Balance Cant be negative, hence negative amoritization
table(train$loan_default[train$PRI.CURRENT.BALANCE < 0])

table(train$loan_default[train$SEC.CURRENT.BALANCE < 0])




#========== Impute Employment.Type ===========================

# impute with mode  value
Employment.Type_train <- with(train, impute(Employment.Type, median))

write.csv(Employment.Type_train,"Employment.Type_train.csv",row.names = F)

Employment.Type_train <- read.csv("Employment.Type_train.csv",stringsAsFactors = T)

train$Employment.Type <- Employment.Type_train$x



# impute with mode  value
Employment.Type_test <- with(test, impute(Employment.Type, median))

write.csv(Employment.Type_test,"Employment.Type_test.csv",row.names = F)

Employment.Type_test <- read.csv("Employment.Type_test.csv",stringsAsFactors = T)

test$Employment.Type <- Employment.Type_test$x


# Remove Garbage variables
rm(Employment.Type_train,Employment.Type_test)
gc()



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#---------------------- Feature Engineering ---------------------------------#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#== Risk Amount = 	Asset Cost  -  Disbursed Amount

train$Risk_Amount  <- train$asset_cost - train$disbursed_amount
test$Risk_Amount  <- test$asset_cost - test$disbursed_amount


#xxxxx Primary Type

# == Overdue Accounts rate	PRI.OVERDUE.ACCTS /PRI no. of accnts	[0/0 Handling]

train$Overdue_Accounts_rate <- ifelse(train$PRI.OVERDUE.ACCTS==0 & train$PRI.NO.OF.ACCTS == 0,0,
                                      round(train$PRI.OVERDUE.ACCTS/train$PRI.NO.OF.ACCTS,digits = 2))

test$Overdue_Accounts_rate <- ifelse(test$PRI.OVERDUE.ACCTS==0 & test$PRI.NO.OF.ACCTS == 0,0,
                                      round(test$PRI.OVERDUE.ACCTS/test$PRI.NO.OF.ACCTS,digits = 2))


# == Active Accounts Rate	PRI.ACTIVE.ACCTS / PRI no. of accnts	[0/0 Handling]

train$Active_Accounts_rate <- ifelse(train$PRI.ACTIVE.ACCTS==0 & train$PRI.NO.OF.ACCTS == 0,0,
                                      round(train$PRI.ACTIVE.ACCTS/train$PRI.NO.OF.ACCTS,digits = 2))

test$Active_Accounts_rate <- ifelse(test$PRI.ACTIVE.ACCTS==0 & test$PRI.NO.OF.ACCTS == 0,0,
                                     round(test$PRI.ACTIVE.ACCTS/test$PRI.NO.OF.ACCTS,digits = 2))


#  Ratio	Active Accounts Rate / Overdue Accounts rate 	[0/0 Handling]

train$Active_Overdue_ratio <- ifelse(train$Active_Accounts_rate==0 & train$Overdue_Accounts_rate == 0,0,
                                     round(train$Active_Accounts_rate/train$Overdue_Accounts_rate,digits = 2))

train$Active_Overdue_ratio[is.infinite(train$Active_Overdue_ratio)] <- 0


test$Active_Overdue_ratio <- ifelse(test$Active_Accounts_rate==0 & test$Overdue_Accounts_rate == 0,0,
                                    round(test$Active_Accounts_rate/test$Overdue_Accounts_rate,digits = 2))

test$Active_Overdue_ratio[is.infinite(test$Active_Overdue_ratio)] <- 0



#  Curr vs Sanc Ratio	PRI.CURRENT.BALANCE / PRI.SANCTIONED.AMOUNT	[0/0 Handling]
train$curr_sanc_ratio <- ifelse(train$PRI.CURRENT.BALANCE==0 & train$PRI.SANCTIONED.AMOUNT == 0,0,
                                round(train$PRI.CURRENT.BALANCE/train$PRI.SANCTIONED.AMOUNT,digits = 2))

train$curr_sanc_ratio[is.infinite(train$curr_sanc_ratio)] <- 0



test$curr_sanc_ratio <- ifelse(test$PRI.CURRENT.BALANCE==0 & test$PRI.SANCTIONED.AMOUNT == 0,0,
                                round(test$PRI.CURRENT.BALANCE/test$PRI.SANCTIONED.AMOUNT,digits = 2))


test$curr_sanc_ratio[is.infinite(test$curr_sanc_ratio)] <- 0


#  Curr vs Disb Ratio	PRI.CURRENT.BALANCE / PRI. Disb .AMOUNT	[0/0 Handling]
train$curr_disb_ratio <- ifelse(train$PRI.CURRENT.BALANCE==0 & train$PRI.DISBURSED.AMOUNT == 0,0,
                                round(train$PRI.CURRENT.BALANCE/train$PRI.DISBURSED.AMOUNT,digits = 2))


train$curr_disb_ratio[is.infinite(train$curr_disb_ratio)] <- 0



test$curr_disb_ratio <- ifelse(test$PRI.CURRENT.BALANCE==0 & test$PRI.DISBURSED.AMOUNT == 0,0,
                               round(test$PRI.CURRENT.BALANCE/test$PRI.DISBURSED.AMOUNT,digits = 2))


test$curr_disb_ratio[is.infinite(test$curr_disb_ratio)] <- 0


#xxxxx Secondary Type

# == Overdue Accounts rate	SEC.OVERDUE.ACCTS /SEC no. of accnts	[0/0 Handling]

train$Overdue_Accounts_rate_sec <- ifelse(train$SEC.OVERDUE.ACCTS==0 & train$SEC.NO.OF.ACCTS == 0,0,
                                      round(train$SEC.OVERDUE.ACCTS/train$SEC.NO.OF.ACCTS,digits = 2))

test$Overdue_Accounts_rate_sec <- ifelse(test$SEC.OVERDUE.ACCTS==0 & test$SEC.NO.OF.ACCTS == 0,0,
                                     round(test$SEC.OVERDUE.ACCTS/test$SEC.NO.OF.ACCTS,digits = 2))


# == Active Accounts Rate	SEC.ACTIVE.ACCTS / SEC no. of accnts	[0/0 Handling]

train$Active_Accounts_rate_sec <- ifelse(train$SEC.ACTIVE.ACCTS==0 & train$SEC.NO.OF.ACCTS == 0,0,
                                     round(train$SEC.ACTIVE.ACCTS/train$SEC.NO.OF.ACCTS,digits = 2))

test$Active_Accounts_rate_sec <- ifelse(test$SEC.ACTIVE.ACCTS==0 & test$SEC.NO.OF.ACCTS == 0,0,
                                    round(test$SEC.ACTIVE.ACCTS/test$PRI.NO.OF.ACCTS,digits = 2))




#  Curr vs Sanc Ratio	SEC.CURRENT.BALANCE / SEC.SANCTIONED.AMOUNT	[0/0 Handling]
train$curr_sanc_ratio_sec <- ifelse(train$SEC.CURRENT.BALANCE==0 & train$SEC.SANCTIONED.AMOUNT == 0,0,
                                round(train$SEC.CURRENT.BALANCE/train$SEC.SANCTIONED.AMOUNT,digits = 2))


train$curr_sanc_ratio_sec[is.infinite(train$curr_sanc_ratio_sec)] <- 0


test$curr_sanc_ratio_sec <- ifelse(test$SEC.CURRENT.BALANCE==0 & test$SEC.SANCTIONED.AMOUNT == 0,0,
                               round(test$SEC.CURRENT.BALANCE/test$SEC.SANCTIONED.AMOUNT,digits = 2))

test$curr_sanc_ratio_sec[is.infinite(test$curr_sanc_ratio_sec)] <- 0


#  Curr vs Disb Ratio	SEC.CURRENT.BALANCE / SEC. Disb .AMOUNT	[0/0 Handling]
train$curr_disb_ratio_sec <- ifelse(train$SEC.CURRENT.BALANCE==0 & train$SEC.DISBURSED.AMOUNT == 0,0,
                                round(train$SEC.CURRENT.BALANCE/train$SEC.DISBURSED.AMOUNT,digits = 2))

train$curr_disb_ratio_sec[is.infinite(train$curr_disb_ratio_sec)] <- 0


test$curr_disb_ratio_sec <- ifelse(test$SEC.CURRENT.BALANCE==0 & test$SEC.DISBURSED.AMOUNT == 0,0,
                               round(test$SEC.CURRENT.BALANCE/test$SEC.DISBURSED.AMOUNT,digits = 2))

test$curr_disb_ratio_sec[is.infinite(test$curr_disb_ratio_sec)] <- 0


# Total Install Amt.	= SEC.INSTAL.AMT +  PRIMARY.INSTAL.AMT

train$tota_install_amt <- train$SEC.INSTAL.AMT + train$PRIMARY.INSTAL.AMT

test$tota_install_amt <- test$SEC.INSTAL.AMT + test$PRIMARY.INSTAL.AMT


# Default to New Ratio = 	DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS / NEW.ACCTS.IN.LAST.SIX.MONTHS	0/0 Handling


train$def_new_ratio <- ifelse(train$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS==0 & train$NEW.ACCTS.IN.LAST.SIX.MONTHS == 0,0,
                              round(train$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS/train$NEW.ACCTS.IN.LAST.SIX.MONTHS,digits = 2))

train$def_new_ratio[is.infinite(train$def_new_ratio)] <- 0



test$def_new_ratio <- ifelse(test$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS==0 & test$NEW.ACCTS.IN.LAST.SIX.MONTHS == 0,0,
                              round(test$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS/test$NEW.ACCTS.IN.LAST.SIX.MONTHS,digits = 2))


test$def_new_ratio[is.infinite(test$def_new_ratio)] <- 0


#  Number of legal documents : Get count if Flag is Yes
train$num_doc <- rowSums(train == "YES")

test$num_doc <- rowSums(test == "YES")

train$num_doc <- factor(train$num_doc,levels = c(0,1,2,3,4,5,6),labels = c(0,1,2,3,4,5,6))

test$num_doc <- factor(test$num_doc,levels = c(0,1,2,3,4,5,6),labels = c(0,1,2,3,4,5,6))



#---------------- Class Label Encoding  ---------------------#
train$loan_default[train$loan_default==1] <- "YES"
train$loan_default[train$loan_default==0] <- "NO"


train$loan_default <- factor(train$loan_default,levels = c("YES","NO"),labels =  c("YES","NO"))




#---------------- Remove Zero and Near Zero-Variance Predictors--------------------#

nzv <- nearZeroVar(train)

names(train)[nzv]


train2 <- train[, -nzv]

test2 <- test[,colnames(test) %in% colnames(train2)]


#---------------- Remove Highly Correlated Predictors--------------------#
# Identifying numeric variables
numericData <- train2[sapply(train2, is.numeric)]

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
train3 <- train2[, -which(colnames(train2) %in% highlyCorCol)]

test3 <- test2[,colnames(test2) %in% colnames(train3)]



rm(train,train2,test,test2,numericData)



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#---------------- Training using H2o ------------------#

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# Running on 6GB RAm

h2o.init()


output <- "loan_default"
input  <- setdiff( names(train3), output )

train = as.h2o(train3)


# Predictive Modelling for 1 hour

aml <- h2o.automl(y = output,x = input,training_frame = train,nfolds = 7,seed = 123,max_runtime_secs = 3600,balance_classes = TRUE,stopping_metric = "AUC",exclude_algos = c("DeepLearning"))


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


#== get features where relative importance is 0 ================#
not_imp <- imp[imp$relative_importance==0,]

#---------------- Create New Train and Test set Accordingly -----------------#
train_final <- train3[,colnames(train3) %!in% not_imp$variable]

test_final <- test3[,colnames(test3) %in% colnames(train_final)]


output <- "loan_default"
input  <- setdiff( names(train_final), output )


train = as.h2o(train_final)


# Predictive Modelling for 1 hour

aml <- h2o.automl(y = output,x = input,training_frame = train,nfolds = 7,seed = 123,max_runtime_secs = 3600,balance_classes = TRUE,stopping_metric = "AUC",exclude_algos = c("DeepLearning"))


# check the leaderboard
lb <- aml@leaderboard
lb

# get model ids  for all models 
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]

# Get all model stacked ensembled model
se <- h2o.getModel(grep("StackedEnsemble_AllModels",model_ids,value = TRUE)[1])

metalearner <- h2o.getModel(se@model$metalearner$name)

h2o.varimp(metalearner)

imp  <- as.data.frame(h2o.varimp(gbm))


gbm <- h2o.getModel(grep("GBM",model_ids,value=TRUE)[1])


h2o.varimp_plot(gbm)


#-------------- Make Predictions -------------------------#
test = as.h2o(test_final)


pred <- h2o.predict(gbm, test)

pred = as.data.frame(pred)

sample_submission$loan_default <- pred$YES


write.csv(sample_submission,"h2o_Automl_nzv_corr_imp_feature.csv",row.names = F)






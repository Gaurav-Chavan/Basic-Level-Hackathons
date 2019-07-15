#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxx Import Libraries and set path xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Install Missing Packages Autoamtically.
list.of.packages <- c("lubridate","maps","data.table","ggthemes","SnowballC","lubridate", "readxl","h2o","mice","VIM","Hmisc","dplyr","caret","tidyr","stringr","corrplot","reshape","tidyverse","tm","topicmodels")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Load Libraries
library(maps) # for getting locations
library(h2o) # for data modelling
library(mice)  #Missing Values Imputation 
library(VIM) #plotting
library(Hmisc) # Imputing Missing Values
library(caret)  # For ML operations
library(tidyr) # for cleaning data
library(stringr) # for cleaning text data
library(corrplot) # for correlation plotting
library(reshape)  # for reshaping dataframes
library(lubridate) # for date operations


# read in the libraries we're going to use
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

library(data.table)
library(ggplot2)
library(ggthemes)



setwd("D:/Dropbox (eClerx Services Ltd.)/Gaurav Profile/Desktop/New folder/HackerEarth/ericsson/Predict ratings")


# Import Train, Test, Sample SUbmission  files


train <- read.csv("train.csv", na.strings="",stringsAsFactors = T)

test <- read.csv("test.csv", na.strings="",stringsAsFactors = T)

sample_submission <- read.csv("sample_submission.csv", na.strings="",stringsAsFactors = T)




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxxxxxxxxxxx Data Preparation  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# Get summary of training data
summary(train)
str(train)

colSums(is.na(train))

#------------ # Remove  ID from train as well as test as ML model generated should be 
# expalainable via features and not ID's

train$ID <- NULL
test$ID <- NULL

#-------------- From Location Get Region as Region is separated as comma -------------#

train$region <- sub('.*,\\s*','', train$location)
test$region <- sub('.*,\\s*','', test$location)

#-------- Correctiing location Data -----------------------------------#
train$location <- sub('\\s*,.*','', train$location)
test$location <- sub('\\s*,.*','', test$location) 



#--------------------- Dealing with Dates --------------------------#


#------------Train Set ----------------#

# Converting character Date into Actual Date
train$date <- mdy(as.character(train$date))
#---Outlier or missing  date at position 12752

# Get Year
train$Year <- year(train$date)

# Get month
train$month <- month(train$date)

# Get day of month
train$day <- day(train$date)

# Get day of week
train$wday <- wday(train$date)

# Get day of quarter
train$qday <- qday(train$date)

# Get week
train$week <- week(train$date)

# Get Quarter
train$quarter <- quarter(train$date)

# remove date as most of the features are been extracted
train$date <- NULL


#------------Test Set ----------------#

# Converting character Date into Actual Date
test$date <- mdy(as.character(test$date))
#---Outlier or missing  date at position 12752

# Get Year
test$Year <- year(test$date)

# Get month
test$month <- month(test$date)

# Get day of month
test$day <- day(test$date)

# Get day of week
test$wday <- wday(test$date)

# Get day of quarter
test$qday <- qday(test$date)

# Get week
test$week <- week(test$date)

# Get Quarter
test$quarter <- quarter(test$date)

# remove date as most of the features are been extracted
test$date <- NULL



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxxxxxxxxxxx Natural Language Processing  xxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#------------------------- Create a Corpus Data set -------------------------------------#


# Unite job_title, summary, positives ,negatives ,advice_to_mgmt into content

train1 <- unite(train, Content, c(job_title, summary,positives,negatives,advice_to_mgmt),remove = T)
test1 <- unite(test, Content, c(job_title, summary,positives,negatives,advice_to_mgmt),remove = T)



temp1 <- as.data.frame(train1$Content)
temp2 <- as.data.frame(test1$Content)

colnames(temp1)[1] <- "Content"
colnames(temp2)[1] <- "Content"


temp <- rbind(temp1,temp2)

rm(temp1, temp2)


# #------------- Merge train and test set --------------------#
# 
# essay_score <- train$essay_score
# train$essay_score <- NULL
# 
# data <- rbind(train,test)


Corpus_data <- VCorpus(VectorSource(temp$Content))


##------------- Removing Punctuation  --------------------#
Corpus_data <- tm_map(Corpus_data, content_transformer(removePunctuation))

##------------- Removing numbers  --------------------#
Corpus_data <- tm_map(Corpus_data, removeNumbers)

##------------- Converting to lower case  --------------------#
Corpus_data <- tm_map(Corpus_data, content_transformer(tolower))

##------------- Removing stop words  --------------------#
Corpus_data <- tm_map(Corpus_data, content_transformer(removeWords), stopwords("english"))

##------------- Stemming  --------------------#
Corpus_data <- tm_map(Corpus_data, stemDocument)

##------------- Whitespace  --------------------#
Corpus_data <- tm_map(Corpus_data, stripWhitespace)

# #-------------  Create Document Term Matrix  --------------------#
dtm_data <- DocumentTermMatrix(Corpus_data)


# #-------------  Sparse Term Removal  --------------------#  
dtm_data_corpus <- removeSparseTerms(dtm_data, 0.90)


# #-------------  Word Count Distribution   --------------------#  
colS <- colSums(as.matrix(dtm_data_corpus))
doc_features <- data.table(name = attributes(colS)$names, count = colS)
ggplot(doc_features[count>2000],aes(name, count)) + geom_bar(stat = "identity",fill='lightblue',color='black')+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme_economist()+ scale_color_economist()


dtm_train_matrix <- as.matrix(dtm_data_corpus)

dtm_data <- as.data.frame(dtm_train_matrix)

colnames(dtm_data) <- paste("Content", colnames(dtm_data), sep = "_")

# split the result into train and test parts

dtm_train <- dtm_data[1:nrow(train),]
dtm_test <- dtm_data[(nrow(train)+1):nrow(dtm_data),]



# #-------------  Dataset Renewal  --------------------#  
train1 <- cbind(train1,dtm_train)
test1 <- cbind(test1,dtm_test)


#--------------------- Remove Garbage Data ------------------------------#
rm(Corpus_data,dtm_data,doc_features,dtm_train,dtm_data_corpus,dtm_train_matrix,temp,dtm_test)

train1$Content <- NULL
test1$Content <- NULL

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#-------------- Feature Preparation ---------------------------------#

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Get summary of training data
summary(train1)
str(train1)

colSums(is.na(train1))




'%!in%' <- function(x,y)!('%in%'(x,y))

# 1. Identifying and converting Categorical variables
str(train1)


#----------------------------------------------------------------------------------------------------#

# Place
unique(unique(train1$Place) %in% unique(test1$Place))


#----------------------------------------------------------------------------------------------------#

# location
unique(unique(train1$location) %in% unique(test1$location))

train1$location[train1$location %!in% test1$location]

#---- There are locations which are in train set but not in test set -----------#
# ------------------# trying to Get Country Codes of all locations and remove location column ------------------#

train1$location <- NULL
test1$location <- NULL








#------ Remove 1 Garbage  record where days are missing
train1 <- train1[-which(is.na(train1$Year)),]




# score_1 to score_5 are categorical in nature
train1$score_1 <- as.factor(train1$score_1)
train1$score_2 <- as.factor(train1$score_2)
train1$score_3 <- as.factor(train1$score_3)
train1$score_4 <- as.factor(train1$score_4)
train1$score_5 <- as.factor(train1$score_5)

test1$score_1 <- as.factor(test1$score_1)
test1$score_2 <- as.factor(test1$score_2)
test1$score_3 <- as.factor(test1$score_3)
test1$score_4 <- as.factor(test1$score_4)
test1$score_5 <- as.factor(test1$score_5)

# overall is outcome variable
train1$overall <- as.factor(train1$overall)


#Year is continuous in nature


# month Convert into factors
train1$month <- factor(train1$month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
test1$month <-  factor(test1$month,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))


# Disbursal_day
train1$day <- factor(train1$day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)

test1$day <- factor(test1$day,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
)


# Disbursal_qday
train1$qday <- factor(train1$qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                               41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                               81,82,83,84,85,86,87,88,89,90,91,92),
                               
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                          81,82,83,84,85,86,87,88,89,90,91,92)
)


test1$qday <- factor(test1$qday,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                             41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                                             81,82,83,84,85,86,87,88,89,90,91,92),
                              
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                                         81,82,83,84,85,86,87,88,89,90,91,92)
)

# Disbursal_week
train1$week <- factor(train1$week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                             41,42,43,44,45,46,47,48,49,50,51,52,53),
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                          41,42,43,44,45,46,47,48,49,50,51,52,53))

test1$week <- factor(test1$week,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                                           41,42,43,44,45,46,47,48,49,50,51,52,53),
                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                         41,42,43,44,45,46,47,48,49,50,51,52,53))

#weekday
train1$wday <- factor(train1$wday,levels=c(1,2,3,4,5,6,7),labels = c(1,2,3,4,5,6,7))
test1$wday <- factor(test1$wday,levels=c(1,2,3,4,5,6,7),labels = c(1,2,3,4,5,6,7))



# Disbursal_quarter
train1$quarter <- factor(train1$quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))

test1$quarter <- factor(test1$quarter,levels=c(1,2,3,4),labels=c(1,2,3,4))




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#------------------ Missing value Imputation using mice -------------------------#

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

overall <- train1$overall
train1$overall<- NULL


data <- rbind(train1,test1)


mice_plot <- aggr(data, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(data), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))




imputed_Data <- mice(data, m=3, maxit = 7, method = 'pmm', seed = 500)



#get complete data
completeData <- mice::complete(imputed_Data)

str(completeData)
colSums(is.na(completeData))


#Create train and test set

train_final <- completeData[1:nrow(train1),]
test_final <- completeData[(nrow(train1)+1):nrow(completeData),]


train_final$overall <- overall


# Remove Garbage Space 
rm(train1,train,test1,mice_plot,imputed_Data,data,a,completeData,overall)



#-------------------Class Balancing-------------------------#
train_final <- unique(train_final)

table(train_final$overall)


prop.table(table(train_final$overall))*100
# Data Should be balanced before giving to predictive Modelling


colSums(is.na(train_final))

#remove region
train_final$region <- NULL
test_final$region <- NULL


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#---------------- Training using H2o ------------------#

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


h2o.init()


output <- "overall"
input  <- setdiff( names(train_final), output )

train = as.h2o(train_final)


# Predictive Modelling for 1 hour

aml <- h2o.automl(y = output,x = input,training_frame = train,nfolds = 7,seed = 123,balance_classes = T,max_runtime_secs = 4200)



# check the leaderboard
lb <- aml@leaderboard
lb

# get model ids  for all models 
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]


# Get all model stacked ensembled model
se <- h2o.getModel(grep("StackedEnsemble_AllModels",model_ids,value = TRUE)[1])

metalearner <- h2o.getModel(se@model$metalearner$name)

h2o.varimp(metalearner)



gbm <- h2o.getModel(grep("DeepLearning",model_ids,value=TRUE)[1])

imp  <- as.data.frame(h2o.varimp(gbm))

h2o.varimp_plot(gbm)


#-------------- Make Predictions -------------------------#
test = as.h2o(test_final)

pred <- h2o.predict(gbm, test)

pred = as.data.frame(pred)


submission <- read.csv("test.csv",header = T,sep = ",")

submission$overall <- pred$predict

submission <- submission[,c(1,17)]


write.csv(submission,"h2o_Automl_deeplearning.csv",row.names = F)

rm(list = ls())
gc()

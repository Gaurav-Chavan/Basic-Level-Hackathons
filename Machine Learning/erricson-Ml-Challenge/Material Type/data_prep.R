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



setwd("D:/Dropbox (eClerx Services Ltd.)/Gaurav Profile/Desktop/New folder/HackerEarth/ericsson/Material Type")


# Import Train, Test, Sample SUbmission  files


train <- read.csv("train_file.csv", na.strings="",stringsAsFactors = T)

test <- read.csv("test_file.csv", na.strings="",stringsAsFactors = T)

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


# PublicationYear
#------------Keep Only Years and extract garbage values from text----------------#

train$PublicationYear <- as.numeric(str_extract(train$PublicationYear, "[0-9]+"))
test$PublicationYear <- as.numeric(str_extract(test$PublicationYear, "[0-9]+"))



# Creator --- Remove garbage numbers 
train$Creator <- gsub("[[:digit:]]","",train$Creator)
test$Creator <- gsub("[[:digit:]]","",test$Creator)



# Split author based on separator "Author 1" "Author 2"
temp = transform(train, Creator = colsplit(Creator, split = "\\,", names = c('Author1', 'Author2')))

train$Creator_Author1 <- temp$Creator$Author1
train$Creator_Author2 <- temp$Creator$Author2

train$Creator <- NULL

train$Creator_Author1 <- as.factor(train$Creator_Author1)
train$Creator_Author2 <- as.factor(train$Creator_Author2)

rm(temp)

temp = transform(test, Creator = colsplit(Creator, split = "\\,", names = c('Author1', 'Author2')))

test$Creator_Author1 <- temp$Creator$Author1
test$Creator_Author2 <- temp$Creator$Author2

test$Creator <- NULL

test$Creator_Author1 <- as.factor(test$Creator_Author1)
test$Creator_Author2 <- as.factor(test$Creator_Author2)
rm(temp)



# Unite Title & Subjects into content

train1 <- unite(train, Content, c(Title, Subjects),remove = T)
test1 <- unite(test, Content, c(Title, Subjects),remove = T)



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxxxxxxxxxxx Natural Language Processing  xxxxxxxxxxxxxxxxxxx#


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxx For Content Data  xxxxxxxxxxxxxxxxxxx#


#------------------------- Create a Corpus Data set -------------------------------------#
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
dtm_data_corpus <- removeSparseTerms(dtm_data, 0.95)


# #-------------  Word Count Distribution   --------------------#  
colS <- colSums(as.matrix(dtm_data_corpus))
doc_features <- data.table(name = attributes(colS)$names, count = colS)
ggplot(doc_features[count>2000],aes(name, count)) + geom_bar(stat = "identity",fill='lightblue',color='black')+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme_economist()+ scale_color_economist()


dtm_train_matrix <- as.matrix(dtm_data_corpus)

dtm_data <- as.data.frame(dtm_train_matrix)

colnames(dtm_data) <- paste("Content", colnames(dtm_data), sep = "_")

# split the result into train and test parts

dtm_train <- dtm_data[1:nrow(train1),]
dtm_test <- dtm_data[(nrow(train1)+1):nrow(dtm_data),]



# #-------------  Dataset Renewal  --------------------#  
train1 <- cbind(train1,dtm_train)
test1 <- cbind(test1,dtm_test)


#--------------------- Remove Garbage Data ------------------------------#
rm(Corpus_data,dtm_data,doc_features,dtm_train,dtm_data_corpus,dtm_train_matrix,temp,dtm_test)


#-----------Remove summary column -----------------------#
train1$Content <- NULL
test1$Content <- NULL




#----------------- Publisher -----------------------------#
temp = transform(train1, Publisher = colsplit(Publisher, split = "\\;", names = c('Publisher1', 'Publisher2')))


train1$Publisher1 <- temp$Publisher$Publisher1
train1$Publisher2 <- temp$Publisher$Publisher2


train1$Publisher <- NULL
rm(temp)


temp = transform(test1, Publisher = colsplit(Publisher, split = "\\;", names = c('Publisher1', 'Publisher2')))


test1$Publisher1 <- temp$Publisher$Publisher1
test1$Publisher2 <- temp$Publisher$Publisher2


test1$Publisher <- NULL
rm(temp)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#-------------- Feature Preparation  and Feature Engineering ---------------------------------#

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



# CheckoutYear - Publication Year = Age

train1$age <- train1$CheckoutYear - train1$PublicationYear
test1$age <- test1$CheckoutYear - test1$PublicationYear



# CheckoutYear & Publication Year are treated as continuous #
# as out of box data may contain future years


# CheckoutMonth
train1$CheckoutMonth <- factor(train1$CheckoutMonth ,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))

test1$CheckoutMonth <- factor(test1$CheckoutMonth ,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c(1,2,3,4,5,6,7,8,9,10,11,12))



# CheckoutType checking whether train and test sampes have sane categorical distribution
unique(unique(train1$CheckoutType) %in% unique(test1$CheckoutType))



# UsageClass
train1$UsageClass <- factor(train1$UsageClass,levels = c("Physical","Digital"))
test1$UsageClass <- factor(test1$UsageClass,levels = c("Physical","Digital"))

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#------------------ Missing value Imputation using mice -------------------------#

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

MaterialType <- train1$MaterialType
train1$MaterialType<- NULL


data <- rbind(train1,test1)


mice_plot <- aggr(data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))





# PublicationYear
# impute with mean  value
temp1 <- with(data, impute(PublicationYear, mean))

write.csv(temp1,"temp1.csv",row.names = F)

temp1 <- read.csv("temp1.csv",stringsAsFactors = T)

data$PublicationYear <- temp1$x


# age
# impute with mean  value
temp1 <- with(data, impute(age, mean))

write.csv(temp1,"temp1.csv",row.names = F)

temp1 <- read.csv("temp1.csv",stringsAsFactors = T)

data$age <- temp1$x


data$PublicationYear <- round(data$PublicationYear)
data$age <- round(data$age)



# encoding NA with Not Available info.

data$Creator_Author1 <- as.character(data$Creator_Author1)

data$Creator_Author1[is.na(data$Creator_Author1)] <- "Not Available"



data$Creator_Author2 <- as.character(data$Creator_Author2)

data$Creator_Author2[is.na(data$Creator_Author2)] <- "Not Available"



data$Publisher1 <- as.character(data$Publisher1)

data$Publisher1[is.na(data$Publisher1)] <- "Not Available"



data$Publisher2 <- as.character(data$Publisher2)

data$Publisher2[is.na(data$Publisher2)] <- "Not Available"



# --------------- Converting Imputed Categories -------------------#
data$Publisher2 <- as.factor(data$Publisher2)
data$Publisher1 <- as.factor(data$Publisher1)


data$Creator_Author2 <- as.factor(data$Creator_Author2)
data$Creator_Author1 <- as.factor(data$Creator_Author1)



#---------------------Split data into train and test -----------------------#
train_final <- data[1:nrow(train1),]
test_final <- data[(nrow(train1)+1) : nrow(data),]

train_final$MaterialType <- MaterialType



rm(train1,test1,train,test,data,df,mice_plot,temp1,MaterialType)



##---------------- checking whether train and test have same categorical distributions  ----------------# 
unique(unique(train_final$Creator_Author1) %in% unique(test_final$Creator_Author1))



#As they are not same it can lead to untrained samples hence removing it
train_final$Creator_Author1 <- NULL
test_final$Creator_Author1 <- NULL

unique(unique(train_final$Creator_Author2) %in% unique(test_final$Creator_Author2))

#As they are not same it can lead to untrained samples hence removing it

train_final$Creator_Author2 <- NULL
test_final$Creator_Author2 <- NULL

unique(unique(train_final$Publisher1) %in% unique(test_final$Publisher1))
train_final$Publisher1 <- NULL
test_final$Publisher1 <- NULL


unique(unique(train_final$Publisher2) %in% unique(test_final$Publisher2))
train_final$Publisher2 <- NULL
test_final$Publisher2 <- NULL


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#---------------- Training using H2o ------------------#

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


h2o.init()


output <- "MaterialType"
input  <- setdiff( names(train_final), output )

train = as.h2o(train_final)


# Predictive Modelling for 1 & 1/2 hour

aml <- h2o.automl(y = output,x = input,training_frame = train,nfolds = 7,seed = 123,balance_classes = T,max_runtime_secs = 3900,exclude_algos = c("DRF"))



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
test = as.h2o(test_final)

pred <- h2o.predict(gbm, test)

pred = as.data.frame(pred)


submission <- read.csv("test_file.csv",header = T,sep = ",")

submission$MaterialType <- pred$predict

submission <- submission[,c(1,12)]


write.csv(submission,"h2o_Automl_gbm.csv",row.names = F)



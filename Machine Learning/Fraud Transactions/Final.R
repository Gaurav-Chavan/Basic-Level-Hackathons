library(caret)
# machine learning and advanced analytics

library(DMwR)
library(caret)
library(caretEnsemble)
library(pROC)

# natural language processing

# library(msLanguageR) 
library(tm)
library(jiebaR)

# tools

library(httr)
library(XML)
library(jsonlite)

# data visualization

library(scales)
library(ggplot2)
library(wordcloud)


library(dplyr)
library(magrittr)
library(stringr)
library(stringi)
library(readr)



setwd("D:/Data Science/Hackathons/Hacker Earth/Challenges/BrainWaves/fraud transactions")
train<-read.csv(file.choose())
test<-read.csv(file.choose())

############# summary ########################
head(train)
colSums(is.na(train))

############# Remove NearZeroVariance Variables #####################
train$transaction_id<-NULL

# # # Remove Near Zero  Variance Variables ############################
train2 <- nearZeroVar(train, saveMetrics = T)
nzv_T <- which(train2$nzv == T)
train1 <- train[,-(nzv_T)]
test2<-test[(colnames(test) %in% (colnames(train1)))]


########Train output train1, test data: test2
str(train1)

########################## Convert Factors into numeric ##################3
library(magrittr)
train1$cat_var_1<-unclass(train1$cat_var_1) %>% as.numeric 
train1$cat_var_2<-unclass(train1$cat_var_2) %>% as.numeric 
train1$cat_var_3<-unclass(train1$cat_var_3) %>% as.numeric 
train1$cat_var_4<-unclass(train1$cat_var_4) %>% as.numeric 
train1$cat_var_5<-unclass(train1$cat_var_5) %>% as.numeric 
train1$cat_var_6<-unclass(train1$cat_var_6) %>% as.numeric 
train1$cat_var_8<-unclass(train1$cat_var_8) %>% as.numeric 
train1$cat_var_9<-unclass(train1$cat_var_9) %>% as.numeric 
train1$cat_var_10<-unclass(train1$cat_var_10) %>% as.numeric 
train1$cat_var_11<-unclass(train1$cat_var_11) %>% as.numeric 
train1$cat_var_12<-unclass(train1$cat_var_12) %>% as.numeric 
train1$cat_var_13<-unclass(train1$cat_var_13) %>% as.numeric 
train1$cat_var_14<-unclass(train1$cat_var_14) %>% as.numeric 
train1$cat_var_15<-unclass(train1$cat_var_15) %>% as.numeric 
train1$cat_var_16<-unclass(train1$cat_var_16) %>% as.numeric 
train1$cat_var_17<-unclass(train1$cat_var_17) %>% as.numeric 
train1$cat_var_18<-unclass(train1$cat_var_18) %>% as.numeric 

#################### Remove highly correlated variables ##################
#Calculate correlation matrix
correlationMatrix <- cor(train1)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

unselected<- names(train1[,highlyCorrelated])

selected<-names(train1[,-highlyCorrelated])
train2<-train1[colnames(train1) %in% selected ]
test.data<-test2[(colnames(test2) %in% (colnames(train2)))]

str(test.data)

test.data$cat_var_1<-unclass(test.data$cat_var_1) %>% as.numeric 
test.data$cat_var_3<-unclass(test.data$cat_var_3) %>% as.numeric 
test.data$cat_var_5<-unclass(test.data$cat_var_5) %>% as.numeric 
test.data$cat_var_6<-unclass(test.data$cat_var_6) %>% as.numeric 
test.data$cat_var_8<-unclass(test.data$cat_var_8) %>% as.numeric 
test.data$cat_var_9<-unclass(test.data$cat_var_9) %>% as.numeric 
test.data$cat_var_10<-unclass(test.data$cat_var_10) %>% as.numeric 
test.data$cat_var_11<-unclass(test.data$cat_var_11) %>% as.numeric 
test.data$cat_var_12<-unclass(test.data$cat_var_12) %>% as.numeric 
test.data$cat_var_13<-unclass(test.data$cat_var_13) %>% as.numeric 
test.data$cat_var_14<-unclass(test.data$cat_var_14) %>% as.numeric 
test.data$cat_var_16<-unclass(test.data$cat_var_16) %>% as.numeric 
test.data$cat_var_17<-unclass(test.data$cat_var_17) %>% as.numeric 
test.data$cat_var_18<-unclass(test.data$cat_var_18) %>% as.numeric 
##########Output train2 ,test.data ##################################


########### Sampling #################################3

# Encoding 0 and 1
temp<-train2
train2<-temp
train2$target<-factor(train2$target,labels = c("NonFraudulent","Fraudulent"))

############# Sampling using SMOTE  ############################
library(DMwR)

smote_train <- SMOTE(target ~ ., data  = train2)
table(smote_train$target) 

#####################
write.csv(smote_train,"smote_train.csv",sep = ",",quote = F)


#############  Feature Engineering ########################
smote_train<-read.csv(file.choose())

library(caret)
################################# training and validation phase #########
ctrl <- trainControl(method="repeatedcv",summaryFunction = twoClassSummary,classProbs=TRUE,number = 10,repeats=3, savePredictions =TRUE)

############################### gbm   model ######################################################
smote_train$X<-NULL

gbm_fit <- train(target ~ .,data = smote_train,method = "gbm", metric = "ROC", trControl = ctrl)
summary(gbm_fit)
# num_var_7 37.3478176
# num_var_5   num_var_5 22.8588179
# cat_var_8   cat_var_8  8.8234107
# num_var_4   num_var_4  5.0628714
# cat_var_18 cat_var_18  3.5118089
# cat_var_9   cat_var_9  3.4560219
# num_var_2   num_var_2  3.0428540
# cat_var_14 cat_var_14  2.5538584
# cat_var_1   cat_var_1  2.3696551

smote_train$cat_var_21<-NULL
smote_train$cat_var_12<-NULL
smote_train$cat_var_11<-NULL
smote_train$cat_var_5<-NULL
smote_train$cat_var_16<-NULL
smote_train$cat_var_3<-NULL
smote_train$cat_var_6<-NULL
smote_train$cat_var_10<-NULL
smote_train$cat_var_13<-NULL
smote_train$num_var_1<-NULL

test.dataf<-test.data[(colnames(test.data) %in% (colnames(smote_train)))]


################## BUild Predicitve Model ##############################
library(caret)
library(caretEnsemble)


# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE,verboseIter = T)
algorithmList <- c('lda', 'gbm', 'rf', 'knn', 'xgbTree')
models <- caretList(target~., data=smote_train, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)


# stack using random forest
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)


predict.stack<-predict(stack.rf,data=test.dataf)
final<-data.frame(transaction_id=test$transaction_id, target=predict.stack$Fraudulent)
write.csv(final,'Final_Output.csv',sep = ',',row.names = F,quote = F)

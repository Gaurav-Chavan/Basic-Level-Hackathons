path <- "D:/Data Science/Hackathons/Hacker Earth/Challenges/Challenge 4/"
setwd(path)
library(caret)
## load data
library(data.table)
train <- fread("train_data.csv")
test <- fread("test_data.csv")

train1<-as.data.frame(train)
test1<-as.data.frame(test)

target1<-factor(train$target,labels = c(0,1,2))
target1

# # # Remove Near Zero  Variance Variables ############################
train2 <- nearZeroVar(train1, saveMetrics = T)
nzv_T <- which(train2$nzv == T)
train1 <- train1[,-(nzv_T)]
test2<-test1[(colnames(test1) %in% (colnames(train1)))]

# train1 and test2 
# train1$target<-NULL
Id<-train1$connection_id
train1$connection_id<-NULL
Idtest<-test2$connection_id
test2$connection_id<-NULL

train1$target<-factor(x = train1$target,labels = c("Seg0","Seg1","Seg2"))
target<-train1$target
train1$target<-NULL


#Calculate correlation matrix
correlationMatrix <- cor(train1)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

unselected<- names(train1[,highlyCorrelated])

selected<-names(train1[,-highlyCorrelated])
train.data<-train1[colnames(train1) %in% selected ]
test.data<-test2[(colnames(test2) %in% (colnames(train.data)))]

train.data$target<-target
table(train.data$target)




# ################### Feature Selection #######################
# # define the control using a random forest selection function
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
# results <- rfe(train.data[,1:8], train.data[,9], sizes=c(1:8), rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))

# The top 5 variables (out of 7):
#   cont_2, cat_2, cat_20, cat_9, cont_12

train.data$cont_9<-NULL
train.data$cont_15<-NULL
train.data$cont_17<-NULL
train.data$cont_12<-NULL
train.data$cat_9<-NULL


test.data$cont_9<-NULL
test.data$cont_15<-NULL
test.data$cont_17<-NULL
test.data$cont_12<-NULL
test.data$cat_9<-NULL


# cont_12   0.134
# cat_9     0.000

remove(test2)
remove(test1)
remove(test)
remove(train1)
remove(train)
remove(train2)
remove(nzv_T)
remove(selected)
remove(Id)
remove(target)
remove(target1)

# dump("train.data",file = "train_data.R")
# dump("test.data",file = "test_data.R")
# gc()
# source("train_data.R")
# #####################################gbm ########################################
library(gbm)
library(caret)

fitControl <- trainControl(method="repeatedcv",
                           number=7,
                           repeats=1,
                           verboseIter=TRUE,summaryFunction = multiClassSummary,sampling = "down")
# # 
# gbmFit <- train(target ~ ., data=train.data,
#                 method="gbm",
#                 trControl=fitControl,distribution="multinomial",
#                 verbose=TRUE)
# print(gbmFit)

# 
# pred<-predict(gbmFit,newdata = train.data)
# actual<-train.data$target
# confusionMatrix(pred, actual, dnn = c("Prediction", "actual"))

train.data<-unique(train.data)
train.data1<-train.data[!duplicated(train.data[, c("cont_2","cat_2","cat_20")]), ]
remove(train.data)
#Creating grid
grid <- expand.grid(n.trees=c(80,100),shrinkage=c(0.01,0.05,0.1),n.minobsinnode = c(5,10),interaction.depth=c(2,3))

# training the model
model_gbm<-train(target ~ .,data=train.data1,method='gbm',distribution="multinomial",trControl=fitControl,tuneGrid=grid)

# summarizing the model
print(model_gbm)

pred<-predict(model_gbm,newdata = train.data1)
actual<-train.data1$target
confusionMatrix(pred, actual, dnn = c("Prediction", "actual"))

#using tune length
model_gbm1<-train(target ~ .,data=train.data1,method='gbm',trControl=fitControl,tuneLength=10)
print(model_gbm1)

pred<-predict(model_gbm1,newdata = train.data1)
actual<-train.data1$target
confusionMatrix(pred, actual, dnn = c("Prediction", "actual"))

# > varImp(object=model_gbm1)
# gbm variable importance
# 
# Overall
# cat_20  100.000
# cat_2    10.661
# cont_2    3.283
# cont_12   0.134
# cat_9     0.000

plot(varImp(object=model_gbm1),main="GBM - Variable Importance")

#Checking variable importance for GBM

#Variable Importance
varImp(object=model_gbm1)

#Plotting Varianle importance for GBM
plot(varImp(object=model_gbm1),main="GBM - Variable Importance")

pred<-predict(model_gbm,newdata = test.data)
predictions<-factor(x = pred,labels = c("0","1","2"))
predictions<-as.data.frame(predictions)
Idtest<-as.data.frame(Idtest)
final<-data.frame(connection_id=Idtest$Idtest,target=predictions$predictions)
write.csv(final,"feature_gbm_tuning_number10_no2.csv",row.names = F)

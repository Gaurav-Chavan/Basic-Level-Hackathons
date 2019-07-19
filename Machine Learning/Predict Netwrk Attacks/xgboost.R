################ Author: Gaurav Chavan #####################
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
# Output <- train1 and test2 

################# SET train_ids to NULL ###################
Id<-train1$connection_id
train1$connection_id<-NULL
Idtest<-test2$connection_id
test2$connection_id<-NULL

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
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train.data[,1:8], train.data[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# The top 5 variables (out of 7):
# cont_2, cat_2, cat_20, cat_9, cont_12

#########select top 3, eliminate others ##############

############ idea taken from   gbm method varImp################

#@ example code for gbm varImp below##################

#using tune length
# model_gbm1<-train(target ~ .,data=train.data1,method='gbm',trControl=fitControl,tuneLength=10)
# print(model_gbm1)
# 
# pred<-predict(model_gbm1,newdata = train.data1)
# actual<-train.data1$target
# confusionMatrix(pred, actual, dnn = c("Prediction", "actual"))
# 
# # > varImp(object=model_gbm1)
# # gbm variable importance
# # 
# # Overall
# # cat_20  100.000
# # cat_2    10.661
# # cont_2    3.283
# # cont_12   0.134
# # cat_9     0.000
# 
# plot(varImp(object=model_gbm1),main="GBM - Variable Importance")
# 
# #Checking variable importance for GBM
# 
# #Variable Importance
# varImp(object=model_gbm1)
# 
# #Plotting Varianle importance for GBM
# plot(varImp(object=model_gbm1),main="GBM - Variable Importance")



  
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


################ Removing garbage data #######################

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

# #####################################xgboost data ########################################
train.data<-unique(train.data)

##############filter data for max class separability ######################33
train.data1<-train.data[!duplicated(train.data[, c("cont_2","cat_2","cat_20")]), ]


train.data2<-as.matrix(train.data1)
head(train.data2)

library(xgboost)
library(Matrix)
library(magrittr)

trainm<-sparse.model.matrix(target ~ .,-4,data=train.data1)
train_label<-train.data1[,"target"]
train_Matrix<-xgb.DMatrix(data = as.matrix(trainm),label=train_label)


test.data[] <- lapply(test.data, as.numeric)
testm<-xgb.DMatrix(data=data.matrix((test.data)))

###############parameters################
nc<-unique(train.data1$target)
xgb_params<-list("objective"="multi:softmax",
                 "eval_metric"="mlogloss",
                 "num_class"=3)
watchlist<-list(train=train_Matrix,test=train_Matrix)

################### XGB MODEL ################3
bst_model<-xgb.train(params = xgb_params,data=train_Matrix,nrounds = 2000,watchlist = watchlist,eta=0.5)

############For multi:softprob ##############
# e<-data.frame(bst_model$evaluation_log)
# plot(e$iter,e$train_mlogloss,col="red")
# min(e$train_mlogloss)
# e[e$train_mlogloss==0.007147,]

# predtrain<-matrix(predtrain,nrow=3,ncol=length(predtrain)/3)%>%
#       t() %>%
#       data.frame() %>%
#       mutate(label=train_label,max_prob= max.col(.,"last")-1)

#####################################################


################ Confusion Matrix #############################
predtrain<-predict(bst_model,newdata = train_Matrix)
predicted<-predtrain
actual<-train.data1$target
confusionMatrix(predicted, actual, dnn = c("Prediction", "actual"))


#################### Test Dataset ###############################
pred<-predict(bst_model,newdata = testm)
pred<-as.data.frame(pred)
Idtest<-as.data.frame(Idtest)
final<-data.frame(connection_id=Idtest$Idtest,target=pred$pred)
write.csv(final,"xgboost.csv",row.names = F)

setwd("D:/Data Science/Hackathons/Analytics Vidhya/Mckinsey")
train.data<-read.csv("pca_train.csv")
test.data<-read.csv("pca_test.csv")

############# Model Building ##################################
library(caret)
################################# training and validation phase #########
ctrl <- trainControl(method="repeatedcv",number = 7,repeats=1,verboseIter = T,summaryFunction = twoClassSummary,classProbs = T,preProcOptions = list(thresh = 0.95))

parametersGrid <-  expand.grid(eta = 0.001,
                               colsample_bytree=0.5,
                               max_depth=c(12,15),
                               nrounds=c(400,800,1200),
                               gamma=c(1,3),
                               min_child_weight=c(2,4),
                               subsample=0.6
)

xgb_fit <- train(Approved ~ .,data = train.data,method = "xgbTree", metric = "ROC", trControl = ctrl,tuneGrid = parametersGrid)
predictions_xgb <- predict(object=xgb_fit, test.data, type='prob')




##################### random forest#########################################
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid",verboseIter = T,summaryFunction = twoClassSummary,classProbs = T,preProcOptions = list(thresh = 0.95))
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train.data))))
modellist <- list()
for (ntree in c(500,750,1000)) {
  fit <- train(Approved~., data=train.data, method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

predictions_rf <- predict(object=fit, test.data, type='prob')

################## svmRadial ###################
ctrl <- trainControl(method="repeatedcv",number = 5,repeats=1,verboseIter = T,summaryFunction = twoClassSummary,classProbs = T,preProcOptions = list(thresh = 0.95))

svmFit <- train(Approved ~ ., data = train.data, 
                method = "svmRadial", 
                trControl = ctrl, 
                preProc = c("center", "scale"),
                tuneLength = c(8,9,10),
                metric = "ROC")
svmFit
predictions_svm <- predict(object=svmFit, test.data, type='prob')


################## gbm  ##########################################
# Max shrinkage for gbm
nl = nrow(train.data)
max(0.01, 0.1*min(1, nl/10000))
# Max Value for interaction.depth
floor(sqrt(NCOL(train.data)))
gbmGrid <-  expand.grid(interaction.depth = c(3, 6, 9),
                        n.trees = (0:50)*10, 
                        shrinkage = seq(.05,.005),
                        n.minobsinnode = c(10,20)) # you can also put somethinglike c(5, 10, 15, 20)

fitControl <- trainControl(method = "repeatedcv",number = 7,
                           repeats = 1,
                           preProcOptions = list(thresh = 0.95),
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,verboseIter = T)

gbmFit <- train(Approved ~ ., data = train.data, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = T, 
                metric = "ROC",
                 tuneGrid = gbmGrid)

predictions_gbm <- predict(object=gbmFit, test.data, type='prob')


############################ avNNet model ##################################

fitControl <- trainControl(method = "repeatedcv",number = 7,
                           repeats = 1,
                           preProcOptions = list(thresh = 0.95),
                           classProbs = TRUE, summaryFunction = twoClassSummary,verboseIter = T)

adaFit <- train(Approved ~ ., data = train.data, 
                method = "avNNet", 
                trControl = fitControl, 
                verbose = T, 
                metric = "ROC")

predictions_avnnet <- predict(object=adaFit, test.data, type='prob')

#Taking average of predictions
pred_avg<-(predictions_rf$approved + predictions_gbm$approved + 
             predictions_svm$approved + predictions_xgb$approved + 
             predictions_avnnet$approved)/5

pred_wavg<-((predictions_rf$approved*0.25) + (predictions_gbm$approved*0.25) + 
             (predictions_svm$approved*0.10) + (predictions_xgb$approved*0.15) + 
             (predictions_avnnet$approved*0.25))/5

data<-read.csv(file = "test.csv")
final<-data.frame(ID=data$ID,Approved=pred_wavg)

write.csv(final,"ensemble_waverage.csv",row.names = F,quote = F)
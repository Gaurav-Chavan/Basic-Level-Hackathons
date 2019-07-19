setwd("D:/Data Science/Hackathons/Hacker Earth/Challenges/BrainWaves/annual returns")
# we'll use the ggplot2 package for some plots
library(ggplot2)
library(ggbiplot)
library(gridExtra)

# corrplot is needed for correlation plots
library(corrplot)

# we'll use plyr and dplyr to manipulate the data
library(plyr)
library(dplyr)

# we'll use caret to dummify factors
# and to order the predictors by their 
# importance using Random Feature Elimination (RFE)
library(caret)

# imputaion of missing values
library(mice)
library(VIM)

# parallel computing
library(doParallel)

train<-read.csv("train.csv")
summary(train)
test<-read.csv("test.csv")
summary(test)

colSums(is.na(train))
colSums(is.na(test))

#############################################################################
str(train)
# Continuous : start_date,sold,euribor_rate,libor_rate,bought,creation_date
# Categorical : type,currency,office_id,pf_category,country_code


test$return<-0

combi<-rbind(train,test)
colSums(is.na(combi))

################## Imputing Missing Values ############################
#indicator_code
unique(combi$indicator_code)
# drop indicator_code
combi$indicator_code<-NULL


#status
unique(combi$status)
combi$status<-NULL
colSums(is.na(combi))

#bught
library(e1071)
a<-as.matrix(combi$bought)
a<-impute(a,what = "mean")
a<-as.vector(a)
combi$bought<-a
colSums(is.na(combi))

#sold
library(e1071)
a<-as.matrix(combi$sold)
a<-impute(a,what = "mean")
a<-as.vector(a)
combi$sold<-a
colSums(is.na(combi))

#libor_rate
library(e1071)
a<-as.matrix(combi$libor_rate)
a<-impute(a,what = "mean")
a<-as.vector(a)
combi$libor_rate<-a
colSums(is.na(combi))

# # hedge_value
unique(combi$hedge_value)
combi$hedge_value<-NULL
colSums(is.na(combi))


############ cleaned data part1##########################
str(combi)

#remove the dependent and identifier variables
my_data <- subset(combi, select = -c(portfolio_id, desk_id,return))
str(my_data)


############ Create Dummy Data Frame #################33
# type, currency, country_code, pf_category,office_id 

#load library
library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("type","currency",
                                                   "country_code","pf_category","office_id"
))

str(new_my_data)


############ Feature Importance using Boruta ##################################
# Change the Logical Values into Numeric

# new_my_data$hedge_value<-as.numeric(new_my_data$hedge_value)
# dat <- sapply( new_my_data, as.numeric )
# dat<-as.data.frame(dat)


# ################## Feature Engineering ##################################3
# library(Boruta)
# 
# boruta.train <- Boruta(return~., data = dat, doTrace = 2,maxRuns=175)
# print(boruta.train)
# 
# plot(boruta.train, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
#   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta.train$ImpHistory)
# Labels <- sort(sapply(lz,median))
#  axis(side = 1,las=2,labels = names(Labels),
# at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
#  
# final.boruta <- TentativeRoughFix(boruta.train)
#  print(final.boruta)
#  
#  # 4 attributes confirmed unimportant: hedge_value, pf_categoryE, typeD, typeG;
#  
# a<-getSelectedAttributes(final.boruta, withTentative = F)


# new_my_data<-subset(new_my_data,select = -c(pf_categoryE, typeD, typeG))

################### PCA ######################3

#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.test <- new_my_data[-(1:nrow(train)),]

#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation[1:5,1:4]

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
train.data <- data.frame(return = train$return, prin_comp$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:16]



################ Predictive Modelling ##################################

library(caret)
 
################### GBM Model #######################################

################################# training and validation phase #########
ctrl <- trainControl(method="repeatedcv",number = 10,repeats=3,verboseIter = T)
############################### glm   model ######################################################
gbm_fit <- train(return ~ .,data = train.data,method = "gbm", metric = "RMSE", trControl = ctrl)
saveRDS(gbm_fit,"gbm_model.rds")


################### Tuned GBM Model #######################################

################################# training and validation phase #########

ctrl <- trainControl(method="repeatedcv",number = 7,repeats=3,verboseIter = T)
#Creating grid
grid <- expand.grid(n.trees=c(150,200,250,300),shrinkage=c(0.05,0.1,0.15),n.minobsinnode = c(10,15,20),interaction.depth=c(4,5))

tune_gbm_fit <- train(return ~ .,data = train.data,method = "gbm", metric = "RMSE", trControl = ctrl,tuneGrid=grid)
saveRDS(tune_gbm_fit,"tuned_gbm_model.rds")


################### Tuned GBM Model #######################################

################################# training and validation phase #########

ctrl <- trainControl(method="repeatedcv",number = 7,repeats=3,verboseIter = T)
#Creating grid
grid <- expand.grid(n.trees=c(150,200,250,300),shrinkage=c(0.05,0.1,0.15),n.minobsinnode = c(10,15,20),interaction.depth=c(4,5))

tune_gbm_fit <- train(return ~ .,data = train.data,method = "gbm", metric = "RMSE", trControl = ctrl,tuneGrid=grid)
saveRDS(tune_gbm_fit,"tuned_gbm_model.rds")


########################## XGBTree Model ##############################
ctrl <- trainControl(method="repeatedcv",number = 10,repeats=3,verboseIter = T)

xgbTree <- train(return ~ .,data = train.data,method = "xgbTree", metric = "RMSE", trControl = ctrl)
saveRDS(xgbTree,"xgbTree.rds")



################# Example of Stacking algorithms ##################
# create submodels
library(caretEnsemble)
control <- trainControl(method="repeatedcv", number=10, repeats=3,verboseIter = T)
algorithmList <- c('xgbLinear','xgbTree', 'gbm')

models <- caretList(return ~ ., data=train.data, trControl=control, methodList=algorithmList)
results <- resamples(models)

summary(results)
dotplot(results)

saveRDS(models,"models.rds")

# stack using gbm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3,verboseIter = T)
stack.rf <- caretStack(models, method="gbm", metric="RMSE", trControl=stackControl)
print(stack.rf)



#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:15]

#make prediction on test data
rpart.prediction <- predict(stack.rf, test.data)

final.sub <- data.frame(portfolio_id = test$portfolio_id, return = rpart.prediction)
# final.sub$Item_Outlet_Sales<-abs(final.sub$Item_Outlet_Sales)
write.csv(final.sub, "pca_caret_stack.csv",row.names = F)



# #########benchmark model ###########################
# temp<-mean(train$return)
# final.sub <- data.frame(portfolio_id = sample$portfolio_id, return = temp)
# write.csv(final.sub, "benchmark model.csv",row.names = F)

#-- Import Libraries---#
library(caret)
library(h2o)
library(mice)
library(DMwR)
library(ggplot2)


#--set working directory
setwd("D:/Data Science/Hackathons/Hacker Earth/Challenges/MAKE MY TRIP/datasete9dc3ed/dataset")

#--Import Dataset
train <- read.csv("train.csv",header = T,sep = ",",na.strings = "")
test <- read.csv("test.csv",header = T,sep = ",",na.strings = "")



#---Data Checkpoints
summary(train)
str(train)
# continuous variables:  B, C, H, K, N, O
# categorical variables: A,D,E,F,G,I,J, L, M 

summary(test)
str(test)

colSums(is.na(train))
colSums(is.na(test))

#-- Remove identifier Variables
train$id<-NULL
id<-test$id
test$id<-NULL

#-- Class Encoding for caret
train$P[train$P==1]<-"YES"
train$P[train$P==0]<-"NO"


#--- Missing value imputation
#--- remove categorical variables
train.mis <- subset(train, select = -c(P))
data<-rbind(train.mis,test)
md.pattern(data)

imputed_Data <- mice(data, m=5, maxit = 250, seed = 500,printFlag = T)
summary(imputed_Data)

#-- complete the imputation
data_imputed<-complete(imputed_Data)


#--- create dummy variables---#
library(dummies)
data.imputed<- dummy.data.frame(data_imputed, sep = ".")

train_imputed<-data.imputed[1:nrow(train),]
test_imputed<-data.imputed[-(1:nrow(train)),]


train_imputed$P<-train$P

#--- Data Balancing
train_imputed$P<-factor(train_imputed$P,levels =c("YES","NO"))

#--- Class Balancing  ---#
table(train_imputed$P)
prop.table(table(train_imputed$P))
#$$$ Inference ; Data is completly balanced



#---- Exploratory Data Analysis   -#

#-- Categorical Continuous
# continuous variables:  B, C, H, K, N, O
ggplot(train_imputed,aes(P,B)) + geom_boxplot()
ggplot(train_imputed,aes(P,C)) + geom_boxplot()
ggplot(train_imputed,aes(P,H)) + geom_boxplot()
ggplot(train_imputed,aes(P,K)) + geom_boxplot()
ggplot(train_imputed,aes(P,N)) + geom_boxplot()
ggplot(train_imputed,aes(P,O)) + geom_boxplot()

#--- Capping --#

x <- train_imputed$B
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
train_imputed$B<-x


x <- test_imputed$B
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
test_imputed$B<-x



x <- train_imputed$C
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
train_imputed$C<-x


x <- test_imputed$C
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
test_imputed$C<-x


x <- train_imputed$H
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
train_imputed$H<-x


x <- test_imputed$H
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
test_imputed$H<-x

x <- train_imputed$K
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
train_imputed$K<-x


x <- test_imputed$K
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
test_imputed$K<-x


x <- train_imputed$N
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
train_imputed$N<-x


x <- test_imputed$N
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
test_imputed$N<-x


x <- train_imputed$O
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
train_imputed$O<-x


x <- test_imputed$O
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
test_imputed$O<-x

#--- Feature Engineering
library(caret)
library(corrplot)
library(plyr)



# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(train_imputed)
train_imputed1 <- train_imputed[, -nzv]
dim(train_imputed1)

test_imputed1<-test_imputed[,(colnames(test_imputed) %in% colnames(train_imputed1))]


#-- Feature Importance--#

library(randomForest)
train_imputed1$P<-factor(train_imputed1$P,levels =c("YES","NO"))

#Train Random Forest
rf <-randomForest(P~.,data=train_imputed1, importance=TRUE,ntree=250)

#Evaluate variable importance
imp = importance(rf, type=1)
imp <- data.frame(predictors=rownames(imp),imp)

# Order the predictor levels by importance
imp.sort <- arrange(imp,desc(MeanDecreaseAccuracy))
imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)

# Select the top 20 predictors
imp.20<- imp.sort[1:20,]
print(imp.20)

# Plot Important Variables
varImpPlot(rf, type=1)

########## predictive modelling #####################
library(h2o)
h2o.init()
output <- "P"
input  <- setdiff( names(train_imputed1), output )

train_imputed1 = as.h2o(train_imputed1)

model_dl_1 <- h2o.deeplearning(
  model_id = "dl_1", # (optional) assign a user-specified id to the model
  training_frame = train_imputed1, 
  # validation dataset: used for scoring and early stopping
  x = input,
  y = output,
  #activation = "Rectifier", # default (a.k.a Relu)
  hidden = c(600,400,400),    # default = 2 hidden layers with 200 neurons each
  epochs = 3, # How many times the dataset should be iterated
  variable_importances = TRUE # allows obtaining the variable importance, not enabled by default
)

library(data.table)

# h2o.varimp : obtaining the variable importance
var_imp= as.data.table( h2o.varimp(model_dl_1))

test_imputed1 = as.h2o(test_imputed1)

pred <- h2o.predict(model_dl_1, test_imputed1)

pred = as.data.frame(pred)

# pred<-predict(fit,newdata = test_imputed1)
# pred<-as.data.frame(pred)
pred$predict<-as.character(pred$predict)


#-- Class decoding 

pred$predict[pred$predict=="YES"]<-1
pred$predict[pred$predict=="NO"]<-0
pred$NO<-NULL
pred$YES<-NULL




output<-cbind(id,P=pred$predict)
colnames(output)<-c("id","P")


write.csv(x = output,"output_h2o_deep.csv",row.names = F,quote = F)
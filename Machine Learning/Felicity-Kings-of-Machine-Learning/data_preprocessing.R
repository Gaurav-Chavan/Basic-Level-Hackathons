setwd("D:/Data Science/Hackathons/Analytics Vidhya/Hackathons/felicity")
train9<-read.csv("train9.csv",sep = ",",na.strings = "")
train1<-read.csv("train1.csv",sep = ",",na.strings = "")
# test9<-read.csv("test9.csv",sep = ",",na.strings = "")
# test1<-read.csv("test1.csv",sep = ",",na.strings = "")
hero<-read.csv("hero_data.csv",sep = ",",na.strings = "")

################Check for missing values #####################3
colSums(is.na(train9))
colSums(is.na(train1))
colSums(is.na(hero))
# No missing values 

###################  Feature Creation ######################
str(train9)
str(hero)
# merge two data frames by ID
train9_merged <- merge(train9,hero,by="hero_id")
summary(train9_merged)
train1_merged<-merge(train1,hero,by="hero_id")
summary(train1_merged)

actual<-train1_merged$kda_ratio
########################### Data Cleaning #####################################3
# load required libraries
library(caret)
library(corrplot)
library(plyr)


######################## Feature Importance Boruta ######################################
# library(Boruta)
# boruta_train<-Boruta(kda_ratio ~.,data=train9_merged,doTrace = 2)
# print(boruta_train)
#
# plot(boruta_train, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta_train$ImpHistory),function(i)
#   boruta_train$ImpHistory[is.finite(boruta_train$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta_train$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#        at = 1:ncol(boruta_train$ImpHistory), cex.axis = 0.7)



# final.boruta <- TentativeRoughFix(boruta_train)
# print(final.boruta)


# 3 attributes confirmed unimportant: base_health, base_mana, base_mana_regen;

# getSelectedAttributes(final.boruta, withTentative = F)
# 
# boruta.df <- attStats(final.boruta)
# class(boruta.df)
# print(boruta.df)

train9_merged$base_health<-NULL
train9_merged$base_mana<-NULL
train9_merged$base_mana_regen<-NULL

train1_merged$base_health<-NULL
train1_merged$base_mana<-NULL
train1_merged$base_mana_regen<-NULL

#################### Feature Engineering ###################################

a<-train9_merged$intelligence_gain + train9_merged$strength_gain 

b<-((train9_merged$intelligence_gain + train9_merged$strength_gain)/2)

c<-((train9_merged$strength_gain + train9_merged$agility_gain 
     + train9_merged$intelligence_gain))

d<-((train9_merged$strength_gain + train9_merged$agility_gain 
     + train9_merged$intelligence_gain)/3)


e<-((train9_merged$base_health_regen + train9_merged$base_armor + train9_merged$base_magic_resistance
      + train9_merged$base_attack_min + train9_merged$base_attack_max + train9_merged$base_strength
       + train9_merged$base_agility + train9_merged$base_intelligence))

f<-((train9_merged$base_health_regen + train9_merged$base_armor + train9_merged$base_magic_resistance
     + train9_merged$base_attack_min + train9_merged$base_attack_max + train9_merged$base_strength
     + train9_merged$base_agility + train9_merged$base_intelligence)/8)

g<-log(train9_merged$base_intelligence)
h<-log(train9_merged$intelligence_gain)
i<-log(train9_merged$strength_gain)




featureeng<-data.frame(a=a,b=b,c=c,d=d,e=e,f=f,g=g,h=h,i=i,kda_ratio=train9_merged$kda_ratio)



# ######################## Feature Importance Boruta ######################################
# library(Boruta)
# boruta_train1<-Boruta(kda_ratio ~.,data=featureeng,doTrace = 2)
# print(boruta_train)
# 
# plot(boruta_train1, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta_train1$ImpHistory),function(i)
#   boruta_train1$ImpHistory[is.finite(boruta_train1$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta_train1$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta_train1$ImpHistory), cex.axis = 0.7)



# final.boruta <- TentativeRoughFix(boruta_train1)
# print(final.boruta)
# 
# 
# getSelectedAttributes(final.boruta, withTentative = F)
# 
# boruta.df <- attStats(final.boruta)
# class(boruta.df)
# print(boruta.df)

train9_merged_f<-cbind(train9_merged,featureeng)
train9_merged_f<-train9_merged_f[,-35]


############################### Perform Same Operations on train_1 #################

a<-train1_merged$intelligence_gain + train1_merged$strength_gain 

b<-((train1_merged$intelligence_gain + train1_merged$strength_gain)/2)

c<-((train1_merged$strength_gain + train1_merged$agility_gain 
     + train1_merged$intelligence_gain))

d<-((train1_merged$strength_gain + train1_merged$agility_gain 
     + train1_merged$intelligence_gain)/3)


e<-((train1_merged$base_health_regen + train1_merged$base_armor + train1_merged$base_magic_resistance
     + train1_merged$base_attack_min + train1_merged$base_attack_max + train1_merged$base_strength
     + train1_merged$base_agility + train1_merged$base_intelligence))

f<-((train1_merged$base_health_regen + train1_merged$base_armor + train1_merged$base_magic_resistance
     + train1_merged$base_attack_min + train1_merged$base_attack_max + train1_merged$base_strength
     + train1_merged$base_agility + train1_merged$base_intelligence)/8)

g<-log(train1_merged$base_intelligence)
h<-log(train1_merged$intelligence_gain)
i<-log(train1_merged$strength_gain)

featureeng<-data.frame(a=a,b=b,c=c,d=d,e=e,f=f,g=g,h=h,i=i,kda_ratio=train9_merged$kda_ratio)
train1_merged_f<-cbind(train1_merged,featureeng)
train1_merged_f<-train9_merged_f[,-35]




################### Predictive Modelling #################################
actual<- train1_merged_f$kda_ratio
train1_merged_f$kda_ratio<-NULL
train9_merged_f$id<-NULL
train1_merged_f$id<-NULL


library(dummies)
train9_final<-dummy.data.frame(train9_merged_f,names = c("primary_attr","attack_type","roles"))
train1_final<-dummy.data.frame(train1_merged_f,names = c("primary_attr","attack_type","roles"))

library(caret)
ctrl <- trainControl(method="repeatedcv",number = 10,repeats=2,verboseIter = T)

############################### Tree Based model  without  ######################################################
rf_fit <- train(kda_ratio ~ .,data = train9_final,method = "xgbTree", metric = "RMSE", trControl = ctrl)

pred<-predict(rf_fit,train1_final)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(pred,actual) 596.1127 #xgbTree

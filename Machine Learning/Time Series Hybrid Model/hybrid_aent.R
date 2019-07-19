setwd("D:/Data Science/Hackathons/Analytics Vidhya/Hackathons/McKinsey")

library("xts")
library("highfrequency")
library("timeDate")
train<-read.csv(file = "train_aWnotuB.csv",stringsAsFactors = F)

train1<-train[1:33680,]
test<-train[33681:48120,]
train<-train1
# test<-read.csv(file = "test_BdBKkAj.csv")

ID<-test$ID

###############          ID's are set to null         ####################
train$ID<-NULL
test$ID<-NULL

############## Added date to find the appropirate holidays on training set ########################
train$Date <- as.Date(train$DateTime)

holiday<-holidayLONDON(year = c(2015,2016,2017))
holiday<-as.character(holiday)
holiday<-as.data.frame(holiday)
train$Date<-as.character(train$Date)

train1<-train
train.holiday<-train1[train1$Date %in% holiday$holiday,]

############## Added date to find the appropirate holidays on test set considering 25:75 split ########################
test$Date <- as.Date(test$DateTime)

testholiday<-holidayLONDON(year = c(2017,2018,2019,2020,2021))
testholiday<-as.character(testholiday)
testholiday<-as.data.frame(testholiday)
test$Date<-as.character(test$Date)

test1<-test
test.holiday<-test1[test1$Date %in% testholiday$testholiday,]



#####################Train Test Split According to weekdays and Weekends and holidays(LONDONTIME) #############################
train.weekday<-train[isWeekday(train$DateTime),]
train.weekday1<-train.weekday[!(train.weekday$Date %in% holiday$holiday),]

train.weekend<-train[isWeekend(train$DateTime),]
train.weekend1<-train.weekend[!(train.weekend$Date %in% holiday$holiday),]


test.weekend<-test1[isWeekend(test1$DateTime),]
test.weekend1<-test.weekend[!(test.weekend$Date %in% testholiday$testholiday),]

test.weekday<-test1[isWeekday(test1$DateTime),]
test.weekday1<-test.weekday[!(test.weekday$Date %in% testholiday$testholiday),]


############ LIST OF Training Samples #####################
# train.weekday1
# train.weekend1
# train.holiday


############################# LIST of Testing Samples #############################
# test.weekday1
# test.weekend1
# test.holiday

#################################ARIMA MODEL  ###############################3

########################WEEKDAY ############################
############# Training Data According to Junctions ######################
junction1.data<-train.weekday1[train.weekday1$Junction==1,]
junction2.data<-train.weekday1[train.weekday1$Junction==2,]
junction3.data<-train.weekday1[train.weekday1$Junction==3,]
junction4.data<-train.weekday1[train.weekday1$Junction==4,]

############# Testing Data According to Junctions ######################
junction1.testdata<-test.weekday1[test.weekday1$Junction==1,]
junction2.testdata<-test.weekday1[test.weekday1$Junction==2,]
junction3.testdata<-test.weekday1[test.weekday1$Junction==3,]
junction4.testdata<-test.weekday1[test.weekday1$Junction==4,]



################### For Junction 1#############################
# #convert data into xts
r_xts=xts(junction1.data, order.by=as.POSIXct(junction1.data$DateTime))
head(r_xts)



junction1.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))

#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y =(log10((junction1.ts))),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE) )
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h =2064)

a<-((10^(pred$mean)))

a<-as.data.frame(a)



################### For Junction 2#############################333
# #convert data into xts
r_xts=xts(junction2.data, order.by=as.POSIXct(junction2.data$DateTime))
head(r_xts)

junction2.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))

#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction2.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)
######################### get predictions for Test Data  Junction 3####################
pred <- forecast(ARIMAfit, h = 2064)

b<-((10^(pred$mean)))

b<-as.data.frame(b)


################### For Junction 3#############################333
# #convert data into xts
r_xts=xts(junction3.data, order.by=as.POSIXct(junction3.data$DateTime))
head(r_xts)

junction3.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))


#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction3.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 2064)


c<-((10^(pred$mean)))
c<-as.data.frame(c)

################### For Junction 4#############################333
# #convert data into xts
r_xts=xts(junction4.data, order.by=as.POSIXct(junction4.data$DateTime))
head(r_xts)

junction4.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))


#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction4.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 2064)


d<-((10^(pred$mean)))
d<-as.data.frame(d)


########################Weekend ############################
############# Training Data According to Junctions ######################
junction1.data<-train.weekend1[train.weekend1$Junction==1,]
junction2.data<-train.weekend1[train.weekend1$Junction==2,]
junction3.data<-train.weekend1[train.weekend1$Junction==3,]
junction4.data<-train.weekend1[train.weekend1$Junction==4,]

############# Testing Data According to Junctions ######################
junction1.testdata<-test.weekend1[test.weekend1$Junction==1,]
junction2.testdata<-test.weekend1[test.weekend1$Junction==2,]
junction3.testdata<-test.weekend1[test.weekend1$Junction==3,]
junction4.testdata<-test.weekend1[test.weekend1$Junction==4,]


################### For Junction 1#############################333
# #convert data into xts
r_xts=xts(junction1.data, order.by=as.POSIXct(junction1.data$DateTime))
head(r_xts)

junction1.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))

#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction1.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 864)


e<-((10^(pred$mean)))

e<-as.data.frame(e)

################### For Junction 2#############################333
# #convert data into xts
r_xts=xts(junction2.data, order.by=as.POSIXct(junction2.data$DateTime))
head(r_xts)

junction2.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))

#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction2.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 864)

f<-((10^(pred$mean)))

f<-as.data.frame(f)



################### For Junction 3#############################333
# #convert data into xts
r_xts=xts(junction3.data, order.by=as.POSIXct(junction3.data$DateTime))
head(r_xts)

junction3.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))


#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction3.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 864)


g<-((10^(pred$mean)))
g<-as.data.frame(g)

################### For Junction 4#############################333
# #convert data into xts
r_xts=xts(junction4.data, order.by=as.POSIXct(junction4.data$DateTime))
head(r_xts)

junction4.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))


#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction4.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 864)


h<-((10^(pred$mean)))
h<-as.data.frame(h)


######################## Holiday ############################
############# Training Data According to Junctions ######################
junction1.data<-train.holiday[train.holiday$Junction==1,]
junction2.data<-train.holiday[train.holiday$Junction==2,]
junction3.data<-train.holiday[train.holiday$Junction==3,]
junction4.data<-train.holiday[train.holiday$Junction==4,]

############# Testing Data According to Junctions ######################
junction1.testdata<-test.holiday[test.holiday$Junction==1,]
junction2.testdata<-test.holiday[test.holiday$Junction==2,]
junction3.testdata<-test.holiday[test.holiday$Junction==3,]
junction4.testdata<-test.holiday[test.holiday$Junction==4,]


################### For Junction 1#############################333
# #convert data into xts
r_xts=xts(junction1.data, order.by=as.POSIXct(junction1.data$DateTime))
head(r_xts)

junction1.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))

#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction1.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 24)


i1<-((10^(pred$mean)))

i1<-as.data.frame(i1)

################### For Junction 2#############################333
# #convert data into xts
r_xts=xts(junction2.data, order.by=as.POSIXct(junction2.data$DateTime))
head(r_xts)

junction2.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))

#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction2.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10), 
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 24)


j1<-((10^(pred$mean)))

j1<-as.data.frame(j1)



################### For Junction 3#############################333
# #convert data into xts
r_xts=xts(junction3.data, order.by=as.POSIXct(junction3.data$DateTime))
head(r_xts)

junction3.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))


#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction3.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10),
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 24)


k<-((10^(pred$mean)))
k<-as.data.frame(k)

################### For Junction 4#############################333
# #convert data into xts
r_xts=xts(junction4.data, order.by=as.POSIXct(junction4.data$DateTime))
head(r_xts)

junction4.ts<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,3]))))


#Identification of best fit ARIMA model using auto.arima function 
require(forecastHybrid)
ARIMAfit <- hybridModel(y = log10((junction4.ts)),models="aent",errorMethod="RMSE",a.args =list(approximation = TRUE, trace=FALSE,allowdrift = FALSE),n.args = list(repeats = 10),
                        t.args = list(use.arma.errors = FALSE),errorMethod="RMSE")
summary(ARIMAfit)

######################### get predictions for Test Data  Junction 1####################
pred <- forecast(ARIMAfit, h = 24)


l<-((10^(pred$mean)))
l<-as.data.frame(l)


test1$Vehicles<-0

# a,e,i1 ###########Junction1 weekday and weekend  and holiday
j<-1
for (i in 1:nrow(test1))
{
if((test1$Junction[i]==1) & (isWeekday(test1$DateTime[i])==TRUE) & (!(isWeekday(test1$Date[i]) %in% test.holiday$Date )))
{
  
test1$Vehicles[i]<-a$x[j]

j<-j+1
}
}


j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==1) & (isWeekend(test$DateTime[i])==TRUE) & (!(isWeekend(test1$Date[i]) %in% test.holiday$Date )))
  {
    
    test1$Vehicles[i]<-e$x[j]
    
    j<-j+1
  }
}

j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==1) &  ((isWeekend(test1$Date[i]) %in% test.holiday$Date ) | (isWeekday(test1$Date[i]) %in% test.holiday$Date)))
  {
    
    test1$Vehicles[i]<-i1$x[j]
    
    j<-j+1
  }
}




# b,f,j1 ###########Junction2 weekday and weekend and holiday
j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==2) & (isWeekday(test1$DateTime[i])==TRUE) & (!(isWeekday(test1$Date[i]) %in% test.holiday$Date )))
  {
    
    test1$Vehicles[i]<-b$x[j]
    
    j<-j+1
  }
}


j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==2) & (isWeekend(test$DateTime[i])==TRUE) & (!(isWeekend(test1$Date[i]) %in% test.holiday$Date )))
  {
    
    test1$Vehicles[i]<-f$x[j]
    
    j<-j+1
  }
}

j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==2) &  ((isWeekend(test1$Date[i]) %in% test.holiday$Date ) | (isWeekday(test1$Date[i]) %in% test.holiday$Date)))
  {
    
    test1$Vehicles[i]<-j1$x[j]
    
    j<-j+1
  }
}



# c,g,k ###########Junction3 weekday and weekend
j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==3) & (isWeekday(test1$DateTime[i])==TRUE) & (!(isWeekday(test1$Date[i]) %in% test.holiday$Date )))
  {
    
    test1$Vehicles[i]<-c$x[j]
    
    j<-j+1
  }
}


j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==3) & (isWeekend(test$DateTime[i])==TRUE) & (!(isWeekend(test1$Date[i]) %in% test.holiday$Date )))
  {
    
    test1$Vehicles[i]<-g$x[j]
    
    j<-j+1
  }
}

j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==3) &  ((isWeekend(test1$Date[i]) %in% test.holiday$Date ) | (isWeekday(test1$Date[i]) %in% test.holiday$Date)))
  {
    
    test1$Vehicles[i]<-k$x[j]
    
    j<-j+1
  }
}

# d,h,l ###########Junction4 weekday and weekend and holiday
j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==4) & (isWeekday(test1$DateTime[i])==TRUE) & (!(isWeekday(test1$Date[i]) %in% test.holiday$Date )))
  {
    
    test1$Vehicles[i]<-d$x[j]
    
    j<-j+1
  }
}


j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==4) & (isWeekend(test$DateTime[i])==TRUE) & (!(isWeekend(test1$Date[i]) %in% test.holiday$Date )))
  {
    
    test1$Vehicles[i]<-h$x[j]
    
    j<-j+1
  }
}

j<-1
for (i in 1:nrow(test1))
{
  if((test1$Junction[i]==4) &  ((isWeekend(test1$Date[i]) %in% test.holiday$Date ) | (isWeekday(test1$Date[i]) %in% test.holiday$Date)))
  {
    
    test1$Vehicles[i]<-l$x[j]
    
    j<-j+1
  }
}


test1$DateTime<-NULL
test1$ID<-ID
test1$Junction<-NULL
test1$Date<-NULL


final<-data.frame(ID=test1$ID,Vehicles=test1$Vehicles)
write.csv(final,"weekday-weekend-holiday-aent.csv",row.names = F)
# Start the clock!

ptm <- proc.time()
setwd("D:/M.tech  Second Year/Time Series Weka/HAckathon/train_RTwONnY")

library("xts")
library("highfrequency")

##################### Preporcessing ################################

################ train readings#############################
readings<- read.csv("train.csv", stringsAsFactors=FALSE)
readings$Category_1=NULL
readings$Category_2=NULL
readings$Category_3=NULL
readings <- as.data.frame(readings)

##############test readings ########################
test_readings<-read.csv("test.csv", stringsAsFactors=FALSE)
test_readings$Category_1=NULL
test_readings$Category_2=NULL
test_readings$Category_3=NULL
test_readings <- as.data.frame(test_readings)

############ filter according to itemId for training ###################
newreadings<-readings[order(readings$Item_ID),]

############filter according to itemId for testing ###################
ftest_readings <-test_readings[order(test_readings$Item_ID),]


write.table("ID,Number_Of_Sales,Price", file="myfile29.csv",sep = "," ,row.names= F, col.names=F, quote=F)

########################### sample a set of unique Item_Id from test set ##################################

xyz_test=unique(ftest_readings$Item_ID)
xyz_test=as.data.frame(xyz_test)

################# for loop #############################################

for(i in xyz_test$xyz_test)
{  
  traindata <- newreadings[ which(newreadings$Item_ID==i), ]
  
  testdata <-ftest_readings[ which(ftest_readings$Item_ID==i), ]
  

  
  # #convert data into xts
  r_xts=xts(traindata, order.by=as.POSIXct(traindata$Datetime))
  head(r_xts)
  
  ########################### ARIMA model for nUMBER OF SALES ######################
  datan<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,5]))))
  
  #Identification of best fit ARIMA model using auto.arima function 
  require(forecast)
  ARIMAfit <- auto.arima((log10(datan)),d=1,D=3,ic="aic",approximation = TRUE, trace=FALSE,allowdrift = FALSE)
  summary(ARIMAfit)
  #print(ARIMAfit)
  
  ######################### get predictions ####################
  
  pred <- predict(ARIMAfit, n.ahead = 184)
  #pred <- forecast(ARIMAfit,h = 184)

  b<-((10^(pred$pred)))
  #b<-(10^(pred$mean))
  
  b<-as.data.frame(b)
  
  
  ############## append results to a solution data frame ##############
  soln <- append(testdata,b)
  soln <- as.data.frame(soln)
  
  # #################### set Item_ID and DateTime to null as they are not needed in solution ##############
  soln$Item_ID=NULL
  soln$Datetime=NULL
  
  
  
  ######################### arima model for price #####################
  
  datat<-ts(as.matrix(as.numeric(gsub("", "", r_xts[,4]))))
  #Identification of best fit ARIMA model using auto.arima function 
  
  require(forecast)
  ARIMAfit <- auto.arima((log10(datat)), d=1,D=3,ic ="aic" ,approximation = TRUE,trace=FALSE,allowdrift = FALSE)
  summary(ARIMAfit)
  #print(ARIMAfit)
  
  
  # ######################### get predictions ####################
  #pred <- forecast(ARIMAfit,h = 184)
  pred <- predict(ARIMAfit, n.ahead = 184)
  
  
  a<-((10^(pred$pred)))
  a<-as.data.frame(a)
  
  # ############## append results to a solution data frame ##############
  nsoln<- append(soln,a)
  nsoln<-as.data.frame(nsoln)
  
  write.table(nsoln, file="myfile29.csv",row.names=F,col.names=F, append=T, sep=",")
  
}

#computing memory used 
library(pryr)
mem_used()

############## computing CPU Utilzation ###############################
#  Computing CPU Utilization 
#R's computation is implicitly single-threaded, and so it cannot use more than one CPU and one core per CPU


# Stop the clock
proc.time() - ptm

# The values presented (user, system, and elapsed) will be defined by your operating system, 
# but generally, the user time relates to the execution of the code, the system time relates to your CPU, 
# and the elapsed time is the difference in times since you started the stopwatch (and will be equal to the
# sum of user and system times if the chunk of code was run altogether). 

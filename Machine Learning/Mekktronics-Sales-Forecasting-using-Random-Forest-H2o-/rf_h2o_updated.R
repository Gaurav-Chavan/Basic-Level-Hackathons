setwd("C:/Users/acer/Desktop/Hackathons/ZS6")

require(dplyr)
library(readxl)
library(lubridate)

# Load data
train_data<-read.csv("C:/Users/acer/Desktop/Hackathons/ZS6/yds_train2018.csv",as.is = T)
test_data <- read.csv("C:/Users/acer/Desktop/Hackathons/ZS6/yds_test2018.csv",as.is = T)
holidays <- read_excel("C:/Users/acer/Desktop/Hackathons/ZS6/holidays.xlsx")
promotional_expense <- read.csv("C:/Users/acer/Desktop/Hackathons/ZS6/promotional_expense.csv",as.is = T)


###### Data Pre-processing ######################################

# remove S_No as its identifier and will not add much value to model builidng

train_data$S_No <-NULL
test_data$S_No <-NULL



#xxxxxxxxxxxxxxxx For Holidays xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# holidays date conversion into proper format
str(holidays)
holidays$Date1 <- as.Date(holidays$Date,format = "%Y, %m, %d")
holidays$Date <- NULL

# get month year and date and week from holidays
library(lubridate)
holidays$Month <- month(holidays$Date1)
holidays$Year <- year(holidays$Date1)
holidays$Day <- day(holidays$Date1)
holidays$Week <- ceiling(holidays$Day / 7) 


holidays$Date1 <- NULL

holidays <-holidays[,c(5,6,3,4,1,2)]

unique(holidays$Holiday)
#82 holidays are unique 

holidays$Holiday <-as.factor(holidays$Holiday)
holidays$Country <- as.factor(holidays$Country)

 
#xxxxxxxxxxxxxxxx For Promotional Expenses xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# filter column names for promotional expense
colnames(promotional_expense) <- c("Year","Month","Country","Product_ID","Expense_Price")


# Important Point As local currencies are currencies to be forecasted
# I am using standarization Considering England Currency - Pound Sterling

# Currency Standardization #
# Argetine peso =0.028 pound
promotional_expense$Expense_Price[promotional_expense$Country=="Argentina" ] <- round(0.028 * (promotional_expense$Expense_Price[promotional_expense$Country=="Argentina" ]),digits = 3)

# Belgium euro 0.89 pound
promotional_expense$Expense_Price[promotional_expense$Country=="Belgium" ] <- round(0.89  * (promotional_expense$Expense_Price[promotional_expense$Country=="Belgium" ]),digits = 3)

# Columbia peso * 0.00027 pound
promotional_expense$Expense_Price[promotional_expense$Country=="Columbia" ] <- round(0.00027  * (promotional_expense$Expense_Price[promotional_expense$Country=="Columbia" ]),digits = 3)

# Denmark 0.12 * pound
promotional_expense$Expense_Price[promotional_expense$Country=="Denmark" ] <- round(0.12  * (promotional_expense$Expense_Price[promotional_expense$Country=="Denmark" ]),digits = 3)

# Finland euro 0.89 pound
promotional_expense$Expense_Price[promotional_expense$Country=="Finland" ] <- round(0.89  * (promotional_expense$Expense_Price[promotional_expense$Country=="Finland" ]),digits = 3)


promotional_expense$Expense_Price <-round(promotional_expense$Expense_Price,digits = 2)


# Make changes in training set aS well

# Argetine peso =0.028 pound
train_data$Sales[train_data$Country=="Argentina" ] <- round(0.028 * (train_data$Sales[train_data$Country=="Argentina" ]),digits = 2)

# Belgium euro 0.89 pound
train_data$Sales[train_data$Country=="Belgium" ] <- round(0.89  * (train_data$Sales[train_data$Country=="Belgium" ]),digits = 2)

# Columbia peso * 0.00027 pound
train_data$Sales[train_data$Country=="Columbia" ] <- round(0.00027  * (train_data$Sales[train_data$Country=="Columbia" ]),digits = 2)

# Denmark 0.12 * pound
train_data$Sales[train_data$Country=="Denmark" ] <- round(0.12  * (train_data$Sales[train_data$Country=="Denmark" ]),digits = 2)

# Finland euro 0.89 pound
train_data$Sales[train_data$Country=="Finland" ] <- round(0.89  * (train_data$Sales[train_data$Country=="Finland" ]),digits = 2)


#### Filter train data according to test set  ######################3
# Remove week and Merchand id it does not add value to test set

train_data$Week <-NULL
train_data$Merchant_ID <-NULL

# Roll data at monthly resolution
train_data_monthly = train_data%>% group_by(Year, Month, Product_ID, Country) %>% summarise(monthly_sales=sum(Sales))







#xxxxxxxxxxxxxxxx eMBEDDING NUMBER OF HOLIDAYS IN TRAINING AND TEST SET xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#  FOR PARTICULAR MONTH AND PARTICUALR YEAR
holidays$Day<- NULL
holidays$Week <-NULL


x2 <- holidays %>% 
  dplyr::group_by(Country,Month, Year) %>%
  dplyr::summarise(ncount=length(Month))

x2 <- as.data.frame(x2)

# logic will be applied on train and test data to get new feature as number of holidays


train_data_monthly$n_holiday_per_month <-0

for(i in 1: nrow(train_data_monthly))
{
  
  for( j in 1: nrow(x2))
  {    
    if((train_data_monthly$Month[i]==x2$Month[j]) & (train_data_monthly$Year[i] == x2$Year[j]) & (train_data_monthly$Country[i] ==  x2$Country[j]))
    {
      train_data_monthly$n_holiday_per_month[i] <- x2$ncount[j]
    }
    
  }

  
} # end for i


test_data$n_holiday_per_month <-0

for(i in 1: nrow(test_data))
{
  
  for( j in 1: nrow(x2))
  {    
    if((test_data$Month[i]==x2$Month[j]) & (test_data$Year[i] == x2$Year[j]) & (test_data$Country[i] ==  x2$Country[j]))
    {
      test_data$n_holiday_per_month[i] <- x2$ncount[j]
    }
    
  }

  
} # end for i



#xxxxxxxxxxxxxxxx eMBEDDING promotional expenses in train and test set xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#  FOR PARTICULAR MONTH AND PARTICUALR YEAR and country and product_id

train_data_monthly$Expense_Price <-0

for(i in 1: nrow(train_data_monthly))
{
  
  for( j in 1: nrow(promotional_expense))
  {    
    if((train_data_monthly$Month[i]==promotional_expense$Month[j]) & (train_data_monthly$Year[i] == promotional_expense$Year[j]) 
        & (train_data_monthly$Country[i] ==  promotional_expense$Country[j]) & (train_data_monthly$Product_ID[i] ==  promotional_expense$Product_ID[j]))
    {
      train_data_monthly$Expense_Price[i] <- promotional_expense$Expense_Price[j]
    }
    
  }

  
} # end for i


test_data$Expense_Price <-0

for(i in 1: nrow(test_data))
{
  
  for( j in 1: nrow(promotional_expense))
  {    
    if((test_data$Month[i]==promotional_expense$Month[j]) & (test_data$Year[i] == promotional_expense$Year[j]) 
       & (test_data$Country[i] ==  promotional_expense$Country[j]) & (test_data$Product_ID[i] ==  promotional_expense$Product_ID[j]))
    {
      test_data$Expense_Price[i] <- promotional_expense$Expense_Price[j]
    }
    
  }

} # end for i


#### Arrange Data Properly #############
train_data_monthly <- train_data_monthly[,c(3,2,6,1,4,7,5)]
test_data <-test_data[,c(3,2,6,1,4,7,5)]


train_data_monthly <-as.data.frame(train_data_monthly)

train_data_monthly$Product_ID <-as.factor(train_data_monthly$Product_ID)
test_data$Product_ID <- as.factor(test_data$Product_ID)

train_data_monthly$Month <-as.factor(train_data_monthly$Month)
test_data$Month <- as.factor(test_data$Month)

train_data_monthly$n_holiday_per_month <-as.factor(train_data_monthly$n_holiday_per_month)
test_data$n_holiday_per_month <- as.factor(test_data$n_holiday_per_month)


train_data_monthly$Country <-as.factor(train_data_monthly$Country)
test_data$Country <- as.factor(test_data$Country)


str(train_data_monthly)
str(test_data)


# remove extra data
rm(promotional_expense,holidays)

###############  Model Building Stage  ########################



test_data$Sales <-0

#filter column names 
colnames(test_data) <- colnames(train_data_monthly)


unique(train_data_monthly$Year)
unique(test_data$Year)


#xxxxxxxxxxxxxxxxxxxxxxxxxxx EDA for Numeric Values xxxxxxxxxxxxxxxx
# year and Expense_Price


# use log transform to convert expense data to normal form
train_data_monthly$Expense_Price = log1p(train_data_monthly$Expense_Price)
hist(train_data_monthly$Expense_Price)

# number of bins to set is 4 ideal


library(caret)
intrain<-createDataPartition(y=train_data_monthly$Year,p=0.7,list=FALSE)
training<-train_data_monthly[intrain,]
testing<-train_data_monthly[-intrain,]

table(training$Year)
table(testing$Year)



str(training)
str(testing)

actual <-testing$monthly_sales
testing$monthly_sales <-NULL

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
training$monthly_sales=log1p(training$monthly_sales)


library(h2o)
## Use H2O's random forest
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='6G')
## Load data into cluster from R
trainHex<-as.h2o(training)
## Set up variable to use all features other than those specified here
features<-colnames(training)[!(colnames(training) %in% c("monthly_sales"))]
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="monthly_sales", 
                          ntrees = 100,
                          max_depth = 30,
                          nbins=4,
                          seed=123,
                          nfolds=10,
                          nbins_cats = 28, # number of categorical variables
                          training_frame=trainHex)

## Load test data into cluster from R
testHex<-as.h2o(testing)

## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(rfHex,testHex))


## Return the predictions to the original scale of the Sales data
pred <- expm1(predictions[,1])
summary(pred)


#Used to estimate sMAPE
smape_cal <- function(outsample, forecasts){
  outsample <- as.numeric(outsample)
  forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts))/(abs(outsample)+abs(forecasts))
  return(smape)
}


# Test SMAPE Function
SMAPE_ERR <- mean(smape_cal(outsample=actual, forecasts=pred))


# changes in test set.
test_data$monthly_sales <- NULL
test_data$Expense_Price = log1p(test_data$Expense_Price)

## Load test data into cluster from R
holdout<-as.h2o(test_data)

## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(rfHex,holdout))


## Return the predictions to the original scale of the Sales data
pred2 <- expm1(predictions[,1])
summary(pred2)



### submssion ####
submit <- read.csv("C:/Users/acer/Desktop/Hackathons/ZS6/yds_test2018.csv",as.is = T)
submit$Sales <-pred2

# cross conversion of currencies to their acutal values
# pound to  Argetine peso 36.17
submit$Sales[submit$Country=="Argentina" ] <- round(36.17 * (submit$Sales[submit$Country=="Argentina" ]),digits = 2)

# pound to Belgium euro  1.12
submit$Sales[submit$Country=="Belgium" ] <- round(1.12  * (submit$Sales[submit$Country=="Belgium" ]),digits = 2)

# pound to  Columbia peso 3758.93
submit$Sales[submit$Country=="Columbia" ] <- round(3758.93  * (submit$Sales[submit$Country=="Columbia" ]),digits = 2)

# pound to  Denmark 8.35
submit$Sales[submit$Country=="Denmark" ] <- round(8.35  * (submit$Sales[submit$Country=="Denmark" ]),digits = 2)

# pound to  Finland euro 1.12
submit$Sales[submit$Country=="Finland" ] <- round(1.12  * (submit$Sales[submit$Country=="Finland" ]),digits = 2)


write.csv(submit,"rf_h2o_updated.csv",row.names = F)


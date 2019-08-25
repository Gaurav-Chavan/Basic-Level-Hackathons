##global:==============================================================================================================================
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#Script to install missing  Libraries----

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

rm(list=ls())



# Install Missing Packages Autoamtically.
list.of.packages <- c("lubridate", "readxl","h2o","mice","VIM","Hmisc","dplyr","plotly","mltools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)




# Load Libraries
library(h2o) # for data modelling
library(readxl) # for excel import
library(lubridate) # for date operations
library(mice)  #Missing Values Imputation 
library(VIM) #plotting
library(Hmisc) # Imputing Missing Values
library(tidyverse) # Data cleaning and wrangling
library(plotly) # For data visualizat
library(caret) # For One-Hot Encoding
library(DMwR) # FOr Sampling using SMOTE


options(scipen = 999999) # dealing with reciprocals


# Import Train  files

train <- read.table(unz("train_u5jK80M.zip", "train.csv"),na.strings ="", header=T, sep=",",stringsAsFactors = F)

test <- read.table(unz("test_3BA6GZX.zip", "test.csv"),na.strings ="", header=T, sep=",",stringsAsFactors = F)

sample_submit <- read.table(unz("sample_submission_1Sfyqeb.zip", "sample_submission.csv"),na.strings ="", header=T, sep=",")



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# Data Preparation Phase. ----


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#Remove Loan_id  as model needs to be build on features and not on identifiers

train$loan_id <- NULL
test$loan_id <- NULL

# See the data
glimpse(train)

# Get the Summary
summary(train)

# Check for missingness
colSums(is.na(train)) # no missingness in train
colSums(is.na(test))  # no missingness in test

# Check for class distribution
prop.table(table(train$m13)) # highly imblanaced data, needs to be balanced while building model.

#      0           1 
# 0.994519981 0.005480019




# Check for uniqueness of Categorical Feature ----
# If some classes in test are not present in train, the model would fail. 
# its better to treat such variables properly before modelling

# 1. Source
unique(unique(test$source) %in% unique(train$source)) # all Values in test are present in train

#2. financial_institution
unique(unique(test$financial_institution) %in% unique(train$financial_institution)) # all Values in test are present in train

#3. loan_purpose
unique(unique(test$loan_purpose) %in% unique(train$loan_purpose)) # all Values in test are present in train


# Encoding Categorical Variables Properly ----
# m13 encoding target:: loan deliquency status (0 = non deliquent, 1 = deliquent)

train$m13[which(train$m13==0)] <- "non_deliquent"
train$m13[which(train$m13==1)] <- "deliquent"

train$insurance_type[which(train$insurance_type==0)] <- "borrower_premium"
train$insurance_type[which(train$insurance_type==1)] <- "lender_premium"

test$insurance_type[which(test$insurance_type==0)] <- "borrower_premium"
test$insurance_type[which(test$insurance_type==1)] <- "lender_premium"

# Dealing with Dates ----

# Origination Date
train$origination_date <-  ymd(train$origination_date)
test$origination_date <-  ymd(test$origination_date)

# First Payment Date
unique(train$first_payment_date)
unique(test$first_payment_date)


# Tweak:: get month and Year from  feature

train <- train %>% separate(first_payment_date, c("first_payment_month", "first_payment_year"))
test  <- test %>% separate(first_payment_date, c("first_payment_month", "first_payment_year"))

# In test set,  first_payment_month are abbreviated as Jan, Feb etc ., lets encode it to numeric
test$first_payment_month <-  match(test$first_payment_month,month.abb)

# In test set, first_payment_year are abbreviated as 12,13,14 instead of 2012,2013,2014... lets encode it to proper year.
test$first_payment_year <- paste("20",test$first_payment_year,sep = "")

#Type Conversion
train$first_payment_year <- as.numeric(train$first_payment_year)
train$first_payment_month <- as.numeric(train$first_payment_month)

test$first_payment_year <- as.numeric(test$first_payment_year)
test$first_payment_month <- as.numeric(test$first_payment_month)


# Encode Categorical Variables & Class Variables to Factors ----
cols <- c("source","financial_institution","loan_purpose","insurance_type")

train[cols] <- lapply(train[cols], factor)

test[cols] <- lapply(test[cols], factor)

train$m13 <- as.factor(train$m13)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Feature Engineering ----

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Actual Value of an Asset ----
# loan_to_value =  loan_amount / actual_value_of_asset 
# actual_value_of_asset = loan_amount * loan_to_value

train$actual_value_of_asset <-  train$unpaid_principal_bal * train$loan_to_value
test$actual_value_of_asset <-  test$unpaid_principal_bal * test$loan_to_value


#  Monthly Payment ----
# https://www.calculatorsoup.com/calculators/financial/loan-calculator.php
# https://www.businesstoday.in/moneytoday/banking/how-to-calculate-emi-on-your-loans-formula-explanation/story/198944.html

# PV is the loan amount
# PMT is the monthly payment
# i is the interest rate per month in decimal form (interest rate percentage divided by 12)
# n is the number of months (term of the loan in months)

# PMT = PV*i(1+i)^n /(1+i)^n - 1

 train$Monthly_payment <-  round(((train$unpaid_principal_bal * round(train$interest_rate/(12*100),digits = 3))*((1 + round(train$interest_rate/(12*100),digits = 3))^(round(train$loan_term/30)))) /(((1 + round(train$interest_rate/(12*100),digits = 3))^(round(train$loan_term/30))) - 1))
  
 test$Monthly_payment <-  round(((test$unpaid_principal_bal * round(test$interest_rate/(12*100),digits = 3))*((1 + round(test$interest_rate/(12*100),digits = 3))^(round(test$loan_term/30)))) /(((1 + round(test$interest_rate/(12*100),digits = 3))^(round(test$loan_term/30))) - 1))


# initial_payment_gap [months] ----

# First Payment Date[Months,Year] - Loan_Origination_Date[Months,Year] 
# will give index of how quick was first payment done.
train$loan_origination_month <- month(train$origination_date)
train$loan_origination_year <- year(train$origination_date)

test$loan_origination_month <- month(test$origination_date)
test$loan_origination_year <- year(test$origination_date)


# Check if originiation & first payment are on same year or not
# if not compute difference accordingly
# Bad data:: if loan_origination_year > first_payment_year treating it as 0

train$initial_payment_gap_months <- ifelse(train$loan_origination_year == train$first_payment_year,
                              train$first_payment_month - train$loan_origination_month,
                              ifelse(train$loan_origination_year < train$first_payment_year,
                                     (12 + train$first_payment_month) - train$loan_origination_month,
                                     0
                                     )
                              )


test$initial_payment_gap_months <- ifelse(test$loan_origination_year == test$first_payment_year,
                                    test$first_payment_month - test$loan_origination_month,
                                    ifelse(test$loan_origination_year < test$first_payment_year,
                                           (((test$first_payment_year - test$loan_origination_year)*12) + test$first_payment_month) - test$loan_origination_month,
                                           0
                                    )
)




# Deliquency score ----
# sum of m1:m12

train$Deliquency_score <-  rowSums(train[,c("m1","m2","m3",                        
             "m4","m5","m6",                       
             "m7","m8","m9",                        
             "m10","m11","m12")])

test$Deliquency_score <-  rowSums(test[,c("m1","m2","m3",                        
                                            "m4","m5","m6",                       
                                            "m7","m8","m9",                        
                                            "m10","m11","m12")])




# Remove Origination_date
train$origination_date <- NULL
test$origination_date <- NULL




# Scaling Convert loan term in to months scale
train$loan_term <- round(train$loan_term/30)
test$loan_term <-  round(test$loan_term/30)



# arrange columns properly 
train <- train[,c(1:27,29:34,28)]



# Remove redundant columns ----
# first payment year
# loan_origination_year

train$first_payment_year <- NULL
train$loan_origination_year <- NULL
train$first_payment_month <- NULL
train$loan_origination_month <- NULL

test$first_payment_year <- NULL
test$loan_origination_year <- NULL
test$first_payment_month <- NULL
test$loan_origination_month <- NULL


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# EDA ----

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# As we are more interested in behaviour of loan_deliquents, 
# my infereces will be surrounded by them

# Target Distribution
count_target<-train%>%group_by(target=m13)%>%summarise(count=n())
colors <- c('rgb(211,94,96)', 'rgb(114,147,203)')

plot_ly(count_target, labels = ~target, values = ~count, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~target,
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Target Distribution',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))




# Source ----
source_df<-train %>%
  group_by(source,m13) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(source = reorder(source,Count))



plot_source<- ggplot(data=source_df,aes(x = source,y = Count,fill=m13)) +
  geom_bar(stat='identity', position=position_dodge(width=1)) +
  geom_text(aes(x = source, y = Count+2, label = Count,group=m13),
            position=position_dodge(width=1), size=4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Source', 
       y = 'Count', 
       title = 'Count of Sources') +
  theme_bw()
ggplotly(plot_source)

# Inference : Source Z has less loan deliquents as compared to others



# Financial Institution ----
financial_institution_df<-train %>%
  group_by(financial_institution,m13) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(financial_institution = reorder(financial_institution,Count))


plot_financial_institution <- ggplot(data=financial_institution_df,aes(x = financial_institution,y = Count,fill=m13)) +
  geom_bar(stat='identity', position=position_dodge(width=1)) +
  geom_text(aes(x = financial_institution, y = Count+2, label = Count,group=m13),
            position=position_dodge(width=1), size=4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Source', 
       y = 'Count', 
       title = 'Count of financial_institution') +
  theme_bw()
ggplotly(plot_financial_institution)

# Inference : 
# Most common institution Browning-Hart, OTHERS
# Least commont institution Sanchez-Robinson,	Richardson Ltd



# interest_rate ----
interest_rate_df<-data.frame(target=train$m13,interest_rate=train$interest_rate)

ggplotly(ggplot(interest_rate_df, aes(x=interest_rate, color=target)) +
  geom_histogram(fill="white", alpha=0.5, position="identity"))


# Inference : Most of the distribution is scattered around 3.7 to 4.1



#unpaid_principal_bal ----
unpaid_principal_bal_df <- data.frame(target=train$m13,unpaid_principal_bal=train$unpaid_principal_bal)


p <- ggplot(unpaid_principal_bal_df, aes(x=target, y=unpaid_principal_bal)) + 
  geom_boxplot()

ggplotly(p)


# Inference :

# Deliquent ->     on an average unpaid_principal_bal  =  153000 
# Non deliquent -> on an average unpaid_principal_bal  = 183000


# loan_to_value ----

loan_to_value_df <- data.frame(target=train$m13,loan_to_value =train$loan_to_value )


p <- ggplot(loan_to_value_df, aes(x=target, y=loan_to_value)) + 
  geom_boxplot()

ggplotly(p)


# Inference:
# Deliquent ->     lower fence = 40
# Non deliquent -> lower fence  = 23


# debt_to_income_ratio ----

debt_to_income_ratio_df <- data.frame(target=train$m13,debt_to_income_ratio =train$debt_to_income_ratio )


p <- ggplot(debt_to_income_ratio_df, aes(x=target, y=debt_to_income_ratio)) + 
  geom_boxplot()

ggplotly(p)

# Inference 
# debt_to_income_ratio is more in deliquents as compared to that of non_deliquents


#  borrower_credit_score
borrower_credit_scoredf <- data.frame(target=train$m13,borrower_credit_score =train$borrower_credit_score )


p <- ggplot(borrower_credit_scoredf, aes(x=target, y=borrower_credit_score)) + 
  geom_boxplot()

ggplotly(p)

# Inference 
# borrower_credit_score is less on an average in deliquents as compared to that of non_deliquents


# co.borrower_credit_score ----
plot_borrower_credit_score <- ggplot(data=train,aes(x = co.borrower_credit_score,color = m13)) +
  geom_histogram(bins = 30,fill="light green")+
  labs(x= 'Co-Borrower Credit Score',y = 'Count', title = paste("Distribution of", ' Co Borrower Credit score ')) +
  theme_bw()
ggplotly(plot_borrower_credit_score)

# Inference::
# Most of them overlap


# actual_value_of_asset ----
plot_actual_value_of_asset <- ggplot(data=train,aes(x = actual_value_of_asset,color = m13)) +
  geom_histogram(bins = 30,fill="light green")+
  labs(x= 'actual_value_of_asset',y = 'Count', title = paste("Distribution of", ' actual_value_of_asset ')) +
  theme_bw()
ggplotly(plot_actual_value_of_asset)




# Monthly_payment ----

Monthly_payment_df <- data.frame(target=train$m13,Monthly_payment =train$Monthly_payment )

p <- ggplot(Monthly_payment_df, aes(x=target, y=Monthly_payment)) + 
  geom_boxplot()

ggplotly(p)

# Inference: 
# On an average the deliquents have to pay around 15000 per month 
# On an average the non-deliquents have to pay around 20000 per month 


# Deliquency_score ----

Deliquency_score_df <- data.frame(target=train$m13,Deliquency_score =train$Deliquency_score)

p <- ggplot(Deliquency_score_df, aes(x=target, y=Deliquency_score)) + 
  geom_boxplot()

ggplotly(p)


plot_Deliquency_score <- ggplot(data=train,aes(x = Deliquency_score,color = m13)) +
  geom_histogram(bins = 30,fill="light green")+
  labs(x= 'Deliquency_score',y = 'Count', title = paste("Distribution of", ' Deliquency_score ')) +
  theme_bw()
ggplotly(plot_Deliquency_score)


# Inference :
# Deliquents have high Deliquency_score


# loan_purpose ----
loan_purpose_df<-train %>%
  group_by(loan_purpose,m13) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(loan_purpose = reorder(loan_purpose,Count))


plot_loan_purpose <- ggplot(data=loan_purpose_df,aes(x = loan_purpose,y = Count,fill=m13)) +
  geom_bar(stat='identity', position=position_dodge(width=1)) +
  geom_text(aes(x = loan_purpose, y = Count+2, label = Count,group=m13),
            position=position_dodge(width=1), size=4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Source', 
       y = 'Count', 
       title = 'Count of loan_purpose') +
  theme_bw()
ggplotly(plot_loan_purpose)


# insurance_type  ----
insurance_type_df<-train %>%
  group_by(insurance_type,m13) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(insurance_type = reorder(insurance_type,Count))


plot_insurance_type <- ggplot(data=insurance_type_df,aes(x = insurance_type,y = Count,fill=m13)) +
  geom_bar(stat='identity', position=position_dodge(width=1)) +
  geom_text(aes(x = insurance_type, y = Count+2, label = Count,group=m13),
            position=position_dodge(width=1), size=4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Source', 
       y = 'Count', 
       title = 'Count of insurance_type') +
  theme_bw()
ggplotly(plot_insurance_type)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Predictive Modelling ----

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


# One_hot_Encoding



dmy <- dummyVars(" ~ .", data = train)
train1 <- data.frame(predict(dmy, newdata = train))
train1$m13.deliquent <- NULL
train1$m13.non_deliquent <- NULL

train1$m13 <-  train$m13


dmy <- dummyVars(" ~ .", data = test)
test1 <- data.frame(predict(dmy, newdata = test))



# Create a predictive Model -----

ctrl <- trainControl(method="repeatedcv",number = 10,repeats=3,verboseIter = T,savePredictions = T,classProbs = F)


# Logistic Regression model over complete data.----
log_reg <- train(m13 ~ .,data = train1,method = "xgbTree",family=binomial(),metric ='Accuracy', trControl = ctrl)

output <- predict(log_reg,newdata = test1,type = "raw")

sample_submit$m13 <- output

sample_submit$m13 <- as.character(sample_submit$m13)

sample_submit$m13[which(sample_submit$m13=="non_deliquent")] <- 0
sample_submit$m13[which(sample_submit$m13=="deliquent")] <- 1

sample_submit$m13 <- as.numeric(sample_submit$m13 )

write.csv(sample_submit,"caret_log_reg_full.csv",row.names = F)


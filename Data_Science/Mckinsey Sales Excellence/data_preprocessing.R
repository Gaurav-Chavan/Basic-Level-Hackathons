setwd("D:/Data Science/Hackathons/Analytics Vidhya/Mckinsey")

train<-read.csv("train.csv",na.strings = "",sep = ",",header = T,stringsAsFactors = T)
head(train)
test<-read.csv("test.csv",na.strings = "",sep=",",header = T,stringsAsFactors = T)
head(test)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ Remove ID $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
train$ID<-NULL
test$ID<-NULL


################# Identify the missing values ###################
((colSums(is.na(train))))
# Gender, Lead_Creation_Date, Monthly_Income, Contacted , Source,Source_Category, Var1 does not contain missing values
((colSums(is.na(test))))


#$$$$$$$$$$$$$$ Impute missing values  $$$$$$$$$$$$$$$$$$$ #
appr<-train$Approved
train$Approved<-NULL

combi<-rbind(train,test)
colSums(is.na(combi))


str(combi)
#DOB
# Convert Date Time Appropriately
combi$DOB<-strptime(combi$DOB,format ="%d/%m/%y")

class(combi$DOB)
p<-as.POSIXlt(combi$DOB)



combi$DOB_mday<-p$mday
combi$DOB_mon<-p$mon
combi$DOB_year<-p$year
combi$DOB_wday<-p$wday
combi$DOB_yday<-p$yday
# combi$DOB<-NULL




############# Imputation using ts ########################3
ts<-as.POSIXct(combi$DOB)
ts.date<-as.Date(ts)

ts1<-ts(ts.date)

library(imputeTS)
plot.ts(ts1)

ts1.imputed<-na.interpolation(ts1, option = "stine")
class(ts1.imputed)
ts1.imputed<-as.numeric(ts1.imputed)
combi$DOBts<-ts1.imputed

# rm(p,appr,ts,ts.date,ts1,ts1.imputed)
# rm(train,test)
gc()
# combi$DOB<-NULL

######### Impute DOB ##########################
unique(combi$DOB_mday)
hist(combi$DOB_mday)

library(Hmisc)
# impute with mean value
combi$DOB_mday <- with(combi, impute(DOB_mday, median))
str(combi$DOB_mday)
combi$DOB_mday<-as.numeric(combi$DOB_mday)

unique(combi$DOB_mon)
hist(combi$DOB_mon)
combi$DOB_mon <- with(combi, impute(DOB_mon, median))
str(combi$DOB_mon)
combi$DOB_mon<-as.numeric(combi$DOB_mon)


unique(combi$DOB_year)
hist(combi$DOB_year)
combi$DOB_year <- with(combi, impute(DOB_year, median))
str(combi$DOB_year)
combi$DOB_year<-as.numeric(combi$DOB_year)



unique(combi$DOB_wday)
hist(combi$DOB_wday)
combi$DOB_wday <- with(combi, impute(DOB_wday, median))
str(combi$DOB_wday)
combi$DOB_wday<-as.numeric(combi$DOB_wday)


unique(combi$DOB_yday)
hist(combi$DOB_yday)
combi$DOB_yday <- with(combi, impute(DOB_yday, median))
str(combi$DOB_yday)
combi$DOB_yday<-as.numeric(combi$DOB_yday)

################## Impute City_code ##################33
unique(combi$City_Code)
combi$City_Code <- with(combi, impute(City_Code, median))
combi$City_Code<-factor(combi$City_Code)
str(combi$City_Code)


colSums(is.na(combi))
################## Impute City_Category ##################33
unique(combi$City_Category)
combi$City_Category <- with(combi, impute(City_Category, mode))
combi$City_Category<-factor(combi$City_Category)
str(combi$City_Category)



colSums(is.na(combi))
################## Impute Employer_Code ##################33
unique(combi$Employer_Code)
combi$Employer_Code <- with(combi, impute(Employer_Code, mode))
combi$Employer_Code<-factor(combi$Employer_Code)
str(combi$Employer_Code)


str(combi)

################## Impute Employer_Category1 ##################33
unique(combi$Employer_Category1)
combi$Employer_Category1 <- with(combi, impute(Employer_Category1, mode))
combi$Employer_Category1<-factor(combi$Employer_Category1)
str(combi$Employer_Category1)

################## Impute Employer_Category2 ##################
unique(combi$Employer_Category2)
combi$Employer_Category2 <- with(combi, impute(Employer_Category2, mode))
combi$Employer_Category2<-factor(combi$Employer_Category2)
str(combi$Employer_Category2)



################## Impute Customer_Existing_Primary_Bank_Code ##################33
unique(combi$Customer_Existing_Primary_Bank_Code)
combi$Customer_Existing_Primary_Bank_Code <- with(combi, impute(Customer_Existing_Primary_Bank_Code, median))
combi$Customer_Existing_Primary_Bank_Code<-factor(combi$Customer_Existing_Primary_Bank_Code)
str(combi$Customer_Existing_Primary_Bank_Code)



################## Impute Primary_Bank_Type ##################33
unique(combi$Primary_Bank_Type)
combi$Primary_Bank_Type <- with(combi, impute(Primary_Bank_Type, median))
combi$Primary_Bank_Type<-factor(combi$Primary_Bank_Type)
str(combi$Primary_Bank_Type)





################## Impute Loan_Amount ##################33
unique(combi$Loan_Amount)
combi$Loan_Amount <- with(combi, impute(Loan_Amount, mean))
combi$Loan_Amount<-as.numeric(combi$Loan_Amount)
str(combi$Loan_Amount)

################## Impute Existing_EMI ##################33
unique(combi$Existing_EMI)
combi$Existing_EMI <- with(combi, impute(Existing_EMI, mean))
combi$Existing_EMI<-as.numeric(combi$Existing_EMI)
str(combi$Existing_EMI)


################## Impute Loan_Period ##################33
unique(combi$Loan_Period)
combi$Loan_Period <- with(combi, impute(Loan_Period, median))
combi$Loan_Period<-factor(combi$Loan_Period)
str(combi$Loan_Period)



################## Impute Interest_Rate ##################33
unique(combi$Interest_Rate)
combi$Interest_Rate <- with(combi, impute(Interest_Rate, mean))
combi$Interest_Rate<-as.numeric(combi$Interest_Rate)
str(combi$Interest_Rate)

################## Impute EMI ##################33
unique(combi$EMI)
combi$EMI <- with(combi, impute(EMI, mean))
combi$EMI<-as.numeric(combi$EMI)
str(combi$EMI)

combi$DOB<-NULL



################ Lead_Creation_Date ###############################
# Convert Date Time Appropriately
combi$Lead_Creation_Date<-strptime(combi$Lead_Creation_Date,format ="%d/%m/%y")

class(combi$Lead_Creation_Date)
p<-as.POSIXlt(combi$Lead_Creation_Date)



combi$Lead_Creation_Date_mday<-p$mday
combi$Lead_Creation_Date_mon<-p$mon
combi$Lead_Creation_Date_year<-p$year
combi$Lead_Creation_Date_wday<-p$wday
combi$Lead_Creation_Date_yday<-p$yday

ts<-as.POSIXct(combi$Lead_Creation_Date)
ts.date<-as.Date(ts)

ts1<-ts(ts.date)
combi$Lead_Creation_Date_ts<-ts1
combi$Lead_Creation_Date_ts<-as.numeric(combi$Lead_Creation_Date_ts)

combi$Lead_Creation_Date<-NULL

#divide the new data
train1 <- combi[1:nrow(train),]
test1 <- combi[-(1:nrow(train)),]

################ See class distribution ###############33
train1$Approved<-appr

prop.table(table(train1$Approved))
# 0          1 
# 0.98536858 0.01463142 


############# Sampling the data ###############################
# SMOTE
library(caret)
library(DMwR)
train1$Approved<-factor(train1$Approved,labels = c("nonapproved","approved"))
smote_train<-SMOTE(Approved ~.,data=train1)
prop.table(table(smote_train$Approved))

rm(train,test,combi,p)
rm(appr,ts,ts.date,ts1,ts1.imputed)



################# Dimesnionality reduction  #############################3
# load required libraries
library(caret)
library(corrplot)
library(plyr)

# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(smote_train)
train2 <- smote_train[, -nzv]
dim(train2)
test2<-test1[colnames(test1) %in% colnames(train2)]

# Identifying numeric variables
numericData <- train2[sapply(train2, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
train3 <- train2[, -which(colnames(train2) %in% highlyCorCol)]
dim(train3)
test3<-test2[colnames(test2) %in% colnames(train3)]
rm(train1,train2,test1,test2)


##################  Variable Transformation  #########################
#Boxplot
library(ggplot2)
ggplot(train3, aes(City_Code,fill=  Approved)) + geom_bar() + theme_bw()
table(train3$City_Code)

library(car)
train3$City_Code<-recode(train3$City_Code,"c('C10093','C10094','C10101','C10108','C10113','C10118','C10121','C10123','C10131','C10133','C10142','C10149','C10153',
'C10160','C10170','C10175','C10176','C10177','C10178','C10179','C10190','C10194','C10196','C10199','C10200','C10201',
                         'C10204','C10207','C10208','C10212','C10213','C10218','C10222','C10231','C10232','C10233','C10235','C10236','C10237',
                         'C10244','C10248','C10254','C10257','C10259','C10264','C10265','C10266','C10269','C10270','C10271','C10276','C10279',
                         'C10282','C10283','C10286','C10289','C10290','C10291','C10295','C10302','C10304','C10306','C10307','C10309','C10310',
                         'C10311','C10313','C10314','C10317','C10318','C10321','C10324','C10327','C10328','C10329','C10330','C10332','C10333',
                         'C10334','C10335','C10336','C10337','C10341','C10342','C10344','C10345','C10346','C10347','C10348','C10349','C10350',
                         'C10352','C10353','C10355','C10356','C10359','C10360','C10364','C10365','C10367','C10368','C10369','C10371','C10372',
                         'C10373','C10375','C10376','C10377','C10378','C10379','C10380','C10381','C10382','C10383','C10385','C10386','C10387',
                         'C10388','C10390','C10391','C10392','C10393','C10394','C10395','C10396','C10397','C10398','C10400','C10401','C10403',
                         'C10404','C10405','C10406','C10407','C10408','C10409','C10410','C10413','C10414','C10415','C10416','C10418','C10419',
                         'C10421','C10422','C10423','C10424','C10426','C10427','C10428','C10429','C10431','C10433','C10435','C10436','C10439',
                         'C10441','C10442','C10443','C10444','C10445','C10446','C10447','C10449','C10450','C10451','C10452','C10453','C10454',
                         'C10455','C10456','C10457','C10458','C10459','C10461','C10462','C10463','C10465','C10466','C10467','C10468','C10469',
                         'C10470','C10471','C10473','C10474','C10475','C10476','C10477','C10479','C10480','C10481','C10482','C10483','C10484',
                         'C10485','C10486','C10487','C10488','C10489','C10492','C10495','C10496','C10497','C10498','C10499','C10502','C10503',
                         'C10504','C10505','C10506','C10507','C10508','C10511','C10512','C10513','C10514','C10515','C10516','C10517','C10518',
                         'C10519','C10521','C10522','C10524','C10525','C10526','C10528','C10529','C10530','C10532','C10533','C10534','C10535',
                         'C10536','C10537','C10539','C10540','C10541','C10542','C10543','C10544','C10545','C10546','C10547','C10548','C10550',
                         'C10551','C10552','C10554','C10555','C10557','C10558','C10559','C10560','C10562','C10563','C10564','C10565','C10566',
                         'C10569','C10570','C10571','C10572','C10573','C10574','C10578','C10579','C10580','C10581','C10582','C10583','C10586',
                         'C10587','C10588','C10589','C10591','C10592','C10593','C10594','C10595','C10596','C10597','C10598','C10599','C10600',
                         'C10601','C10602','C10603','C10604','C10605','C10607','C10608','C10610','C10611','C10612','C10613','C10616','C10617',
                         'C10618','C10619','C10621','C10622','C10624','C10626','C10628','C10629','C10631','C10633','C10634','C10635','C10637',
                         'C10640','C10641','C10643','C10644','C10645','C10646','C10647','C10648','C10650','C10653','C10655','C10658','C10659',
                         'C10660','C10661','C10662','C10663','C10664','C10665','C10667','C10668','C10672','C10673','C10674','C10675','C10676',
                         'C10677','C10680','C10681','C10682','C10684','C10685','C10689','C10690','C10692','C10693','C10695','C10699','C10700',
                         'C10702','C10708','C10709','C10712','C10714','C10715','C10717','C10720','C10722','C10538','C10549','C10556','C10567',
                         'C10568','C10584','C10590','C10614','C10625','C10636','C10639','C10649','C10652','C10656','C10657','C10666','C10669',
                         'C10678','C10687','C10691','C10694','C10696','C10698','C10704','C10707','C10719','C10070','C10073','C10078','C10092',
                         'C10100','C10111','C10125','C10126','C10132','C10134','C10144','C10145','C10150','C10151','C10157','C10162','C10163',
                         'C10164','C10167','C10168','C10169','C10180','C10182','C10185','C10187','C10188','C10189','C10191','C10192','C10197',
                         'C10202','C10206','C10209','C10214','C10219','C10220','C10221','C10226','C10227','C10228','C10229','C10230','C10234',
                         'C10240','C10242','C10243','C10250','C10252','C10255','C10256','C10258','C10260','C10261','C10263','C10267','C10272',
                         'C10273','C10274','C10275','C10280','C10284','C10285','C10287','C10292','C10293','C10294','C10296','C10299','C10301',
                         'C10303','C10305','C10308','C10315','C10316','C10320','C10322','C10323','C10325','C10326','C10331','C10338','C10339',
                         'C10343','C10351','C10354','C10357','C10358','C10361','C10362','C10363','C10366','C10370','C10374','C10389','C10399',
                         'C10402','C10417','C10430','C10432','C10434','C10437','C10438','C10440','C10460','C10464','C10472','C10478','C10490',
                         'C10491','C10493','C10494','C10500','C10501','C10509','C10520','C10527','C10531','C10553','C10561','C10575','C10576',
                         'C10577','C10585','C10606','C10609','C10615','C10620','C10627','C10638','C10651','C10654','C10686','C10713','C10060',
                         'C10098','C10099','C10103','C10107','C10110','C10114','C10116','C10117','C10119','C10122','C10124','C10135','C10137',
                         'C10138','C10140','C10146','C10147','C10148','C10152','C10154','C10156','C10159','C10166','C10171','C10172','C10174',
                         'C10184','C10198','C10216','C10217','C10223','C10224','C10239','C10241','C10245','C10246','C10251','C10253','C10262',
                         'C10278','C10281','C10288','C10297','C10298','C10300','C10312','C10319','C10384','C10425','C10448','C10510','C10523',
                         'C10642','C10061','C10064','C10065','C10066','C10067','C10080','C10081','C10085','C10086','C10087','C10091','C10096',
                         'C10102','C10104','C10115','C10120','C10127','C10128','C10136','C10155','C10165','C10186','C10195','C10205','C10215',
                         'C10225','C10247','C10249','C10268','C10340','C10411','C10412','C10055','C10068','C10072','C10082','C10095','C10097',
                         'C10106','C10129','C10130','C10139','C10141','C10143','C10158','C10173','C10181','C10183','C10193','C10203','C10210',
                         'C10211','C10277','C10052','C10054','C10077','C10083','C10161','C10238','C10041','C10071','C10074','C10075','C10076',
                         'C10088','C10089','C10105','C10044','C10049','C10050','C10051','C10053','C10062','C10069','C10079','C10084','C10112',
                         'C10037','C10043','C10046','C10045','C10059','C10040','C10056','C10063','C10090','C10109','C10032','C10033','C10038',
                         'C10039','C10047','C10058','C10028','C10042','C10057','C10027','C10048','C10021','C10034','C10031','C10035','C10026')='Others'")

test3$City_Code<-recode(test3$City_Code,"c('C10093','C10094','C10101','C10108','C10113','C10118','C10121','C10123','C10131','C10133','C10142','C10149','C10153',
'C10160','C10170','C10175','C10176','C10177','C10178','C10179','C10190','C10194','C10196','C10199','C10200','C10201',
                        'C10204','C10207','C10208','C10212','C10213','C10218','C10222','C10231','C10232','C10233','C10235','C10236','C10237',
                        'C10244','C10248','C10254','C10257','C10259','C10264','C10265','C10266','C10269','C10270','C10271','C10276','C10279',
                        'C10282','C10283','C10286','C10289','C10290','C10291','C10295','C10302','C10304','C10306','C10307','C10309','C10310',
                        'C10311','C10313','C10314','C10317','C10318','C10321','C10324','C10327','C10328','C10329','C10330','C10332','C10333',
                        'C10334','C10335','C10336','C10337','C10341','C10342','C10344','C10345','C10346','C10347','C10348','C10349','C10350',
                        'C10352','C10353','C10355','C10356','C10359','C10360','C10364','C10365','C10367','C10368','C10369','C10371','C10372',
                        'C10373','C10375','C10376','C10377','C10378','C10379','C10380','C10381','C10382','C10383','C10385','C10386','C10387',
                        'C10388','C10390','C10391','C10392','C10393','C10394','C10395','C10396','C10397','C10398','C10400','C10401','C10403',
                        'C10404','C10405','C10406','C10407','C10408','C10409','C10410','C10413','C10414','C10415','C10416','C10418','C10419',
                        'C10421','C10422','C10423','C10424','C10426','C10427','C10428','C10429','C10431','C10433','C10435','C10436','C10439',
                        'C10441','C10442','C10443','C10444','C10445','C10446','C10447','C10449','C10450','C10451','C10452','C10453','C10454',
                        'C10455','C10456','C10457','C10458','C10459','C10461','C10462','C10463','C10465','C10466','C10467','C10468','C10469',
                        'C10470','C10471','C10473','C10474','C10475','C10476','C10477','C10479','C10480','C10481','C10482','C10483','C10484',
                        'C10485','C10486','C10487','C10488','C10489','C10492','C10495','C10496','C10497','C10498','C10499','C10502','C10503',
                        'C10504','C10505','C10506','C10507','C10508','C10511','C10512','C10513','C10514','C10515','C10516','C10517','C10518',
                        'C10519','C10521','C10522','C10524','C10525','C10526','C10528','C10529','C10530','C10532','C10533','C10534','C10535',
                        'C10536','C10537','C10539','C10540','C10541','C10542','C10543','C10544','C10545','C10546','C10547','C10548','C10550',
                        'C10551','C10552','C10554','C10555','C10557','C10558','C10559','C10560','C10562','C10563','C10564','C10565','C10566',
                        'C10569','C10570','C10571','C10572','C10573','C10574','C10578','C10579','C10580','C10581','C10582','C10583','C10586',
                        'C10587','C10588','C10589','C10591','C10592','C10593','C10594','C10595','C10596','C10597','C10598','C10599','C10600',
                        'C10601','C10602','C10603','C10604','C10605','C10607','C10608','C10610','C10611','C10612','C10613','C10616','C10617',
                        'C10618','C10619','C10621','C10622','C10624','C10626','C10628','C10629','C10631','C10633','C10634','C10635','C10637',
                        'C10640','C10641','C10643','C10644','C10645','C10646','C10647','C10648','C10650','C10653','C10655','C10658','C10659',
                        'C10660','C10661','C10662','C10663','C10664','C10665','C10667','C10668','C10672','C10673','C10674','C10675','C10676',
                        'C10677','C10680','C10681','C10682','C10684','C10685','C10689','C10690','C10692','C10693','C10695','C10699','C10700',
                        'C10702','C10708','C10709','C10712','C10714','C10715','C10717','C10720','C10722','C10538','C10549','C10556','C10567',
                        'C10568','C10584','C10590','C10614','C10625','C10636','C10639','C10649','C10652','C10656','C10657','C10666','C10669',
                        'C10678','C10687','C10691','C10694','C10696','C10698','C10704','C10707','C10719','C10070','C10073','C10078','C10092',
                        'C10100','C10111','C10125','C10126','C10132','C10134','C10144','C10145','C10150','C10151','C10157','C10162','C10163',
                        'C10164','C10167','C10168','C10169','C10180','C10182','C10185','C10187','C10188','C10189','C10191','C10192','C10197',
                        'C10202','C10206','C10209','C10214','C10219','C10220','C10221','C10226','C10227','C10228','C10229','C10230','C10234',
                        'C10240','C10242','C10243','C10250','C10252','C10255','C10256','C10258','C10260','C10261','C10263','C10267','C10272',
                        'C10273','C10274','C10275','C10280','C10284','C10285','C10287','C10292','C10293','C10294','C10296','C10299','C10301',
                        'C10303','C10305','C10308','C10315','C10316','C10320','C10322','C10323','C10325','C10326','C10331','C10338','C10339',
                        'C10343','C10351','C10354','C10357','C10358','C10361','C10362','C10363','C10366','C10370','C10374','C10389','C10399',
                        'C10402','C10417','C10430','C10432','C10434','C10437','C10438','C10440','C10460','C10464','C10472','C10478','C10490',
                        'C10491','C10493','C10494','C10500','C10501','C10509','C10520','C10527','C10531','C10553','C10561','C10575','C10576',
                        'C10577','C10585','C10606','C10609','C10615','C10620','C10627','C10638','C10651','C10654','C10686','C10713','C10060',
                        'C10098','C10099','C10103','C10107','C10110','C10114','C10116','C10117','C10119','C10122','C10124','C10135','C10137',
                        'C10138','C10140','C10146','C10147','C10148','C10152','C10154','C10156','C10159','C10166','C10171','C10172','C10174',
                        'C10184','C10198','C10216','C10217','C10223','C10224','C10239','C10241','C10245','C10246','C10251','C10253','C10262',
                        'C10278','C10281','C10288','C10297','C10298','C10300','C10312','C10319','C10384','C10425','C10448','C10510','C10523',
                        'C10642','C10061','C10064','C10065','C10066','C10067','C10080','C10081','C10085','C10086','C10087','C10091','C10096',
                        'C10102','C10104','C10115','C10120','C10127','C10128','C10136','C10155','C10165','C10186','C10195','C10205','C10215',
                        'C10225','C10247','C10249','C10268','C10340','C10411','C10412','C10055','C10068','C10072','C10082','C10095','C10097',
                        'C10106','C10129','C10130','C10139','C10141','C10143','C10158','C10173','C10181','C10183','C10193','C10203','C10210',
                        'C10211','C10277','C10052','C10054','C10077','C10083','C10161','C10238','C10041','C10071','C10074','C10075','C10076',
                        'C10088','C10089','C10105','C10044','C10049','C10050','C10051','C10053','C10062','C10069','C10079','C10084','C10112',
                        'C10037','C10043','C10046','C10045','C10059','C10040','C10056','C10063','C10090','C10109','C10032','C10033','C10038',
                        'C10039','C10047','C10058','C10028','C10042','C10057','C10027','C10048','C10021','C10034','C10031','C10035','C10026')='Others'")


ggplot(train3, aes(Employer_Code,fill=  Approved)) + geom_bar() + theme_bw()
table(train3$Employer_Code)
a<-sort(table(train3$Employer_Code))
a<-as.data.frame(a)
a1<-a[(a$Freq==0 | a$Freq==1 | a$Freq==2 | a$Freq==3 | a$Freq==4 | a$Freq==5 | a$Freq==6 | a$Freq==7 | a$Freq==8 | a$Freq==9 | a$Freq==10),]


train3$Employer_Code<-as.character(train3$Employer_Code)
train3$Employer_Code[train3$Employer_Code %in% a1$Var1]<-'Others'

test3$Employer_Code<-as.character(test3$Employer_Code)
test3$Employer_Code[test3$Employer_Code %in% a1$Var1]<-'Others'

a<-sort(table(train3$Employer_Code))
a<-as.data.frame(a)
a<-a[-29,]
train3$Employer_Code[train3$Employer_Code %in% a$Var1]<-'Main'
test3$Employer_Code[test3$Employer_Code %in% a$Var1]<-'Main'


ggplot(train3, aes(Employer_Category2,fill=  Approved)) + geom_bar() + theme_bw()
table(train3$Employer_Category2)


library(car)
train3$Employer_Category2<-recode(train3$Employer_Category2,"c('1','2','3','numeric')='Others'")
test3$Employer_Category2<-recode(test3$Employer_Category2,"c('1','2','3','numeric')='Others'")


ggplot(train3, aes(Customer_Existing_Primary_Bank_Code,fill=  Approved)) + geom_bar() + theme_bw()
table(train3$Customer_Existing_Primary_Bank_Code)

library(car)
train3$Customer_Existing_Primary_Bank_Code<-recode(train3$Customer_Existing_Primary_Bank_Code,"c('B007','B008','B009','B010','B011','B012','B013','B014','B015','B016','B017','B018','B019',
'B020','B021','B022','B023','B024','B025','B026','B027','B028','B029','B030','B031','B032','B033','B034','B035','B036','B037','B038',
                                                   'B039','B040','B041','B042','B043','B044','B045','B046','B047','B048','B049','B050','B051','B052','B053','B054','B055','B056','B057'
)='Others'")



test3$Customer_Existing_Primary_Bank_Code<-recode(test3$Customer_Existing_Primary_Bank_Code,"c('B007','B008','B009','B010','B011','B012','B013','B014','B015','B016','B017','B018','B019',
'B020','B021','B022','B023','B024','B025','B026','B027','B028','B029','B030','B031','B032','B033','B034','B035','B036','B037','B038',
                                 'B039','B040','B041','B042','B043','B044','B045','B046','B047','B048','B049','B050','B051','B052','B053','B054','B055','B056','B057'
)='Others'")

train3$Customer_Existing_Primary_Bank_Code<-recode(train3$Customer_Existing_Primary_Bank_Code,"c('B004','B005','B006','B003')='Others'")
test3$Customer_Existing_Primary_Bank_Code<-recode(test3$Customer_Existing_Primary_Bank_Code,"c('B004','B005','B006','B003')='Others'")

ggplot(train3, aes(Source,fill=  Approved)) + geom_bar() + theme_bw()
table(train3$Source)
sort(table(train3$Source))

train3$Source<-recode(train3$Source,"c('S124','S129','S130','S136','S138','S140','S150','S154','S126','S131','S132','S142','S135','S139','S155','S160','S162','S123','S141',
'S156','S158','S144','S153','S157','S161','S151','S137','S127','S159','S143','S134')='Others'")

test3$Source<-recode(test3$Source,"c('S124','S129','S130','S136','S138','S140','S150','S154','S126','S131','S132','S142','S135','S139','S155','S160','S162','S123','S141',
'S156','S158','S144','S153','S157','S161','S151','S137','S127','S159','S143','S134')='Others'")

ggplot(train3, aes(Source_Category,fill=  Approved)) + geom_bar() + theme_bw()

train3$Source_Category<-recode(train3$Source_Category,"c('A','C','D','E','F')='Others'")
test3$Source_Category<-recode(test3$Source_Category,"c('A','C','D','E','F')='Others'")


ggplot(train3, aes(Loan_Period,fill=  Approved)) + geom_bar() + theme_bw()
train3$Loan_Period<-recode(train3$Loan_Period,"c('1','2','3')='Others'")
test3$Loan_Period<-recode(test3$Loan_Period,"c('1','2','3')='Others'")


################### Do Principal Component Analysis #######################################33
#combine the data set
Approved<-train3$Approved
train3$Approved<-NULL
combi <- rbind(train3, test3)

my_data <- combi

#check available variables
colnames(my_data)

str(my_data)
#load library
library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("Loan_Period","Source_Category",
                                                   "Source","Contacted","Primary_Bank_Type","Customer_Existing_Primary_Bank_Code",
                                                    "Employer_Category2","Employer_Category1","Employer_Code","City_Category",
                                                   "City_Code","Gender"))

#check the data set
str(new_my_data)

# load required libraries
library(caret)
library(corrplot)
library(plyr)


# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(new_my_data)
new_my_data2 <- new_my_data[, -nzv]
dim(new_my_data2)


#divide the new data
pca.train <- new_my_data2[1:nrow(train3),]
pca.test <- new_my_data2[-(1:nrow(train3)),]


#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
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

sum(prop_varex[1:34])
#add a training set with principal components
train.data <- data.frame(Approved = Approved, prin_comp$x)

#we are interested in first 34 PCAs
train.data <- train.data[,1:34]
train.data<-train.data[,c(2:34,1)]

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 33 components
test.data <- test.data[,1:33]

write.csv(train.data,"pca_train.csv",row.names = F)
write.csv(test.data,"pca_test.csv",row.names = F)

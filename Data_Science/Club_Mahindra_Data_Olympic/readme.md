# Club-Mahindra-Data-Olympic-Hackathon

# Food & Beverages Spend Prediction in Club Mahindra Resorts

Club Mahindra (Club M) makes significant revenue from Food and Beverages (F&B) sales in their resorts. The members of Club M are offered a wide variety of items in either buffet or À la carte form. 

Following are some benefits that the model to predict the spend by a member in their next visit to a resort will bring:
1. Predicting the F&B spend of a member in a resort would help in improving the pre-sales during resort booking through web and mobile app
2. Targeted campaigns to suit the member taste and preference of F&B
3. Providing members in the resort with a customized experience and offers
4. Help resort kitchen to plan the inventory and food quantity to be prepared in advance

Given the information related to resort, club member, reservation etc. the task is to predict average spend per room night on food and beverages for the each reservation in the test set.



**********************************************************************************************

# xxxxxxxxxxxxxxxx Approach xxxxxxxxxxxxxxxxxxxxxxxxxxxx




****************** System Features      ***********************************************

Platform R 3.5.2 (RStudio)
Processor i7
Ram : 8G
ETA : 2hrs


**********************************************************************************************


****************** Code Pre-requisites   ***********************************************

1. Set working directory(Line 32) and connect to Internet( for Package installer and h2o automl load)


**********************************************************************************************


****************** Data Preparation   ***********************************************

1. Get summary of data.

2. Remove identifier Variables as model build should be dependant upon features & independant of Identifiers[persontravellingid,member id,reservation_id,resort_id]

3. Extracting Features from Date variables like day,weekday,quarter,month,year, day of quarter [checkin_date,booking_date, checkout_date]

4. Missing value imputation for [season_holidayed_code,state_code_residence,roomnights(negative values) are treated as NA] data using median imputation 

5. Categorizing Features Properly into Continuous and Categorical. Categorizing Date Features like Month, Quarter.


**********************************************************************************************


****************** Feature Engineering  ***********************************************
1. booking_date - checkin_date = time_gap

2. checkin_date - checkout_date = duration_stay

3. numberofadults + numberofchildren = family_size

4. numberofadults/family_size = adult_ratio [Dealing with NA -> 0]

5. numberofchildrens/family_size = kids_ratio [Dealing with NA -> 0]



**********************************************************************************************




****************** Model Building Stage ***********************************************

1. Tree based models are not distance based models and can handle varying ranges of features. Hence, Scaling is not required while modelling trees.

2. PCA can be used but, too many categorical variables will increase computational complexity and would not give insight as to which features impacted the model (Business strategy).

3. Predictive Modelling for 1 and 1/2 hour using h2o Automl functionality, excluded DeepLearning and RandomForest and used GBM Learner.

4. Feature Engineering helped to get more important features for modelling. [duration_stay,time_gap]


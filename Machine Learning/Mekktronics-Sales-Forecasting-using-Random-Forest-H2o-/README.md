# Mekktronics-Sales-Forecasting-using-Random-Forest-H2o-

Dataset Description:

Sales Data was spread across country levels. (Daily Basis) 
Holiday information was given 
Promotional Expense details were given (Weekly Level)

Task:
Forecasting at Monthly Level



Data Preparation Steps Taken
1.  holidays date conversion into proper format( date standardization)
2.  Sales Currency standardization ( Scaled to one currency)
3.  Promotional Currency standardization ( Scaled to one currency)
4. Data Aggregation at Monthly Level
5. Feature Engg ( Finding region wise:  number of holidays in a month)


Model Building Stage
1. Did EDA to find exactly number of bins for random forest model
2. Used H20 model with n.trees =100 as aggregated data was less.
3. Train-Test split gave results of Symmetric mean absolute percentage error as 0.12

 



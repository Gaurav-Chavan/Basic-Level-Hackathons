# Emplyee Attrition Prediction using SVM in R

Algortihm used: Support Vector Regression
Programming Language used : R in Rstudio
Libraries used:
e1071: to build svm model
tidyquant: for preprocessing operations like converting character variables into factors of training dataset

SVR is prediction based algorithm so a special condition was added in test outputs
that is predicted probability should not be negative and greater than 1.

If such conditon occurs, then if probability is -ve output will be assigned as 0
and if probability is  greater than 1 output will be assigned as 1

Example:
for (i in 1:length(Attrition))
{
  if (Attrition[i]<0)
  {
    Attrition[i]=0
  }
  
  if (Attrition[i]>1)
  {
    Attrition[i]=1
  }
  
}

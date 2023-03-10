# Titanic-ML

These files contain EDA and model building I did for the Titanic ML Kaggle Competition. I entered this competition for an "Intro to Data Science" course project, applying the skills I learned from that course.  

Titanic.R contains analysis of the train and test data sets provided by Kaggle, where I performed some EDA and made models (Logistic and Random Forest). The test data set does not contain the survival of the passengers (dependent variable).  

Titanic2.R contains analysis which compares the data provided by Kaggle with the same data set from another source which has information on the survival of all the passengers also in the Kaggle data set. In this file I create an answer key for the test data set so I can test the accuracy of my models without submitting new predictions to the Kaggle competition.  

Future work includes correctly building an XGBoost model as well as using parallel computing to speed up the random forest model creation time. 

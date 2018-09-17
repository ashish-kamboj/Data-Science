## Business Understanding:

Based on the past and current customer information, the company has maintained a database containing personal/demographic information, the services availed by a customer and the expense information related to each customer.

## Goal:

The aim is to automate the process of predicting if a customer would telecom or not and to find the factors affecting the telecom. Whether a customer will telecom or not will depend on data from the following three buckets:
  - Demographic Information
  - Services Availed by the customer
  - Overall Expenses

## Data Understanding
  - **churn_data:** 7043 obs of 9 variables including the target variable
  - **customer_data:** 7043 obs of 5 variables
  - **internet_data:** 7043 obs of 9 variables

## Data Preparation & EDA
  - Merged the 3 data files
  - Checked for duplicate records
  - Missing value treatment
  - Outlier treatment
  - Scaling of variables or standardisation
  - Converted the categorical variables in to factor
  - Converted the continuousvariables in to numeric
  - Created dummy variables for factor attributes

## Model Building
  - Divided the data into train and test
  - Built a Logistic Regression Model using glm() function in R
  - Performed stepwise selection method for removing in-significant variables

## Model Evaluation
  - Predicted the probabilities of Churn for test data using predict() function
  - Created confusion matrix for checking the correct and incorrect classification of records
  - Found out the optimal probalility cutoff 
  - KS -statistic for Test Data
  - Plotted Lift & Gain Chart 

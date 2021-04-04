## Problem Description
Analyse the transactional data and Identify such factors which impacts the daily transaction volume. Also, predict the daily transaction volume for next 5 days.

## Data Description
Daily Transactional data for each month i.e. Jan-2017 to Oct-2017 per day transactional Data

 - Jan - 41668
 - Feb - 54522
 - Mar - 156429
 - Apr - 187696
 - May - 231851
 - Jun - 318340
 - Jul - 385274
 - Aug - 423830
 - Sep - 463834
 - Oct - 472852

## Solution

* Data Preparation
  - Synchronize the column name in each dataset
  - Checked for the duplicate entries for transactions
  - Checked for the **'NA'** values
  - Repalce the NA values witha appropriate values (e.g. Replace **NA** Credit_card_fee values to 0)
* Exploratory Data Analysis
  - Univariate analysis
  - Bi-variate analysis
* Combined the Monthly data in to one
* Performed the EDA on Merged dataset (i.e. Combined data for all the months)
* converted the data into timeseries
* Built a Time-series model
   * PLotted time-series
   * Checked for the white noise
   * Performed ADF and KPSS test
* Predicted the next 5 Days Transaction

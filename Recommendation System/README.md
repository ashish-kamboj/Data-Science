## Problem Statement:

You are the sales manager for "BeerMart", an online beer store in the United States.  You want to build a recommendation system (collaborative) for your store, where customers will be recommended the beer that they are most likely to buy. You collected the data about the ratings that the customers have provided in the past.

## Data Description:

Each record is composed of a beer's name, the name of the user along with ratings provided by users. All ratings are on a scale from 1 to 5 with 5 being the best.

## Solution:

1. **Data preparation**
	- Choose only those beers that have at least N number of reviews
		- Figure out an appropriate value of N using EDA; this may not have one correct answer, but you shouldn't choose beers having extremely low 		  number of ratings

	- Converted the data frame to a “realratingMatrix”

2. **Data Exploration**

	- Determine how similar the first ten users are with each other and visualise it
	- Compute and visualise the similarity between the first 10 beers
	- What are the unique values of ratings?
	- Visualise the rating values and notice:
		- The average beer ratings
		- The average user ratings
		- The average number of ratings given to the beers
		- The average number of ratings given by the users

3. **Recommendation Models**

	- Divide your data into training and testing datasets
		- Experiment with 'split' and 'cross-validation' evaluation schemes

	- Build IBCF and UBCF models

	- Compare the performance of the two models and suggest the one that should be deployed
		- Plot the ROC curves for UBCF and IBCF and compare them

	- Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"

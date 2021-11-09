## Problem Statement:

Build a movie recommendation system (collaborative). Data having the ratings that the customers have provided in the past.

## Data Description:

Each record is composed of a Movie name, the id of the user along with ratings provided by users. All ratings are on a scale from 1 to 5 with 5 being the best.

## Solution:

1. **Data preparation**

	- Converted the data frame to a “realratingMatrix”

2. **Data Exploration**

	- Histogram of ratings
	- Histogram of normalized ratings


3. **Recommendation Models**

	- Divided the data into training and testing datasets
		- Experimented with 'split' and 'cross-validation' evaluation schemes

	- Build **IBCF** and **UBCF** models

	- Compared the performance of the two models and found which one should be deployed
		- Ploted the ROC curves for UBCF and IBCF and compared them

	- Recommended the top N Movies to the users

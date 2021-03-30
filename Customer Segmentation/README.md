## Problem Statement
An online store incharge want to group the customers into different clusters, so that he can make a customised marketing campaign for each of the group. We do not have any label in mind, such as good customer or bad customer. We want to just look at patterns in customer data and then try and find segments. Clustering techniques use the raw data to form clusters based on common factors among various data points. This is exactly what will also be done in segmentation, where various people or products will be grouped together on the basis of similarities and differences between them.

As an incharge, he would have to decide what the important business criteria are on which he would want to segregate the customers. So, he would need a method or an algorithm that itself decides which customers to group together based on this criteria.

## Solution
1. **Data Prepartion**
	- Preparing RFM data
		- **Recency:** It measures how recently you visited the store or made a purchase
		- **Frequency:** It measures the frequency of the transactions the customers made
		- **Monetary:** It measures how much the customer spent on purchases he/she made
	- Outlier Treatment
	- Variable Transformation
	- Unified date format

2. **Model Building**
	- k-means clustering
	- Hierarchical Clustering


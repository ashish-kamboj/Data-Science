# Problem Statement:

A large company having around 4000 employees. However, every year, around 15% of its employees leave the company and need to be replaced with the talent pool available in the job market. The management believes that this level of attrition (employees leaving, either on their own or because they got fired) is bad for the company, because of the following reasons -

1. The former employees’ projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss among consumers and partners
2. A sizeable department has to be maintained, for the purposes of recruiting new talent
3. More often than not, the new employees have to be trained for the job and/or given time to acclimatise themselves to the company

Hence, the management wants to understand what factors they should focus on, in order to curb attrition. In other words, they want to know what changes they should make to their workplace, in order to get most of their employees to stay. Also, they want to know which of these variables is most important and needs to be addressed right away.

# About Data:

Data is divide in to five files-
1. Employee information (Demographic information, education details, salary etc.)
2. Employee survey details
3. manager Survey Details
4. Employee in-time(check-in time)
5. Employee out-time(check-out-time)

# Solution:

As a part of the solution, I've done the
1. Collating all the files in to one

2. Data preparation
	- Cleaning data
	- checking for missing or NA values
	- oulier detection/treatment
	- Correcting the variable format
3. Exploratory Data Analysis (by plotting different graphs)

4. Feature standardisation
	- Normalising continuous features 
	- Created the dummy variables for the categorical features

5. Divided the data in to the training and the test data

6. Build a Logistic regression model using the glm() function in R and also need to set the "family" parameter in function, it was binomial in our case.
	- First build a general model with all the variables
	- Then used the StepAIC function to remove the insignificant variables (direction as "both")
	- I've checked for the p-value (which should be less than 0.05) and VIF (usually around 2 or 3 but we can consider variables with high value if p-value is significant or we can also try to remove the variable with high VIF and significant p-value, if it doesn't decrease the accuracy then we're good to remove those variables) for further removing the variables

7. Predicted the probabilities of Attrition for test data (as the output of the Logistic regression model is the probablities not the class)

8. Tested for the various probability cutoff (like 50%, 40% etc.) [in order to convert the proabalities in to class] and checked how many records are classified as correct by confusionMatrix.

9. Then find the optimal probalility cutoff and calculated the acuracy, specificity and sensitivity
   accuracy - yeses (or positives) correctly predicted by it as yeses (or positives) and nos (or negatives) correctly predicted by the model as nos (or negatives)
   - **Specificity -** specificity is equal to the proportion of nos (or negatives) correctly predicted by the model as nos (or negatives).
   - **Sensitivity -** Sensitivity of a model is the proportion of yeses (or positives) correctly predicted by it as yeses (or positives).

10. Also, calculated the KS-statistis and plotted the Gain and Lift chart

    * **KS-statistics:** A high KS statistic means that not only does your model have all churns at the top, it has has all non-churns at the bottom. For a good model, KS statistic would be more than 40% and would lie in the top few deciles

	* **ROC curve:** I plotted ROC curve which can be plotted between % of bad and % of good, or in simple language, % of eventhappen and % of non-event happen.The perfect model is pretty much a right triangle, whereas the random model is a straight line. Basically, a model that rises steeply is a good model.

    * **Gain and Lift Chart:** Gain and Lift chart are mainly concerned to check the rank ordering of the probabilities

# Which variables are indicators/predictor of attrition?

1. We’d seen employees in early 20’s and Single or having less experience are more prone to the attrition, So we have to emphasis on them either my meeting, survey’s etc.
2. Keep an eye on the employee past companies work duration. Make sure it’s not frequent.
3. Recently promoted
4. working environment and job satisfaction 

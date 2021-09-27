## Problem Statement:

Contains the 26 English alphabets as classes, so it's a multiclass classification problem with 26 classes. Each row in the data set represents an image of a handwritten alphabet. Using some basic image processing, the images are converted into m X n pixels, where m and n depend on the size and resolution of the original image. Each pixel contains numeric values, with higher values denoting the presence of dense 'ink'. In the pixels where nothing is written, the pixel value is 0.

A pixel is called 'on' if it contains to a positive numeric value, else it is called 'off'. Using the pixelated images, 16 features are derived for each image, such as the width of the box, the ratio of the mean variance of x divided by the width of the box, etc.  


## About Dataset:

* Training dataset contained - 16000 obs. of 17 variables
* Test dataset was having - 4000 obs. of 17 variables

## Solution

As a part of the solution, I've done the

1. Data preparation
	- Renaming columns
2. scaing of variables, as they are of varied ranges
3. Build a SVM model using **Linear Kernel** and **RBF Kernel**
4. Hyperparameter tuning and Cross Validation
	- Performed 5-fold cross validation by "svmLinear" method and metric used is accuracy, to get the optimal value of C (cost of misclassification)
	- Performed 5-fold cross validation by "svmRadial" method and metric used is accuracy,  in order to get the optimal value of C and Sigma (sigma to control the amount of nonlinearity in the model. The higher the value of sigma, the more is the nonlinearity introduced).

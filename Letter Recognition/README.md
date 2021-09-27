## Problem Statement:

Contains the 26 English alphabets as classes, so it's a multiclass classification problem with 26 classes. Each row in the data set represents an image of a handwritten alphabet, as shown in figure 1(A). Using some basic image processing, the images are converted into m X n pixels, where m and n depend on the size and resolution of the original image. Each pixel contains numeric values, with higher values denoting the presence of dense 'ink'. In the pixels where nothing is written, the pixel value is 0.

A pixel is called 'on' if it contains to a positive numeric value, else it is called 'off'. Using the pixelated images, 16 features are derived for each image, such as the width of the box, the ratio of the mean variance of x divided by the width of the box, etc.  


## About Dataset:

* Training dataset contained - 60000 obs. of 785 variables
* Test dataset was having - 10000 obs. of 785 variables

## Solution

As a part of the solution, I've done the

1. Data preparation
	- Cleaning data
	- checking for missing or NA values
	- Making target class to factor
	- Renaming columns

2. As those are the pixel values (i.e. between 0-255) we don't need scaing of variables
3. Build a SVM model using ksvm() function in R.


## What is SVM and How does it work?

**“Support Vector Machine” (SVM)** is a supervised machine learning algorithm which can be used for both classification or regression challenges. However,  it is mostly used in classification problems. In this algorithm, we plot each data item as a point in n-dimensional space (where n is number of features you have) with the value of each feature being the value of a particular coordinate. Then, we perform classification by finding the hyper-plane that differentiate the two classes

There could be multiple lines (hyperplanes) possible, which perfectly separate the two classes. But the best line is the one that maintains the largest possible equal distance from the nearest points of both the classes (This distance is called as Margin). So for the separator to be optimal, the margin — or the distance of the nearest point to the separator — should be maximum. This is called the Maximal Margin Classifier.

Instead of searching for the margin that exactly classifies each and every data point to the correct class. The points that are close to the hyperplane are only considered for constructing the hyperplane, and they are called support vectors.

As mentioned about hyperplanes, the Maximal Margin Classifier, and the Support Vector Classifier. All of these are linear models (since they use linear hyperplanes to separate the classes). However, many real-world data sets are not separable by linear boundaries. To solve those problems SVM has a technique called Kernel, These are functions which takes low dimensional input space and transform it to a higher dimensional space i.e. it converts not separable problem to separable problem.

- **Linear Kernel:** This is the same as the Support Vector Classifier or the hyperplane, without any transformation at all.
- **Polynomial Kernel:** This kernel function is capable of creating nonlinear, polynomial decision boundaries.
- **RBF Kernel:** This is the most complex kernel function; it is capable of transforming highly nonlinear feature spaces to linear ones. It is even capable of creating elliptical (i.e. enclosed) decision boundaries.

For our problem I've tried out all the three kernel, RBF kernel gave the best accuracy.

4. **Hyperparameter tuning and Cross Validation**
	- Performed 5-fold cross validation by "svmLinear" method and metric used is accuracy, to get the optimal value of C (cost of misclassification)
	- Performed 5-fold cross validation by "svmRadial" method and metric used is accuracy,  in order to get the optimal value of C and Sigma (sigma to control the amount of nonlinearity in the model. The higher the value of sigma, the more is the nonlinearity introduced).

I'd also tried the Logistic Regression for this classification problem but the accuracy was very low as this is a linear classifier to able to classify all the points.

**Comparison: SVM with Logistic Regression and Decision Trees**
  - Though SVM is complex model but it provides good accuracy
  - Logistic regression is simple model and easilt interpretable but the accuracyy wasn't not good if the dtaa is not linearly separable
  - Decision Tree is not so complex, easily interpretable and expalainable, accuracy is also very good

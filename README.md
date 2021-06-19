## EDA and ML Projects
Repository contains the various projects with code written in Pythona and R Language for-
* Exploratory Data Analysis
* Machine Learning Models (Linear Regression, Logistic Regression, k-means Clustering, Hierarchical Clustering, SVM, Desicion Tree, Random Forest, Time-Series Analysis, XGBoost)

Below are the list of some common Packages/Libraries which were used as a part of the data analyis and building Machine learning model
* **Data Manipulation:** 
  - **R ::** dplyr, plyr, tidyr, stringr, data.table, lubridate(for date manipulation)
  - **Python ::** Pandas
* **Data Visualization:** 
  - **R ::** ggplot2, cowplot, ggthemes, scales
  - **Python ::** matplotlib, seaborn
* **ML models:** 
  - **R ::** randomForest, caret(for Data splitting, cross validation, Pre-processing, Feature selection, Variable importance estimation, etc.)
  - **Python ::** scikit-learn
* **Recommendation model:** recommenderlab
* **Text Mining:** 
  - **R ::** tm, tidyverse
  - **Python ::** nltk, spaCy

## Additional Reading:

- To know more about dummy variables [(here)](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-dummy-coding)
- Why it's necessary to create dummy variables [(here)](https://stats.stackexchange.com/questions/89533/convert-a-categorical-variable-to-a-numerical-variable-prior-to-regression)
- When to Normalise date and when to standardise? [(here)](https://stackoverflow.com/questions/32108179/linear-regression-normalization-vs-standardization)
- Various scaling techniques [(here)](https://en.wikipedia.org/wiki/Feature_scaling)
- Recursive Feature Elimination(RFE) - scikit-learn [(here)](https://scikit-learn.org/stable/modules/generated/sklearn.feature_selection.RFE.html)
  - Recursive feature elimination is based on the idea to repeatedly construct a model (for example an SVM or a regression model) and choose either the best or worst performing feature (for example based on coefficients), setting the feature aside and then repeating the process with the rest of the features. This process is applied until all features in the dataset are exhausted. Features are then ranked according to when they were eliminated. As such, it is a greedy optimization for finding the best performing subset of features. Read more at this [link](http://blog.datadive.net/selecting-good-features-part-iv-stability-selection-rfe-and-everything-side-by-side/)
- Parametric v/s non parametric models in [short](https://stats.stackexchange.com/questions/268638/what-exactly-is-the-difference-between-a-parametric-and-non-parametric-model) and [detailed](https://machinelearningmastery.com/parametric-and-nonparametric-machine-learning-algorithms/)
- Regression guarantees interpolation of data and not extrapolation
  - Interpolation basically means using the model to predict the value of a dependent variable on independent values that lie within the range of data you already have. Extrapolation, on the other hand, means predicting the dependent variable on the independent values that lie outside the range of the data the model was built on.
- Optimization Methods [(here)](https://www.springer.com/cda/content/document/cda_downloaddocument/9783642378454-c2.pdf?SGWID=0-0-45-1425030-p175100176)
- A brief overview of Feature Scaling [(here)](https://en.wikipedia.org/wiki/Feature_scaling)
- When to standardise, when to normalise [(here)](https://stackoverflow.com/questions/32108179/linear-regression-normalization-vs-standardization)
  - [When and Why to stardardize a variable](https://www.listendata.com/2017/04/how-to-standardize-variable-in-regression.html)
- All about When and How to do train_test_split and pre_processing
  - [Things to know before train and test split](https://towardsdatascience.com/3-things-you-need-to-know-before-you-train-test-split-869dfabb7e50)
  - [Data Preparation without data leakage](https://machinelearningmastery.com/data-preparation-without-data-leakage/)
- Dimensionality Reduction Algorithmns [(here)](https://machinelearningmastery.com/dimensionality-reduction-algorithms-with-python/)
- Feature Selection [(here)](https://machinelearningmastery.com/feature-selection-machine-learning-python/)
- Naive Bayes Classification explanation [(here)](https://stackoverflow.com/questions/10059594/a-simple-explanation-of-naive-bayes-classification)
- Factor Analysis
  - [Introduction to factor analysis](http://www.tqmp.org/RegularArticles/vol09-2/p079/p079.pdf)
  - [Factor analysis Notes](http://cs229.stanford.edu/notes2020spring/cs229-notes9.pdf)
  - [Theory and practice questions on factor analysis](http://www.yorku.ca/ptryfos/f1400.pdf)\
- Implementing recommendation systems
  - [Recommender systems 101 – A step-by-step practical example in R](https://www.r-bloggers.com/recommender-systems-101-a-step-by-step-practical-example-in-r/)
  - [A framework for developing and testing recommendation algorithms](https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf)
  - [Netflix implementation of recommendation engine](https://medium.com/netflix-techblog/netflix-recommendations-beyond-the-5-stars-part-1-55838468f429)
- Understanding ROC curve [(here)](https://stats.stackexchange.com/questions/105501/understanding-roc-curve)
- Feature Engineering and it's importance [(here)](https://machinelearningmastery.com/discover-feature-engineering-how-to-engineer-features-and-how-to-get-good-at-it/)
- Explanation of **linear** or **linearity** in Linear Regression
  - The term **'linear'** in linear regression refers to the linearity in the coefficients, i.e. the target variable y is **linearly related to the model coefficients**. It does not require that y should be linearly related to the raw attributes or features. Feature functions could be linear or non-linear.
- Techniques for handling Class Imbalance in Dataset
  - [8 Tactics to Combat Imbalanced Classes in Your Machine Learning Dataset](https://machinelearningmastery.com/tactics-to-combat-imbalanced-classes-in-your-machine-learning-dataset/)
  - [10 Techniques to deal with Imbalanced Classes in Machine Learning](https://www.analyticsvidhya.com/blog/2020/07/10-techniques-to-deal-with-class-imbalance-in-machine-learning/)
- XGBoost
  - [XGBoost Algorithm - Medium](https://towardsdatascience.com/https-medium-com-vishalmorde-xgboost-algorithm-long-she-may-rein-edd9f99be63d)
  - [A Gentle Introduction to XGBoost for Applied Machine Learning](https://machinelearningmastery.com/gentle-introduction-xgboost-applied-machine-learning/)
- LightGBM [(here)](https://medium.com/@pushkarmandot/https-medium-com-pushkarmandot-what-is-lightgbm-how-to-implement-it-how-to-fine-tune-the-parameters-60347819b7fc)

## Related Mathematics
- [Mathplanet - Exponentials](https://www.mathplanet.com/education/algebra-1/exponents-and-exponential-functions/properties-of-exponents)
- [Mathplanet - Logarithms](https://www.mathplanet.com/education/algebra-2/exponential-and-logarithmic-functions/logarithm-property)

## Model Evaluation [(here)](https://machinelearningmastery.com/metrics-evaluate-machine-learning-algorithms-python/)
  - **Regression**
    - R-squared/Adj. R-squared
    - Root Mean Squared Error(RMSE) / Mean Squared Error
    - Mean Absolute Error(MAE)
  - **Classification** [(here)](https://towardsdatascience.com/the-5-classification-evaluation-metrics-you-must-know-aa97784ff226)
    - Accuracy, Precision, and Recall
    - Log Loss/Binary Crossentropy
    - Categorical Crossentropy
    - Confusion Matrix
    - F1 Score
    - AUC

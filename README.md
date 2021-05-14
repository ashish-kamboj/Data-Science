## EDA and ML Projects
Repository contains the various projects with code written in R Language for-
* Exploratory Data Analysis
* Machine Learning Models (Linear Regression, Logistic Regression, k-means Clustering, Hierarchical Clustering, SVM, Desicion Tree, Random Forest, Time-Series Analysis, XGBoost)

Below are the list of some common Packages/Libraries which were used as a part of the data analyis and building Machine learning model
* **Data Manipulation:** dplyr, plyr, tidyr, stringr, data.table, lubridate(for date manipulation),
* **Data Visualization:** ggplot2, cowplot, ggthemes, scales
* **ML models:** randomForest, caret(for Data splitting, cross validation, Pre-processing, Feature selection, Variable importance estimation, etc.)
* **Recommendation model:** recommenderlab
* **Text Mining:** tm, tidyverse

## Additional Reading:

- To know more about dummy variables [(here)](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-dummy-coding)
- Why it's necessary to create dummy variables [(here)](https://stats.stackexchange.com/questions/89533/convert-a-categorical-variable-to-a-numerical-variable-prior-to-regression)
- When to Normalise date and when to standardise? [(here)](https://stackoverflow.com/questions/32108179/linear-regression-normalization-vs-standardization)
- Various scaling techniques [(here)](https://en.wikipedia.org/wiki/Feature_scaling)
- Recursive Feature Elimination(RFE) - scikit-learn [(here)](https://scikit-learn.org/stable/modules/generated/sklearn.feature_selection.RFE.html)
  - Recursive feature elimination is based on the idea to repeatedly construct a model (for example an SVM or a regression model) and choose either the best or worst performing feature (for example based on coefficients), setting the feature aside and then repeating the process with the rest of the features. This process is applied until all features in the dataset are exhausted. Features are then ranked according to when they were eliminated. As such, it is a greedy optimization for finding the best performing subset of features. Read more at this [link](http://blog.datadive.net/selecting-good-features-part-iv-stability-selection-rfe-and-everything-side-by-side/)
- Parametric v/s non parametric models in [short](https://stats.stackexchange.com/questions/268638/what-exactly-is-the-difference-between-a-parametric-and-non-parametric-model) and [detailed](https://machinelearningmastery.com/parametric-and-nonparametric-machine-learning-algorithms/)

############################ Email Classification Problem ############################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear SVM Model at C=1
#  4.2 Linear SVM Model at C=2

# 5  Cross validation 
# 5.1. Tunning linear SVM model (will cover in the next lecture)

#####################################################################################

# 1. Business Understanding: 

# The "spam" concept is diverse:
# 1. Advertisements for products/web
# 2. Sites,make money fast schemes etc.

# Based on the past mails,our objective is to identify whether a given email is spam or not.

#####################################################################################

# 2. Data Understanding: 
# https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.names
# Number of Instances: 4601
# Number of Attributes: 58 (57 continuous, 1 nominal class label)

#3. Data Preparation: 

##Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(caTools)

#Loading Data
email_rec <- read.delim("Spam.txt",sep = ",", stringsAsFactors = F,header = F)


# Renaming the column names

colnames(email_rec) <- c("word_freq_make",
                         "word_freq_address",
                         "word_freq_all",
                         "word_freq_3d",
                         "word_freq_our",
                         "word_freq_over",
                         "word_freq_remove",
                         "word_freq_internet",
                         "word_freq_order",
                         "word_freq_mail",
                         "word_freq_receive",
                         "word_freq_will",
                         "word_freq_people",
                         "word_freq_report",
                         "word_freq_addresses",
                         "word_freq_free",
                         "word_freq_business",
                         "word_freq_email",
                         "word_freq_you",
                         "word_freq_credit",
                         "word_freq_your",
                         "word_freq_font",
                         "word_freq_000",
                         "word_freq_money",
                         "word_freq_hp",
                         "word_freq_hpl",
                         "word_freq_george",
                         "word_freq_650",
                         "word_freq_lab",
                         "word_freq_labs",
                         "word_freq_telnet",
                         "word_freq_857",
                         "word_freq_data",
                         "word_freq_415",
                         "word_freq_85",
                         "word_freq_technology",
                         "word_freq_1999",
                         "word_freq_parts",
                         "word_freq_pm",
                         "word_freq_direct",
                         "word_freq_cs",
                         "word_freq_meeting",
                         "word_freq_original",
                         "word_freq_project",
                         "word_freq_re",
                         "word_freq_edu",
                         "word_freq_table",
                         "word_freq_conference",
                         "char_freq_;",
                         "char_freq_(",
                         "char_freq_[",
                         "char_freq_!",
                         "char_freq_$",
                         "char_freq_hash",
                         "capital_run_length_average",
                         "capital_run_length_longest",
                         "capital_run_length_total",
                         "spam")

#Understanding Dimensions
dim(email_rec)

#Structure of the dataset
str(email_rec)

#printing first few rows
head(email_rec)

#Exploring the data
summary(email_rec)

# Changing output variable "spam" to factor type 

email_rec$spam <- as.factor(email_rec$spam)

# Checking missing value
sapply(email_rec, function(x) sum(is.na(x))) # No missing values

#####################################################################################

# Splitting the data between train and test

set.seed(100)

indices = sample.split(email_rec$spam, SplitRatio = 0.7)

train = email_rec[indices,]

test = email_rec[!(indices),]

# 4. Model Building

#--------------------------------------------------------------------
# 4.1 Linear model - SVM  at Cost(C) = 1
#####################################################################

# Model with C =1
model_1<- ksvm(spam ~ ., data = train,scale = FALSE,C=1)

# Predicting the model results 
evaluate_1<- predict(model_1, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_1, test$spam)

# Accuracy    : 0.9261
# Sensitivity : 0.9593         
# Specificity : 0.8750

#--------------------------------------------------------------------
# 4.2 Linear model - SVM  at Cost(C) = 10
#####################################################################

# Model with C =10.
model_10<- ksvm(spam ~ ., data = train,scale = FALSE,C=10)

# Predicting the model results
evaluate_10<- predict(model_10, test)

# Confusion Matrix - finding accuracy, sensitivity and specificity
confusionMatrix(evaluate_10, test$spam)

# Accuracy    : 0.929
# Sensitivity : 0.9581         
# Specificity : 0.8842 
#####################################################################

#####################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

set.seed(100)


# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm <- train(spam~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm)
# Best tune at C=2, 
# Accuracy - 0.924

# Plotting "fit.svm" results
plot(fit.svm)

###############################################################################

# Valdiating the model after cross validation on test data

evaluate_linear_test<- predict(fit.svm, test)
confusionMatrix(evaluate_linear_test, test$spam)

# Accuracy    - 0.9304
# Sensitivity - 0.9545
# Specificity - 0.8934
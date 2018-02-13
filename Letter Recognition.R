############################ SVM Letter Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a large number of black-and-white
#rectangular pixel displays as one of the 26 capital letters in the English alphabets

#####################################################################################

# 2. Data Understanding: 
# https://archive.ics.uci.edu/ml/datasets/letter+recognition
# Number of Instances: 20,000
# Number of Attributes: 17 

#3. Data Preparation: 

  ## Loading Neccessary libraries

    library(kernlab)
    library(readr)
    library(caret)

  ## Loading Data
    Data <- read_csv("Letter-recognition.csv")

    #Understanding Dimensions
      dim(Data)

    #Structure of the dataset
      str(Data)
    
    #printing first few rows
      head(Data)

    #Exploring the data
      summary(Data)

  ## checking missing value
      sapply(Data, function(x) sum(is.na(x)))

  ## Making our target class to factor
      Data$letter<-factor(Data$letter)


## Split the data into train and test set

  set.seed(1)
  train.indices = sample(1:nrow(Data), 0.7*nrow(Data))
  train = Data[train.indices, ]
  test = Data[-train.indices, ]

##Constructing Model

  #Using Linear Kernel
    Model_linear <- ksvm(letter~ ., data = train, scale = FALSE, kernel = "vanilladot")
    Eval_linear<- predict(Model_linear, test)

    #confusion matrix - Linear Kernel
      confusionMatrix(Eval_linear,test$letter)


  #Using RBF Kernel
    Model_RBF <- ksvm(letter~ ., data = train, scale = FALSE, kernel = "rbfdot")
    Eval_RBF<- predict(Model_RBF, test)

    #confusion matrix - RBF Kernel
      confusionMatrix(Eval_RBF,test$letter)


############   Hyperparameter tuning and Cross Validation #####################

## Using the train function from caret package to perform Cross Validation. 

  #traincontrol function Controls the computational nuances of the train function.
  # i.e. method =  CV means  Cross Validation.
  #      Number = 2 implies Number of folds in CV.

  trainControl <- trainControl(method="cv", number=5)

  # Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
    metric <- "Accuracy"

  #Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
    set.seed(7)
    grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )


  #train function takes Target ~ Prediction, Data, Method = Algorithm
  #Metric = Type of metric, tuneGrid = Grid of Parameters,
  # trcontrol = Our traincontrol method.

  fit.svm <- train(letter~., data=train, method="svmRadial", metric=metric,tuneGrid=grid, trControl=trainControl)

  print(fit.svm)
  plot(fit.svm)
## Load Libraries
  library(randomForest)
  
## Read the data
  datafile <- 'credit-card-default.csv'
  df <- read.csv(datafile)
  View(df)

## Change the ID column to NULL, as it's not required
  df[, 1] <- NULL

## Convert continuous variables into numeric and categorical variables as factors
  numericcols <- c('AGE', 'PAY_0', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6',
                 'BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5',
                 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5',
                 'PAY_AMT6')

  factorcols <- c('SEX', 'EDUCATION', 'MARRIAGE', 'defaulted')

  df[, numericcols] <- lapply(numericcols, function(x) as.numeric(as.character(df[, x])))
  df[, factorcols] <- lapply(factorcols, function(x) as.factor(as.character(df[, x])))

## Shuffle the data
  shuffledata <- df[sample(nrow(df)), ]
  View(shuffledata)

## Split the data into train and test
  ntrain <- as.integer(nrow(shuffledata)*0.8)
  traindata <- shuffledata[1:ntrain, ]
  testdata <- shuffledata[(ntrain+1):nrow(shuffledata), ]

## Build the random forest
  set.seed(71)
  data.rf <- randomForest(defaulted ~ ., data=traindata, proximity=FALSE,ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)
  data.rf
  
  testPred <- predict(data.rf, newdata=testdata)
  table(testPred, testdata$defaulted)


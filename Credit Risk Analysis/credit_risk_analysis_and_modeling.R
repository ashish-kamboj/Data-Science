
##################################### BFS Capstone ###################################
#####--------------------------------##############---------------------------#######
# Loading the libraries
library(caret)
library(ggplot2)
library(Information)
library(caTools)
library(randomForest)
library(stringr)
library(car)
library(Hmisc)
library(ROCR)
library(dplyr)
library(DMwR)
library(GGally)
library(ranger)
library(MASS)
#####-----------------------------------------------------------------------------
### Load credit bureau and demographic dataset  
credit <- read.csv("Credit_Bureau.csv")
demo <- read.csv("Demogs.csv")
#####-----------------------------------------------------------------------------
# Basic data quality checks
# Check for duplicate values
# Credit bureau
str(credit)
dup <- duplicated(credit$Application.ID)
sum(dup)
# There are 3 duplicate IDs in the credit bureau data, removing them
credit <- credit[!duplicated(credit$Application.ID), ]
str(credit)

# Check for duplicate values in demographic data
str(demo)
dup <- duplicated(demo$Application.ID)
sum(dup)
demo <- demo[!duplicated(demo$Application.ID), ]
str(demo)

#####-----------------------------------------------------------------------------

# Merging both datasets by a common ID i.e "Application.ID"
master <- merge(credit, demo, by="Application.ID")
str(master)

# Check if performance tags of both data sets are same i.e "Performance.Tag.x" and "Performance.Tag.y" should be same
na_match <- sum(which(is.na(master$Performance.Tag.x)) == which(is.na(master$Performance.Tag.y)))

performance_match <- sum(master$Performance.Tag.x == master$Performance.Tag.y, na.rm=T)

na_match + performance_match == nrow(master)
# conclusion: all na values and data are matching in performance tags
# removing performance tag y

master <- master[, -30]

#####-----------------------------------------------------------------------------
# Also, the ones with performance tag NA are the rejected population
# Storing it as a separate dataframe
reject_rows <- which(is.na(master$Performance.Tag.x)) 
rejected <- master[reject_rows, ]
str(rejected)

str(master)

nrow(rejected)
rejection_rate <- nrow(rejected)/nrow(master)
rejection_rate*100
approval_rate <- (1-rejection_rate)
approval_rate*100   # 98% approval rate

#####-----------------------------------------------------------------------------
# Missing value analysis and imputation
# Let's check the summary 
master <- master[-reject_rows, ]
str(master)

# Missing values in all the variables 
sapply(master, function(x) sum(is.na(x)))

#####-----------------------------------------------------------------------------
# let's analyse the missing values:

# Avgas.CC.Utilization.in.last.12.months
avgcc_missing <- which(is.na(master$Avgas.CC.Utilization.in.last.12.months)) #1023 missing values
# We don't know what is the best way of treating NAs here, so let them be NA for now.

# missing_utilisation <- master[avgcc_missing, ]
  

#####-----------------------------------------------------------------------------
# Let's see the missing values in outstanding balance. 
Outstanding_missing <- which(is.na(master$Outstanding.Balance))
View(master[Outstanding_missing, ])
length(Outstanding_missing)

# The variables "presence of open home loan"  "Outstanding balance" & "Avgas.cc.utilisation" are missing alltogether
#  also contain NAs. Thus removing 272 observations(which is 0.3% of total observation) do not impact on the models. 
# Let's remove these from the analysis

master <- master[-which(is.na(master$Outstanding.Balance)),]

#####-----------------------------------------------------------------------------
# In "No. of dependents" variable, there are only 3 blank observations. Thus, its better to get rid off it. 

No.of.dependents <- which(is.na(master$No.of.dependents))
master <- master[-No.of.dependents,]

#####-----------------------------------------------------------------------------

# In "No.of.trades.opened.in.last.6.months" variable contains 1 Na, let's remove this too.

master <- master[-which(is.na(master$No.of.trades.opened.in.last.6.months)),]

#####-----------------------------------------------------------------------------
# See the summary of age variable 
summary(master$Age)

# Age should always be positive, also typically  you must be at least 18 to apply for a credit card.
# So, less than 18 years of job applicant should be removed from the analysis. 

age_under18 <- master[which(master$Age<18),]

# Let's remove these observations 

master <- master[-which(master$Age<18),]

#####-----------------------------------------------------------------------------
# In "Income" variable, you could notice that some people have negative incomes. 

summary(master$Income)
quantile(master$Income,seq(0,1,0.001))

# Outlier Treatment
# Flooring with 4.5 
master$Income[which(master$Income<4.5)]<- 4.5

#####-----------------------------------------------------------------------------
# The variables- "Gender","Type.of.residence" and "Marital.Status" do have very less blanks. 
# In "Gender", you will notice that it only contains 1 blank.Even in "Marital.Status" variable, it does have
# only 5 blanks whereas "Type.of.residence" does have 8 blanks. 
# Thus, these values need to be removed from the analysis. 

# Gender
master <- master[-which(master$Gender==""),]
master$Gender <- factor(master$Gender)

#Marital_status
master <- master[-which(master$Marital.Status..at.the.time.of.application.==""),]
master$Marital.Status..at.the.time.of.application. <- factor(master$Marital.Status..at.the.time.of.application.)

#Type.of.residence
master <- master[-which(master$Type.of.residence==""),]
master$Type.of.residence <- factor(master$Type.of.residence)

# Profession

master <- master[-which(master$Profession==""),]
master$Profession <- factor(master$Profession)

#####-----------------------------------------------------------------------------
# Let's see the outliers as well in continuous variables. 

# Outstanding.Balance

quantile(master$Outstanding.Balance, seq(0,1,0.01),na.rm = T)

# Capping at 4251075.18 (99th percentile)
master$Outstanding.Balance[which(master$Outstanding.Balance>4250993)] <- 4250993
summary(master$Outstanding.Balance)

#####-----------------------------------------------------------------------------
# Exploratory Data Analysis: 

# For Categorical Variables: 

# Creating a dashboard of defaults 

continuous_var <- colnames(master)[c(8,16,17,20,24,28:29)]

# All Categorical variables: 
categorical_var <- master[,!colnames(master)%in%continuous_var]
categorical_var <- categorical_var[,-1]
View(categorical_var)


# All Continuous variables: 
continuous_var <- master[,colnames(master)%in%continuous_var]
View(continuous_var)

# Removing performance variable
categorical_var <- categorical_var[,-15]
#####################################################################
# Let's Analyse the continuous variables: 

# Correlation matrix



#ggpairs(continuous_var[,-1])
corrs = round(cor(continuous_var, use = "pairwise.complete.obs"), 2)
View(corrs)
write.csv(corrs, "correlations.csv")
# there's27% correlation in number of months in current residence and avg credit utilisation

#####################################################################
# Creating buckets:

# Age: 
# Group-1:

summary(continuous_var$Age)

categorical_var$Age <- cut(continuous_var$Age, include.lowest = TRUE,breaks = c(18,30,40,50,65))

# No.of.months.in.current.residence
# Group-2:

summary(continuous_var$No.of.months.in.current.residence)
quantile(continuous_var$No.of.months.in.current.residence,seq(0,1,0.01))

# Based on the quantile distribution, let's bucket this variables: 
categorical_var$current_residence <- cut(continuous_var$No.of.months.in.current.residence, include.lowest = TRUE,breaks = c(6,10,40,70,100,130))

# Group-3
# No.of.months.in.current.company
summary(continuous_var$No.of.months.in.current.company)
quantile(continuous_var$No.of.months.in.current.company,seq(0,1,0.01))

# Based on the quantile distribution, let's bucket this variables: 
categorical_var$current_company <- cut(continuous_var$No.of.months.in.current.company, include.lowest = TRUE,breaks = c(3,10,40,70,100,133))

# Grouop-4
# Outstanding.Balance
summary(continuous_var$Outstanding.Balance)
quantile(continuous_var$Outstanding.Balance,seq(0,1,0.01))

# Based on the quantile distribution, let's bucket this variables: 
categorical_var$Outstanding.Balance <- cut(continuous_var$Outstanding.Balance, include.lowest = TRUE,breaks = c(586,6853,25604,386879,585402,774181,972320,1357216,2960918,3282457,4251075))

# Group-5 
# Avgas.CC.Utilization.in.last.12.months
summary(continuous_var$Avgas.CC.Utilization.in.last.12.months)
quantile(continuous_var$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01),na.rm = T)

# Based on the quantile distribution, let's bucket this variables: 
categorical_var$Avgas.CC.Utilization.in.last.12.months <- cut(continuous_var$Avgas.CC.Utilization.in.last.12.months, include.lowest = TRUE,breaks = c(0,6,8,11,15,25,45,65,113))

# Grouop-6
# Total.No.of.Trades
summary(continuous_var$Total.No.of.Trades)
quantile(continuous_var$Total.No.of.Trades,seq(0,1,0.01))

# Based on the quantile distribution, let's bucket this variables: 
categorical_var$Total.No.of.Trades <- cut(continuous_var$Total.No.of.Trades, include.lowest = TRUE,breaks = c(0,2,3,4,6,9,10,16,44))

## Grouop-7
# Income
summary(continuous_var$Income)
quantile(continuous_var$Income,seq(0,1,0.01))

# Based on the quantile distribution, let's bucket this variables: 
categorical_var$Income <- cut(continuous_var$Income, include.lowest = TRUE,breaks = c(0,7,14,20,27,36,40,46,60))


# Let's analyse all the categorical variables: 

plot_default <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag.x~cat_var, master, mean)
  b <- data.frame(table(cat_var))
  
  colnames(a)[1] <- var_name
  colnames(b)[1] <- var_name
  agg_response <- merge(a, b, by = var_name)
  
  colnames(agg_response) <- c(var_name, "Default","count")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  
  agg_response <- arrange(agg_response, desc(Default))
  agg_response[, 2] <- as.numeric(agg_response[, 2])
  
  p.plot <- ggplot(agg_response, aes(agg_response[, 1], Default, label = count)) +
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
  return(list((agg_response[1, 2] - agg_response[nrow(agg_response), 2]),p.plot))
  
}

###################################################################################
# Categorical Variables V/s "Performance.Tag.x" variable 

####################################################################################
EDA_plots <- list()

for (i in 1:length(colnames(categorical_var))){
  EDA_plots[[i]] <- plot_default(categorical_var[,i],colnames(categorical_var)[i])
  
}


## If check EDA_plots[[1]]
# You get a list of two: 
#1. It contains the maximumn difference of min and max default rate within the levels(as show in the plot).
# 2. It shows the bar graph of the variables "No. of times 90DPD in last 6 months"
# frequency mentioned on thetop of each bar whereas default rate is on the y-axis 


EDA_plots[[1]]
# 90 DPD-6 months seems to be an important predictor - those with 2 and 3 late payments 
# seem to have significantly higher default rates than 0 and 1
# It corresponds to a difference of 0.08% increase in default rate between 0 and 4

EDA_plots[[2]]
# 60 DPD seems a mild predictor

EDA_plots[[3]]
# 30 DPD seems a mild predictor as well

EDA_plots[[4]]
# 90 DPD-12 months seems quite significant - causes an increase of 0.11 default rate

EDA_plots[[5]]

EDA_plots[[6]]

# and so on..


###################################################################################################
### Let's understanding the incremental gain within the levels of categorical variables ###
###################################################################################################
# Creating a dataframe which contains the incremental values.
Increment <- data.frame(sapply(categorical_var, function(x) plot_default(as.factor(x),"Plots")[[1]]))
Increment <- cbind(variables =row.names(Increment),Increment)
rownames(Increment) <- NULL
colnames(Increment) <- c("Variables","Increment_diff")
Increment <- arrange(Increment, desc(Increment_diff))

# View(Increment)

############################################################################################
# Let's plot the Increments on one chart
############################################################################################

ggplot(data=Increment, aes(x=reorder(Variables,Increment_diff),y=Increment_diff)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("EDA- Important variables based on incremental analysis") +xlab("Variables")

############################################################################################
# Based on the exploratory data analysis: 
# Top-5 important variables are: 
# 1. No.of.times.60.DPD.or.worse.in.last.12.months
# 2. No.of.times.90.DPD.or.worse.in.last.12.months
# 3. No.of.times.30.DPD.or.worse.in.last.12.months
# 4. No.of.trades.opened.in.last.12.months
# 5. No.of.times.90.DPD.or.worse.in.last.6.months

# No demographic variables seem to be important for predicting default.  
########################################################################################
########################################################################################
################################## Woe Analysis ########################################
# The method used above is a good but crude way to understand the importance of variables
# for default prediction. We want to quantify the importance of each predictor variable.

# In other words, we want to find the 'information value' of each variable. 
# The weight of evidence (WOE) shows the predictive power of an independent variable 
# in relation to the dependent variable.

# Woe is a measure of how well a variable separates the good customers from the bad ones. 
# So, woe= ln(Distribution of good/Distribution of Bads)


# Information value and WOE calculation
# The information package is used for woe calculation. 
# One slight change you need to perform before applying the "create_infotable"
# This package indicates "1" as a good customer but in our dataset, we have assigned "1" bad customers
# Thus we have to change the label to get the correct value of woe value. 

# First let's create a copy of master dataset
master_test <- master

# Change the levels to "good " and "bad"
master_test$Performance.Tag.x <- ifelse(master_test$Performance.Tag.x==1,"bad","good")

# Rechange the levels to "1" for good customer and "0" for bad customers (This change is only for this package)
master_test$Performance.Tag.x <- ifelse(master_test$Performance.Tag.x=="good",1,0)

IV_test <- create_infotables(master_test[,-1],y="Performance.Tag.x",ncore = 2)

IV_test$Tables

IV_test$Summary

# Let's create a dataframe containing IV values of all the variables 
IV_dataframe <- IV_test$Summary
str(IV_dataframe)

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for(i in 1:nrow(IV_dataframe)){
  
  if (IV_dataframe$IV[i]<0.02){
    IV_dataframe$feedback[i] = "Useless"
    
  } else if(IV_dataframe$IV[i]>=0.02& IV_dataframe$IV[i]<0.1){
    IV_dataframe$feedback[i] = "Weak"
    
  } else if(IV_dataframe$IV[i]>=0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Medium"
    
  }else if(IV_dataframe$IV[i]>=0.3 & IV_dataframe$IV[i]<0.5){
    IV_dataframe$feedback[i] = "Strong"
    
  }else if(IV_dataframe$IV[i]>0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Suspicious"
  }
}

str(IV_dataframe)
IV_dataframe$Variable <- as.factor(IV_dataframe$Variable)
IV_dataframe$feedback <- as.factor(IV_dataframe$feedback)

##################################################################
# Based on feedback 
## Extract "Strong" and "Medium" variables  
imp_vars <- which(IV_dataframe$feedback=="Strong"|IV_dataframe$feedback=="Medium")
k1 <- IV_dataframe[imp_vars, 1]
imp <- which(colnames(master_test) %in% k1)
str(master_test)
master_test <- master_test[, c(1, imp, 19)]

# Also, we'll take only the important variables in master dataset
# Difference between master and master_test is the performance tag variable 
# In master_test : 1 indicate good customers and 0 indicates bad and this dataset is only used for calculating woe value
# In master dataset: 1 indicates bad customer and 0 indicates good customers. Throughout the analysis we will use this dataset 
master <- master[, c(1, imp, 19)]

str(master_test)
str(master)


info_val <- create_infotables(master_test[, -1], y="Performance.Tag.x", ncore = 2)

info_val$Tables

info_val$Summary

woe_table <- data.frame(info_val$Summary)

knitr::kable(head(info_val$Summary))
# Woe Analysis
# The most crucial variables seem to be:
# Avgas.CC.Utilization.in.last.12.months
# No.of.trades.opened.in.last.12.months
# No.of.PL.trades.opened.in.last.12.months
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# Outstanding.Balance
# No.of.times.30.DPD.or.worse.in.last.6.months
# Total.No.of.Trades

# These should appear in the model unless they are not collinear
#############################################################
###########################################################################
# let's see the variables on the plot 

ggplot(data=woe_table, aes(x=reorder(Variable,IV),y=IV)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Woe- Important variables based on Information value analysis") +xlab("Variables")

############################################################################
# Creating a master_woe file where the actual values will be replaced with woe values
master_woe <- master
str(master_woe)

# Let's replace the variables with woe value for model building 
woe_list <- list()

typeof(info_val$Tables)
length(info_val$Tables)

# Extracting only the bins and the woe values of each bin
for(i in 1:length(info_val$Tables)) {
  woe_list[[i]] = cbind(info_val$Tables[[i]][1],info_val$Tables[[i]][4])
}
woe_list
# woe_list is a list of dataframes with each df having the following structure:
woe_list[[1]]
str(woe_list[[1]])

# We now need to replace the values in the master df with the corresponding woe values 
# There are two methods to convert dataset with woe values

# Method 1: By creating a function which takes in the woe_list and a column of master and replaces
# the actual values by woe values

## Method 2. By using a for loop (higher run time) 

## Let's see the function method first. 
woe_function <- function(df, variable) {
  for(i in 1:nrow(df)){
    s <- df[i,1]
    if(s=="NA"){
      replace_by = df[i,2]
      variable[which(is.na(variable))] = replace_by
    } else {
      s <- str_replace_all(s, fixed(" "), "")
      s_list <- strsplit(gsub("\\[|\\]", "", s), split=",")
      n = as.integer(s_list[[1]][[1]])
      m = as.integer(s_list[[1]][[2]])
      
      range <- n:m
      replace_by = df[i,2]
      
      variable[which(variable %in% range)] = replace_by
    }
  }
  return(variable)
}

empty_matrix <- matrix(0, nrow(master_woe), ncol(master_woe))
str(master_woe)
col_replace <- which(!colnames(master_woe) %in% c("Application.ID", "Performance.Tag.x") )
col_replace

for(i in col_replace){
  master_woe[, i] = woe_function(df=woe_list[[i-1]], variable=master_woe[,i])  
}


str(master_woe)

summary(master_woe)

# let's create plots of woe for all the variables 
names <- names(info_val$Tables)
names

# store all the graphs in list
plots <- list()

for (i in 1:length(names)){
  plots[[i]] <- plot_infotables(info_val, names[i])
}

# WoE plot for "No. of times 90 DPD or worse in last 6 months"
plots[[1]]


##################################################
# Default rate
sum(master_woe$Performance.Tag.x)*100/nrow(master_woe) # 4.22% only

# if you see the default rate in master dataset, it shows 4.22% which clearly indicates that the dataset contains 
# very larger number of good customers than the bad ones. Thus, for building the model on such kind of data requires 
# some way of balancing the data.

# But, before balancing the data, let's build the model on unbalance data and see how well it works.

# Additional model : Not required for evaluation purpose
## For unbalanced Data ####################################################################
################################################################################
################ Model building steps - Without balancing the data ########################
## 1.Logistic regression model ############################################################

 # Creating a copy of master_woe

master_woe_1 <- master_woe

# Let's remove applicant id from the data
master_woe <- master_woe[, -1]
 
set.seed(11111)
split_indices <- sample.split(master_woe$Performance.Tag.x, SplitRatio = 0.70)

 # Train Dataset 
 train <- master_woe[split_indices, ]
 sum(train$Performance.Tag.x)/nrow(train)
 
 # Test Dataset
 test <- master_woe[!split_indices, ]
 sum(test$Performance.Tag.x)/nrow(test)

#  Tag= 1 implies default, 0 implies good

 # Logistic model
initial_model = glm(Performance.Tag.x ~ ., data = train, family = "binomial")

# Summary initial model
summary(initial_model)
 
# Run stepwise feature selection to remove the insignificant independent variables 
 
best_model_1 = step(initial_model, direction = "both")
 
summary(best_model_1)

# Note that the coefficients are negative because we have converted to woe values
# such that a higher woe value indicates 'good' customer, thus, p(bad) should go down 
# with increasing woe (or increasing good customers)

# Checking the variance inflation factor to detect the highly correlated independent variable.

  vif(best_model_1)
 
# Let's remove "No.of.times.30.DPD.or.worse.in.last.6.months" variable:
 
 best_model_2 <- glm(formula = Performance.Tag.x ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
                       No.of.times.90.DPD.or.worse.in.last.12.months + 
                       No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                       No.of.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                       Outstanding.Balance, family = "binomial", data = train)
 
# Summary "best_model_2"
 summary(best_model_2)

#  VIF
vif(best_model_2)


 
# Let's remove "No.of.times.90.DPD.or.worse.in.last.6.months" variable:

best_model_3 <- glm(formula = Performance.Tag.x ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                      No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                      No.of.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      Outstanding.Balance, family = "binomial", data = train)


summary(best_model_3)

vif(best_model_3)


# Let's remove "No.of.trades.opened.in.last.12.months" variable:

best_model_4 <- glm(formula = Performance.Tag.x ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                      No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      Outstanding.Balance, family = "binomial", data = train)


summary(best_model_4)

vif(best_model_4)


# Let's remove "No.of.times.90.DPD.or.worse.in.last.12.months " variable:

best_model_5 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      Outstanding.Balance, family = "binomial", data = train)


summary(best_model_5)

vif(best_model_5)



final_model_unbalance<- best_model_5
 
 summary(final_model_unbalance)
 
 
 ###################################################################################################
 ############################# Model Evaluation ####################################################
 str(test)
 
 test$Performance.Tag.x <- as.factor(test$Performance.Tag.x) 
 
 predictions_logit_unbalace <- predict(final_model_unbalance, newdata = test[, -16], type = "response")
 
 summary(predictions_logit_unbalace)# returns p(bad)
 #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 #0.01211 0.01715 0.03571 0.04226 0.06376 0.10500 
 
# ################################################################################

  # Let's find out the optimal probablility cutoff 
 
 summary(test$Performance.Tag.x)
 
 
 perform_fn <- function(cutoff) {
  predicted_response <- as.factor(ifelse(predictions_logit_unbalace >= cutoff, "1", "0"))

  conf <- confusionMatrix(predicted_response,test$Performance.Tag.x, positive = "1")

  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#######################################################################################

#-------------------------------------------------------------------------------------

# Creating cutoff values from 0.014 to 0.11 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.014,0.10 ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
}

# #######################################################################################
# 
# # plotting cutoffs 
# 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.2,length=5),seq(0,0.2,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.08,.70, col=c(1,"darkgreen", 2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))
 

cutoff_logistic_unbalance <- s[which(abs(OUT[,1]-OUT[,2])<0.025)]

cutoff_logistic_unbalance <- cutoff_logistic_unbalance[1]
# the optimal cutoff seems to be somewhere around 4.7 to 4.8%

#---------------------------------------------------------    
# ### Confusion Matrix
predicted_response_unbalance <- as.factor(ifelse(predictions_logit_unbalace >= cutoff_logistic_unbalance, 1, 0))


 conf_logistic_unbalance <- confusionMatrix(predicted_response_unbalance,test$Performance.Tag.x, positive = "1")

 conf_logistic_unbalance

# Accuracy :    0.61  
# Sensitivity : 0.63         
# Specificity : 0.61   

 #####################################################################
  # Let's also see the  KS statistic for test data. 

 actual_test<- ifelse(test$Performance.Tag.x=="1",1,0)
 
 predicted_response_unbalance <- ifelse(predicted_response_unbalance=="1",1,0)
 
 model_unbalance_test_logistic <- prediction(predicted_response_unbalance,actual_test)
 
 model_perf_test_unbalance_logistic <- performance(model_unbalance_test_logistic, "tpr", "fpr")
 
 plot(model_perf_test_unbalance_logistic,col = "red", lab = c(10,10,10))
 
 
 #KS Statistics
 
 ks_table <- attr(model_perf_test_unbalance_logistic, "y.values")[[1]] - (attr(model_perf_test_unbalance_logistic, "x.values")[[1]])
 
 ks_test_unbalance_logistic = max(ks_table)
 
 # Maximum KS
 ks_test_unbalance_logistic
 #0.2427, neither too bad nor good
 ####################################################################
 #
 # Let's also see the KS statistic on master data. 

 predicted_prob_unbalace_master <- predict(final_model_unbalance, newdata = master_woe[, -16], type = "response")
 
 predicted_response_unbalance_master <- ifelse(predicted_prob_unbalace_master>=cutoff_logistic_unbalance,1,0)
 
 
 model_unbalance_logistic_master <- prediction(predicted_response_unbalance_master,master_woe$Performance.Tag.x )
 
 model_perf_unbalance_logistic_master <- performance(model_unbalance_logistic_master, "tpr", "fpr")
 
 plot(model_perf_unbalance_logistic_master,col = "red", lab = c(10,10,10))
 
 
 #KS Statistics
 
 ks_table <- attr(model_perf_unbalance_logistic_master, "y.values")[[1]] - (attr(model_perf_unbalance_logistic_master, "x.values")[[1]])
 
 ks_train_unbalance_logistic_master = max(ks_table)
 
 # Maximum KS
 ks_train_unbalance_logistic_master
 # 0.2620
 
 # Evaluation is poor for logistic model
 ######################################################################################
 # Let's try another model: Random forest
#######################################################################################
############Let's build the randomForrest model on unbalanced Dataset
#######################################################################################
### Random Forest ###

### Data preparation for modelling

RF_data <- master_woe


# Spliting the bank data in 70:30 ratio

RF_data$Performance.Tag.x <- as.factor(ifelse(RF_data$Performance.Tag.x==1,"yes","no"))

set.seed(1010)
split_indices <- sample.split(RF_data$Performance.Tag.x, SplitRatio = 0.70)

train_rf <- RF_data[split_indices, ]

test_rf <- RF_data[!split_indices, ]

#### Modelling ############################### 

library(randomForest)

rf_model <- randomForest( Performance.Tag.x~., data = train_rf, proximity = F, do.trace = T, mtry = 5,ntree=500)

rf_pred <- predict(rf_model, test_rf[, -16], type = "prob")

summary(rf_pred)
perform_fn_rf <- function(cutoff) {
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.x, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(0.01,0.6 ,length=100)

OUT_rf = matrix(0,100,3)


for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
}

#######################################################################################
# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Let's find the cutoff 
cutoff_unbalance_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.045)]
cutoff_unbalance_rf

predicted_response_rf <- as.factor(ifelse(rf_pred[, 2] >= 0.01595960, "yes", "no"))
conf_unbalance_rf <- confusionMatrix(predicted_response_rf, test_rf$Performance.Tag.x, positive = "yes")

conf_unbalance_rf

# Model results: 
# Sensitivity = around 63%
# Specificity = around 60%
# Accuracy =    around 60%

# Model identifies around 63% of defaulters out of total defaults. 

# The model results are decent. Let's see if model improvements are possible

# So, Let's balance the data and see how the model results improve: 

###########################################################################################
###########################################################################################
################ Model building steps - With balancing the data ###########################
############################################################3##############################
####### 1 Logistic regression model 

# Data preparation for model building

master_woe$Performance.Tag.x <- as.factor(master$Performance.Tag.x)

set.seed(1000)
split_indices <- sample.split(master_woe$Performance.Tag.x, SplitRatio = 0.70)

# Train data
train <- master_woe[split_indices, ]

# Sample test data
test <- master_woe[!split_indices, ]
#Since the data is highly imbalanced,It is required to balance the data.SMOTE function is used to balance the same

train_swote <- SMOTE(Performance.Tag.x ~ ., train, perc.over = 100, perc.under=200)

summary(train_swote$Performance.Tag.x)

##################################################################################################
# Modelling : Logistic Regresion

# Tag= 1 implies default, 0 implies good
initial_model = glm(Performance.Tag.x ~ ., data = train_swote, family = "binomial")
summary(initial_model)

# Run stepwise feature selection to remove the insignificant independent variables 
best_model_1 = step(initial_model, direction = "both")
summary(best_model_1)


# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(best_model_1)


# Excluding "No.of.times.90.DPD.or.worse.in.last.12.months"

best_model_2 <- glm(formula = Performance.Tag.x ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
                      No.of.times.30.DPD.or.worse.in.last.6.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                    family = "binomial", data = train_swote)


summary(best_model_2)

vif(best_model_2)


# Excluding "No.of.times.90.DPD.or.worse.in.last.6.months" based on VIF and variable significance 

best_model_3 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                    family = "binomial", data = train_swote)

summary(best_model_3)

vif(best_model_3)

# Final Model for Prediction
final_model_balanced <- best_model_3

summary(final_model_balanced)
########################################################################################################
######################################## Model Evaluation: Master Data #################################
predictions_logit_balance <- predict(final_model_balanced, newdata = test[, -16], type = "response")

summary(predictions_logit_balance)# returns p(bad)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2463  0.2734  0.4552  0.4554  0.5993  0.7032

# ################################################################################
# Let's find out the optimal probablility cutoff 

summary(test$Performance.Tag.x)


perform_fn <- function(cutoff) {
  predicted_response <- as.factor(ifelse(predictions_logit_balance >= cutoff, "1", "0"))
  
  conf <- confusionMatrix(predicted_response,test$Performance.Tag.x, positive = "1")
  
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#######################################################################################

#-------------------------------------------------------------------------------------

# Creating cutoff values from 0.014 to 0.11 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.3,0.7 ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
}

# #######################################################################################
# 
# # plotting cutoffs 
# 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.2,length=5),seq(0,0.2,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.08,.70, col=c(1,"darkgreen", 2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))


cutoff_logistic_balance <- s[which(abs(OUT[,1]-OUT[,2])<0.011)]

cutoff_logistic_balance <- cutoff_logistic_balance[1]
# the optimal cutoff seems to be somewhere around 0.52

#---------------------------------------------------------    
# ### Confusion Matrix
predicted_response_balance <- as.factor(ifelse( predictions_logit_balance >= 0.5222222, 1, 0))


conf_logistic_balance <- confusionMatrix(predicted_response_balance,test$Performance.Tag.x, positive = "1")

conf_logistic_balance

######################################################################################################
# Apply model on master Data: 

str(master_woe)

predictions_logit_balance_master <- predict(final_model_balanced, newdata = master_woe[, -16], type = "response")

summary(predictions_logit_balance_master)# returns p(bad)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2463  0.2734  0.4552  0.4558  0.5993  0.7032 


predicted_master_response <- factor(ifelse(predictions_logit_balance_master>=0.5222222,1,0))

conf_logistic_balance_master <- confusionMatrix(predicted_master_response,master_woe$Performance.Tag.x, positive = "1")

conf_logistic_balance_master

#####################################################################################################
# Let's build Random forest model on the balance data 
#####################################################################################################
#####################################################################################################
### Random Forest ###

RF_train_swote <- train_swote

str(RF_train_swote)

summary(RF_train_swote$Performance.Tag.x)

library(randomForest)

rf_model <- randomForest( Performance.Tag.x~., data = RF_train_swote, proximity = F, do.trace = T, mtry = 5,ntree=500)

# Variables importance 
rf_imp_var <- data.frame(rf_model$importance)
rf_imp_var$MeanDecreaseGini <- round(rf_imp_var$MeanDecreaseGini,1)

# Test

str(test_rf)

rf_pred <- predict(rf_model, test_rf[,-16] , type = "prob")

summary(rf_pred)

perform_fn_rf <- function(cutoff) {
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.x, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(0.01,0.8,length=100)

OUT_rf = matrix(0,100,3)


for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
}

#######################################################################################
# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# Choosing cutoff 
cutoff_rf_balance <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.015)]
cutoff_rf_balance 
# Somewhere around 0.465 
# lets take cutoff 0.46

predicted_rf_balance <- as.factor(ifelse(rf_pred[, 2] >= 0.46, "yes", "no"))

# evaluating the rf model on the test data, which is also imbalanced
summary(test_rf$Performance.Tag.x)
conf_rf_balance <- confusionMatrix(predicted_rf_balance, test_rf$Performance.Tag.x, positive = "yes")

conf_rf_balance


predicted_response_rf <- ifelse(predicted_rf_balance=="yes",1,0)

actual_response_rf <- ifelse(test_rf$Performance.Tag.x=="yes",1,0)

model_score_test_rf <- prediction(predicted_response_rf,actual_response_rf )

model_perf_test_rf <- performance(model_score_test_rf, "tpr", "fpr")

plot(model_perf_test_rf,col = "red", lab = c(10,10,10))

#KS Statistics

ks_table <- attr(model_perf_test_rf, "y.values")[[1]] - (attr(model_perf_test_rf, "x.values")[[1]])

ks = max(ks_table)

ks # approx.0.47, this is much better than the previous models (which were trained on imbalanced data)

##################################################################################################
# evaluate the model on the entire master data: 

master_pred <- predict(rf_model, master_woe[,-16], type = "prob")

# Summarise the probabilities

summary(master_pred)

master_response_rf <- as.factor(ifelse(master_pred[,2] >=0.46 , 1, 0))

conf_rf <- confusionMatrix(master_response_rf,master_woe$Performance.Tag.x, positive = "1")

conf_rf # this looks quite good; all three metric > 70%

################ Let's see the KS ##########################

actual_master_default<- ifelse(master_woe$Performance.Tag.x=="1",1,0)

predicted_master_default <- ifelse(master_response_rf=="1",1,0)

model_score_master <- prediction(predicted_master_default,actual_master_default)

model_perf_master <- performance(model_score_master, "tpr", "fpr")

plot(model_perf_master,col = "red", lab = c(10,10,10))


#KS Statistics

ks_table_master <- attr(model_perf_master, "y.values")[[1]] - (attr(model_perf_master, "x.values")[[1]])

ks_master = max(ks_table_master)

ks_master # 0.4714

## apart from the master dataset, we should also evaluate the model on a randomly chosen 
## subset (say with 20% data)  - this will help us validate the stability further


set.seed(1010101)
sample_indices <- sample.split(master_woe$Performance.Tag.x, SplitRatio = 0.30)

sample_master <- master_woe[sample_indices,]

sample_master_pred <- predict(rf_model, sample_master[,-16], type = "prob")

# Summarise the probabilities

summary(sample_master_pred)

sample_master_response_rf <- as.factor(ifelse(sample_master_pred[,2] >=0.46 , 1, 0))

sample_conf_rf <- confusionMatrix(sample_master_response_rf,sample_master$Performance.Tag.x, positive = "1")

sample_conf_rf # this looks quite good; all three metric > 70%

################ Let's see the KS ##########################

actual_sample_master_default<- ifelse(sample_master$Performance.Tag.x=="1",1,0)

predicted_master_default <- ifelse(sample_master_response_rf=="1",1,0)

model_score_sample_master <- prediction(predicted_master_default,actual_sample_master_default)

model_perf_sample_master <- performance(model_score_sample_master, "tpr", "fpr")

plot(model_perf_sample_master,col = "red", lab = c(10,10,10))


#KS Statistics

ks_table_sample_master <- attr(model_perf_sample_master, "y.values")[[1]] - (attr(model_perf_sample_master, "x.values")[[1]])

ks_master = max(ks_table_sample_master)

ks_master # 0.4730

######################################################################################################

# Application Scorecard on master dataset

# For master Dataset: Let's create a new dataframe which contains "Applicant_id","Actual_response","Predicted_Response" & "Predicted_prob" from the test data

score_data <- master_woe_1[,c(1,17)]

# Lets store probabilities of bad in store_data
score_data$predicted_prob_bad <- master_pred[,2]

# append "probabilies of good" to store_data
score_data$predicted_prob_good <- master_pred[,1]

score_data$odds <- score_data$predicted_prob_good/score_data$predicted_prob_bad

score_data$log_odds <- log(score_data$odds)

#Points to double the odds i.e.20

PDO<-20

#Base Score=400 & odds = 10

BaseScore<-400

Odds<-10

#Calaculating Factor & Offset

Factor=PDO/log(2)

Offset=BaseScore-(Factor*log(Odds))

score_data$score <- Offset+(Factor*score_data$log_odds)

# Rounding to the near integer
score_data$score <- round(score_data$score,0)

###########################################################
#### Now because of 0 odds, log of odds returns Infinity 

score_data <- score_data[-which(score_data$score=="Inf"|score_data$score=="-Inf"),]


# Summary "score"

summary(score_data$score)

#######################################################
# Let's sort the "score_data" with "score"

score_data <- score_data[order(score_data$score,decreasing = T),]

score_data$Performance.Tag.x <- ifelse(score_data$Performance.Tag.x=="1",1,0)

score_data$predicted_default <- ifelse(score_data$predicted_prob_bad>=0.46,1,0)

# Finding the score optimal value
score_data$score[which(score_data$predicted_prob_bad==0.46)]
# optimal value is 338


g<- ggplot(score_data , aes(score,predicted_prob_bad),colour="red")+
  geom_line()+  ggtitle("Score v/s probability of bad")+
  geom_hline(yintercept=0.48, col="red")+geom_vline(xintercept= 338)

g

###########################################
# Scores percentile
quantile(score_data$score,seq(0,1,0.1))
# 30% customers are defaulters if the optimal score is 338

# Score distribution

ggplot(score_data,aes(score))+geom_histogram() +geom_vline(xintercept= 338,col="blue")

######################################################################

## Modelling on Rejected dataset. 
## Now we will calculate the predicted scores and thus the performance (0/1) for the 
## rejected population

## This is a test of model's 'population' stability - it should predict that a high fraction of the rejected
## population is 'bad'

# Replacing variables with woe values: 
summary(rejected) 

###################
# Additional copy of "rejected file"

rejected_1 <- rejected

#rejected<- rejected_1
################### Data Preparation ############################
#################################################################

rejected <- rejected[-which(rejected$Profession==""),]
rejected$Profession <- factor(rejected$Profession)

rejected$Type.of.residence <- factor(rejected$Type.of.residence)

rejected <- rejected[-which(rejected$Education==""),]
rejected$Education <- factor(rejected$Education)
rejected$Gender <- factor(rejected$Gender)
rejected$Marital.Status..at.the.time.of.application. <- factor(rejected$Marital.Status..at.the.time.of.application.)

######################################################################

## Considering only variables based on woe analysis
col_replace <- which(colnames(rejected) %in% colnames(master_woe_1) )
col_replace

rejected <- rejected[,col_replace]

col_replace <- which(!colnames(rejected) %in% c("Application.ID", "Performance.Tag.x") )
col_replace

###########################################################################################

for(i in col_replace){
  rejected[, i] = woe_function(df=woe_list[[i-1]], variable=rejected[,i])  
}

######################################################################
rejected$Performance.Tag.x <- NULL

rejected_prob <- predict(rf_model, rejected[,-1], type = "prob")

#####################################################################################
################# Scoring for rejected applications #################################

rejected$predicted_prob_bad <- rejected_prob[,2]
rejected$predicted_prob_good <- rejected_prob[,1]
rejected$odds <- rejected$predicted_prob_good/rejected$predicted_prob_bad
rejected$log_odds <- log(rejected$odds)

rejected$score <- Offset+(Factor*rejected$log_odds)

# Rounding it to 0 
rejected$score <- round(rejected$score,0)

# Let's create a derived metric "Default" if the score is greater than 337. 

rejected$defaults <- ifelse(rejected$score>338,0,1)

# find the percentage of correctly identified out of rejected population: 

percentage_correctly_identified <- (sum(rejected$defaults)*100)/nrow(rejected)

percentage_correctly_identified

# 70.38 correctly identified, i.e. 70% are predicted to be 'bad'
###########################################################
rejected_prob_plot<- ggplot(rejected , aes(score,predicted_prob_bad),colour="red")+
  geom_line()+  ggtitle("Rejected applicants - Score v/s probability of bad")+
  geom_vline(xintercept= 338,col="blue")

rejected_prob_plot



###################################################################
# let's compare the score of rejected and approved datasets

approved_pop <- data.frame(score = score_data$score,decision = rep("approved",nrow(score_data))) 

rejected_pop <- data.frame(score = rejected$score,decision = rep("rejected",nrow(rejected)))

# Let's bind "approved_pop" and "rejected_pop"

combined_scores <-  rbind(approved_pop, rejected_pop)


# Let see the difference 
ggplot(combined_scores, aes(x=factor(decision), y=score))+geom_boxplot()


###########################################################
#########################################################################################
# Modelling on Demographic variables: 

######################### Logistic Regression on Demographic Data ############################
##############################################################################################
demo <- read.csv("Demogs.csv")
summary(demo)


#############################################################################################
str(demo)
dup <- duplicated(demo$Application.ID)
sum(dup)
demo <- demo[!duplicated(demo$Application.ID), ]
str(demo)

####################################################################

summary(demo$Age)


# Age should always be positive, also typically  you must be at least 18 to apply for a credit card.
# So, less than 18 years of job applicant should be removed from the analysis. 

age_under18 <- demo[which(demo$Age<18),]

# You saw that the data is completely unrealistic such as age under the 18 applicants show that they have a phd degree and married. Because you can see that 
# So, let's remove theses observation 

demo <- demo[which(demo$Age>=18),]

#####-----------------------------------------------------------------------------


# In "Income" variable, you could notice that, 

summary(demo$Income)
quantile(demo$Income,seq(0,1,0.01))

# Outlier Treatment
# Flooring with 4.5 
demo$Income[which(demo$Income<4.5)]<- 4.5

#####-----------------------------------------------------------------------------

# The variables- "Gender","Type.of.residence" and "Marital.Status" do have very less blanks. 
# In "Gender", you will notice that it only contains 1 blank.Even in "Marital.Status" variable, it does have
# only 5 blanks whereas "Type.of.residence" does have 8 blanks. Thus, these values need to be removed from the analysis. 

# Gender
demo <- demo[-which(demo$Gender==""),]
demo$Gender <- factor(demo$Gender)

#Marital_status
demo <- demo[-which(demo$Marital.Status..at.the.time.of.application.==""),]
demo$Marital.Status..at.the.time.of.application. <- factor(demo$Marital.Status..at.the.time.of.application.)

#Type.of.residence
demo <- demo[-which(demo$Type.of.residence==""),]
demo$Type.of.residence <- factor(demo$Type.of.residence)

# Profession

demo <- demo[-which(demo$Profession==""),]
demo$Profession <- factor(demo$Profession)



sapply(demo, function(x) sum(is.na(x)))

#####-----------------------------------------------------------------------------

# let's analyse the missing values:

# Once you checked the missing values in outstanding balance, you could see that the "presence of open home loan" & "Average_ccutilization"
# are also contain NAs. Thus removing 272 observations(which is 0.3% of total observation) do not impact on the models. 
# Let's remove these from the analysis

#####-----------------------------------------------------------------------------

# Even in the "No. of dependents" variable, we could find only 3 observation. Thus, its better to get rid off it. 

No.of.dependents <- which(is.na(demo$No.of.dependents))

demo <- demo[-No.of.dependents,]


# Education 

demo <- demo[-which(demo$Education==""),]
demo$Education <- factor(demo$Education)

###############################################################################################

# Performance.Tag
reject_rows <- which(is.na(demo$Performance.Tag)) 

demo <- demo[-reject_rows, ]

###############################################################
## Woe Analysis ###

IV_demo <- create_infotables(demo[,-1],y="Performance.Tag",ncore = 2)
IV_demo$Tables
IV_demo$Summary

##############################################################

# Let's create a dataframe containing IV values of all the variables 
IV_demo_dataframe <- IV_demo$Summary
str(IV_demo_dataframe)

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for(i in 1:nrow(IV_demo_dataframe)){
  
  if (IV_demo_dataframe$IV[i]<0.02){
    IV_demo_dataframe$feedback[i] = "Useless"
    
  } else if(IV_demo_dataframe$IV[i]>=0.02& IV_demo_dataframe$IV[i]<0.1){
    IV_demo_dataframe$feedback[i] = "Weak"
    
  } else if(IV_demo_dataframe$IV[i]>=0.1 & IV_demo_dataframe$IV[i]<0.3){
    IV_demo_dataframe$feedback[i] = "Medium"
    
  }else if(IV_demo_dataframe$IV[i]>=0.3 & IV_demo_dataframe$IV[i]<0.5){
    IV_demo_dataframe$feedback[i] = "Strong"
    
  }else if(IV_demo_dataframe$IV[i]>0.1 & IV_demo_dataframe$IV[i]<0.3){
    IV_demo_dataframe$feedback[i] = "Suspicious"
  }
}

str(IV_demo_dataframe)
IV_demo_dataframe$Variable <- as.factor(IV_demo_dataframe$Variable)
IV_demo_dataframe$feedback <- as.factor(IV_demo_dataframe$feedback)

###################################################################################
# Plotting the variables on the plot:
ggplot(data=IV_demo_dataframe, aes(x=reorder(Variable,IV),y=IV)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Demograhpic-Important variables - Information Values") +xlab("Variables")

####################################################################################

# Let's Build model on demographic data : Without replacing with woe values. You can try building
# model on woe values for this part as well 

set.seed(10001)

demo <- demo[, -1]

split_indices <- sample.split(demo$Performance.Tag, SplitRatio = 0.70)

demo_train <- demo[split_indices, ]

demo_test <- demo[!split_indices, ]

##################################################################################

# Logistic Regression Model: 

initial_model = glm(Performance.Tag ~ ., data = demo_train, family = "binomial")

# Summary 

summary(initial_model)

# Sepwise 

#best_model_1 = step(initial_model, direction = "both")


best_model_1 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                      No.of.months.in.current.company, family = "binomial", data = demo_train)

summary(best_model_1)

vif(best_model_1)

######################################################

final_model <- best_model_1

###################################################
# Model Evaluation

demo_predictions_logit <- predict(final_model, newdata = demo_test[, -1], type = "response")

# Summary
summary(demo_predictions_logit)

#   Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
# 0.02257 0.03568 0.04143 0.04217 0.04807   0.06856 

###################################################

perform_fn <- function(cutoff){
  
  predicted_response <- as.factor(ifelse(demo_predictions_logit >= cutoff, 1, 0))
  
  conf <- confusionMatrix(demo_test$Performance.Tag,predicted_response, positive = "1")
  
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(0.03,0.05 ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
}

#######################################################################################

# plotting cutoffs

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

########################################################################################
# At cutoff of 0.001
predicted_response <- as.factor(ifelse(demo_predictions_logit >= 0.05, 1, 0))

demo_test$Performance.Tag  <- as.factor(demo_test$Performance.Tag)

conf <- confusionMatrix(demo_test$Performance.Tag,predicted_response, positive = "1")

acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]

acc

sens

spec


#####################################################################
# Very bad model. The results are very poor. Thus, Demographic dataset alone could not provide a good results 
#####################################################################

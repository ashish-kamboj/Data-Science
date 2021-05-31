#############################Telecom Solution###################
################################################################

### Business Understanding:

# Based on the past and current customer information,
# the company has maintained a database containing personal/demographic information,
# the services availed by a customer and the expense information related to each customer.

## AIM:

# The aim is to automate the process of predicting 
# if a customer would telecom or not and to find the factors affecting the telecom. 
# Whether a customer will telecom or not will depend on data from the following three buckets:

# 1. Demographic Information
# 2. Services Availed by the customer
# 3. Overall Expenses

################################################################

### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

# Loading 3 files
churn_data<- read.csv("churn_data.csv", stringsAsFactors = F)
customer_data<- read.csv("customer_data.csv", stringsAsFactors = F)
internet_data<- read.csv("internet_data.csv", stringsAsFactors = F)

str(churn_data)    # 7043 obs of 9 variables including the target variable
str(customer_data) # 7043 obs of 5 variables
str(internet_data) # 7043 obs of 9 variables

# Collate the data together in one single file
length(unique(tolower(churn_data$customerID)))    # 7043, confirming customerID is key 
length(unique(tolower(customer_data$customerID))) # 7043, confirming customerID is key
length(unique(tolower(internet_data$customerID))) # 7043, confirming customerID is key

setdiff(churn_data$customerID,customer_data$customerID) # Identical customerID across these datasets
setdiff(churn_data$customerID,internet_data$customerID) # Identical customerID across these datasets

telecom<- merge(churn_data,customer_data, by="customerID", all = F)
telecom<- merge(telecom,internet_data, by="customerID", all = F)

View(telecom) #master file

################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(telecom) #7043 obs. of 21 variables;

# tenure, MonthlyCharges, TotalCharges are continuous
# SeniorCitizen need to be changed from integer to categorical


# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(telecom, aes(x=PhoneService,fill=Churn))+ geom_bar(), 
          ggplot(telecom, aes(x=Contract,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=PaperlessBilling,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=PaymentMethod,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=gender,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=factor(SeniorCitizen),fill=Churn))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(telecom, aes(x=Partner,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=Dependents,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=MultipleLines,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=InternetService,fill=Churn))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(telecom, aes(x=OnlineSecurity,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=OnlineBackup,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=DeviceProtection,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=TechSupport,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=StreamingTV,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=StreamingMovies,fill=Churn))+ geom_bar()+bar_theme1,
          align = "h") 

#reveals strong contrast for telecom wrt Contract,InternetServices, (not availing) OnlineSecurity and Techsupport and PaymentMethod,
#moderate wrt whether SeniorCitizen, having partner, OnlineBackup and DeviceProtection

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(telecom, aes(tenure))+ geom_histogram(binwidth = 10),
          ggplot(telecom, aes(x="",y=tenure))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(telecom, aes(MonthlyCharges))+ geom_histogram(binwidth = 20),
          ggplot(telecom, aes(x="",y=MonthlyCharges))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(telecom, aes(TotalCharges))+ geom_histogram(),
          ggplot(telecom, aes(x="",y=TotalCharges))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#No outliers in numeric variables

# Boxplots of numeric variables relative to telecom status
plot_grid(ggplot(telecom, aes(x=Churn,y=tenure, fill=Churn))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(telecom, aes(x=Churn,y=MonthlyCharges, fill=Churn))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(telecom, aes(x=Churn,y=TotalCharges, fill=Churn))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Shorter tenure sees more telecom

# Correlation between numeric variables
library(GGally)
ggpairs(telecom[, c("tenure", "MonthlyCharges", "TotalCharges")])

#As expected, tenure and TotalCharges are highly correlated (corr 0.83)


################################################################
### Data Preparation

# De-Duplication
# not needed

# Bringing the variables in the correct format
telecom$SeniorCitizen<- ifelse(telecom$SeniorCitizen==1,"Yes","No")

# Outlier treatment and imputing missing value
#Boxplot showed no outlier, Nevertheless confirming it also with percentiles
sapply(telecom[,c("tenure","MonthlyCharges","TotalCharges")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier


# Missing value
sapply(telecom, function(x) sum(is.na(x))) # shows all 11 NAs are in Totalcharges column
View(subset(telecom, is.na(TotalCharges))) # All 11 NAs in TotalCharges correspond to tenure 0
View(subset(telecom, telecom$tenure==0)) #vice-versa of above is also true

# It means that 11/7043 = 0.001561834 i.e 0.1%, best is to remove these observations from the analysis
telecom <- telecom[!is.na(telecom$TotalCharges),]

################################################################
# Feature standardisation

# Normalising continuous features 
telecom$tenure<- scale(telecom$tenure) # scale used: mean 32.4, sd 24.6
telecom$MonthlyCharges<- scale(telecom$MonthlyCharges) # scale used: mean 64.8, sd 30.1
telecom$TotalCharges<- scale(telecom$TotalCharges) # scale used: mean 2280, sd 2267

# converting target variable telecom from No/Yes character to factorwith levels 0/1 
telecom$Churn<- ifelse(telecom$Churn=="Yes",1,0)

# Checking churn rate of prospect customer

Churn <- sum(telecom$Churn)/nrow(telecom)
Churn # 26.57% churn rate. 

# creating a dataframe of categorical features
telecom_chr<- telecom[,-c(1,2,7,8,9)]

# converting categorical attributes to factor
telecom_fact<- data.frame(sapply(telecom_chr, function(x) factor(x)))
str(telecom_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(telecom_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =telecom_fact))[,-1]))

# For variables having only two levels, verified PhoneService, PaperlessBilling, Partner, Dependents "yes" is 1,
#gender "male" is 1 and SeniorCitizen "1" is 1 

# Final dataset
telecom_final<- cbind(telecom[,c(9,2,7,8)],dummies) 
View(telecom_final) #7032 obs. of  31 variables

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(telecom_final$Churn, SplitRatio = 0.7)

train = telecom_final[indices,]

test = telecom_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Churn ~ ., data = train, family = "binomial")
summary(model_1) #AIC 4150.1....31 coeff..nullDev 5699.5...resDev 4102.1

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

#Excluding Department
model_3<- glm(formula = Churn ~ tenure + MonthlyCharges +  
                PhoneService + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + SeniorCitizen + 
                MultipleLines.xYes + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineBackup.xYes + DeviceProtection.xYes + StreamingTV.xYes + 
                StreamingMovies.xYes, family = "binomial", data = train) 

summary(model_3) 

vif(model_3) 

# MonthlyCharges has high vif and but high significance, also, it came in the above model as well, even the VIF
# has not reduced much,so better is to remove this variable

# Excluding MonthlyCharges 
model_4<- glm(formula = Churn ~ tenure +
                PhoneService + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + SeniorCitizen + 
                MultipleLines.xYes + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineBackup.xYes + DeviceProtection.xYes + StreamingTV.xYes + 
                StreamingMovies.xYes, family = "binomial", data = train)

summary(model_4)

vif(model_4) # cannot exclude any more variable based on vif 
#as most of them have low vif; those with higher vif are very significant and not correlated

#Excluding OnlineBackup.xYes  due to lower significance
model_5<- glm(formula = Churn ~ tenure +
                PhoneService + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + SeniorCitizen + 
                MultipleLines.xYes + InternetService.xFiber.optic + InternetService.xNo + 
                DeviceProtection.xYes + StreamingTV.xYes + 
                StreamingMovies.xYes, family = "binomial", data = train)  

summary(model_5) 

#Excluding DeviceProtection.xYes  due to lower significance
model_6<- glm(formula = Churn ~ tenure +
                PhoneService + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + SeniorCitizen + 
                MultipleLines.xYes + InternetService.xFiber.optic + InternetService.xNo + 
                 StreamingTV.xYes + StreamingMovies.xYes, family = "binomial", data = train)  

summary(model_6)

#Excluding SeniorCitizen  due to lower significance
model_7<- glm(formula = Churn ~ tenure +
                PhoneService + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + 
                MultipleLines.xYes + InternetService.xFiber.optic + InternetService.xNo + 
                StreamingTV.xYes + StreamingMovies.xYes, family = "binomial", data = train)   

summary(model_7) 

#Excluding StreamingTV.xYes due to lower significance with respect to other 10 variables
model_8<- glm(formula = Churn ~ tenure +
                PhoneService + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + 
                MultipleLines.xYes + InternetService.xFiber.optic + InternetService.xNo + 
                StreamingMovies.xYes, family = "binomial", data = train)   

summary(model_8) 

########################################################################
# With 10 significant variables in the model

final_model<- model_8

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                              newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_churn <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_churn <- factor(ifelse(test$Churn==1,"Yes","No"))


table(test_actual_churn,test_pred_churn)


#######################################################################
test_pred_churn <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)

test_conf <- confusionMatrix(test_pred_churn, test_actual_churn, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_churn, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_churn <- factor(ifelse(test_pred >=0.3132, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_churn, test_actual_churn, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_churn <- ifelse(test_cutoff_churn=="Yes",1,0)
test_actual_churn <- ifelse(test_actual_churn=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.53568


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, test_pred, groups = 10)

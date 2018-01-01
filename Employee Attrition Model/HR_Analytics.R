############################################ HR Analytics ##################################

### Business Understanding

# Based on the past and current customer information company XYZ has maintained employee data 
# having personal/demographic information,Education details,Working Hours and JOb related information.

## AIM:

# The aim is to automate the process of predicting employee's attrition (whether an employee will leave a company or not)
# to find the factors leads to the employye leave a company
# Whether an employee will leave a company or not will depend on data from the following three buckets:

# 1. Demographic Information
# 2. Education Details
# 3. Job Details (including working hours, area of expertise/domain)


#############################################################################################

### Data Understanding----
  ## Load Libraries
    library(plyr)
    library(ggplot2)
    library(cowplot)
    library(mltools)
    library(MASS)
    library(car)
    library(GGally)
    library(e1071)
    library(ROCR)
    library(dplyr)
    library(caTools)
    library(caret)


  ## Load  5 data files----

    emp_survey <- read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
    mngr_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
    emp_info <- read.csv("general_data.csv",stringsAsFactors = FALSE)
    in_time <- read.csv("in_time.csv",stringsAsFactors = FALSE)
    out_time <- read.csv("out_time.csv",stringsAsFactors = FALSE)
    
    str(emp_survey)   # 4410 obs. of 4 variables
    str(mngr_survey)  # 4410 obs. of 3 variables
    str(emp_info)     # 4410 obs. of 24 variables
    str(in_time)      # 4410 obs. of 262 variables
    str(out_time)     # 4410 obs. of 262 variables
    
###################################################################################
    
### Data Preparation & Exploratory Data Analysis----
  ## Convert in_time and out_time in to hours
      in_time[2:262] <- sapply(in_time[2:262], function(x) round((as.numeric(format(as.POSIXct(x,format="%m/%d/%Y %H:%M"),"%H")) + as.numeric(format(as.POSIXct(x,format="%m/%d/%Y %H:%M"),"%M"))/60),2))
      out_time[2:262] <- sapply(out_time[2:262], function(x) round((as.numeric(format(as.POSIXct(x,format="%m/%d/%Y %H:%M"),"%H")) + as.numeric(format(as.POSIXct(x,format="%m/%d/%Y %H:%M"),"%M"))/60),2))
    
  ## create a data frame which stores hours worked per day by each employee
      total_time <- out_time
      total_time[2:262] <- out_time[2:262]-in_time[2:262]
    
  ## Calculate average hours worked by each employee
      avg_hrs_worked <- vector('numeric')
      for (i in 1:4410){
        avg_hrs_worked[i] <- round(sum(total_time[i,2:262],na.rm = TRUE)/12,2)
      }
    
      emp_avg_hrs <- cbind(total_time[1],avg_hrs_worked)
      colnames(emp_avg_hrs)[1] <-"EmployeeID"
    
  ## Merge/Collate data together in single file----
    # Merge emp_info,emp_survey,mngr_survey and emp_avg_hrs dataframe in to one by "EmployeeID"
      
      length(unique(emp_info$EmployeeID))     # 4410, confirming EmployeeID is key 
      length(unique(emp_survey$EmployeeID))   # 4410, confirming EmployeeID is key
      length(unique(mngr_survey$EmployeeID))  # 4410, confirming EmployeeID is key
      length(unique(emp_avg_hrs$EmployeeID))  # 4410, confirming EmployeeID is key
      
      employee <- join_all(list(emp_info,emp_survey,mngr_survey,emp_avg_hrs), by ="EmployeeID",type = "inner")
      View(employee) # Master file
    
    
  ## Understanding the structure of the collated file
      str(employee) #4410 obs. of  30 variables
      
      # Continuous Variables: Age, DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentageSalaryHike, TotalWorkingYears, trainingTimesLastYear, YyearsAtCompany, 
      #                       YearsSinceLastPromotion, YearsWithCurrManager, avg_hrs_worked
      # Categorical variables: Attrition, BusinessTravel, Department, Education, EducationField, Gender, JobLevel, JobRole, MaritalStatus, StockOptionLevel, EnvironmentSatisfaction,
      #                        JobSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating
      
      
  ## Barcharts for categorical features with stacked telecom information
      bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                         legend.position="none")
      
      
      plot_grid(ggplot(employee, aes(x=Age,fill=Attrition))+ geom_bar(), 
                ggplot(employee, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=DistanceFromHome,fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(EducationField),fill=Attrition))+ geom_bar()+bar_theme1,align = "h")
      
      #reveals strong contrast for Attrition wrt Age, BusinessTravel, Department
      #moderate wrt DistanceFromHome, Education and EducationFields
      
      
      plot_grid(ggplot(employee, aes(x=Gender,fill=Attrition))+ geom_bar(), 
                ggplot(employee, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=MonthlyIncome,fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1,align = "h") 
      
      #reveals fairly contrast for Attrition wrt MaritalStatus, NumCompaniesWorked and Gender
      #moderate wrt JobLevel, JobRole and MonthlyIncome
      
      
      plot_grid(ggplot(employee, aes(x=PercentSalaryHike,fill=Attrition))+ geom_bar(), 
                ggplot(employee, aes(x=factor(StockOptionLevel),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=TotalWorkingYears,fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(TrainingTimesLastYear),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=YearsAtCompany,fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar()+bar_theme1,align = "h")
      
      #reveals Fairly contrast for Attrition wrt TotalWorkingYears,YearsSinceLastPromotion
      
      
      plot_grid(ggplot(employee, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar(), 
                ggplot(employee, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
                ggplot(employee, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,align = "h")
      
      #reveals strongly contrast for Attrition wrt YearsWithCurrManager, EnvironmentSatisfaction, WorkLifeBalance
      
   ## Histogram and Boxplots for numeric variables 
      box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                        axis.ticks=element_blank(), axis.text=element_blank())
      
      box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                          axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                          legend.position="none")
      
      plot_grid(ggplot(employee, aes(Age))+ geom_histogram(binwidth = 10),
                ggplot(employee, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) # No outlier
      
      plot_grid(ggplot(employee, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
                ggplot(employee, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) # No outlier
      
      # Variables with outliers
      plot_grid(ggplot(employee, aes(avg_hrs_worked))+ geom_histogram(binwidth=2),
                ggplot(employee, aes(x="",y=avg_hrs_worked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) #Outlier found
      
      plot_grid(ggplot(employee, aes(MonthlyIncome))+ geom_histogram(binwidth=20000),
                ggplot(employee, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) #Outlier found
      
      plot_grid(ggplot(employee, aes(TotalWorkingYears))+ geom_histogram(binwidth=2),
                ggplot(employee, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) #Outlier found
      
      plot_grid(ggplot(employee, aes(TrainingTimesLastYear))+ geom_histogram(binwidth=2),
                ggplot(employee, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) #Outlier found
      
      plot_grid(ggplot(employee, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth=2),
                ggplot(employee, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) #Outlier found
      
      plot_grid(ggplot(employee, aes(YearsAtCompany))+ geom_histogram(binwidth=2),
                ggplot(employee, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) #Outlier found
      
      plot_grid(ggplot(employee, aes(YearsWithCurrManager))+ geom_histogram(binwidth=2),
                ggplot(employee, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
                align = "v",ncol = 1) #Outlier found
      
      plot_grid(ggplot(employee, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                  coord_flip() +theme(legend.position="none"),
                ggplot(employee, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
                  coord_flip() + box_theme_y,
                ggplot(employee, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
                  coord_flip() + box_theme_y,
                ggplot(employee, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
                  coord_flip() + box_theme_y,
                align = "v",nrow = 1)
      
      plot_grid(ggplot(employee, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                  coord_flip() +theme(legend.position="none"),
                ggplot(employee, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
                  coord_flip() + box_theme_y,
                ggplot(employee, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
                  coord_flip() + box_theme_y,
                ggplot(employee, aes(x=Attrition,y=avg_hrs_worked, fill=Attrition))+ geom_boxplot(width=0.2)+
                  coord_flip() + box_theme_y,
                align = "v",nrow = 1)
      
      
    ## Correlation between numeric variables
        ggpairs(employee[, c("Age", "DistanceFromHome", "MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","avg_hrs_worked")])
          # YearsAtCompany and YearsWithCurrManager are highly coorealted (.802)
          # YearsAtCompany and YearsSinceLastPromotion are fairly coorealted (.599)
          # YearsSinceLastPromotion and YearsWithCurrManager are fairly coorelated (.519)

        
############################################################################################
### Data Preparation
      
    ## De-Duplication not required
        
    ## Outlier treatment and imputing missing value
        
      # Treating column with values as NA
        
        # Assigining '0' (signifies No Repsonse from employees) to NA values in survey variables
        employee$EnvironmentSatisfaction[which(is.na(employee$EnvironmentSatisfaction))] <- 0
        employee$JobSatisfaction[which(is.na(employee$JobSatisfaction))] <- 0
        employee$WorkLifeBalance[which(is.na(employee$WorkLifeBalance))] <- 0
        
        # Replacing NA values in "NumCompaniesWorked" and "TotalWorkingYears" variables
        employee$NumCompaniesWorked <- ifelse(is.na(employee$NumCompaniesWorked) & (employee$TotalWorkingYears-employee$YearsAtCompany) == 0,1,
                                              ifelse(is.na(employee$NumCompaniesWorked) & (employee$TotalWorkingYears-employee$YearsAtCompany) ==1,0,employee$NumCompaniesWorked))
        
        employee$TotalWorkingYears <- ifelse(is.na(employee$TotalWorkingYears) & (employee$NumCompaniesWorked+employee$YearsAtCompany) == (employee$YearsAtCompany+1),employee$YearsAtCompany,
                                             ifelse(is.na(employee$TotalWorkingYears) & (employee$NumCompaniesWorked+employee$YearsAtCompany) ==(employee$YearsAtCompany),0,employee$TotalWorkingYears))

      
      # Outlier treatment
        
        employee$avg_hrs_worked[which(employee$avg_hrs_worked>213.7784)]<-213.7784
        employee$MonthlyIncome[which(employee$MonthlyIncome>137756.0)]<-137756.0
        employee$TotalWorkingYears[which(employee$TotalWorkingYears>32)]<-32
        employee$TrainingTimesLastYear[which(employee$TrainingTimesLastYear>4)]<-4
        employee$TrainingTimesLastYear[which(employee$TrainingTimesLastYear<1)]<-1
        employee$YearsSinceLastPromotion[which(employee$YearsSinceLastPromotion>9)]<-9
        employee$YearsWithCurrManager[which(employee$YearsWithCurrManager>14)]<-14
        employee$YearsAtCompany[which(employee$YearsAtCompany>24)]<-24
      
      # Missing value
        
        sapply(employee, function(x) sum(is.na(x))) # shows all 14 NAs 
        View(subset(employee, is.na(NumCompaniesWorked))) # 9 NAs in NumCompaniesWorked , 3-Yes and 6-No
        View(subset(employee, is.na(TotalWorkingYears))) # 9 NAs in TotalWorkingYears , 1-Yes and 4-No
      
      # It means that 14/4410 = 0.003174603 i.e 0.3%, best is to remove these observations from the analysis
        employee <- employee[!is.na(employee$NumCompaniesWorked),]
        employee <- employee[!is.na(employee$TotalWorkingYears),]
        View(employee)
        
      # Bringing the variables in the correct format (numeric to character)
        
        employee$Education <- mapvalues(employee$Education, from = c(1,2,3,4,5), to = c("Below College","College","Bachelor","Master","Doctor"))
        employee$EnvironmentSatisfaction <- mapvalues(employee$EnvironmentSatisfaction, from = c(0,1,2,3,4), to = c("No response","Low","Medium","High","Very High"))
        employee$JobSatisfaction <- mapvalues(employee$JobSatisfaction, from = c(0,1,2,3,4), to = c("No response","Low","Medium","High","Very High"))
        employee$WorkLifeBalance <- mapvalues(employee$WorkLifeBalance, from = c(0,1,2,3,4), to = c("No response","Bad","Good","Better","Best"))
        employee$JobInvolvement <- mapvalues(employee$JobInvolvement, from = c(1,2,3,4), to = c("Low","Medium","High","Very High"))
        employee$PerformanceRating <- mapvalues(employee$PerformanceRating, from = c(3,4), to = c("Excellent","Outstanding"))
        
      
#################################################################################
### Feature standardisation
      
    ## Normalising continuous features 
        employee$Age<- scale(employee$Age) # scale used: mean 36.9172, sd 9.135808
        employee$DistanceFromHome<- scale(employee$DistanceFromHome) # scale used: mean 9.195177, sd 8.105035
        employee$MonthlyIncome<- scale(employee$MonthlyIncome) # scale used: mean 61099.25, sd 38050.28
        employee$NumCompaniesWorked<- scale(employee$NumCompaniesWorked) # scale used: mean 2.686306, sd 2.497071
        employee$PercentSalaryHike<- scale(employee$PercentSalaryHike) # scale used: mean 15.20769, sd 3.660675
        employee$TotalWorkingYears<- scale(employee$TotalWorkingYears) # scale used: mean 11.21201, sd 7.59882
        employee$TrainingTimesLastYear<- scale(employee$TrainingTimesLastYear) # scale used: mean 2.66697, sd 0.9000624
        employee$YearsAtCompany<- scale(employee$YearsAtCompany) # scale used: mean 6.886943, sd 5.665793
        employee$YearsSinceLastPromotion<- scale(employee$YearsSinceLastPromotion) # scale used: mean 2.022748, sd 2.713206
        employee$YearsWithCurrManager<- scale(employee$YearsWithCurrManager) # scale used: mean 4.10646, sd 3.504075
        employee$avg_hrs_worked<- scale(employee$avg_hrs_worked) # scale used: mean 151.5999, sd 27.27653
      
      
    ## Converting target variable Attrition from No/Yes character to factorwith levels 0/1 
        employee$Attrition<- ifelse(employee$Attrition=="Yes",1,0)
      
    ## Checking Attrition rate
        attrition <- sum(employee$Attrition)/nrow(employee)
        attrition # 16.08% churn rate. 
      
    ## Creating a dataframe of categorical features
        employee_chr<- employee[,-c(1,2,5,8,9,14,15,16,17,18,20,21,22,23,24,30)]
      
    ## Converting categorical attributes to factor
        employee_fact<- data.frame(sapply(employee_chr, function(x) factor(x)))
        str(employee_fact)
      
    ## Creating dummy variables for factor attributes
        dummies<- data.frame(sapply(employee_fact, function(x) data.frame(model.matrix(~x-1,data =employee_fact))[,-1]))
        View(dummies)
    ## Final dataset
        employee_final<- cbind(employee[,-c(3,4,6:13,16,18,19,25:29)],dummies)
        View(employee_final) #4396 obs. of  60 variables
      
        
#######################################################################################
### splitting the data between train and test
      set.seed(100)
      indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)
      train = employee_final[indices,]
      test = employee_final[!(indices),]
  
#######################################################################################
### Logistic Regression: 
      
    ## Initial model
        model_1 = glm(Attrition ~ ., data = train, family = "binomial")
        summary(model_1) #AIC 2135.4 | nullDev 2714.6 | resDev 2017.4
      
    ## Stepwise selection
        model_2<- stepAIC(model_1, direction="both")
        summary(model_2)
      
    ## Removing multicollinearity through VIF check
        vif(model_2)
        # no variable to remove based on the vif, there are variables with high vif but p-value is very low, can't remove athis point of time
        # lets take a look at p-value in-order to remove the insignificant variables
      
        # Excluding "WorkLifeBalance.No.response", "JobInvolvement.xLow" and "JobRole.xManufacturing.Director"
          model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                       TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                       avg_hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                       Department.xResearch...Development + Department.xSales + 
                       Education.xCollege + Education.xDoctor + JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                       JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                       MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                       JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                       WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium, family = "binomial", 
                     data = train)
      
          summary(model_3)
      
        # Excluding "Education.xCollege", "Education.xDoctor"
          model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                       TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                       avg_hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                       Department.xResearch...Development + Department.xSales + JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                       JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                       MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                       JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                       WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium, family = "binomial", 
                     data = train)
      
          summary(model_4)
          vif(model_4)
      
        # Excluding "StockOptionLevel.x1"
          model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                       TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                       avg_hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                       Department.xResearch...Development + Department.xSales + JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                       JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                       MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                       JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                       WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xMedium, family = "binomial", 
                     data = train)
      
          summary(model_5)
          vif(model_5)
      
        # Excluding "JobInvolvement.xMedium", "JobLevel.x2" and "TrainingTimesLastYear"
          model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                       avg_hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                       Department.xResearch...Development + Department.xSales + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                       JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                       MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                       JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                       WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                     data = train)
      
          summary(model_6)
          vif(model_6)
      
        # Excluding "JobRole.xLaboratory.Technician  "
          model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                       avg_hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                       Department.xResearch...Development + Department.xSales + JobRole.xResearch.Director + 
                       JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                       MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                       JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                       WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                     data = train)
      
          summary(model_7)
          vif(model_7)
      
        # Excluding "JobRole.xResearch.Scientist" and "JobRole.xSales.Executive"
          model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                       avg_hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                       Department.xResearch...Development + Department.xSales + JobRole.xResearch.Director + 
                       MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                       JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                       WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                     data = train)
      
          summary(model_8)
      
        # Excluding "JobRole.xResearch.Director" and "JobSatisfaction.xLow'
          model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                       avg_hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                       Department.xResearch...Development + Department.xSales + 
                       MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                       WorkLifeBalance.xBetter + WorkLifeBalance.xGood, family = "binomial", 
                     data = train)
      
          summary(model_9)
          # Final variables in Model - Age, NumCompaniesWorked, TotalWorkingYears, YearsSinceLastPromotion, YearsWithCurrManager, avg_hrs_worked, BusinessTravel.Travel_Frequently 
          # BusinessTravel.Travel_Rarely, Department.Research & Development, Department.Sales, MaritalStatus.xSingle, EnvironmentSatisfaction.Low, JobSatisfaction.Very.High 
          # WorkLifeBalance.Best, WorkLifeBalance.Better + WorkLifeBalance.Good
      
#####################################################################################
### With 10 significant variables in the model
      
      final_model<- model_9
      
####################################################################################
### Model Evaluation
      
### Test Data ####
      
    ## Predicted probabilities of Attrition for test data
      
        test_pred = predict(final_model, type = "response", newdata = test[,-2])
      
    ## Summary 
        summary(test_pred)
        test$prob <- test_pred
        View(test)
      
    ## Let's use the probability cutoff of 50%.
          test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
          test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
          table(test_actual_attrition,test_pred_attrition)
          
    ## Let's use the probability cutoff of 40%.
          test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
          test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
          table(test_actual_attrition,test_pred_attrition)
          
    ## Let's use the probability cutoff of 30%.
          test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
          test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
          table(test_actual_attrition,test_pred_attrition)
          
    ## Let's use the probability cutoff of 20%.
          test_pred_attrition <- factor(ifelse(test_pred >= 0.20, "Yes", "No"))
          test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
          table(test_actual_attrition,test_pred_attrition)
          
          test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
          test_conf
          
          
#########################################################################################
    ## Let's Choose the cutoff value. 
          
    ## Let's find out the optimal probalility cutoff 
          
          perform_fn <- function(cutoff) 
          {
            predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
            conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
            acc <- conf$overall[1]
            sens <- conf$byClass[1]
            spec <- conf$byClass[2]
            out <- t(as.matrix(c(sens, spec, acc))) 
            colnames(out) <- c("sensitivity", "specificity", "accuracy")
            return(out)
          }
          
    ## Summary of test probability
          
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
          
          
          cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
          cutoff # 0.169596
          
    ## Take the cutoff value of 0.169596 for final model
          
          test_cutoff_attrition <- factor(ifelse(test_pred >=0.169596, "Yes", "No"))
          View(as.data.frame(test_cutoff_attrition))
          
          conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
          
          acc <- conf_final$overall[1]
          acc # 0.7354056 or 73.5%
          
          sens <- conf_final$byClass[1]
          sens # 0.745283 or 74.5%
          
          spec <- conf_final$byClass[2]
          spec # 0.733514 or 73.4%
          
          View(test)
          
##################################################################################################
### KS -statistic - Test Data ######
          
      test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
      test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)
    
    ## on testing  data
      pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
      performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
      ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
    
      max(ks_table_test) # 0.478797
 
####################################################################
### Lift & Gain Chart 
    
  ## plotting the lift chart
    
      lift <- function(labels , predicted_prob,groups=10) {
      if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
      if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
      helper = data.frame(cbind(labels , predicted_prob))
      helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
      gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(labels ), funs(total = n(),totalresp=sum(., na.rm = TRUE))) %>%
      mutate(Cumresp = cumsum(totalresp),Gain=Cumresp/sum(totalresp)*100,Cumlift=Gain/(bucket*(100/groups))) 
      return(gaintable)
    }
    
    Churn_decile = lift(test_actual_attrition, test_pred, groups = 10)
    Churn_decile
    
    
          
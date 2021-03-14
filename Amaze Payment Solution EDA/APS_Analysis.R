
### Load Libraries
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(ggthemes)

### Load Data
  credit_data <- read.csv("credit_data.csv", stringsAsFactors = F)
  cycles <- read.csv("cycles.csv", stringsAsFactors = F)
  users_data <- read.csv("users_data.csv", stringsAsFactors = F)
  transactions_data <- read.csv("transactions_data.csv", stringsAsFactors = F)
  settlements_data <- read.csv("settlements_data.csv", stringsAsFactors = F)
  failure_events_data <- read.csv("failure_events_data.csv", stringsAsFactors = F)
  
  str(credit_data)  #18468 obs. of  3 variables
  str(cycles) #23 obs. of  3 variables
  str(users_data) #16050 obs. of  5 variables
  str(transactions_data) #73731 obs. of  8 variables
  str(settlements_data) #30683 obs. of  10 variables
  str(failure_events_data)  #17993 obs. of  10 variables
  
  
  
###############################################################################################################################################
#                                                        :: Data Preparation ::
###############################################################################################################################################
  
### Checking for NA values
  sapply(credit_data, function(x) sum(is.na(x)))  #No missing or NA values
  sapply(failure_events_data, function(x) sum(is.na(x)))  #No missing or NA values
  sapply(settlements_data, function(x) sum(is.na(x))) #8288 NA values in days_delayed column
  sapply(transactions_data, function(x) sum(is.na(x)))  #No missing or NA values
  sapply(users_data, function(x) sum(is.na(x))) #3105 NA values in city_id column
  
  
### Transactional Data Preparation::
  
  ## Converting merchant_id to factor
    transactions_data$merchant_id <- as.factor(transactions_data$merchant_id)
    
  ## Extracting combination of Year and Month
    transactions_data$Year_Month <- substr(transactions_data$created_at,1,7)

  ## Extracting Hours from the created date
    transactions_data$Hour <- strftime(transactions_data$created_at, format = "%H")
    
  ## Extracting Date from the created date ( as it includes time also)
    transactions_data$Date <- substr(transactions_data$created_at,1,10)
    transactions_data$Date <- as.Date(transactions_data$Date) #Converting Date column in to date data type
    

### Settlement Data Preparation::
    
  ## Missing values treatement for days_delayed column
    settlements_data$days_delayed[settlements_data$settlement_status == "onetime_settlement"] <- 0
    settlements_data$days_delayed[settlements_data$settlement_status == "bill_pending"] <- -1
    
  ## Extracted combination of Year and Month
    settlements_data$Year_Month <- ifelse(settlements_data$settlement_status != "onetime_settlement", 
                                    substr(settlements_data$bill_created_at,1,7), substr(settlements_data$settled_at,1,7))
    
  ## Extracted Hours from the settled date
    settlements_data$Hour <- ifelse(settlements_data$settlement_status != "onetime_settlement", 
                                    substr(settlements_data$bill_created_at,12,13), substr(settlements_data$settled_at,12,13))
    
  ## Replacing the #NA in "cycle_id" column with proper cycle_id
    settlements_data$cycle_id[settlements_data$cycle_id == "#N/A"] <- 11
    settlements_data$cycle_id <- as.factor(as.numeric(settlements_data$cycle_id))  #Converting cycle_id to factor
    
    
    
### Failure Events Data Preparation::
  
  ## Extracted combination of Year and Month
    failure_events_data$Year_Month <- substr(failure_events_data$created_at,1,7)
  
  ## Extracted Hours from the created date
    failure_events_data$Hour <- strftime(failure_events_data$created_at, format = "%H")
  
  ## Merging failure event data user_id with city_id (in user_data)
    failure_events_data <- merge(x = failure_events_data, y = users_data[ , c("user_id", "city_id")], by = "user_id", all.x=TRUE)
    
  ## Replacing NA values in "city_id" with 0
    failure_events_data$city_id[which(is.na(failure_events_data$city_id))] <- 0
    
  ## Converting city_Id into factor
    failure_events_data$city_id <- as.factor(failure_events_data$city_id)
    
  
### User data Preparation::
  
  ## Replacing blank/empty referer to "unknown"
    users_data$referrer[users_data$referrer == ""] <- "unknown"
  
  
  
  
###############################################################################################################################################
#                                                     :: Exploratory Data Analysis ::
###############################################################################################################################################

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
### Credit data analysis:
  
  dim(credit_data) # 18468
  length(unique(credit_data$user_id)) #14285
  length(unique(credit_data$cycle_id)) #20
  
  #For Credit data we can't analyze much
  #Some of the combinations of "userid" and "cycle_id" not present in credit data but present in settlements data, 
  #So I've assumed that the credit limit for that cycle (for that user) was same as the previous cycle_id.

  
    
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
### cycle data analysis::
  #No analysis is required, will be helpful for analysing further dataset inordr to get cycle_id based on the date
  #This data is not of much importance for analysis
 
  summary(cycles)
  View(cycles)
  
   
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
### Users data analysis::
  # Contains the information about user like name, email_id, city etc.
  unique(users_data$referrer)
  
  ## Frequency of Users Referred by Referrer
    users_data %>% group_by(referrer) %>% summarise(count=n()) %>% data.frame() %>% ggplot(aes(x=referrer, y=count)) + 
    geom_bar(stat = "identity", fill="lightslateblue") + theme_hc(base_size = 18, base_family = "sans") + geom_text(aes(label=count), vjust=-0.3, size=3.5) + 
    labs(x="Referrer",y="Frequency of Users Referred") + ggtitle("Number Of Users Referred by Referrer") + theme(plot.title = element_text(hjust = 0.5))
    
    # "a" had referred the most of the users
  

      
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
### Transactional Data analysis::
  dim(transactions_data)
  length(unique(transactions_data$merchant_id)) #18
  
  ## Number of Transactions per merchant ID
    transactions_data %>% group_by(merchant_id) %>% summarise(count=n()) %>% data.frame() %>% ggplot(aes(x=merchant_id, y=count)) + 
    geom_bar(stat = "identity", fill="goldenrod1") + theme_hc(base_size = 18, base_family = "sans") + geom_text(aes(label=count), vjust=-0.3, size=3.5) + 
    labs(x="Merchant ID's",y="Number of Transactions") + ggtitle("Number Of Transaction for Merchant ID") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Monthly transaction amount
    transactions_data %>% group_by(Year_Month) %>% summarise(total_amount=sum(transaction_amount)) %>% data.frame() %>% 
    ggplot(aes(x=Year_Month, y=total_amount/100000)) + geom_bar(stat = "identity", fill="steelblue") + theme_hc(base_size = 18, base_family = "sans") + 
    labs(x="Months",y="Transaction Amount in Lacs") + ggtitle("Monthly Transaction Amount") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Hourly Transaction amount detail
    transactions_data %>% group_by(Hour) %>% summarise(total_amount=sum(transaction_amount)) %>% data.frame() %>% 
    ggplot(aes(x=Hour, y=total_amount/100000)) + geom_bar(stat = "identity", fill="darkred") + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Hour",y="Transaction Amount in Lacs") + ggtitle("Hourly Transaction Amount") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Hourly Transactions
    transactions_data %>% group_by(Hour) %>% summarise(count=n()) %>% data.frame() %>% ggplot(aes(x=Hour, y=count)) + 
    geom_bar(stat = "identity", fill="purple") + theme_hc(base_size = 18, base_family = "sans") + geom_text(aes(label=count), vjust=-0.3, size=3.5) +
    labs(x="Hour",y="Number Of Transactions") + ggtitle("Hourly Transactions") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Aggregated daily transactional amount
    transactions_data %>% group_by(Date) %>% summarise(total_amount=sum(transaction_amount)) %>% data.frame() %>% 
    ggplot(aes(x=Date, y=total_amount/100000)) + geom_line(size = 0.5,color = "steelblue") + geom_point() + theme_bw() + 
    scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + theme(axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5)) + 
    ggtitle("Daily Transactional Amount") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Limits of transaction amount by different merchant ID's
    ggplot(transactions_data, aes(x=merchant_id,y=transaction_amount/100000, fill=merchant_id)) + geom_boxplot() + theme_light() +
    theme(legend.position = 'none') + labs(x="Merchant ID's",y="Transaction Amount in Lacs")
    

        
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Settlement data analysis::
  View(settlements_data)
  sort(unique(settlements_data$cycle_id))
  
  ## Counts of different settlement status
    settlements_data %>% group_by(settlement_status) %>% summarise(count=n()) %>% data.frame() %>% ggplot(aes(x=settlement_status, y=count)) + 
    geom_bar(stat = "identity", width = 0.5, fill="yellow") + geom_text(aes(label=count), vjust=-0.3, size=5) + theme_hc(base_size = 18, base_family = "sans") + 
    labs(x="Settlement Status",y="Number Of Settlements") + ggtitle("Counts of Settlemnt Status") + theme(plot.title = element_text(hjust = 0.5))  
    
    
  ## Total settlement amount of different settlement status
    settlements_data %>% group_by(settlement_status) %>% summarise(total_amount=sum(settlement_amount)) %>% data.frame() %>% 
    ggplot(aes(x=settlement_status, y=total_amount/100000)) + geom_bar(stat = "identity", width = 0.7, fill="tan1") + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Settlement Status",y="Settlement Amount in Lacs") + ggtitle("Settlement Amount Of Different Settlement Status") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Monthly aggregated settlement amount
    settlements_data %>% group_by(Year_Month) %>% summarise(total_amount=sum(settlement_amount)) %>% data.frame() %>% 
    ggplot(aes(x=Year_Month, y=total_amount/100000)) + geom_bar(stat = "identity", width = 0.7, fill="seagreen4") + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Months",y="Settlement Amount in Lacs") + ggtitle("Monthly Settlement Amount") + theme(plot.title = element_text(hjust = 0.5))
    #September month settlement amount detail is less because it didn't contain all the settlement (it included only the onetime_settlemnt
    #which had happened befor 15-Sep, so we can ignore the sep month amount)
    
    
  ## Aggregated Settlement amount by Cycle Id
    settlements_data %>% group_by(cycle_id) %>% summarise(total_amount=sum(settlement_amount)) %>% data.frame() %>% 
    ggplot(aes(x=cycle_id, y=total_amount/100000)) + geom_bar(stat = "identity", width = 0.7, fill="maroon4") + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Cycle ID",y="Settlement Amount in Lacs") + ggtitle("Settlement Amount By Cycle ID") + theme(plot.title = element_text(hjust = 0.5))
    #cycle 21 settlement amount detail is less because it didn't contain all the settlement (it included only the onetime_settlemnt
    #so we can ignore the cycle 21 amount)
  
    
  ## Aggregated monthly settlement amount by different settlement status
    settlements_data %>% group_by(Year_Month,settlement_status) %>% summarise(total_amount=sum(settlement_amount)) %>% data.frame() %>% 
    ggplot(aes(x=Year_Month, y=total_amount/100000, fill=settlement_status)) + geom_bar(stat = "identity", width = 0.6) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Months",y="Settlement Amount in Lacs") + ggtitle("Monthly Settlement Amount By Status Type") + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual("Settlement Status: ", values = c("bill_pending" = "red1", "bill_settled" = "green4", "onetime_settlement" = "navyblue"))
    #September month settlement amount detail is less because it didn't contain all the settlement (it included only the onetime_settlemnt
    #which had happened befor 15-Sep, so we can ignore the sep month amount)
    
    
  ## Aggregated settlement amount by cycle_id for different settlement status
    settlements_data %>% group_by(cycle_id,settlement_status) %>% summarise(total_amount=sum(settlement_amount)) %>% data.frame() %>% 
    ggplot(aes(x=cycle_id, y=total_amount/100000, fill=settlement_status)) + geom_bar(stat = "identity", width = 0.6) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Cycle ID",y="Settlement Amount in Lacs") + ggtitle("Settlement Amount By Cycle ID") + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual("Settlement Status: ", values = c("bill_pending" = "orangered", "bill_settled" = "palegreen4", "onetime_settlement" = "yellow1"))
    #cycle 21 settlement amount detail is less because it didn't contain all the settlement (it included only the onetime_settlemnt
    #so we can ignore the cycle 21 amount)
    
     
  ## Monthly numbers of amount settlement required
    settlements_data %>% group_by(Year_Month,settlement_status) %>% summarise(count=n()) %>% data.frame() %>% 
    ggplot(aes(x=Year_Month, y=count, fill=settlement_status)) + geom_bar(stat = "identity", width = 0.7) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Months",y="Frequency of Amount Settlement") + ggtitle("Monthly Settlement Amount Counts") + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual("Settlement Status: ", values = c("bill_pending" = "red1", "bill_settled" = "green4", "onetime_settlement" = "navyblue"))
    #September month settlement amount detail is less because it didn't contain all the settlement (it included only the onetime_settlemnt)
    
    
  ## Settlement Amount Counts For Each Cyle
    settlements_data %>% group_by(cycle_id,settlement_status) %>% summarise(count=n()) %>% data.frame() %>% 
    ggplot(aes(x=cycle_id, y=count, fill=settlement_status)) + geom_bar(stat = "identity", width = 0.7) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Cycle ID",y="Frequency of Amount Settlement") + ggtitle("Settlement Amount Counts For Each Cyle") + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual("Settlement Status: ", values = c("bill_pending" = "red1", "bill_settled" = "green4", "onetime_settlement" = "navyblue"))
    #cycle 21 settlement amount detail is less because it didn't contain all the settlement (it included only the onetime_settlemnt)
    
   
  ## Hourly Settlement Amount Frequency
    settlements_data %>% group_by(Hour,settlement_status) %>% summarise(count=n()) %>% data.frame() %>% 
    ggplot(aes(x=Hour, y=count, fill=settlement_status)) + geom_bar(stat = "identity", width = 0.7) + theme_hc(base_size = 18, base_family = "sans") + 
    labs(x="Hours",y="Frequency of Amount Settlement") + ggtitle("Hourly Settlement Amount Frequency") + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual("Settlement Status: ", values = c("bill_pending" = "orangered", "bill_settled" = "palegreen4", "onetime_settlement" = "goldenrod1"))
    
    
  ## Monthly Average Days Delayed For payment for 'bill_settled' Status
    settlements_data %>% filter(settlement_status=="bill_settled") %>% group_by(Year_Month) %>% summarise(mean_days_delayed=mean(days_delayed)) %>% 
    data.frame() %>% ggplot(aes(x=Year_Month, y=mean_days_delayed)) + geom_bar(stat = "identity", width = 0.5, fill="turquoise3") + theme_hc(base_size = 18, base_family = "sans") + 
    labs(x="Months",y="Average days delayed for settlement") + ggtitle("Monthly Average Days Delayed For 'bill_settled' Status") + theme(plot.title = element_text(hjust = 0.5))

    
  ## Average Days Delayed for 'bill_settled' settlement Status in each cycle
    settlements_data %>% filter(settlement_status=="bill_settled") %>% group_by(cycle_id) %>% summarise(mean_days_delayed=mean(days_delayed)) %>% 
    data.frame() %>% ggplot(aes(x=cycle_id, y=mean_days_delayed)) + geom_bar(stat = "identity", width = 0.5, fill="chocolate4") + theme_hc(base_size = 18, base_family = "sans") + 
    labs(x="Cycle ID",y="Average days delayed for settlement") + ggtitle("Average Days Delayed For 'bill_settled' Status") + theme(plot.title = element_text(hjust = 0.5))
    
  
    
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
### Failure event data analysis::

  ## Number of time a particular error appears
    failure_events_data %>% group_by(failure_type) %>% summarise(count=n()) %>% data.frame() %>% ggplot(aes(x=failure_type, y=count)) + 
    geom_bar(stat = "identity", fill="coral") + geom_text(aes(label=count), vjust=-0.3, size=5) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Failure Type",y="Frequency of Failure") + ggtitle("Frequency of Failure Types") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Aggregated Transactional amount per error type
    failure_events_data %>% group_by(failure_type) %>% summarise(total_amount=sum(amount_in_paise)) %>% data.frame() %>% 
    ggplot(aes(x=failure_type, y=total_amount/100000)) + geom_bar(stat = "identity", fill="blue") + theme_hc(base_size = 18, base_family = "sans") + 
    labs(x="Failure Type",y="Amount in Lacs") + ggtitle("Total Amount Involved In Each Failure") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Count of errors month wise
    failure_events_data %>% group_by(Year_Month) %>% summarise(count=n()) %>% data.frame() %>% ggplot(aes(x=Year_Month, y=count)) + 
    geom_bar(stat = "identity", fill="darkcyan") + geom_text(aes(label=count), vjust=-0.3, size=5) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Months",y="Frequency of Failure") + ggtitle("Monthly Frequency Of Total Failures") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Monthly Distribution of different failure type 
    failure_events_data %>% group_by(Year_Month,failure_type) %>% summarise(count=n()) %>% data.frame() %>% 
    ggplot(aes(x=Year_Month, y=count, fill=failure_type)) + geom_bar(stat = "identity") + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Months",y="Frequency of Failure") + ggtitle("Monthly Frequency Of Each Failure") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Aggregated monthly frequency of users associated with failed transactions
    failure_events_data %>% group_by(Year_Month) %>% summarise(count=length(unique(user_id))) %>% data.frame() %>% ggplot(aes(x=Year_Month, y=count)) + 
    geom_bar(stat = "identity", fill="darkorange") + geom_text(aes(label=count), vjust=-0.3, size=5) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Months",y="Frequency of Unique Users") + ggtitle("Monthly Unique Users Associated With Failed Transactions") + theme(plot.title = element_text(hjust = 0.5))

    
  ## Hourly count of errors
    failure_events_data %>% group_by(Hour) %>% summarise(count=n()) %>% data.frame() %>% ggplot(aes(x=Hour, y=count)) + 
    geom_bar(stat = "identity", fill="darkslategray4")  + geom_text(aes(label=count), vjust=-0.3, size=5) + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Hours",y="Frequency of Failure") + ggtitle("Failed Transactions By Hours") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Hourly distribution of errors
    failure_events_data %>% group_by(Hour, failure_type) %>% summarise(count=n()) %>% data.frame() %>% 
    ggplot(aes(x=Hour, y=count, fill=failure_type)) + geom_bar(stat = "identity") + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Hours",y="Frequency of Failure") + ggtitle("Frequency Of Failure Types By Hours") + theme(plot.title = element_text(hjust = 0.5))
    
    
  ## Aggregated failed transactional Amount by each hours
    failure_events_data %>% group_by(Hour) %>% summarise(total_amount=sum(amount_in_paise)) %>% data.frame() %>% 
    ggplot(aes(x=Hour, y=total_amount/100000)) + geom_bar(stat = "identity", fill="deeppink3") + theme_hc(base_size = 18, base_family = "sans") +
    labs(x="Hours",y="Amount in Lacs") + ggtitle("Hourly Total Transactional Failure Amount") + theme(plot.title = element_text(hjust = 0.5))
    

  ## Top 10 Cities from where most of the transactions failed
    failure_events_data %>% group_by(city_id) %>% summarise(count=n()) %>% data.frame() %>% arrange(desc(count)) %>% head(10) %>% 
    ggplot(aes(x=city_id, y=count)) + geom_bar(stat = "identity", fill="yellowgreen") + theme_hc(base_size = 18, base_family = "sans") +
    geom_text(aes(label=count), vjust=-0.3, size=5) + labs(x="City ID",y="Frequency of Failures") + 
    ggtitle("City With Most Failed Transactions") + theme(plot.title = element_text(hjust = 0.5))
    
  
  
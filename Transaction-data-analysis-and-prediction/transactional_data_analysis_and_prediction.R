############################################################################################################################################################
#                                                         Transactional Data Analysis and Prediction
############################################################################################################################################################

### Load Libraries----
  library(dplyr)
  library(ggpubr)
  library(forecast)
  library(tseries)


### Load Data----

  jan_2017 <- read.csv("Jan 2017.csv", stringsAsFactors = FALSE)
  feb_2017 <- read.csv("Feb 2017.csv", stringsAsFactors = FALSE)
  mar_2017 <- read.csv("April 2017.csv", stringsAsFactors = FALSE)
  apr_2017 <- read.csv("May 2017.csv", stringsAsFactors = FALSE)
  may_2017 <- read.csv("June 2017.csv", stringsAsFactors = FALSE)
  jun_2017 <- read.csv("Aug 2017.csv", stringsAsFactors = FALSE)
  jul_2017 <- read.csv("Sept 2017.csv", stringsAsFactors = FALSE)
  aug_2017 <- read.csv("Oct 2017.csv", stringsAsFactors = FALSE)
  sep_2017 <- read.csv("Nov 2017.csv", stringsAsFactors = FALSE)
  oct_2017 <- read.csv("Dec 2017.csv", stringsAsFactors = FALSE)
  
  View(mar_2017)
  
###############################################################################################################################################
#                                                         Data Preparation and EDA 
###############################################################################################################################################
  
## Making the negation of %in%
  `%nin%` = Negate(`%in%`)
  
## Creating Credit Card fee column in Jan_2017 dataset
  jan_2017$Credit.card.fee <- 0

    
## Creating a function
  monthly_data <- function(dataset){
  
    ## Column Rename
    colnames(dataset)[colnames(dataset) == "Transaction.otes"] <- "Transaction.Notes"
    colnames(dataset)[colnames(dataset) == "Transaction.umber"] <- "Transaction.Number"
  
  
    ## Number of Unique users
      print(paste0("Unique Users : ",length(unique(dataset$User.ID))))
  
  
    ## Number of Unique operators
      print(paste0("Unique Operators : ",length(unique(dataset$Operator_code))))
  
  
    ## Checking for the columns having 'NA' value
      print("Removing columns having Large Number of NA values")
      print(sapply(dataset, function(x) sum(is.na(x))))
    
      # Removing column with NA values
      dataset <- subset(dataset, select=-c(Mode.1,Mode.1.Channel,Mode.2,Mode.2.Channel,Discounted.Amount,Round.of.Amount))
    
    
    ## Checking for the columns having blank value
      print("Removing columns having Large Number of Blank values")
      print(sapply(dataset, function(x) length(which(x==""))))
    
      # Removing column with Blank values
        dataset <- subset(dataset, select=-c(Reciept.Code, Promo.Code, Promo.Campaign, TO.bank, From.Bank, Token, Transaction.Notes,Amount.Type))
  
  
    ## Checking for the duplicate records in the dataset
      print(paste0("Unique records in dataset: ",length(unique(dataset$Transaction.Number))))
  
    
    ## Converting the create_date column to teh Date format
      dataset$created_date <- as.Date(dataset$created_date, "%d/%m/%Y")
      print(paste0("Class of create_date column: ",class(dataset$created_date)))
   
     
    ## Extracting out the Hour from the created time
      dataset$Hour <- as.numeric(substr(dataset$created_time, start = 1, stop = 2))
   
     
    ## Deriving Time Slots (i.e. Morning, Day Time, Evening and Late Night) from the "Hour" column
      dataset$Time_Slot[dataset$Hour <= 3] <- c("Late Night")
      dataset$Time_Slot[((dataset$Hour >= 4) & (dataset$Hour <= 10))] <- c("Morning")
      dataset$Time_Slot[((dataset$Hour >= 11) & (dataset$Hour <= 16))] <- c("Day Time")
      dataset$Time_Slot[((dataset$Hour >= 17) & (dataset$Hour <= 21))] <- c("Evening")
      dataset$Time_Slot[which(is.na(dataset$Time_Slot==T))] <- c("Late Night")
   
    
    ## Replacing Credit Card fee NA values to 0
      dataset$Credit.card.fee <- ifelse(is.na(dataset$Credit.card.fee), 0, dataset$Credit.card.fee)
    
    
    ## Aggreagting the number of transactions by Time_slot
      transactions_in_time_slot <- dataset %>% group_by(Time_Slot) %>% summarize(n()) %>% as.data.frame()
      colnames(transactions_in_time_slot)[2] <- "counts"   #Re-naming the column
    
      #Plot
      p1 <- ggplot(transactions_in_time_slot, aes(x=Time_Slot, y=counts, fill=Time_Slot)) + geom_bar(stat = "identity", width = 0.5)
 
    
    ## Aggreagting the number of transactions per day
      transactions_per_day <- dataset %>% group_by(created_date) %>% summarize(n()) %>% as.data.frame()
      colnames(transactions_per_day)[2] <- "counts"   #Re-naming the column
    
      #Plot
      p2 <- ggplot(transactions_per_day, aes(x=created_date, y=counts)) + geom_line() + geom_point()

    
    ## Aggregating the transactions by Operator_code
      transactions_per_operator <- dataset %>% group_by(Operator_code) %>% summarize(n()) %>% as.data.frame() 
      colnames(transactions_per_operator)[2] <- "counts"  ##Re-naming the column
    
      # Re-arranging the "transactions_per_operator" dataframe
        transactions_per_operator <- head(arrange(transactions_per_operator, desc(counts)),7)
      
      #Plot [Top7 Operator_code]
      p3 <- ggplot(transactions_per_operator, aes(x=Operator_code, y=counts, fill=Operator_code)) + geom_bar(stat = "identity")
   
         
    ## Aggregating the transactions by Category
      transactions_per_category <- dataset %>% group_by(Category) %>% summarize(n()) %>% as.data.frame()
      colnames(transactions_per_category)[2] <- "counts"   #Re-naming the column
    
      #Plot
      p4 <- ggplot(transactions_per_category, aes(x=Category, y=counts, fill=Category)) + geom_bar(stat = "identity")

    
    ## Aggregating the transactions by Provider
      transactions_per_provider <- dataset %>% group_by(Provider) %>% summarize(n()) %>% as.data.frame()
      colnames(transactions_per_provider)[2] <- "counts"   #Re-naming the column
    
      #Plot
      p5 <- ggplot(transactions_per_provider, aes(x=Provider, y=counts, fill=Provider)) + geom_bar(stat = "identity", width=0.5)
    
      
    ## Transaction Status frequency
      transaction_status_freq <- dataset %>% group_by(Transaction.Status) %>% summarize(n()) %>% as.data.frame()
      colnames(transaction_status_freq)[2] <- "counts"   #Re-naming the column
    
      #Plot
      p6 <- ggplot(transaction_status_freq, aes(x=Transaction.Status, y=counts, fill=Transaction.Status)) + geom_bar(stat = "identity")
    
    
    ## Transaction status per day frequecncy
      transaction_status_per_day <- dataset %>% group_by(Transaction.Status,created_date) %>% summarize(n()) %>% as.data.frame() %>% 
                                  filter(Transaction.Status %nin% c("Lagi Verifikasi -", "Lagi Verifikasi - No Inquiry P", "Sukses"))
      colnames(transaction_status_per_day)[3] <- "counts"   #Re-naming the column
    
      #Plot
      p7 <- ggplot(transaction_status_per_day, aes(x=created_date, y=counts, fill= as.factor(Transaction.Status))) + 
           geom_bar(stat = "identity", position = "stack", width = 0.6)
    
      
    ## Transaction status frequency by Operator Code
      transaction_status_by_operator <- dataset %>% group_by(Transaction.Status,Operator_code) %>% summarize(n()) %>% as.data.frame()
      colnames(transaction_status_by_operator)[3] <- "counts"   #Re-naming the column
     
      #Plot
      p8 <- ggplot(transaction_status_by_operator, aes(x=Operator_code, y=counts, fill= as.factor(Transaction.Status))) + 
           geom_bar(stat = "identity", position = "stack") 

     
    ## Transaction frequency by Mode
      transaction_freq_by_mode <- dataset %>% group_by(Mode) %>% summarize(n()) %>% as.data.frame()
      colnames(transaction_freq_by_mode)[2] <- "counts"   #Re-naming the column
     
     #Plot
      p9 <- ggplot(transaction_freq_by_mode, aes(x=as.factor(Mode), y=counts, fill= as.factor(Mode))) + geom_bar(stat = "identity")

     
    ## Checking the discount & Cashback columns, incase significant amount of values are 0 then we're good to remove the column   
      quantile(dataset$discount, seq(0,1,.001))
      quantile(dataset$Cashback, seq(0,1,.001)) 

     
    ## As most of values are 0 in cashback and discount, removing the columns from dataset
      dataset <- subset(dataset, select=-c(discount, Cashback))
   
       
    ## Removing other columns
      dataset <- subset(dataset, select=-c(created_time ,Transaction.Number, updated_date,updated_time,User.ID,Split.Mode,
                                            Response.Code, Hour, Product.name))
   
      
    ## Making the column in same order for ll the datasets 
      dataset <- dataset[,c("created_date","Operator_code","Procurement.price","Category","Provider","Transaction.Status","Mode",
                          "price..Rp..","Price.After.Discount...Cashback","Credit.card.fee","Time_Slot")]
     
     
    print(ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3, nrow = 3))
     
    return(dataset)
     
}


## Calling the above function for all the data sets
  jan_data <- monthly_data(jan_2017)
  feb_data <- monthly_data(feb_2017)
  mar_data <- monthly_data(mar_2017)
  apr_data <- monthly_data(apr_2017)
  may_data <- monthly_data(may_2017)
  jun_data <- monthly_data(jun_2017)
  jul_data <- monthly_data(jul_2017)
  aug_data <- monthly_data(aug_2017)
  sep_data <- monthly_data(sep_2017)
  oct_data <- monthly_data(oct_2017)

  View(mar_data)

###############################################################################################################################################
#                                                                 Data Merge and EDA  
###############################################################################################################################################

### Combining all the dataset into one
    month_data <- rbind(jan_data,feb_data,mar_data,apr_data,may_data,jun_data,jul_data,aug_data,sep_data,oct_data)
  
  
### EDA on combined data for all months----
  
  ## Aggreagting the number of transactions by Time_slot
    transactions_in_time_slot <- month_data %>% group_by(Time_Slot) %>% summarize(n()) %>% as.data.frame()
    colnames(transactions_in_time_slot)[2] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transactions_in_time_slot, aes(x=Time_Slot, y=counts, fill=Time_Slot)) + geom_bar(stat = "identity", width = 0.5)
  
    
  ## Aggreagting the number of transactions per day
    transactions_per_day <- month_data %>% group_by(created_date) %>% summarize(n()) %>% as.data.frame()
    colnames(transactions_per_day)[2] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transactions_per_day, aes(x=created_date, y=counts)) + geom_line() + geom_point()
    
  
  ## Aggregating the transactions by Operator_code
    transactions_per_operator <- month_data %>% group_by(Operator_code) %>% summarize(n()) %>% as.data.frame() 
    colnames(transactions_per_operator)[2] <- "counts"  ##Re-naming the column
  
    # Re-arranging the "transactions_per_operator" dataframe
    transactions_per_operator <- head(arrange(transactions_per_operator, desc(counts)),7)
  
    #Plot [Top7 Operator_code]
    ggplot(transactions_per_operator, aes(x=Operator_code, y=counts, fill=Operator_code)) + geom_bar(stat = "identity")
  
    
  ## Aggregating the transactions by Category
    transactions_per_category <- month_data %>% group_by(Category) %>% summarize(n()) %>% as.data.frame()
    colnames(transactions_per_category)[2] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transactions_per_category, aes(x=Category, y=counts, fill=Category)) + geom_bar(stat = "identity")
  
  
  ## Aggregating the transactions by Provider
    transactions_per_provider <- month_data %>% group_by(Provider) %>% summarize(n()) %>% as.data.frame()
    colnames(transactions_per_provider)[2] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transactions_per_provider, aes(x=Provider, y=counts, fill=Provider)) + geom_bar(stat = "identity", width=0.5)
  
    
  ## Transaction Status frequency
    transaction_status_freq <- month_data %>% group_by(Transaction.Status) %>% summarize(n()) %>% as.data.frame()
    colnames(transaction_status_freq)[2] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transaction_status_freq, aes(x=Transaction.Status, y=counts, fill=Transaction.Status)) + geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5)) 
  
  
  ## Transaction status per day frequecncy
    transaction_status_per_day <- month_data %>% group_by(Transaction.Status,created_date) %>% summarize(n()) %>% as.data.frame() %>% 
    filter(Transaction.Status %nin% c("Lagi Verifikasi -", "Lagi Verifikasi - No Inquiry P", "Sukses"))
    colnames(transaction_status_per_day)[3] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transaction_status_per_day, aes(x=created_date, y=counts, fill= as.factor(Transaction.Status))) + 
    geom_bar(stat = "identity", position = "stack", width = 0.6)
    
    ggplot(transaction_status_per_day, aes(x=created_date, y=counts, group = as.factor(Transaction.Status))) + geom_line(aes(color =Transaction.Status))
  
    
  ## Transaction status frequency by Operator Code
    transaction_status_by_operator <- month_data %>% group_by(Transaction.Status,Operator_code) %>% summarize(n()) %>% as.data.frame()
    colnames(transaction_status_by_operator)[3] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transaction_status_by_operator, aes(x=Operator_code, y=counts, fill= as.factor(Transaction.Status))) + 
    geom_bar(stat = "identity", position = "stack") + theme(axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5)) 
  
  
  ## Transaction frequency by Mode
    transaction_freq_by_mode <- month_data %>% group_by(Mode) %>% summarize(n()) %>% as.data.frame()
    colnames(transaction_freq_by_mode)[2] <- "counts"   #Re-naming the column
  
    #Plot
    ggplot(transaction_freq_by_mode, aes(x=as.factor(Mode), y=counts, fill= as.factor(Mode))) + geom_bar(stat = "identity")
  

    
###############################################################################################################################################
#                                                     Data Preparation for Model Building
###############################################################################################################################################
    
### Preparing data for Model Building----
     
  ## Aggregating the 10 months transaction by day
    per_day_transaction_count <- month_data %>% group_by(created_date) %>% summarize(n()) %>% as.data.frame()
    colnames(per_day_transaction_count)[2] <- "counts"   #Re-naming the column
  
  ## Creating a sequence of days
    timeseries_data <- as.data.frame(cbind(1:303, per_day_transaction_count$counts))
    colnames(timeseries_data) <- c("Day", "Transactions") #Re-naming the column
    

       
###############################################################################################################################################
#                                                                 Model Building  
###############################################################################################################################################
 
### Building Model [Time-Series]----   

  ## Converting data into time-series
    total_timeser <- ts(timeseries_data$Transactions)
    
    #Plotting the time series
      plot(total_timeser)
      
  ## Fitting a ARIMA model
      autoarima <- auto.arima(total_timeser)
      autoarima
      
      tsdiag(autoarima)
      
      #Plotting the actual and predicted values
      plot(autoarima$x, col="black")
      lines(fitted(autoarima), col="red")
      
  ## Checking if the residual series is white noise
      resi_auto_arima <- total_timeser - fitted(autoarima)
      
      adf.test(resi_auto_arima,alternative = "stationary")
      kpss.test(resi_auto_arima)
      

###############################################################################################################################################
#                                                               Prediction
###############################################################################################################################################
      
### Prediction----
      
  ##Predicting the transactions for next 5 days
      
    fcast_auto_arima <- forecast(autoarima, h = 5)
      
    fcast_auto_arima
      #Point     Forecast Lo 80    Hi 80    Lo 95    Hi 95
      #304       16982.79 15892.19 18073.39 15314.86 18650.72
      #305       16894.78 15656.20 18133.36 15000.54 18789.02
      #306       16848.77 15525.57 18171.97 14825.11 18872.43
      #307       16832.15 15455.29 18209.01 14726.43 18937.87
      #308       16836.09 15422.27 18249.91 14673.84 18998.34
      
    fcast_auto_arima$mean
      # Time Series:
      # Start = 304 
      # End = 308 
      # Frequency = 1 
      # [1] 16982.79 16894.78 16848.77 16832.15 16836.09
    

        
###############################################################################################################################################
#                                                             Desired Output
###############################################################################################################################################
    
## Desired output [i.e. forecasting for next 5 days]----
     ###~~~~~~~~~~~~~~~~~~  16982.79 16894.78 16848.77 16832.15 16836.09

    
    
###############################################################################################################################################
#                                                             Analysis
###############################################################################################################################################

    
# Majority of Transactions happens in Day time and Evening  
# Increasing trend of number of transactions from Jan to Oct Month
# "Telkomsel" is the preferred/popular Operator
# Majority of Transactions from "Pulsa" Category
# "T" Provider holds maximum number of transactions
# Majority of transactions got succeeded, few were canceled
# Wallet was the preferred Mode
  
#**** Removed the Below attributes *****  
# Discount - 99.6% values are 0
# Cashback - 97.2% values are 0

 
#**********************************************************************************************************************************************      
#********************************************* Factors Affecting the Daily Transaction********************************************************
#    Operator, Category, Mode and Timings [Time_Slot that I derived]

  
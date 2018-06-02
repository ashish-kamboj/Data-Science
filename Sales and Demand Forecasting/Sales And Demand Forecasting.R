#################################################### Retail-Giant Sales Forecasting ####################################

# Need to forecast the sales and the demand for the next 6 months for a super store giant "Global Mart"
# Transactional data are available for over 4 years transactional.

### Analysis Approach
#Group the data in to 21 segments (For & Markets and 3 Segments) and Find 2 most profitable segments among those

#########################################################################################################################

### Load Libraries
    library(dplyr)
    library(forecast)
    library(tseries)
    library(ggplot2)
    library(cowplot)

### Load data

  superstore_data <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE)  #51290 observations   24 variables
  

###########################################################################################################################
### Data Preparation & Exploratory Data Analysis----
  
  ## Checking for missing value
     sapply(superstore_data, function(x) sum(is.na(x)))
     # We've missing values for Zipcode but we won't consider zipcode for analysis, so no need to treat the missing values
  
  ## Extracting month and year from the "Order.Date" column
     superstore_data$Order.Month <- as.numeric(format(as.POSIXct(superstore_data$Order.Date,format="%d-%m-%Y"),"%m"))
     superstore_data$Order.Year <- as.numeric(format(as.POSIXct(superstore_data$Order.Date,format="%d-%m-%Y"),"%Y"))
  
  ## Grouping data
     segmented_data <- superstore_data %>% group_by(Market,Segment,Order.Year,Order.Month) %>%summarise(sum(Sales),sum(Quantity),sum(Profit))
     colnames(segmented_data)[4:7] <- c("Month","Sales","Quantity","Profit")
    
  ## Calculating "Cofficient of Variation" for the 21 segments
     Segments_21 <- segmented_data %>% group_by(Market,Segment)%>%summarise(sd(Profit)/mean(Profit))
     colnames(Segments_21)[3] <- "CV"

     top_2 <- as.data.frame(head(Segments_21[order(Segments_21$CV, decreasing = FALSE), ],2))
     top_2
    
  ## Also, checking for the top two most profitable segments
     Profit_21 <- segmented_data %>% group_by(Market,Segment)%>%summarise(sum(Profit))
     top_2_profitable_segment <- as.data.frame(head(Profit_21[order(Profit_21$`sum(Profit)`, decreasing = TRUE), ],2))
     top_2_profitable_segment
    
  ## Final Top 2 segments " APAC Consumer" and "EU Consumer" (based on Cofficient of Variation and Total Profit)
     segmented_data_APAC_Consumer <- segmented_data[which(segmented_data$Market =="APAC" & segmented_data$Segment =="Consumer"),]
     segmented_data_EU_Consumer <- segmented_data[which(segmented_data$Market =="EU" & segmented_data$Segment =="Consumer"),]
    
     # Plot to check increasing trend for profit for the final 2 segments
       plot_grid(ggplot(segmented_data_APAC_Consumer, aes(x=Month, y=Profit)) + geom_line()+ geom_smooth(method = "lm"),
              ggplot(segmented_data_EU_Consumer, aes(x=Month, y=Profit)) + geom_line()+ geom_smooth(method = "lm"), align = "h")
  
  ## Remove the unwanted columns from the finalized two segments
     segmented_data_APAC_Consumer$Month <- c(1:48)
     segmented_data_APAC_Consumer <- segmented_data_APAC_Consumer[,4:6]
    
     segmented_data_EU_Consumer$Month <- c(1:48)
     segmented_data_EU_Consumer <- segmented_data_EU_Consumer[,4:6]


  ## Series to predict
     APAC_Consumer_Sales <- as.data.frame(segmented_data_APAC_Consumer[,c("Month","Sales")])
     APAC_Consumer_Quantity <- as.data.frame(segmented_data_APAC_Consumer[,c("Month","Quantity")])
     EU_Consumer_Sales <- as.data.frame(segmented_data_EU_Consumer[,c("Month","Sales")])
     EU_Consumer_Quantity <- as.data.frame(segmented_data_EU_Consumer[,c("Month","Quantity")])
     
    
    
#################################################################################################################################################   
##################################################### Forecasting APAC Consumer Sales ###########################################################
    
### Creating the model using the first 42 rows and then we shall test the model on the remaining 6 rows later
    total_APAC_Consumer_Sales_timeser <- ts(APAC_Consumer_Sales$Sales)
    APAC_Consumer_Sales_indata <- APAC_Consumer_Sales[1:42,]
    APAC_Consumer_Sales_timeser <- ts(APAC_Consumer_Sales_indata$Sales)
    
    ## Plotting the APAC consumer Sales timeseries
       plot(APAC_Consumer_Sales_timeser)
    

### Smoothing the series - Moving Average Smoothing, choosing the width=1
    w <-1
    APAC_Consumer_Sales_smoothedseries <- stats::filter(APAC_Consumer_Sales_timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)
    
    ## Smoothing left end of the time series
       diff <- APAC_Consumer_Sales_smoothedseries[w+2] - APAC_Consumer_Sales_smoothedseries[w+1]
       for (i in seq(w,1,-1)) {
         APAC_Consumer_Sales_smoothedseries[i] <- APAC_Consumer_Sales_smoothedseries[i+1] - diff
       }
    
    ## Smoothing right end of the time series
       n <- length(APAC_Consumer_Sales_timeser)
       diff <- APAC_Consumer_Sales_smoothedseries[n-w] - APAC_Consumer_Sales_smoothedseries[n-w-1]
       for (i in seq(n-w+1, n)) {
         APAC_Consumer_Sales_smoothedseries[i] <- APAC_Consumer_Sales_smoothedseries[i-1] + diff
       }
    
### Plot the smoothed time series
    APAC_Consumer_Sales_timevals <- APAC_Consumer_Sales_indata$Month
    lines(APAC_Consumer_Sales_smoothedseries, col="blue", lwd=2)
    
    
### ~~~~~~~~~~~~~~~Building a model on the smoothed time series using classical decomposition~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## Converting the time series to a dataframe
       APAC_Consumer_Sales_smootheddf <- as.data.frame(cbind(APAC_Consumer_Sales_timevals, as.vector(APAC_Consumer_Sales_smoothedseries)))
       colnames(APAC_Consumer_Sales_smootheddf) <- c('Month', 'Sales')
     
    ## Fitting a multiplicative model with trend and seasonality to the data, Seasonality will be modeled using a sinusoid function
       APAC_Consumer_Sales_lmfit <- lm(Sales ~ sin(1.0*Month) * poly(Month,3) + cos(0.473*Month) * poly(Month,2) + Month, data=APAC_Consumer_Sales_smootheddf)
      
    ## Predicting the global values of the series i.e Seasonality and trend
       APAC_Consumer_Sales_global_pred <- predict(APAC_Consumer_Sales_lmfit, Month=APAC_Consumer_Sales_timevals)
       summary(APAC_Consumer_Sales_global_pred)
      
        # Plotting the globally predictable part
          lines(APAC_Consumer_Sales_timevals, APAC_Consumer_Sales_global_pred, col='red', lwd=2)
      
    ## Now, let's look at the locally predictable series, We will model it through ARMA series
       APAC_Consumer_Sales_local_pred <- APAC_Consumer_Sales_timeser-APAC_Consumer_Sales_global_pred
      
        # Plotting the local component of the series
          plot(APAC_Consumer_Sales_local_pred, col='red', type = "l")
      
        # Checking the Autocorrelation for the observations
          acf(APAC_Consumer_Sales_local_pred)
          acf(APAC_Consumer_Sales_local_pred, type="partial")
      
        # Modelling the local component using ARMA or auto.arima function
          APAC_Consumer_Sales_armafit <- auto.arima(APAC_Consumer_Sales_local_pred)
          tsdiag(APAC_Consumer_Sales_armafit)
          APAC_Consumer_Sales_armafit
      
    ## Checking if the residual series is white noise
       APAC_Consumer_Sales_resi <- APAC_Consumer_Sales_local_pred-fitted(APAC_Consumer_Sales_armafit)
      
       # Performing "adf" and "kpss" test for conforming residual series is stationary
         adf.test(APAC_Consumer_Sales_resi,alternative = "stationary")
         kpss.test(APAC_Consumer_Sales_resi)
      
    ## Evaluating the model using MAPE
      
       # First, making a prediction for the last 6 months
         APAC_Consumer_Sales_outdata <- APAC_Consumer_Sales[43:48,]
         APAC_Consumer_Sales_timevals_out <- APAC_Consumer_Sales_outdata$Month
      
       # Predicting the six month values by "lmfit" as ARMA component is absent
         APAC_Consumer_Sales_global_pred_out <- predict((APAC_Consumer_Sales_lmfit), data.frame(Month =APAC_Consumer_Sales_timevals_out))
         APAC_Consumer_Sales_fcast <- APAC_Consumer_Sales_global_pred_out  # Predicted values
        
       # Compare our prediction with the actual values, using MAPE
         APAC_Consumer_Sales_MAPE_class_dec <- accuracy(APAC_Consumer_Sales_fcast,APAC_Consumer_Sales_outdata[,2])[5]
         APAC_Consumer_Sales_MAPE_class_dec  #20.22433
        
       # Plotting the predictions along with original values, to get a visual feel of the fit
         APAC_Consumer_Sales_class_dec_pred <- c(ts(APAC_Consumer_Sales_global_pred),ts(APAC_Consumer_Sales_global_pred_out))
         plot(total_APAC_Consumer_Sales_timeser, col = "black")
         lines(APAC_Consumer_Sales_class_dec_pred, col = "red")
        
         
         
###~~~~~~~~~~~~~~~~~~~~~ Model building using ARIMA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   ## Building the model using ARIMA  
      APAC_Consumer_Sales_autoarima <- auto.arima(APAC_Consumer_Sales_timeser)
      APAC_Consumer_Sales_autoarima
    
      # Timeseries diagram for ARIMA model
        tsdiag(APAC_Consumer_Sales_autoarima)
        plot(APAC_Consumer_Sales_autoarima$x, col="black")
        lines(fitted(APAC_Consumer_Sales_autoarima), col="red")
        
      # Checking if the residual series is white noise
        APAC_Consumer_Sales_resi_autoarima <- APAC_Consumer_Sales_timeser - fitted(APAC_Consumer_Sales_autoarima)
        
      # Performing "adf" and "kpss" test for conforming residual series is stationary
        adf.test(APAC_Consumer_Sales_resi_autoarima,alternative = "stationary")
        kpss.test(APAC_Consumer_Sales_resi_autoarima)
        
   ## Evaluating the model using MAPE
      APAC_Consumer_Sales_fcast_auto_arima <- predict(APAC_Consumer_Sales_autoarima, n.ahead = 6)
      APAC_Consumer_Sales_fcast_auto_arima$pred  # Predicted values
        
      APAC_Consumer_Sales_MAPE_auto_arima <- accuracy(APAC_Consumer_Sales_fcast_auto_arima$pred,APAC_Consumer_Sales_outdata[,2])[5]
      APAC_Consumer_Sales_MAPE_auto_arima  #27.68952
        
   ## Plotting the predictions along with original values, to get a visual feel of the fit
      APAC_Consumer_Sales_auto_arima_pred <- c(fitted(APAC_Consumer_Sales_autoarima),ts(APAC_Consumer_Sales_fcast_auto_arima$pred))
      plot(total_APAC_Consumer_Sales_timeser, col = "black")
      lines(APAC_Consumer_Sales_auto_arima_pred, col = "red")
      

      
#################################################################################################################################################   
##################################################### Forecasting APAC Consumer Quantity ###########################################################
      
### Creating the model using the first 42 rows and then we shall test the model on the remaining 6 rows later
    total_APAC_Consumer_Quantity_timeser <- ts(APAC_Consumer_Quantity$Quantity)
    APAC_Consumer_Quantity_indata <- APAC_Consumer_Quantity[1:42,]
    APAC_Consumer_Quantity_timeser <- ts(APAC_Consumer_Quantity_indata$Quantity)
      
    ## Plotting the APAC consumer Sales timeseries
       plot(APAC_Consumer_Quantity_timeser)
      
      
### Smoothing the series - Moving Average Smoothing, choosing the width=1
    w <-1
    APAC_Consumer_Quantity_smoothedseries <- stats::filter(APAC_Consumer_Quantity_timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)
      
    ## Smoothing left end of the time series
       diff <- APAC_Consumer_Quantity_smoothedseries[w+2] - APAC_Consumer_Quantity_smoothedseries[w+1]
       for (i in seq(w,1,-1)) {
         APAC_Consumer_Quantity_smoothedseries[i] <- APAC_Consumer_Quantity_smoothedseries[i+1] - diff
       }
      
    ## Smoothing right end of the time series
       n <- length(APAC_Consumer_Quantity_timeser)
       diff <- APAC_Consumer_Quantity_smoothedseries[n-w] - APAC_Consumer_Quantity_smoothedseries[n-w-1]
       for (i in seq(n-w+1, n)) {
         APAC_Consumer_Quantity_smoothedseries[i] <- APAC_Consumer_Quantity_smoothedseries[i-1] + diff
       }
      
### Plot the smoothed time series
    APAC_Consumer_Quantity_timevals <- APAC_Consumer_Quantity_indata$Month
    lines(APAC_Consumer_Quantity_smoothedseries, col="blue", lwd=2)
      
### ~~~~~~~~~~~~~~~Building a model on the smoothed time series using classical decomposition~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   ## Converting the time series to a dataframe
      APAC_Consumer_Quantity_smootheddf <- as.data.frame(cbind(APAC_Consumer_Quantity_timevals, as.vector(APAC_Consumer_Quantity_smoothedseries)))
      colnames(APAC_Consumer_Quantity_smootheddf) <- c('Month', 'Sales')
      
   ## Fitting a multiplicative model with trend and seasonality to the data, Seasonality will be modeled using a sinusoid function
      APAC_Consumer_Quantity_lmfit <- lm(Sales ~ sin(1.0*Month) * poly(Month,3) + cos(0.469*Month) * poly(Month,2) + Month, data=APAC_Consumer_Quantity_smootheddf)
      #APAC_Consumer_Quantity_lmfit <- lm(Sales ~ sin(1.0*Month) * poly(Month,3) + cos(0.473*Month) * poly(Month,2) + Month, data=APAC_Consumer_Quantity_smootheddf)
      #0.469
   ## Predicting the global values of the series i.e Seasonality and trend
      APAC_Consumer_Quantity_global_pred <- predict(APAC_Consumer_Quantity_lmfit, Month=APAC_Consumer_Quantity_timevals)
      summary(APAC_Consumer_Quantity_global_pred)
      
      # Plotting the globally predictable part
        lines(APAC_Consumer_Quantity_timevals, APAC_Consumer_Quantity_global_pred, col='red', lwd=2)
      
   ## Now, let's look at the locally predictable series, We will model it through ARMA series
      APAC_Consumer_Quantity_local_pred <- APAC_Consumer_Quantity_timeser - APAC_Consumer_Quantity_global_pred
      
      # Plotting the local component of the series
      plot(APAC_Consumer_Quantity_local_pred, col='red', type = "l")
      
      # Checking the Autocorrelation for the observations
      acf(APAC_Consumer_Quantity_local_pred)
      acf(APAC_Consumer_Quantity_local_pred, type="partial")
      
      # Modelling the local component using ARMA or auto.arima function
      APAC_Consumer_Quantity_armafit <- auto.arima(APAC_Consumer_Quantity_local_pred)
      tsdiag(APAC_Consumer_Quantity_armafit)
      APAC_Consumer_Quantity_armafit
      
      ## Checking if the residual series is white noise
      APAC_Consumer_Quantity_resi <- APAC_Consumer_Quantity_local_pred-fitted(APAC_Consumer_Quantity_armafit)
      
      # Performing "adf" and "kpss" test for conforming residual series is stationary
      adf.test(APAC_Consumer_Quantity_resi,alternative = "stationary")
      kpss.test(APAC_Consumer_Quantity_resi)
      
      ## Evaluating the model using MAPE
      
      # First, making a prediction for the last 6 months
      APAC_Consumer_Quantity_outdata <- APAC_Consumer_Quantity[43:48,]
      APAC_Consumer_Quantity_timevals_out <- APAC_Consumer_Quantity_outdata$Month
      
      # Predicting the six month values by "lmfit" as ARMA component is absent
      APAC_Consumer_Quantity_global_pred_out <- predict((APAC_Consumer_Quantity_lmfit), data.frame(Month =APAC_Consumer_Quantity_timevals_out))
      APAC_Consumer_Quantity_fcast <- APAC_Consumer_Quantity_global_pred_out  # predicted values
      
      # Compare our prediction with the actual values, using MAPE
      APAC_Consumer_Quantity_MAPE_class_dec <- accuracy(APAC_Consumer_Quantity_fcast,APAC_Consumer_Quantity_outdata[,2])[5]
      APAC_Consumer_Quantity_MAPE_class_dec  #28.78736
      
      # Plotting the predictions along with original values, to get a visual feel of the fit
      APAC_Consumer_Quantity_class_dec_pred <- c(ts(APAC_Consumer_Quantity_global_pred),ts(APAC_Consumer_Quantity_global_pred_out))
      plot(total_APAC_Consumer_Quantity_timeser, col = "black")
      lines(APAC_Consumer_Quantity_class_dec_pred, col = "red")
      
      
      
###~~~~~~~~~~~~~~~~~~~~~~~~~~ Model building using ARIMA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   ## Building the model using ARIMA  
      APAC_Consumer_Quantity_autoarima <- auto.arima(APAC_Consumer_Quantity_timeser)
      APAC_Consumer_Quantity_autoarima
      
      # Timeseries diagram for ARIMA model
        tsdiag(APAC_Consumer_Quantity_autoarima)
        plot(APAC_Consumer_Quantity_autoarima$x, col="black")
        lines(fitted(APAC_Consumer_Quantity_autoarima), col="red")
      
      # Checking if the residual series is white noise
        APAC_Consumer_Quantity_resi_autoarima <- APAC_Consumer_Quantity_timeser - fitted(APAC_Consumer_Quantity_autoarima)
      
      # Performing "adf" and "kpss" test for conforming residual series is stationary
        adf.test(APAC_Consumer_Quantity_resi_autoarima,alternative = "stationary")
        kpss.test(APAC_Consumer_Quantity_resi_autoarima)
      
   ## Evaluating the model using MAPE
      APAC_Consumer_Quantity_fcast_auto_arima <- predict(APAC_Consumer_Quantity_autoarima, n.ahead = 6)
      APAC_Consumer_Quantity_fcast_auto_arima$pred  # predicted values
      
      APAC_Consumer_Quantity_MAPE_auto_arima <- accuracy(APAC_Consumer_Quantity_fcast_auto_arima$pred,APAC_Consumer_Quantity_outdata[,2])[5]
      APAC_Consumer_Quantity_MAPE_auto_arima  #26.24458
      
   ## Plotting the predictions along with original values, to get a visual feel of the fit
      APAC_Consumer_Quantity_auto_arima_pred <- c(fitted(APAC_Consumer_Quantity_autoarima),ts(APAC_Consumer_Quantity_fcast_auto_arima$pred))
      plot(total_APAC_Consumer_Quantity_timeser, col = "black")
      lines(APAC_Consumer_Quantity_auto_arima_pred, col = "red")   
      
      
      
#################################################################################################################################################   
##################################################### Forecasting EU Consumer Sales ############################################################
      
### Creating the model using the first 42 rows and then we shall test the model on the remaining 6 rows later
    total_EU_Consumer_Sales_timeser <- ts(EU_Consumer_Sales$Sales)
    EU_Consumer_Sales_indata <- EU_Consumer_Sales[1:42,]
    EU_Consumer_Sales_timeser <- ts(EU_Consumer_Sales_indata$Sales)
      
    ## Plotting the APAC consumer Sales timeseries
       plot(EU_Consumer_Sales_timeser) 
      
### Smoothing the series - Moving Average Smoothing, choosing the width=1
    w <-1
    EU_Consumer_Sales_smoothedseries <- stats::filter(EU_Consumer_Sales_timeser, filter=rep(1/(2*w),(2*w)), method='convolution', sides=2)
      
    ## Smoothing right end of the time series
       n <- length(EU_Consumer_Sales_timeser)
       diff <- EU_Consumer_Sales_smoothedseries[n-w] - EU_Consumer_Sales_smoothedseries[n-w-1]
       for (i in seq(n-w+1, n)) {
         EU_Consumer_Sales_smoothedseries[i] <- EU_Consumer_Sales_smoothedseries[i-1] + diff
       }
      
### Plot the smoothed time series
    EU_Consumer_Sales_timevals <- EU_Consumer_Sales_indata$Month
    lines(EU_Consumer_Sales_smoothedseries, col="blue", lwd=2)
    
      
### ~~~~~~~~~~~~~~~Building a model on the smoothed time series using classical decomposition~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   ## Converting the time series to a dataframe
      EU_Consumer_Sales_smootheddf <- as.data.frame(cbind(EU_Consumer_Sales_timevals, as.vector(EU_Consumer_Sales_smoothedseries)))
      colnames(EU_Consumer_Sales_smootheddf) <- c('Month', 'Sales')
      
   ## Fitting a multiplicative model with trend and seasonality to the data, Seasonality will be modeled using a sinusoid function
      EU_Consumer_Sales_lmfit <- lm(Sales ~ sin(1*Month) * poly(Month,3) + cos(.5*Month) * poly(Month,2) + Month, data=EU_Consumer_Sales_smootheddf)
      
   ## Predicting the global values of the series i.e Seasonality and trend
      EU_Consumer_Sales_global_pred <- predict(EU_Consumer_Sales_lmfit, Month=EU_Consumer_Sales_timevals)
      summary(EU_Consumer_Sales_global_pred)
      
      # Plotting the globally predictable part
        lines(EU_Consumer_Sales_timevals, EU_Consumer_Sales_global_pred, col='red', lwd=2)
      
   ## Now, let's look at the locally predictable series, We will model it through ARMA series
      EU_Consumer_Sales_local_pred <- EU_Consumer_Sales_timeser - EU_Consumer_Sales_global_pred
      
      # Plotting the local component of the series
        plot(EU_Consumer_Sales_local_pred, col='red', type = "l")
      
      # Checking the Autocorrelation for the observations
        acf(EU_Consumer_Sales_local_pred)
        acf(EU_Consumer_Sales_local_pred, type="partial")
      
      # Modelling the local component using ARMA or auto.arima function
        EU_Consumer_Sales_armafit <- auto.arima(EU_Consumer_Sales_local_pred)
        tsdiag(EU_Consumer_Sales_armafit)
        EU_Consumer_Sales_armafit
      
   ## Checking if the residual series is white noise
      EU_Consumer_Sales_resi <- EU_Consumer_Sales_local_pred - fitted(EU_Consumer_Sales_armafit)
      
      # Performing "adf" and "kpss" test for conforming residual series is stationary
        adf.test(EU_Consumer_Sales_resi,alternative = "stationary")
        kpss.test(EU_Consumer_Sales_resi)
      
   ## Evaluating the model using MAPE
      
      # First, making a prediction for the last 6 months
        EU_Consumer_Sales_outdata <- EU_Consumer_Sales[43:48,]
        EU_Consumer_Sales_timevals_out <- EU_Consumer_Sales_outdata$Month
      
      # Predicting the six month values by "lmfit" as ARMA component is absent
        EU_Consumer_Sales_global_pred_out <- predict((EU_Consumer_Sales_lmfit), data.frame(Month =EU_Consumer_Sales_timevals_out))
        EU_Consumer_Sales_fcast <- EU_Consumer_Sales_global_pred_out  # Predicted values
      
      # Compare our prediction with the actual values, using MAPE
        EU_Consumer_Sales_MAPE_class_dec <- accuracy(EU_Consumer_Sales_fcast,EU_Consumer_Sales_outdata[,2])[5]
        EU_Consumer_Sales_MAPE_class_dec  #29.66075
      
      # Plotting the predictions along with original values, to get a visual feel of the fit
        EU_Consumer_Sales_class_dec_pred <- c(ts(EU_Consumer_Sales_global_pred),ts(EU_Consumer_Sales_global_pred_out))
        plot(total_EU_Consumer_Sales_timeser, col = "black")
        lines(EU_Consumer_Sales_class_dec_pred, col = "red")
        
      
      
###~~~~~~~~~~~~~~~~~~~~~ Model building using ARIMA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   ## Building the model using ARIMA  
      EU_Consumer_Sales_autoarima <- auto.arima(EU_Consumer_Sales_timeser)
      EU_Consumer_Sales_autoarima
      
      # Timeseries diagram for ARIMA model
        tsdiag(EU_Consumer_Sales_autoarima)
        plot(EU_Consumer_Sales_autoarima$x, col="black")
        lines(fitted(EU_Consumer_Sales_autoarima), col="red")
      
      # Checking if the residual series is white noise
        EU_Consumer_Sales_resi_autoarima <- EU_Consumer_Sales_timeser - fitted(EU_Consumer_Sales_autoarima)
      
      # Performing "adf" and "kpss" test for conforming residual series is stationary
        adf.test(EU_Consumer_Sales_resi_autoarima,alternative = "stationary")
        kpss.test(EU_Consumer_Sales_resi_autoarima)
      
   ## Evaluating the model using MAPE
      EU_Consumer_Sales_fcast_auto_arima <- predict(EU_Consumer_Sales_autoarima, n.ahead = 6)
      EU_Consumer_Sales_fcast_auto_arima  # Predicted values
      
      EU_Consumer_Sales_MAPE_auto_arima <- accuracy(EU_Consumer_Sales_fcast_auto_arima$pred,EU_Consumer_Sales_outdata[,2])[5]
      EU_Consumer_Sales_MAPE_auto_arima #28.9226
      
   ## Plotting the predictions along with original values, to get a visual feel of the fit
      EU_Consumer_Sales_auto_arima_pred <- c(fitted(EU_Consumer_Sales_autoarima),ts(EU_Consumer_Sales_fcast_auto_arima$pred))
      plot(total_EU_Consumer_Sales_timeser, col = "black")
      lines(EU_Consumer_Sales_auto_arima_pred, col = "red")
      


#################################################################################################################################################   
##################################################### Forecasting EU Consumer Quantity ###########################################################
      
### Creating the model using the first 42 rows and then we shall test the model on the remaining 6 rows later
    total_EU_Consumer_Quantity_timeser <- ts(EU_Consumer_Quantity$Quantity)
    EU_Consumer_Quantity_indata <- EU_Consumer_Quantity[1:42,]
    EU_Consumer_Quantity_timeser <- ts(EU_Consumer_Quantity_indata$Quantity)
      
    ## Plotting the APAC consumer Sales timeseries
       plot(EU_Consumer_Quantity_timeser)
      
      
### Smoothing the series - Moving Average Smoothing, choosing the width=1
    w <-1
    EU_Consumer_Quantity_smoothedseries <- stats::filter(EU_Consumer_Quantity_timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)
      
    ## Smoothing left end of the time series
       diff <- EU_Consumer_Quantity_smoothedseries[w+2] - EU_Consumer_Quantity_smoothedseries[w+1]
       for (i in seq(w,1,-1)) {
         EU_Consumer_Quantity_smoothedseries[i] <- EU_Consumer_Quantity_smoothedseries[i+1] - diff
       }
      
    ## Smoothing right end of the time series
       n <- length(EU_Consumer_Quantity_timeser)
       diff <- EU_Consumer_Quantity_smoothedseries[n-w] - EU_Consumer_Quantity_smoothedseries[n-w-1]
       for (i in seq(n-w+1, n)) {
         EU_Consumer_Quantity_smoothedseries[i] <- EU_Consumer_Quantity_smoothedseries[i-1] + diff
       }
      
### Plot the smoothed time series
    EU_Consumer_Quantity_timevals <- EU_Consumer_Quantity_indata$Month
    lines(EU_Consumer_Quantity_smoothedseries, col="blue", lwd=2)
    
      
### ~~~~~~~~~~~~~~~Building a model on the smoothed time series using classical decomposition~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   ## Converting the time series to a dataframe
      EU_Consumer_Quantity_smootheddf <- as.data.frame(cbind(EU_Consumer_Quantity_timevals, as.vector(EU_Consumer_Quantity_smoothedseries)))
      colnames(EU_Consumer_Quantity_smootheddf) <- c('Month', 'Sales')
      
   ## Fitting a multiplicative model with trend and seasonality to the data, Seasonality will be modeled using a sinusoid function
      EU_Consumer_Quantity_lmfit <- lm(Sales ~ sin(1*Month) * poly(Month,3) + cos(.5*Month) * poly(Month,3) + Month, data=EU_Consumer_Quantity_smootheddf)

   ## Predicting the global values of the series i.e Seasonality and trend
      EU_Consumer_Quantity_global_pred <- predict(EU_Consumer_Quantity_lmfit, Month=EU_Consumer_Quantity_timevals)
      summary(EU_Consumer_Quantity_global_pred)
      
      # Plotting the globally predictable part
        lines(EU_Consumer_Quantity_timevals, EU_Consumer_Quantity_global_pred, col='red', lwd=2)
      
   ## Now, let's look at the locally predictable series, We will model it through ARMA series
      EU_Consumer_Quantity_local_pred <- EU_Consumer_Quantity_timeser - EU_Consumer_Quantity_global_pred
      
      # Plotting the local component of the series
        plot(EU_Consumer_Quantity_local_pred, col='red', type = "l")
      
      # Checking the Autocorrelation for the observations
        acf(EU_Consumer_Quantity_local_pred)
        acf(EU_Consumer_Quantity_local_pred, type="partial")
      
      # Modelling the local component using ARMA or auto.arima function
        EU_Consumer_Quantity_armafit <- auto.arima(EU_Consumer_Quantity_local_pred)
        tsdiag(EU_Consumer_Quantity_armafit)
        EU_Consumer_Quantity_armafit
      
   ## Checking if the residual series is white noise
      EU_Consumer_Quantity_resi <- EU_Consumer_Quantity_local_pred - fitted(EU_Consumer_Quantity_armafit)
      
      # Performing "adf" and "kpss" test for conforming residual series is stationary
        adf.test(EU_Consumer_Quantity_resi,alternative = "stationary")
        kpss.test(EU_Consumer_Quantity_resi)
      
   ## Evaluating the model using MAPE
      
      # First, making a prediction for the last 6 months
        EU_Consumer_Quantity_outdata <- EU_Consumer_Quantity[43:48,]
        EU_Consumer_Quantity_timevals_out <- EU_Consumer_Quantity_outdata$Month
      
      # Predicting the six month values by "lmfit" as ARMA component is absent
        EU_Consumer_Quantity_global_pred_out <- predict((EU_Consumer_Quantity_lmfit), data.frame(Month =EU_Consumer_Quantity_timevals_out))
        EU_Consumer_Quantity_fcast <- EU_Consumer_Quantity_global_pred_out  # Predicted values
      
      # Compare our prediction with the actual values, using MAPE
        EU_Consumer_Quantity_MAPE_class_dec <- accuracy(EU_Consumer_Quantity_fcast,EU_Consumer_Quantity_outdata[,2])[5]
        EU_Consumer_Quantity_MAPE_class_dec  #33.04939
      
      # Plotting the predictions along with original values, to get a visual feel of the fit
        EU_Consumer_Quantity_class_dec_pred <- c(ts(EU_Consumer_Quantity_global_pred),ts(EU_Consumer_Quantity_global_pred_out))
        plot(total_EU_Consumer_Quantity_timeser, col = "black")
        lines(EU_Consumer_Quantity_class_dec_pred, col = "red")
      

      
###~~~~~~~~~~~~~~~~~~~~~~~~~~ Model building using ARIMA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   ## Building the model using ARIMA  
      EU_Consumer_Quantity_autoarima <- auto.arima(EU_Consumer_Quantity_timeser)
      EU_Consumer_Quantity_autoarima
      
      # Timeseries diagram for ARIMA model
        tsdiag(EU_Consumer_Quantity_autoarima)
        plot(EU_Consumer_Quantity_autoarima$x, col="black")
        lines(fitted(EU_Consumer_Quantity_autoarima), col="red")
      
      # Checking if the residual series is white noise
        EU_Consumer_Quantity_resi_autoarima <- EU_Consumer_Quantity_timeser - fitted(EU_Consumer_Quantity_autoarima)
      
      # Performing "adf" and "kpss" test for conforming residual series is stationary
        adf.test(EU_Consumer_Quantity_resi_autoarima,alternative = "stationary")
        kpss.test(EU_Consumer_Quantity_resi_autoarima)
      
   ## Evaluating the model using MAPE
      EU_Consumer_Quantity_fcast_auto_arima <- predict(EU_Consumer_Quantity_autoarima, n.ahead = 6)
      EU_Consumer_Quantity_fcast_auto_arima$pred  # Predicted values
      
      EU_Consumer_Quantity_MAPE_auto_arima <- accuracy(EU_Consumer_Quantity_fcast_auto_arima$pred,EU_Consumer_Quantity_outdata[,2])[5]
      EU_Consumer_Quantity_MAPE_auto_arima  #30.13319
      
   ## Plotting the predictions along with original values, to get a visual feel of the fit
      EU_Consumer_Quantity_auto_arima_pred <- c(fitted(EU_Consumer_Quantity_autoarima),ts(EU_Consumer_Quantity_fcast_auto_arima$pred))
      plot(total_EU_Consumer_Quantity_timeser, col = "black")
      lines(EU_Consumer_Quantity_auto_arima_pred, col = "red")   
      
      
      

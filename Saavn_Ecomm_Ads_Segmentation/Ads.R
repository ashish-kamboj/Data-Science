##########################################################################################################################
#                                                 :: Ecomm Ads Segmentation ::
##########################################################################################################################

### Loading Libraries
  library(data.table)
  library(dplyr)
  library(corrplot)
  library(ggplot2)
  library(scales)
  library(clustMixType)

### Loading data
  ads_df <- fread("ads_dataframe.csv", stringsAsFactors = F, sep = ",", header= TRUE, na.strings=c("\\N","NA"))
  
  dim(ads_df) 
  summary(ads_df)

    
### Data Analysis
  
  ## Checking for missing values
    sapply(ads_df,function(x) sum(is.na(x)))

  
### Data Cleaning
  
  ## Converting "last_active"  and "last_clicked" column to date format
    ads_df$last_active <- as.Date(paste(substr(ads_df$last_active, start = 1, stop = 4),
                                        substr(ads_df$last_active, start = 5, stop = 6),
                                        substr(ads_df$last_active, start = 7, stop = 8),
                                        sep = "-"))
    
    ads_df$last_clicked <- as.Date(paste(substr(ads_df$last_clicked, start = 1, stop = 4),
                                        substr(ads_df$last_clicked, start = 5, stop = 6),
                                        substr(ads_df$last_clicked, start = 7, stop = 8),
                                        sep = "-"))
    
    
  ## Creating "time_diff" column
    ads_df$time_diff <- as.numeric(ads_df$last_active - ads_df$last_clicked)
    

    
### Imputing missing values
    
  ## Replacing NA values with "unknown" in city column
    ads_df$city[is.na(ads_df$city)] <- "unknown"
    
  ## Imputing missing values in "price" column with the mean value
    ads_df$price[is.na(ads_df$price)] <- -9999
    
  ## Imputing missing values in "gen_conversion" AND "ecomm_conversion" column with -1.0 value
    ads_df$gen_conversion[is.na(ads_df$gen_conversion)] <- -1.0
    ads_df$ecomm_conversion[is.na(ads_df$ecomm_conversion)] <- -1.0
    
  ## Imputing the NA values in "time_diff" column with mean value
    ads_df$time_diff[is.na(ads_df$time_diff)] <- mean(ads_df$time_diff, na.rm = T)
    
    
### Converting the data type of columns
    
  ## Converting "price" and "streams" column to numeric
    ads_df$price <- as.numeric(ads_df$price)
    ads_df$price[is.na(ads_df$price)] <- -9999
    
    ads_df$streams <- as.numeric(ads_df$streams)
    
  ## Converting columns to factor
    changeCols <- colnames(ads_df)[which(as.vector(ads_df[,lapply(.SD, class)]) == "character")]
    ads_df[,(changeCols):= lapply(.SD, as.factor), .SDcols = changeCols]
    
    
    
### Removing columns which are not of use for segmentation
  ads_df <- subset(ads_df, select = -c(uid, last_active, last_clicked))
  

###Corr Plot

  numerical = ads_df[,c(1,2,7,9,10)] #Taking only numerical variables
  M <- cor(numerical)
  corrplot(M, method="circle")
  
  
  
### Model Building
  
  ## Appling k-prototype (default model)
   kpres <- kproto(ads_df, 4)
  
   
  ## Check ing for the optimal number of clusters given the data
   wss<-vector()
   for (i in 2:15){ wss[i] <- sum(kproto(ads_df, i)$withinss)}
   
   par(mfrow=c(1,1))
   plot(1:15, wss, type="b", xlab="Number of Clusters",
        ylab="Within groups sum of squares",
        main="Assessing the Optimal Number of Clusters with the Elbow Method",
        pch=20, cex=2)
    
  
### Final Model
   kpres_final <- kproto(ads_df, 8, iter.max = 100, nstart = 1)

  
### Appending the ClusterIDs to "ads_df" data
   ads_df_kpres <-cbind(ads_df,kpres_final$cluster)
   colnames(ads_df_kpres)[11]<- "ClusterID"
   
   ads_df_kpres$ClusterID <- as.factor(ads_df_kpres$ClusterID)
  
    
### Cluster Analysis
  
  ## Checking ecomm conversion (or Click-happy users) for each cluster using boxplot
    ggplot(ads_df_kpres, aes(x= ClusterID, y=ecomm_conversion)) + geom_boxplot() 
    #ClusterID=5 having more number of ecomm_conversion
  
    # Checking mean ecomm conversion (or Click-happy users) for each cluster using barplot.
    # Also, removing the "ecomm_conversion = -1" (which we had substituted during imputation)
      ads_df_kpres %>% subset(ecomm_conversion != -1) %>% group_by(ClusterID) %>% 
      summarise(mean_ecomm_conversion =mean(ecomm_conversion)) %>% as.data.frame() %>% 
      ggplot(aes(x=ClusterID, y=mean_ecomm_conversion, fill=ClusterID)) + geom_bar(stat = "identity", width = 0.7) + 
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
      labs(title = "Mean ecomm conversion by clusterID", y = "Mean ecomm conversion", x = "ClusterID")
    
  
  	
  ## Estimating the percentage of the total number of users classified to be in the "click-happy" ecommerce segment  

    ads_df_kpres %>% group_by(ClusterID) %>% summarise(counts =n()) %>% as.data.frame() %>%
    ggplot(aes(x=ClusterID, y=counts/sum(counts), fill=ClusterID)) + geom_bar(stat = "identity", width = 0.7) + 
    scale_y_continuous(labels = scales::percent) + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
    labs(title = "User share in each cluster", y = "Percent", x = "ClusterID") + 
    geom_text(aes(x = ClusterID, y = counts/sum(counts),label = paste0(round((counts/sum(counts))*100,1),"%")), size=4,vjust = -0.25) 
    #30.4% users
    
  
  ## Analyzing the users in the segment, providing details on the distribution of parameters such as demographics 
  ## and music streaming habits. 
    
    cluster5 <- subset(ads_df_kpres, ClusterID == 5)
    
    #Gender
      cluster5 %>% group_by(predicted_gender) %>% summarise(counts =n()) %>% as.data.frame() %>%
      ggplot(aes(x=predicted_gender, y=counts/sum(counts), fill=predicted_gender)) + geom_bar(stat = "identity", width = 0.5) + 
      scale_y_continuous(labels = scales::percent) + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
      labs(title = "Cluster5 Gender share", y = "Percent", x = "Gender") + 
      geom_text(aes(x = predicted_gender, y = counts/sum(counts),label = paste0(round((counts/sum(counts))*100,1),"%")), size=4,vjust = -0.25) 
      #62.7% users are male
      
      
    #Age Group
      cluster5 %>% group_by(predicted_age) %>% summarise(counts =n()) %>% as.data.frame() %>%
      ggplot(aes(x=predicted_age, y=counts/sum(counts), fill=predicted_age)) + geom_bar(stat = "identity", width = 0.6) + 
      scale_y_continuous(labels = scales::percent) + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
      labs(title = "Cluster5 Age Group share", y = "Percent", x = "Age Group") + 
      geom_text(aes(x = predicted_age, y = counts/sum(counts),label = paste0(round((counts/sum(counts))*100,1),"%")), size=4,vjust = -0.25) 
      #63.7% users belongs to unknown age group (i.e. u)
      
      
    #Language
      # Top 20 preferred languages by users for songs
      top_20_languages <- cluster5 %>% group_by(language) %>% summarise(counts =n()) %>% 
                          as.data.frame() %>% arrange(desc(counts)) %>% head(20)
      
      ggplot(top_20_languages, aes(x=reorder(language, -counts), y=counts/sum(counts), fill=language)) + 
      geom_bar(stat = "identity", width = 0.6) + scale_y_continuous(labels = scales::percent) + 
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
      labs(title = "Cluster5 Top20 Song Languages share", y = "Percent", x = "Languages") + 
      geom_text(aes(x = language, y = counts/sum(counts),label = paste0(round((counts/sum(counts))*100,1),"%")), size=4,vjust = -0.25) 
      #Hindi songs listen by 66.2% users
      
      
    #Login Mode
      cluster5 %>% group_by(login_mode) %>% summarise(counts =n()) %>% as.data.frame() %>%
      ggplot(aes(x=login_mode, y=counts/sum(counts) ,fill=login_mode)) + geom_bar(stat = "identity", width = 0.6) + 
      scale_y_continuous(labels = scales::percent) + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
      labs(title = "Cluster5 Login Mode share", y = "Percent", x = "Login Mode") + 
      geom_text(aes(x = login_mode, y = counts/sum(counts),label = paste0(round((counts/sum(counts))*100,1),"%")), size=4,vjust = -0.25) 
      #70.6% of users listened songs without logging through any accounts(i.e unregistered) 
      


### Details about the model choosed::
      
#I have used "K-Prototype Clustering" for segmentation as we have mixed-type data i.e. both continuous and Categorical variables.
#I have choosen the numbers of clusters which minimize the intra-cluster distance and maximize the inter-cluster distance
      
      
#For further improvement we can try to tune the parameter "lambda" 
#(Parameter > 0 to trade off between Euclidean distance of numeric variables and simple matching coefficient between categorical variables)

      
#In order to scale up the model for the 50 million users, we can use the below approaches-
  #- We can build the model using parallel processing
  #- We can do random sampling of data and then create the clusters
  #- Dataset can be divided in multiple independent datasets and the clustering results can be merged on a final model.
  #- We can use Bigdata tools like Apache Spark for building model(i.e. clustering) on large datasets, Also we can write MapReduce programm inorder
  #   to reduce the execution time by dividing a job into several tasks and executes them in a distributed environment.
      
      
      
      
      
      
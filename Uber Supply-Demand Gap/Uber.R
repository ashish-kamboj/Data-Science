# Load Packages----

        library(lubridate)
        library(tidyr)
        library(dplyr)
        library(ggplot2)
        library(ggthemes)
        library(scales)

# Load the "Uber Request Data.csv" file----

        uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
        View(uber)

        
# Cleaning Date columns----
    
        datetime <- parse_date_time(uber$Request.timestamp,c("dmY HMS"),truncated = 2)
        uber$Request.timestamp <- format(datetime, "%d-%m-%Y %H:%M:%S")  
    
        datetime2 <- parse_date_time(uber$Drop.timestamp,c("dmY HMS"),truncated = 2)
        uber$Drop.timestamp <- format(datetime2, "%d-%m-%Y %H:%M:%S")  

        
# Create Derived variables----
        
    # Separate Date and Time from the "Request.timestamp" column
        uber <- separate(uber, Request.timestamp,c("Request.Date","Request.Time"),sep = " ", remove = FALSE)
    
    # Separate Hours from the "Request.Time" column (which was created in above command)
        uber <- separate(uber, Request.Time,c("Request.Hour"),sep = ":", remove = FALSE)
    
    # Converted Hours in to numeric
        uber$Request.Hour <- as.numeric(uber$Request.Hour)
    
    # Derived Time Slots (i.e. Morning, Day Time, Evening and Late Night) from the "Requeust.Hour" column
        uber$Time_Slot[uber$Request.Hour <= 3] <- c("Late Night")
        uber$Time_Slot[((uber$Request.Hour >= 4) & (uber$Request.Hour <= 10))] <- c("Morning")
        uber$Time_Slot[((uber$Request.Hour >= 11) & (uber$Request.Hour <= 16))] <- c("Day Time")
        uber$Time_Slot[((uber$Request.Hour >= 17) & (uber$Request.Hour <= 21))] <- c("Evening")
        uber$Time_Slot[which(is.na(uber$Time_Slot==T))] <- c("Late Night")

# Uber data analysis----
  # Getting stats and plotting the same using ggplot 
    
  # Code and Plot1 
     # Frequency of Requests - "Completed", "No Cars Available" and "Trip Completed"
         table(uber$Status)
     # Above Stats as a Data frame     
         freq_status <- as.data.frame(table(uber$Status))
         colnames(freq_status)<- c("Status","Frequency")
         freq_status    
     # Plot1
         plot1 <- ggplot(uber,aes(x=Status)) + geom_bar(position = "dodge",width = 0.3, fill="green4") + labs(x=" Trip Status", y="Number of Requests") + 
                  ggtitle("Cab Request Status") + theme_hc(base_size = 18, base_family = "sans") + 
                  theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),plot.title = element_text(hjust = 0.5)) + 
                  geom_text(stat="count",aes(label=..count..),vjust=-1,color="white", size=6)
     # View plot1    
         plot1
 
         
         
  # Code and Plot2       
     # Request status based on Pickup Point
         table(uber$Status,uber$Pickup.point)
     # Above Stats as a Data frame       
         freq_status1 <- as.data.frame(table(uber$Status,uber$Pickup.point))
         colnames(freq_status1)<- c("Status","Pickup_Point","Frequency")
         freq_status1
     # Plot2
         plot2 <- ggplot(uber,aes(x=Status,fill=factor(Pickup.point))) + geom_bar(position = "dodge",width = 0.4) + labs(x=" Trip Status", y="Number of Requests")+
                  ggtitle("Cabs Pickup Status") + theme_hc(base_size = 18, base_family = "sans") + 
                  scale_fill_manual("Pickup Point: ", values = c("Airport" = "red3", "City" = "green4")) + 
                  theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
                  geom_text(stat="count",aes(label=..count..),vjust=-0.4,color="white", size=6,position=position_dodge(width=0.4))
     # View plot2
         plot2
 
                
        
  # Code and Plot3      
     # Hourly Frequency of requests based on Pickup Point    
         table(uber$Pickup.point,uber$Request.Hour)
     # Above Stats as a Data frame 
         freq_status2 <- as.data.frame(table(uber$Pickup.point,uber$Request.Hour))
         colnames(freq_status2)<- c("Pickup_Point","Hours","Frequency")
         freq_status2
     # Plot3
         plot3 <- ggplot(uber,aes(x=factor(Request.Hour),fill=factor(Pickup.point)))+geom_bar(position = "dodge")+ labs(x="Cab Requested Hour",y="Number of Requests")+
                  ggtitle("Cabs Demand Status")+ theme_hc(base_size = 18, base_family = "sans") + 
                  scale_fill_manual("Pickup Point: ", values = c("Airport" = "red3", "City" = "green4")) + 
                  theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))
     # View plot3
         plot3
        
    
  # Code and Plot4
     # Hourly Frequency of requests status (i.e. "Completed", "No Cars Available" and "Trip Completed")   
         table(uber$Request.Hour,uber$Status)
     # Above Stats as a Data frame 
         freq_status3 <- as.data.frame(table(uber$Request.Hour,uber$Status))
         colnames(freq_status3)<- c("Hours","Status","Frequency")
         freq_status3
     # Plot4
         plot4 <- ggplot(uber,aes(x=factor(Request.Hour),fill=factor(Status)))+geom_bar(position = "dodge",width = 0.5)+ labs(x="Cab Requested Hour",y="Number of Requests")+
                  ggtitle("Cabs Non-availability by Hours") + theme_hc(base_size = 18, base_family = "sans") +
                  scale_fill_manual("Status: ", values = c("Trip Completed" = "green4", "No Cars Available" = "yellow", "Cancelled" = "red3"))+ 
                  theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
     # View plot4
         plot4
    
  
               
  # Code and Plot5      
     # Request Status based on the Time Slots  
         table(uber$Time_Slot,uber$Status)
     # Above Stats as a Data frame   
         freq_status4 <- as.data.frame(table(uber$Time_Slot,uber$Status))
         colnames(freq_status4)<- c("Time_Slot","Status","Frequency")
         freq_status4
     # Plot5 - Plot for the time slots when the highest gap exists
         plot5 <- ggplot(uber,aes(x=factor(Time_Slot),fill=factor(Status)))+geom_bar(position = "dodge",width = 0.6)+ labs(x="Cab Requested Time",y="Frequency of Requests")+
                  ggtitle("Cabs Availability / Non-availability")+ theme_hc(base_size = 18, base_family = "sans") + 
                  scale_fill_manual("Status: ", values = c("Cancelled" = "red3", "No Cars Available" = "yellow", "Trip Completed" = "green4")) + 
                  theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
                 geom_text(stat="count",aes(label=..count..),vjust=-0.2,color="white", size=5.5,position=position_dodge(width=0.6))
     # View plot5
         plot5
    
             
    
  # Code and Plot6      
     # Frequecy of Request Status based in each Time Slots for Pickup Point
         table(uber$Time_Slot,uber$Status,uber$Pickup.point)
     # Above Stats as a Data frame
         freq_status_airport <- as.data.frame(summarise(group_by(subset(uber, uber$Pickup.point=="Airport"),Status), n()))
         colnames(freq_status_airport)[2] <- "Frequency"
         freq_status_airport
         
         freq_status_city <- as.data.frame(summarise(group_by(subset(uber, uber$Pickup.point=="City"),Status), n()))
         colnames(freq_status_city)[2] <- "Frequency"
         freq_status_city
     # Plot6 - Plot for types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
         plot6 <- ggplot(uber,aes(x=factor(Time_Slot),fill=factor(Status)))+geom_bar(position = "dodge",width = 0.6)+ labs(x="Cab Requested Time",y="Number of Requests")+
                  ggtitle("Availability / Non-availability of Cabs at Pickup-Point") + facet_wrap( ~ Pickup.point) + theme_hc(base_size = 24, base_family = "sans") + 
                  scale_fill_manual("Status: ", values = c("Trip Completed" = "green4", "No Cars Available" = "yellow", "Cancelled" = "red3")) + theme_dark() + 
                  theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
                  geom_text(stat="count",aes(label=..count..),vjust=-0.5,color="white", size=5.5,position=position_dodge(width=0.6))
     # View plot6
         plot6
        
    
        
     # Percentage of "No Cars Availabe" in Evening at Pickup Point
         no_cars_availabe_airport <- length(which((uber$Pickup.point=="Airport") & (uber$Time_Slot == "Evening") & (uber$Status == "No Cars Available")))
         no_cars_availabe_city <- length(which((uber$Pickup.point=="City") & (uber$Time_Slot == "Evening") & (uber$Status == "No Cars Available")))
        
         # At Aiport
             percent(no_cars_availabe_airport/(no_cars_availabe_airport+no_cars_availabe_city))
         # At City
             percent(no_cars_availabe_city/(no_cars_availabe_airport+no_cars_availabe_city))
        
        
     # Percentage of Cabs Cancellation in Morning at Pickup Point
         Cancelled_airport <- length(which((uber$Pickup.point=="Airport") & (uber$Time_Slot == "Morning") & (uber$Status == "Cancelled")))
         Cancelled_city <- length(which((uber$Pickup.point=="City") & (uber$Time_Slot == "Morning") & (uber$Status == "Cancelled")))
        
         # At Aiport
             percent(Cancelled_airport/(Cancelled_airport+Cancelled_city))
         # At City
             percent(Cancelled_city/(Cancelled_airport+Cancelled_city))
    

    

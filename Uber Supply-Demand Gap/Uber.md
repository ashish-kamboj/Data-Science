---
title: "Uber Demand Supply Gap Analysis"
output: 
  html_document:
    keep_md: true
---

### 1. Load Packages ::


```r
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
```

### 2. Load the "Uber Request Data.csv" file ::

```r
uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
```


        
### 3. Cleaning Date columns ::

```r
datetime <- parse_date_time(uber$Request.timestamp,c("dmY HMS"),truncated = 2)
uber$Request.timestamp <- format(datetime, "%d-%m-%Y %H:%M:%S")  
 
datetime2 <- parse_date_time(uber$Drop.timestamp,c("dmY HMS"),truncated = 2)
uber$Drop.timestamp <- format(datetime2, "%d-%m-%Y %H:%M:%S")  
```

### 4. Create Derived variables ::

#### 4.1. Separate Date and Time from the "Request.timestamp" column

```r
uber <- separate(uber, Request.timestamp,c("Request.Date","Request.Time"),sep = " ", remove = FALSE)
```

#### 4.2. Separate Hours from the "Request.Time" column (which was created in above command)

```r
uber <- separate(uber, Request.Time,c("Request.Hour"),sep = ":", remove = FALSE)
```

#### 4.3. Converted Hours in to numeric

```r
uber$Request.Hour <- as.numeric(uber$Request.Hour)
```
    
#### 4.4. Derived Time Slots (i.e. Morning, Day Time, Evening and Late Night) from the "Requeust.Hour" column

```r
uber$Time_Slot[uber$Request.Hour <= 3] <- c("Late Night")
uber$Time_Slot[((uber$Request.Hour >= 4) & (uber$Request.Hour <= 10))] <- c("Morning")
uber$Time_Slot[((uber$Request.Hour >= 11) & (uber$Request.Hour <= 16))] <- c("Day Time")
uber$Time_Slot[((uber$Request.Hour >= 17) & (uber$Request.Hour <= 21))] <- c("Evening")
uber$Time_Slot[which(is.na(uber$Time_Slot==T))] <- c("Late Night")
```

### 5. Uber data analysis ::
    
### 5.1. Code and Plot1 
#### Frequency of Requests - "Completed", "No Cars Available" and "Trip Completed"

```r
  table(uber$Status)
```

```
## 
##         Cancelled No Cars Available    Trip Completed 
##              1264              2650              2831
```

#### Above Stats as a Data frame

```r
freq_status <- as.data.frame(table(uber$Status))
colnames(freq_status)<- c("Status","Frequency")
freq_status
```

```
##              Status Frequency
## 1         Cancelled      1264
## 2 No Cars Available      2650
## 3    Trip Completed      2831
```

#### Plot1

```r
plot1 <- ggplot(uber,aes(x=Status)) + geom_bar(position = "dodge",width = 0.3, fill="green4") + labs(x=" Trip Status", y="Number of Requests") + 
ggtitle("Cab Request Status") + theme_hc(base_size = 18, base_family = "sans") + theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour =  "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),plot.title = element_text(hjust = 0.5)) + geom_text(stat="count",aes(label=..count..),vjust=-1,color="white", size=6)
```

![](Uber_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
         
         
### 5.2. Code and Plot2

#### Request status based on Pickup Point

```r
table(uber$Status,uber$Pickup.point)
```

```
##                    
##                     Airport City
##   Cancelled             198 1066
##   No Cars Available    1713  937
##   Trip Completed       1327 1504
```

#### Above Stats as a Data frame

```r
freq_status1 <- as.data.frame(table(uber$Status,uber$Pickup.point))
colnames(freq_status1)<- c("Status","Pickup_Point","Frequency")
freq_status1
```

```
##              Status Pickup_Point Frequency
## 1         Cancelled      Airport       198
## 2 No Cars Available      Airport      1713
## 3    Trip Completed      Airport      1327
## 4         Cancelled         City      1066
## 5 No Cars Available         City       937
## 6    Trip Completed         City      1504
```
         
#### Plot2

```r
plot2 <- ggplot(uber,aes(x=Status,fill=factor(Pickup.point))) + geom_bar(position = "dodge",width = 0.4) + labs(x=" Trip Status", y="Number of Requests")+
ggtitle("Cabs Pickup Status") + theme_hc(base_size = 18, base_family = "sans") + scale_fill_manual("Pickup Point: ", values = c("Airport" = "red3", "City" = "green4")) + theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
geom_text(stat="count",aes(label=..count..),vjust=-0.4,color="white", size=6,position=position_dodge(width=0.4))
```
     
![](Uber_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
                
        
### 5.3. Code and Plot3

#### Hourly Frequency of requests based on Pickup Point

```r
table(uber$Pickup.point,uber$Request.Hour)
```

```
##          
##             0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
##   Airport  53  42  41  45  72  92  89  83  73  89  75  64  87  65  50  76  61
##   City     46  43  58  47 131 353 309 323 350 342 168 107  97  95  86  95  98
##          
##            17  18  19  20  21  22  23
##   Airport 308 405 366 378 343 183  98
##   City    110 105 107 114 106 121  96
```

#### Above Stats as a Data frame

```r
freq_status2 <- as.data.frame(table(uber$Pickup.point,uber$Request.Hour))
colnames(freq_status2)<- c("Pickup_Point","Hours","Frequency")
freq_status2
```

```
##    Pickup_Point Hours Frequency
## 1       Airport     0        53
## 2          City     0        46
## 3       Airport     1        42
## 4          City     1        43
## 5       Airport     2        41
## 6          City     2        58
## 7       Airport     3        45
## 8          City     3        47
## 9       Airport     4        72
## 10         City     4       131
## 11      Airport     5        92
## 12         City     5       353
## 13      Airport     6        89
## 14         City     6       309
## 15      Airport     7        83
## 16         City     7       323
## 17      Airport     8        73
## 18         City     8       350
## 19      Airport     9        89
## 20         City     9       342
## 21      Airport    10        75
## 22         City    10       168
## 23      Airport    11        64
## 24         City    11       107
## 25      Airport    12        87
## 26         City    12        97
## 27      Airport    13        65
## 28         City    13        95
## 29      Airport    14        50
## 30         City    14        86
## 31      Airport    15        76
## 32         City    15        95
## 33      Airport    16        61
## 34         City    16        98
## 35      Airport    17       308
## 36         City    17       110
## 37      Airport    18       405
## 38         City    18       105
## 39      Airport    19       366
## 40         City    19       107
## 41      Airport    20       378
## 42         City    20       114
## 43      Airport    21       343
## 44         City    21       106
## 45      Airport    22       183
## 46         City    22       121
## 47      Airport    23        98
## 48         City    23        96
```

### Plot3

```r
plot3 <- ggplot(uber,aes(x=factor(Request.Hour),fill=factor(Pickup.point)))+geom_bar(position = "dodge")+ labs(x="Cab Requested Hour",y="Number of Requests")+ ggtitle("Cabs Demand Status")+ theme_hc(base_size = 18, base_family = "sans") + scale_fill_manual("Pickup Point: ", values = c("Airport" = "red3", "City" = "green4")) + theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))
```

![](Uber_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
    
### 5.4. Code and Plot4

#### Hourly Frequency of requests status (i.e. "Completed", "No Cars Available" and "Trip Completed") 

```r
table(uber$Request.Hour,uber$Status)
```

```
##     
##      Cancelled No Cars Available Trip Completed
##   0          3                56             40
##   1          4                56             25
##   2          5                57             37
##   3          2                56             34
##   4         51                74             78
##   5        176                84            185
##   6        145                86            167
##   7        169                63            174
##   8        178                90            155
##   9        175                83            173
##   10        62                65            116
##   11        15                41            115
##   12        19                44            121
##   13        18                53             89
##   14        11                37             88
##   15        21                48            102
##   16        22                46             91
##   17        35               232            151
##   18        24               322            164
##   19        24               283            166
##   20        41               290            161
##   21        42               265            142
##   22        12               138            154
##   23        10                81            103
```

#### Above Stats as a Data frame

```r
freq_status3 <- as.data.frame(table(uber$Request.Hour,uber$Status))
colnames(freq_status3)<- c("Hours","Status","Frequency")
freq_status3
```

```
##    Hours            Status Frequency
## 1      0         Cancelled         3
## 2      1         Cancelled         4
## 3      2         Cancelled         5
## 4      3         Cancelled         2
## 5      4         Cancelled        51
## 6      5         Cancelled       176
## 7      6         Cancelled       145
## 8      7         Cancelled       169
## 9      8         Cancelled       178
## 10     9         Cancelled       175
## 11    10         Cancelled        62
## 12    11         Cancelled        15
## 13    12         Cancelled        19
## 14    13         Cancelled        18
## 15    14         Cancelled        11
## 16    15         Cancelled        21
## 17    16         Cancelled        22
## 18    17         Cancelled        35
## 19    18         Cancelled        24
## 20    19         Cancelled        24
## 21    20         Cancelled        41
## 22    21         Cancelled        42
## 23    22         Cancelled        12
## 24    23         Cancelled        10
## 25     0 No Cars Available        56
## 26     1 No Cars Available        56
## 27     2 No Cars Available        57
## 28     3 No Cars Available        56
## 29     4 No Cars Available        74
## 30     5 No Cars Available        84
## 31     6 No Cars Available        86
## 32     7 No Cars Available        63
## 33     8 No Cars Available        90
## 34     9 No Cars Available        83
## 35    10 No Cars Available        65
## 36    11 No Cars Available        41
## 37    12 No Cars Available        44
## 38    13 No Cars Available        53
## 39    14 No Cars Available        37
## 40    15 No Cars Available        48
## 41    16 No Cars Available        46
## 42    17 No Cars Available       232
## 43    18 No Cars Available       322
## 44    19 No Cars Available       283
## 45    20 No Cars Available       290
## 46    21 No Cars Available       265
## 47    22 No Cars Available       138
## 48    23 No Cars Available        81
## 49     0    Trip Completed        40
## 50     1    Trip Completed        25
## 51     2    Trip Completed        37
## 52     3    Trip Completed        34
## 53     4    Trip Completed        78
## 54     5    Trip Completed       185
## 55     6    Trip Completed       167
## 56     7    Trip Completed       174
## 57     8    Trip Completed       155
## 58     9    Trip Completed       173
## 59    10    Trip Completed       116
## 60    11    Trip Completed       115
## 61    12    Trip Completed       121
## 62    13    Trip Completed        89
## 63    14    Trip Completed        88
## 64    15    Trip Completed       102
## 65    16    Trip Completed        91
## 66    17    Trip Completed       151
## 67    18    Trip Completed       164
## 68    19    Trip Completed       166
## 69    20    Trip Completed       161
## 70    21    Trip Completed       142
## 71    22    Trip Completed       154
## 72    23    Trip Completed       103
```
         
#### Plot4

```r
plot4 <- ggplot(uber,aes(x=factor(Request.Hour),fill=factor(Status)))+geom_bar(position = "dodge",width = 0.5)+ labs(x="Cab Requested Hour",y="Number of Requests")+ ggtitle("Cabs Non-availability by Hours") + theme_hc(base_size = 18, base_family = "sans") + scale_fill_manual("Status: ", values = c("Trip Completed" = "green4", "No Cars Available" = "yellow", "Cancelled" = "red3"))+ theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 
```
         
![](Uber_files/figure-html/unnamed-chunk-24-1.png)<!-- -->
  
               
### 5.5. Code and Plot5 ::

#### Request Status based on the Time Slots

```r
table(uber$Time_Slot,uber$Status)
```

```
##             
##              Cancelled No Cars Available Trip Completed
##   Day Time         106               269            606
##   Evening          166              1392            784
##   Late Night        36               444            393
##   Morning          956               545           1048
```

#### Above Stats as a Data frame

```r
freq_status4 <- as.data.frame(table(uber$Time_Slot,uber$Status))
colnames(freq_status4)<- c("Time_Slot","Status","Frequency")
freq_status4
```

```
##     Time_Slot            Status Frequency
## 1    Day Time         Cancelled       106
## 2     Evening         Cancelled       166
## 3  Late Night         Cancelled        36
## 4     Morning         Cancelled       956
## 5    Day Time No Cars Available       269
## 6     Evening No Cars Available      1392
## 7  Late Night No Cars Available       444
## 8     Morning No Cars Available       545
## 9    Day Time    Trip Completed       606
## 10    Evening    Trip Completed       784
## 11 Late Night    Trip Completed       393
## 12    Morning    Trip Completed      1048
```

#### Plot5 - Plot for the time slots when the highest gap exists

```r
plot5 <- ggplot(uber,aes(x=factor(Time_Slot),fill=factor(Status)))+geom_bar(position = "dodge",width = 0.6)+ labs(x="Cab Requested Time",y="Frequency of Requests")+ ggtitle("Cabs Availability / Non-availability")+ theme_hc(base_size = 18, base_family = "sans") + scale_fill_manual("Status: ", values = c("Cancelled" = "red3", "No Cars Available" = "yellow", "Trip Completed" = "green4")) + theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + 
geom_text(stat="count",aes(label=..count..),vjust=-0.2,color="white", size=5.5,position=position_dodge(width=0.6))
```
         
![](Uber_files/figure-html/unnamed-chunk-28-1.png)<!-- -->
             
    
### 5.6. Code and Plot6

```r
#### Frequecy of Request Status based in each Time Slots for Pickup Point
table(uber$Time_Slot,uber$Status,uber$Pickup.point)
```

```
## , ,  = Airport
## 
##             
##              Cancelled No Cars Available Trip Completed
##   Day Time          55                74            274
##   Evening          106              1321            373
##   Late Night         3               250            209
##   Morning           34                68            471
## 
## , ,  = City
## 
##             
##              Cancelled No Cars Available Trip Completed
##   Day Time          51               195            332
##   Evening           60                71            411
##   Late Night        33               194            184
##   Morning          922               477            577
```

#### Above Stats as a Data frame

```r
freq_status_airport <- as.data.frame(summarise(group_by(subset(uber, uber$Pickup.point=="Airport"),Status), n()))
colnames(freq_status_airport)[2] <- "Frequency"
freq_status_airport
```

```
##              Status Frequency
## 1         Cancelled       198
## 2 No Cars Available      1713
## 3    Trip Completed      1327
```


```r
freq_status_city <- as.data.frame(summarise(group_by(subset(uber, uber$Pickup.point=="City"),Status), n()))
colnames(freq_status_city)[2] <- "Frequency"
freq_status_city
```

```
##              Status Frequency
## 1         Cancelled      1066
## 2 No Cars Available       937
## 3    Trip Completed      1504
```

#### Plot6 - Plot for types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
 
 ```r
 plot6 <- ggplot(uber,aes(x=factor(Time_Slot),fill=factor(Status)))+geom_bar(position = "dodge",width = 0.6)+ labs(x="Cab Requested Time",y="Number of Requests")+ ggtitle("Availability / Non-availability of Cabs at Pickup-Point") + facet_wrap( ~ Pickup.point) + theme_hc(base_size = 24, base_family = "sans") + scale_fill_manual("Status: ", values = c("Trip Completed" = "green4", "No Cars Available" = "yellow", "Cancelled" = "red3")) + theme_dark() + 
 theme(plot.background = element_rect(fill="gray19"), axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"),title = element_text(colour = "white"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) + geom_text(stat="count",aes(label=..count..),vjust=-0.5,color="white", size=5.5,position=position_dodge(width=0.6))
 ```

![](Uber_files/figure-html/unnamed-chunk-33-1.png)<!-- -->
    
        
### 5.7. Percentage of "No Cars Availabe" in Evening at Pickup Point

```r
no_cars_availabe_airport <- length(which((uber$Pickup.point=="Airport") & (uber$Time_Slot == "Evening") & (uber$Status == "No Cars Available")))
no_cars_availabe_city <- length(which((uber$Pickup.point=="City") & (uber$Time_Slot == "Evening") & (uber$Status == "No Cars Available")))
```

#### At Aiport

```r
percent(no_cars_availabe_airport/(no_cars_availabe_airport+no_cars_availabe_city))
```

```
## [1] "95%"
```

#### At City

```r
percent(no_cars_availabe_city/(no_cars_availabe_airport+no_cars_availabe_city))
```

```
## [1] "5%"
```
        
### 5.8. Percentage of Cabs Cancellation in Morning at Pickup Point

```r
Cancelled_airport <- length(which((uber$Pickup.point=="Airport") & (uber$Time_Slot == "Morning") & (uber$Status == "Cancelled")))
Cancelled_city <- length(which((uber$Pickup.point=="City") & (uber$Time_Slot == "Morning") & (uber$Status == "Cancelled")))
```

#### At Aiport

```r
percent(Cancelled_airport/(Cancelled_airport+Cancelled_city))
```

```
## [1] "4%"
```

#### At City

```r
percent(Cancelled_city/(Cancelled_airport+Cancelled_city))
```

```
## [1] "96%"
```
    







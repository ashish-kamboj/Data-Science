## Importing Library
library(dplyr)
library(ggplot2)
library(readxl)

## Importing the dataset
Online.Retail  <- read_excel("Online Retail.xlsx")

## NA value treatment
order_wise <- na.omit(Online.Retail)

## Making RFM data
Amount <- order_wise$Quantity * order_wise$UnitPrice
order_wise <- cbind(order_wise,Amount)

order_wise <- order_wise[order(order_wise$CustomerID),]
monetary <- aggregate(Amount~CustomerID, order_wise, sum)

frequency <- order_wise[,c(7,1)]
k<-table(as.factor(frequency$CustomerID))
k<-data.frame(k)

colnames(k)[1]<-c("CustomerID")
master <-merge(monetary,k,by="CustomerID")

recency <- order_wise[,c(7,5)]
recency$InvoiceDate<-as.Date(recency$InvoiceDate,"%d-%m-%Y %H:%M")

maximum<-max(recency$InvoiceDate)
maximum<-maximum+1
maximum$diff <-maximum-recency$InvoiceDate

recency$diff<-maximum$diff
df<-aggregate(recency$diff,by=list(recency$CustomerID),FUN="min")

colnames(df)[1]<- "CustomerID"
colnames(df)[2]<- "Recency"

RFM <- merge(monetary, k, by = ("CustomerID"))
RFM <- merge(RFM, df, by = ("CustomerID"))
RFM$Recency <- as.numeric(RFM$Recency)

## Outlier treatment
box <- boxplot.stats(RFM$Amount)
out <- box$out

RFM1 <- RFM[ !RFM$Amount %in% out, ]
RFM <- RFM1

box <- boxplot.stats(RFM$Freq)
out <- box$out

RFM1 <- RFM[ !RFM$Freq %in% out, ]
RFM <- RFM1

box <- boxplot.stats(RFM$Recency)
out <- box$out

RFM1 <- RFM[ !RFM$Recency %in% out, ]
RFM <- RFM1

## Standardization of data
RFM_norm1<- RFM[,-1]

RFM_norm1$Amount <- scale(RFM_norm1$Amount)
RFM_norm1$Freq <- scale(RFM_norm1$Freq)
RFM_norm1$Recency <- scale(RFM_norm1$Recency)
View(RFM_norm1)

## Implementing K-Means algorithm
clus3 <- kmeans(RFM_norm1, centers = 3, iter.max = 50, nstart = 50)

## Finding the optimal value of K
r_sq<- rnorm(20)

for (number in 1:20){clus <- kmeans(RFM_norm1, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

## Running the K-Means algorithm for K =4,5,6
clus4 <- kmeans(RFM_norm1, centers = 4, iter.max = 50, nstart = 50)
clus5 <- kmeans(RFM_norm1, centers = 5, iter.max = 50, nstart = 50)
clus6 <- kmeans(RFM_norm1, centers = 6, iter.max = 50, nstart = 50)

## Appending the ClusterIDs to RFM data
RFM_km <-cbind(RFM,clus5$cluster)
colnames(RFM_km)[5]<- "ClusterID"

## Cluster Analysis
km_clusters<- group_by(RFM_km, ClusterID)
tab1<- summarise(km_clusters, Mean_amount=mean(Amount), Mean_freq=mean(Freq), Mean_recency=mean(Recency))

## Plot
ggplot(tab1, aes(x= factor(ClusterID), y=Mean_amount)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(ClusterID), y=Mean_freq)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(ClusterID), y=Mean_recency)) + geom_bar(stat = "identity")

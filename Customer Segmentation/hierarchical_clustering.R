## Importing Library
library(dplyr)

## Importing the dataset
Online.Retail <- read.csv("Online Retail.csv", stringsAsFactors=FALSE)

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


## Implementing Hierarchical clustering

## Calculating the distance matrix
RFM_dist<- dist(RFM_norm1)

## Constructing the dendrogram using single linkage
RFM_hclust1<- hclust(RFM_dist, method="single")
plot(RFM_hclust1)

## Constructing the dendrogram using complete linkage
RFM_hclust2<- hclust(RFM_dist, method="complete")
plot(RFM_hclust2)

## Visualising the cut in the dendrogram
rect.hclust(RFM_hclust2, k=5, border="red")

## Making the cut in the dendrogram
clusterCut <- cutree(RFM_hclust2, k=5)

## Appending the ClusterIDs to RFM data
RFM_hc <-cbind(RFM,clusterCut)
colnames(RFM_hc)[5]<- "ClusterID"

## Cluster Analysis
hc_clusters<- group_by(RFM_hc, ClusterID)

tab2<- summarise(hc_clusters, Mean_amount=mean(Amount), Mean_freq=mean(Freq), Mean_recency=mean(Recency))
ggplot(tab2, aes(x= factor(ClusterID), y=Mean_recency)) + geom_bar(stat = "identity")
ggplot(tab2, aes(x= factor(ClusterID), y=Mean_amount)) + geom_bar(stat = "identity")
ggplot(tab2, aes(x= factor(ClusterID), y=Mean_freq)) + geom_bar(stat = "identity")
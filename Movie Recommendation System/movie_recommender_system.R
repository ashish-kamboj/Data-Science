
#Packages to be used:
#recommenderlab
#ggplot

#In this case study, we will look at :

#Loading movie lens package from recommenderlabs

library(recommenderlab)
data("MovieLense")

class(MovieLense)

r <- as(MovieLense, "realRatingMatrix")

# get some informtaion
dimnames(r)
rowCounts(r)
colCounts(r)
rowMeans(r)

# coerce the matrix to a dataframe
movies_df <- as(r, "data.frame")
str(movies_df)

#How similar are the first ten users are with each other

similar_users <- similarity(MovieLense[1:10, ],
                               method = "cosine",
                               which = "users")


#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#Inference 
#Users 1, 3, 4 and 9 are similar

#How similar are the first five items are with each other

similar_items <- similarity(MovieLense[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

#You can notice more items being similar :Eg items 2 and 6 are similar; 5 and 10 are similar etc.

#--------------------------Understand users and ratings----------#

# Visualizing ratings
library(ggplot2)
qplot(getRatings(MovieLense), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(MovieLense)) # Skewed to the right

qplot(getRatings(normalize(MovieLense, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(MovieLense, method = "Z-score"))) # seems better


qplot(rowCounts(MovieLense), binwidth = 10, 
      main = "Movies Rated on average", 
      xlab = "# of users", 
      ylab = "# of movies rated")
#Most users rate less number of movies.
#Very few users have rated more movies



#--------------------------Recommendation models ----------------#

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models


#description of recommendation system algorithms/models used
lapply(recommender_models, "[[", "description")

#This gives you different types of recommendation models
#In this case study , let's compare user based and item based
#collaborative filtering

#checking the parameters of these two models
recommender_models$IBCF_realRatingMatrix$parameters

#Divide data into test 
scheme <- evaluationScheme(MovieLense, method = "split", train = .9,
                           k = 1, given = 2, goodRating = 4)
?evaluationScheme

#--arguments
#train and test
#Here we create an evaluation scheme which splits the users 
#into a training set (90%) and a test set (10%). 

#given 
#For the test set 10 items(given argument) will be given to the
#recommender algorithm and the other items will be held out for computing the error

#With goodRating=4 all items with actual user rating of greater or equal 4 are considered 
#positives in the evaluation process

scheme

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results)

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")


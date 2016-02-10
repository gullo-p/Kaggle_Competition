library(class)
library(chron)
library(randomForest)
setwd("~/Documents/BGSE/Assignatures/GSE-2n/Advanced_computational_methods/Kaggle/Kaggle_Competition/DATA")
train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")


#Get the labels
labels <- train$popularity
labels <- as.factor(labels)
train$popularity <- NULL


#Clean data a little bit
train$n_non_stop_words <- NULL
train$rate_negative_words <- NULL
test$n_non_stop_words <- NULL
test$rate_negative_words <- NULL
train$n_unique_tokens[train$n_unique_tokens > 1] <- 1
train$n_non_stop_unique_tokens[train$n_non_stop_unique_tokens > 1] <- 1


#Add year and month
train$year <- as.numeric(substring(train$url, 21,24))
train$month <- as.numeric(substring(train$url, 26,27))
test$year <- as.numeric(substring(test$url, 21,24))
test$month <- as.numeric(substring(test$url, 26,27))


#Random forest computation
rf <- randomForest(x = train[1:3000,3:61], y = labels[1:3000], 
                   xtest = test[,3:61],  ntree=100,nPerm=2,mtry=3,
                   proximity=TRUE,importance=TRUE)

pred <- rf$test$predicted
sum(popularity$popularity == pred)/length(popularity$popularity)

#0.5031107

library(class)
library(chron)
library(randomForest)
setwd("~/Documents/BGSE/Assignatures/GSE-2n/Advanced_computational_methods/Kaggle/Kaggle_Competition/DATA")
train3 <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test3 <- read.csv("../DATA/news_popularity_test.csv", sep = ",")


#Get the labels
labels <- ifelse(train3$popularity == 1, 1, 0)
labels <- as.factor(labels)
train3$popularity <- NULL


#Clean data a little bit
train3$n_non_stop_words <- NULL
train3$rate_negative_words <- NULL
test3$n_non_stop_words <- NULL
test3$rate_negative_words <- NULL
train3$n_unique_tokens[train3$n_unique_tokens > 1] <- 1
train3$n_non_stop_unique_tokens[train3$n_non_stop_unique_tokens > 1] <- 1



#Add year and month
train3$year <- ifelse(as.numeric(substring(train3$url, 21,24)) == 2013, 0, 1)
train3$month <- as.numeric(substring(train3$url, 26,27))
test3$year <- ifelse(as.numeric(substring(test3$url, 21,24))  == 2013, 0, 1)
test3$month <- as.numeric(substring(test3$url, 26,27))
test3$day <- as.numeric(substring(test3$url, 29,30))
train3$day <- as.numeric(substring(train3$url, 29,30))
train3$month <- as.factor(train3$month)
test3$month <- as.factor(test3$month)
train3$day <- as.factor(train3$day)
test3$day <- as.factor(test3$day)


#Create new variable channel for train
train$news_category<-"Other"
train$news_category[which(train$data_channel_is_lifestyle == 1)]<-"Lifestyle"
train$news_category[which(train$data_channel_is_entertainment == 1)]<-"Entertainment"
train$news_category[which(train$data_channel_is_bus == 1)]<-"Business"
train$news_category[which(train$data_channel_is_socmed == 1)]<-"Social Media"
train$news_category[which(train$data_channel_is_tech == 1)]<-"Tech"
train$news_category[which(train$data_channel_is_world == 1)]<-"World"


#Create new variable channel for test
test$news_category<-"Other"
test$news_category[which(test$data_channel_is_lifestyle == 1)]<-"Lifestyle"
test$news_category[which(test$data_channel_is_entertainment == 1)]<-"Entertainment"
test$news_category[which(test$data_channel_is_bus == 1)]<-"Business"
test$news_category[which(test$data_channel_is_socmed == 1)]<-"Social Media"
test$news_category[which(test$data_channel_is_tech == 1)]<-"Tech"
test$news_category[which(test$data_channel_is_world == 1)]<-"World"

#Add new channel
train3$data_channel_is_other <- ifelse(train$news_category == "Other", 1, 0)
test3$data_channel_is_other <- ifelse(test$news_category == "Other", 1, 0)


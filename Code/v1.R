train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv")
#tables
table(train$popularity)
round(prop.table(table(train$popularity)) * 100, digits = 1)


#normalize
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
train_norm <- as.data.frame(lapply(train[4:61], normalize))
train_norm <- cbind(train[1:3],train_norm,train$popularity)



#Sample
ind <- sample(2, nrow(train), replace=TRUE, prob=c(0.67, 0.33))
train.training <- train[ind==1, 1:61]
train.test <- train[ind==2, 1:61]
train.trainLabels <- train[ind==1, 62]
train.testLabels <- train[ind==2, 62]



#kkn
train_pred <- knn(train = train.training, test = train.test, cl = train.trainLabels, k=3)




for(i in 1:nrow(train)){
  if(train$popularity[i] == 1)
    train$target1[i] <- 1
  else
    train$target1[i] <- 0
}

for(i in 1:nrow(train)){
  if(train$popularity[i] == 2)
    train$target2[i] <- 1
  else
    train$target2[i] <- 0
}

for(i in 1:nrow(train)){
  if(train$popularity[i] == 3)
    train$target3[i] <- 1
  else
    train$target3[i] <- 0
}

for(i in 1:nrow(train)){
  if(train$popularity[i] == 4)
    train$target4[i] <- 1
  else
    train$target4[i] <- 0
}

for(i in 1:nrow(train)){
  if(train$popularity[i] == 5)
    train$target5[i] <- 1
  else
    train$target5[i] <- 0
}

datafit1 <- lm(target1 ~ num_hrefs + title_sentiment_polarity + global_rate_negative_words+1, data=train)
weights1 <- coef(datafit1)[c("num_hrefs","title_sentiment_polarity", "global_rate_negative_words")]
bias1 <- coef(datafit1)[1]

datafit2 <- lm(target2 ~ num_hrefs + title_sentiment_polarity + global_rate_negative_words +1, data=train)
weights2 <- coef(datafit2)[c("num_hrefs","title_sentiment_polarity", "global_rate_negative_words")]
bias2 <- coef(datafit2)[1]

datafit3 <- lm(target3 ~  num_hrefs + title_sentiment_polarity + global_rate_negative_words +1, data=train)
weights3 <- coef(datafit3)[c("num_hrefs","title_sentiment_polarity", "global_rate_negative_words")]
bias3 <- coef(datafit3)[1]

datafit4 <- lm(target4 ~  num_hrefs + title_sentiment_polarity + global_rate_negative_words +1, data=train)
weights4 <- coef(datafit4)[c("num_hrefs","title_sentiment_polarity", "global_rate_negative_words")]
bias4 <- coef(datafit4)[1]

datafit5 <- lm(target5 ~  num_hrefs + title_sentiment_polarity + global_rate_negative_words +1, data=train)
weights5 <- coef(datafit5)[c("num_hrefs","title_sentiment_polarity", "global_rate_negative_words")]
bias5 <- coef(datafit5)[1]

fitted.values1 <- bias1 + weights1[1] * test$num_hrefs + weights1[2] * test$title_sentiment_polarity + weights1[3] * test$global_rate_negative_words
length(which(fitted.values1 > 0.5))

fitted.values2 <- bias2 + weights2[1] * test$num_hrefs + weights2[2] * test$title_sentiment_polarity + weights2[3] * test$global_rate_negative_words
length(which(fitted.values2 > 0.5))

fitted.values3 <- bias3 +  weights3[1] * test$num_hrefs + weights3[2] * test$title_sentiment_polarity + weights3[3] * test$global_rate_negative_words
length(which(fitted.values3 > 0.5))

fitted.values4 <- bias4 +  weights4[1] * test$num_hrefs + weights4[2] * test$title_sentiment_polarity + weights4[3] * test$global_rate_negative_words
length(which(fitted.values4 > 0.5))

fitted.values5 <- bias5 +  weights5[1] * test$num_hrefs + weights5[2] * test$title_sentiment_polarity + weights5[3] * test$global_rate_negative_words
length(which(fitted.values5 > 0.5))


for(i in 1:nrow(test))
max[i] <- max(fitted.values5[i], fitted.values4[i], fitted.values3[i], fitted.values2[i], fitted.values1[i])




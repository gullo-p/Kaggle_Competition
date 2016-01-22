train <- read.csv("news_popularity_training.csv", sep = ",")

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

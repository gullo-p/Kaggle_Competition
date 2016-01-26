library(class)
train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")

#tables
table(train$popularity)
round(prop.table(table(train$popularity)) * 100, digits = 1)



#STANDARDIZED VERSION  +  YEAR AND MONTH (THE ONE WHO WORKED BEST)
standardize <- function(x) {
  num <- x - mean(x)
  denom <-sd(x)
  return (num/denom)
}

train_stand <- as.data.frame(lapply(train[4:61], standardize))

#obtain year and month from url
train$year <- sapply(train$url, FUN=function(x) {as.numeric(substring(x, 21,24))})
train$month <- sapply(train$url, FUN=function(x) {as.numeric(substring(x, 26,27))})

train_stand <- cbind(train$id, train$url, train$timedelta,train_stand,train$year, train$month, train$popularity)


ind <- sample(2, nrow(train_stand), replace=TRUE, prob=c(0.67, 0.33))
train.training <- train_stand[ind==1, 3:63]
train.test <- train_stand[ind==2, 3:63]
train.trainLabels <- train_stand[ind==1, 64]
train.testLabels <- train_stand[ind==2, 64]

#kkn
train_pred <- knn(train = train.training, test = train.test, cl = train.trainLabels, k=20)
train_pred <- as.data.frame(train_pred)

#compare the results with the truth
result <- cbind(train_pred, train.testLabels)

#rate of correctness
sum(result$train_pred == train.testLabels)/10008

test_stand <- as.data.frame(lapply(test[4:61], standardize))

#obtain year and month from url
test$year <- sapply(test$url, FUN=function(x) {as.numeric(substring(x, 21,24))})
test$month <- sapply(test$url, FUN=function(x) {as.numeric(substring(x, 26,27))})

test_stand <- cbind(test[1:3],test_stand , test$year , test$month)

#real test to submit
real_train_pred <- knn(train=train_stand[,3:63], test = test_stand[,3:63], cl = train_stand[,64], k=20)
real_train_pred <- as.data.frame(real_train_pred)

#data frame to submit
submit <- cbind(c(30001:39644), real_train_pred)
names(submit) <- c("id", "popularity")

write.csv(submit, file = "submit3.csv", quote = FALSE, row.names = FALSE)


----------------------------------------------------

#NORMALIZED VERSION 
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
 return (num/denom)
}
train_norm <- as.data.frame(lapply(train[4:61], normalize))
train_norm <- cbind(train[1:3],train_norm,train$popularity)



#Sample
ind <- sample(2, nrow(train), replace=TRUE, prob=c(0.67, 0.33))
train.training <- train[ind==1, 3:61]
train.test <- train[ind==2, 3:61]
train.trainLabels <- train[ind==1, 62]
train.testLabels <- train[ind==2, 62]


#kkn
train_pred <- knn(train = train.training, test = train.test, cl = train.trainLabels, k=20)
train_pred <- as.data.frame(train_pred)

#compare the results with the truth
result <- cbind(train_pred, train.testLabels)

#rate of correctness
sum(result$train_pred == train.testLabels)/9912

#real test to submit
real_train_pred <- knn(train=train[,3:61], test = test[,3:61], cl = train[,62], k=20)
real_train_pred <- as.data.frame(real_train_pred)

#data frame to submit
submit <- cbind(c(30001:39644), real_train_pred)
names(submit) <- c("id", "popularity")

write.csv(submit, file = "submit1.csv", quote = FALSE, row.names = FALSE)


#normalized version
ind <- sample(2, nrow(train_norm), replace=TRUE, prob=c(0.67, 0.33))
train.training <- train_norm[ind==1, 3:61]
train.test <- train_norm[ind==2, 3:61]
train.trainLabels <- train_norm[ind==1, 62]
train.testLabels <- train_norm[ind==2, 62]

#kkn
train_pred <- knn(train = train.training, test = train.test, cl = train.trainLabels, k=20)
train_pred <- as.data.frame(train_pred)

#compare the results with the truth
result <- cbind(train_pred, train.testLabels)

#rate of correctness
sum(result$train_pred == train.testLabels)/9952

-----------------------------------------------------------------

#STANDARDIZED VERSION 
standardize <- function(x) {
  num <- x - mean(x)
  denom <-sd(x)
  return (num/denom)
}
train_stand <- as.data.frame(lapply(train[4:61], standardize))
train_stand <- cbind(train[1:3],train_stand,train$popularity)


ind <- sample(2, nrow(train_stand), replace=TRUE, prob=c(0.67, 0.33))
train.training <- train_stand[ind==1, 3:61]
train.test <- train_stand[ind==2, 3:61]
train.trainLabels <- train_stand[ind==1, 62]
train.testLabels <- train_stand[ind==2, 62]

#kkn
train_pred <- knn(train = train.training, test = train.test, cl = train.trainLabels, k=20)
train_pred <- as.data.frame(train_pred)

#compare the results with the truth
result <- cbind(train_pred, train.testLabels)

#rate of correctness
sum(result$train_pred == train.testLabels)/10008

test_stand <- as.data.frame(lapply(test[4:61], standardize))
test_stand <- cbind(test[1:3],test_stand)

#real test to submit
real_train_pred <- knn(train=train_stand[,3:61], test = test_stand[,3:61], cl = train[,62], k=20)
real_train_pred <- as.data.frame(real_train_pred)

#data frame to submit
submit <- cbind(c(30001:39644), real_train_pred)
names(submit) <- c("id", "popularity")

write.csv(submit, file = "submit2.csv", quote = FALSE, row.names = FALSE)
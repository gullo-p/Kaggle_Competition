library(class)

#get the data
train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")

#tables
table(train$popularity)
round(prop.table(table(train$popularity)) * 100, digits = 1)

###############################################################
##DATA MANIPULATION

#standardize the continuous features
standardize <- function(x) {
  num <- x - mean(x)
  denom <-sd(x)
  return (num/denom)
}

#categorical standardization
cat_stand <- function(x){
  num <- x - 1/2
  denom <- length(unique(x))
  return(num/denom)
}



#apply the changes
train_stand <- as.data.frame(lapply(train[4:61], standardize))

#obtain date & holiday variables
#New years, Martin Luther, Washington's Birthday, Memorial day, Independence, labor, Columbus
#Veterans, Thanksgiving, Christmas
myholidays  <- dates(c("2013-01-01","2013-01-21","2013-02-18" , "2013-05-27", "2013-07-04", 
                       "2013-09-02", "2013-10-14", "2013-11-11", "2013-11-28", 
                       "2013-12-25", "2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", 
                       "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27" ,
                       "2014-12-25"), format="Y-M-D")

train$year <- sapply(train$url, FUN=function(x) {as.numeric(substring(x, 21,24))})
train$month <- sapply(train$url, FUN=function(x) {as.numeric(substring(x, 26,27))})
train$day <- sapply(train$url, FUN=function(x) {as.numeric(substring(x, 29,30))})
train$date <- dates(paste(train$year,train$month,train$day, sep = "-"), format="Y-M-D")
train$is_holiday <- is.holiday(as.Date(train$date),myholidays)

train_stand <- cbind(train$id, train$url, train$timedelta,train_stand,train$year, train$month, 
                     train$is_holiday, train$popularity)



####################
#FEATURE EXTRACTION







####################
#CROSS VALIDATION

#test and cross validations
ind <- sample(2, nrow(train_stand), replace=TRUE, prob=c(0.75, 0.25))
train.training <- train_stand[ind==1, 3:64]
train.test <- train_stand[ind==2, 3:64]
train.trainLabels <- train_stand[ind==1, 65]
train.testLabels <- train_stand[ind==2, 65]

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
test$day <- sapply(test$url, FUN=function(x) {as.numeric(substring(x, 29,30))})
test$date <- dates(paste(test$year,test$month,test$day, sep = "-"), format="Y-M-D")
test$is_holiday <- is.holiday(as.Date(test$date),myholidays)

test_stand <- cbind(test[1:3],test_stand , test$year , test$month, test$is_holiday)

#real test to submit
real_train_pred <- knn(train=train_stand[,3:64], test = test_stand[,3:64], cl = train_stand[,65], k=20)
real_train_pred <- as.data.frame(real_train_pred)

#data frame to submit
submit <- cbind(c(30001:39644), real_train_pred)
names(submit) <- c("id", "popularity")

write.csv(submit, file = "submit4.csv", quote = FALSE, row.names = FALSE)



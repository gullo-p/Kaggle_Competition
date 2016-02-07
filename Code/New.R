if(!require("class")) install.packages("class"); library(class)
if(!require("chron")) install.packages("chron"); library(chron)

#get the data
train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")

#tables
table(train$popularity)
round(prop.table(table(train$popularity)) * 100, digits = 1)

###############################################################
##DATA MANIPULATION

#transform number of images & videos into 3-categorical variables (0, 1 or more than 1)
three.cat <- function(x){
  
  for(i in 1:length(x)){
    if(x[i] > 2) x[i] <- 2
    
  }
  return(x)
}

train$num_imgs <- three.cat(train$num_imgs)
train$num_videos <- three.cat(train$num_videos)


#Remove non-sense or redundant features: n-non-stop-words, rate negative_words, abs_title_sent_polarity
train_clean <- train[,-c(7,51,61) ] 

test_clean <- test[, -c(7,51,61) ]



#obtain date & holiday variables
#New years, Martin Luther, Washington's Birthday, Memorial day, Independence, labor, Columbus
#Veterans, Thanksgiving, Christmas

obtain.date <- function(dataset){
  myholidays  <- as.Date(c("2013-01-01","2013-01-21","2013-02-18" , "2013-05-27", "2013-07-04", 
                       "2013-09-02", "2013-10-14", "2013-11-11", "2013-11-28", 
                       "2013-12-25", "2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", 
                       "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27" ,
                       "2014-12-25"))

  year <- sapply(dataset$url, FUN=function(x) {as.numeric(substring(x, 21,24))})
  month <- sapply(dataset$url, FUN=function(x) {as.numeric(substring(x, 26,27))})
  day <- sapply(dataset$url, FUN=function(x) {as.numeric(substring(x, 29,30))})
  date <- paste(year,month,day, sep = "-")
  date <- as.Date(date)
  is_holiday <- is.holiday(date,myholidays)
  
  a <- as.data.frame(cbind(year,month, day, date, is_holiday))
  
  return(a)

}
?is.holiday
#obtain dates for training set
obtained.info <- obtain.date(train_clean)


#append the new created features
train_clean <- cbind(train_clean[,-59], obtained.info$year, obtained.info$month, 
                     obtained.info$is_holiday, train_clean$popularity)



######Standardization of the features
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


#apply the changes (to be corrected)
train_stand1 <- as.data.frame(lapply(train[3:14], standardize))
train_stand2 <- as.data.frame(lapply(train[15:20], cat_stand))
train_stand3 <- as.data.frame(lapply(train[21:32], standardize))
train_stand4 <- as.data.frame(lapply(train[33:40], cat_stand))
train_stand5 <- as.data.frame(lapply(train[41:61], standardize))
train_stand <- cbind(train_stand1, train_stand2, train_stand3, train_stand4, train_stand5)




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



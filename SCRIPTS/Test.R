library(MedMast)

# DATA INPUT

train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")

test$popularity <- NA

dataset <- rbind(train,test)

# DATA CLEANING
# remove outlier 
dataset <- dataset[-which(train$n_unique_tokens == 701),]

# Remove non-sense or redundant features: 
# Remove the almost constant column
dataset$n_non_stop_words <- NULL

# # Remove the rate negative_words
dataset$rate_negative_words <- NULL

dataset$flag <- 0
dataset$flag[is.na(dataset$popularity)] <- 1

# order the dataset by date
time.data <- dataset[order(dataset$timedelta,decreasing = TRUE),]
time.data$ts <- as.numeric(731 - time.data$timedelta)

# initialize a new feature which flags the missing values in the data
time.data$missing.flag <- 0
time.data$missing.flag[time.data$n_tokens_content == 0] <- 1
time.data$missing.flag[time.data$global_subjectivity == 0] <- 1

# add the date variables (year, month and is.holiday)
time.data <- obtain.date(time.data)

# add the quarter variable
time.data$quarter <- cut(time.data$ts,breaks=8,labels=1:8)

# Convert the response to ordinal
time.data$popularity <- factor(time.data$popularity, ordered = TRUE)

# PREDICTIONS

rf.predictions <- rolling.windows.rf(dataset = time.data, ntree = 200)

colnames(rf.predictions) <- c("id","popularity")

#write.table(rf.predictions,"submitl.csv",sep=",",row.names = F)

submit <- rf.predictions

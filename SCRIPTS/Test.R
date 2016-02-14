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
# dataset$n_non_stop_words <- NULL

# Remove the rate of negative words
dataset$rate_negative_words <- NULL

# Remove correlated features
# dataset$is_weekend <- NULL
# dataset$weekday_is_sunday <- NULL
# dataset$LDA_04 <- NULL
# dataset$kw_min_min <- NULL

dataset$flag <- 0
dataset$flag[is.na(dataset$popularity)] <- 1

# order the dataset by date
time.data <- dataset[order(dataset$timedelta,decreasing = TRUE),]
time.data$ts <- as.numeric(731 - time.data$timedelta)

# initialize a new feature which flags the missing values in the data
time.data$missing.flag <- 0
time.data$missing.flag[time.data$n_tokens_content == 0] <- 1
time.data$missing.flag[time.data$global_subjectivity == 0] <- 1

#time.data <- obtain.date(time.data)
#time.data$quarter <- cut(time.data$ts,breaks=8,labels=1:8)

# initialize a binary feature which is 1 for the news whose data channel is "other"
time.data$data_channel_is_others <- 0
time.data$data_channel_is_others[which(rowSums(time.data[,c(14:19)])==0)] <- 1

# Convert the response to ordinal
time.data$popularity <- factor(time.data$popularity, ordered = TRUE)



# PREDICTIONS

rf.predictions <- rolling.windows.rf(dataset = time.data)

colnames(rf.predictions) <- c("id","popularity")

write.table(rf.predictions,"submit.csv",sep=",",row.names = F)


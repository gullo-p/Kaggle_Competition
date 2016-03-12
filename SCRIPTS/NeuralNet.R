library(MedMast)
library(MASS)

train <- read.csv("Kaggle_Competition/DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("Kaggle_Competition/DATA/news_popularity_test.csv", sep = ",")

test$popularity <- NA

dataset <- rbind(train,test)

# remove outlier 
dataset <- dataset[-which(train$n_unique_tokens == 701),]

# Remove non-sense or redundant features: 
# Remove the constant column
dataset$n_non_stop_words <- NULL

# Remove the rate negative_words
dataset$rate_negative_words <- NULL

dataset$is_weekend <- NULL
dataset$weekday_is_sunday <- NULL
dataset$LDA_04 <- NULL
dataset$kw_min_min <- NULL

time.data <- dataset
time.data$ts <- as.numeric(731 - time.data$timedelta)

time.data$missing.flag <- 0
time.data$missing.flag[time.data$n_tokens_content == 0] <- 1
time.data$missing.flag[time.data$global_subjectivity == 0] <- 1

time.data <- obtain.date(time.data)
time.data$quarter <- as.numeric(cut(time.data$ts,breaks=8,labels=1:8))

time.data$popularity <- factor(time.data$popularity)

flag <- is.na(time.data$popularity)

features <- time.data[,-which(colnames(time.data) %in% c("id","url","timedelta","popularity"))]
labels <- time.data$popularity

#features <- handle.missing(features,type = "impute")

features.con <- apply(features[,c(1:10,17:27,34:53)],2,standardize,type = "continuous")
features.dis <- apply(features[,-c(1:10,17:27,34:53)],2,standardize,type = "discrete")

std.features <- data.frame(cbind(features.con,features.dis))

#train.features <- fisher.selection(std.features[flag == FALSE,], labels[flag == FALSE], 20 , 1)

#imp.features <- std.features[,which(colnames(std.features) %in% colnames(train.features))]

train.data <- cbind(std.features[flag == FALSE,],popularity = class.ind(labels[flag == FALSE]))
test.data <- std.features[flag == TRUE,]

library(neuralnet)
n <- names(train.data)
f <- as.formula(paste(paste("popularity",1:5,sep=".",collapse = " + ")," ~", paste(n[1:58], collapse = " + ")))
nn <- neuralnet(f,data=train.data, hidden = 20, linear.output = FALSE)

nn1 <- neuralnet(f,data=train.data, threshold = 0.1, stepmax = 1000 ,linear.output = FALSE)

pr.nn <- compute(nn1,test.data)

pr.nn.mod <- pr.nn$net.result


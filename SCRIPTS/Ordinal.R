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

time.data <- dataset[order(dataset$timedelta,decreasing = TRUE),]
time.data$ts <- as.numeric(731 - time.data$timedelta)

#time.data$missing.flag <- 0
#time.data$missing.flag[time.data$n_tokens_content == 0] <- 1
#time.data$missing.flag[time.data$global_subjectivity == 0] <- 1

#time.data <- obtain.date(time.data)
#time.data$quarter <- cut(time.data$ts,breaks=8,labels=1:8)

time.data$popularity <- factor(time.data$popularity, ordered = TRUE)

flag <- is.na(time.data$popularity)

features <- time.data[,c(4:55)]
labels <- time.data$popularity

#features <- handle.missing(features,type = "impute")

features.con <- apply(features[,c(1:10,17:27,34:52)],2,standardize,type = "continuous")
features.dis <- apply(features[,-c(1:10,17:27,34:52)],2,standardize,type = "discrete")

imp.features <- data.frame(cbind(features.con,features.dis))

train.features <- fisher.selection(imp.features[flag == FALSE,], labels[flag == FALSE], 20 , 1)

imp.features <- imp.features[,which(colnames(imp.features) %in% colnames(train.features))]

rfpred <- data.frame(id = time.data$id[flag == TRUE])
acc <- rep(0,30)

for(i in 1:30){
  
  start <- (1+((i-1)*1000))
  end <- ifelse(i < 30, (10000 + ((i-1)*1000)),nrow(time.data))
  index <- start:end
  data <- imp.features[index,]
  x = data[flag[index] == FALSE,]
  y = labels[index]
  y = y[flag[index] == FALSE]
  xtest = data[flag[index] == TRUE,]
  ytest = data.frame(time.data[index,c("id")])
  ytest = data.frame(id = ytest[flag[index] == TRUE,])
  
  ntrain = length(y)
  ntest = nrow(xtest)
  
  train.data = cbind(x,y)
  
  ord.logit <- polr( y ~ . , data = train.data,Hess = TRUE)
  
  ypred <- factor(predict(ord.logit, train.data, type = "c"),levels = 1:5,ordered = TRUE)
  
  tpred <- factor(predict(ord.logit, xtest, type = "c"),levels = 1:5,ordered = TRUE)
  
  acc[i] = sum(y == ypred)/ntrain
  
  ytest$predicted <- tpred
  colnames(ytest)[2] <- paste0("X",i)
  rfpred <- merge(rfpred,ytest,sort=T,all.x=T)
  
}

rfpred$prediction <- apply(rfpred[,-1],1,function(x) as.numeric(names(tail(sort(table(x)),1))))

plot(acc)

predictions = rfpred[,c(1,32)]
colnames(predictions) <- c("id","popularity")

write.table(predictions,"submit.csv",sep=",",row.names = F)

# DATA INPUT

train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")

test$popularity <- NA

dataset <- rbind(train,test)

features <- dataset[,-which(colnames(dataset) %in% c("popularity"))]
labels <- as.numeric(dataset$popularity)



# DATA MANIPULATION & CLEANING

#transform number of images & videos into 3-categorical variables (0, 1 or more than 1)

features$num_imgs <- as.numeric(cut(features$num_imgs,breaks=c(0,1,2,Inf),include.lowest = TRUE , right = FALSE , labels = c(0,1,2)))
features$num_videos <- as.numeric(cut(features$num_videos,breaks=c(0,1,2,Inf),include.lowest = TRUE , right = FALSE , labels = c(0,1,2)))

# Remove non-sense or redundant features: 
# Remove the constant column
features$n_non_stop_words <- NULL

# Remove the rate of negative_words
features$rate_negative_words <- NULL

# Remove the ukrain outlier
# features <- features[-which(features$n_unique_tokens > 1),]
# labels <- labels[-which(dataset$n_unique_tokens > 1)]

# Handle Missing Values
features <- handle.missing(features,type = "remove")

#obtain date & holiday variables
#New years, Martin Luther, Washington's Birthday, Memorial day, Independence, labor, Columbus
#Veterans, Thanksgiving, Christmas

#obtain dates for training set
features <- obtain.date(features)

# Remove the url field
features$url <- NULL

# Standardization of the features
features.con <- apply(features[,c(2:8,11:12,19:30,39:58)],2,standardize,type = "continuous")
features.dis <- apply(features[,-c(1:8,11:12,19:30,39:58)],2,standardize,type = "discrete")

features <- data.frame(features.con, features.dis)

# FEATURE SELECTION

features$id <- as.numeric(row.names(features))
train.features <- merge(features,data.frame(id = train$id))
train.labels <- labels[train.features$id]
test.features <-  merge(features,data.frame(id = test$id))

# Fisher scoring method for feature selection
fish.features <- fisher.selection(train.features,train.labels,n = 20,threshold = 2)

# Lasso method for feature selection
lass.features <- lasso.model(train.features,train.labels,type = "select",dfmax = 20)

#######################################################################################
# MODELLING
#Try a knn with the 20 variables selected by lasso
train.clean = train.clean[,which(colnames(train.clean) %in% c(topvar,"popularity"))]
test.clean = features_clean[30001:39644,which(colnames(test.clean) %in% topvar)]

real <- knn(train=train.clean[,-21], test = test.clean, cl = train.clean$popularity, k=20)
sum(real == final$popularity )/9644
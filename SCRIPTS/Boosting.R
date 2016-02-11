if(!require("adabag")) install.packages("adabag"); library(adabag)
if(!require("rpart")) install.packages("rpart"); library(rpart)
setwd("/Users/guglielmo/Desktop/BGSE/Kaggle_Competition/DATA")

train <- read.csv("news_popularity_training.csv", sep = ",")
test <- read.csv("news_popularity_test.csv", sep = ",")


train <- train[, 3:62]
train$popularity <- factor(train$popularity)
adaboost <- boosting(popularity~., train, boos=TRUE, mfinal = 100)

res <- predict( adaboost, newdata=test )

fit <- res$class

final <- read.csv("/Users/guglielmo/Desktop/final_competition/final.csv", header = TRUE, sep = ",")

sum(fit== final$popularity )/9644 #50.22812 %

# Now let's try with cleaned but not standardized data

#FEATURE SELECTION (CREATE A DIFFERENT SCRIPT FOR FISHER SCORING)

train.clean <- data.frame(imp.features,popularity = as.numeric(dataset$popularity))[1:30000,]
test.clean <-imp.features[-c(1:30000),]

#Split into popular and not popular
train.clean$dummy[train.clean$popularity < 2] <- 0
train.clean$dummy[train.clean$popularity > 1] <- 1


# Compute the Fisher Scoring for each of the feature
fisher.rank <- function(feature,label){
  num <- (mean(feature[label == 0]) - mean(feature[label == 1]))^2
  denom <- var(feature[label == 0]) + var(feature[label == 1])
  rank <- round(num/denom,4)
  return(rank)
}

# Actual Fisher Scoring
X = train.clean[,-which(colnames(train.clean) %in% c("popularity","dummy"))]
fisher.score = apply(X,2,function(x)fisher.rank(x,train.clean$dummy))
names(fisher.score) = colnames(X)

# Ranking of the top 40 features selected
top.ranks = fisher.score[order(fisher.score,decreasing = T)]
top.vars = names(top.ranks[1:20])

train.clean$popularity <- factor(train.clean$popularity)
test.clean$popularity <- factor(test.clean$popularity)
# Subset the training and test sets with the selected features
train.clean = train.clean[,which(colnames(train.clean) %in% c(top.vars,"popularity"))]
test.clean = imp.features[30001:39644,which(colnames(test.clean) %in% top.vars)]
train.clean$popularity <- factor(train.clean$popularity)

adaboost <- boosting(popularity~., train.clean, boos=TRUE, mfinal = 6)

res <- predict( adaboost, newdata=test.clean )

fit <- res$class

sum(fit== final$popularity )/9644  #50,17628 with 6 trees %


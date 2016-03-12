library(e1071)
train <- read.csv("../DATA/news_popularity_training.csv", sep = ",")
test <- read.csv("../DATA/news_popularity_test.csv", sep = ",")
final <- read.csv("/Users/guglielmo/Desktop/final_competition/final.csv", header = TRUE, sep = ",")

train <- train[,3:62]
test <- test[, 3:61]
#test.clean = features_clean[30001:39644,]
#test.clean <- as.matrix(features_clean[-c(1:30000),])


#Svm with cleaned data (standardized + imputed)
model0 <- svm( train.clean$popularity~., train.clean )
res <- predict( model0, newdata=test.clean )
head(res)

fit <- signif(res, digits = 1)
fit1 <- round(res)
sum(fit1 == final$popularity )/9644 # 51,33762, round in general works better than signif

#svm with the raw data (no changes at all)
model <- svm( train$popularity~., train )
res2 <- predict(model, newdata=as.matrix(test))
fit2 <- signif(res2, digits = 1)
fit3 <- round(res2)

sum(rf.predictions[,2] ==final$popularity)/9644
sum(fit3 == final$popularity)/9644 #51,47242 %
table(rf.predictions[,2], final$popularity)
table(rf.predictions[,2])
table(fit3, final$popularity)
a <- cbind(rf.predictions[,2],fit3)
a$pred <- apply(a, 1, max)
sum(a$pred == final$popularity)/9644


#Last attempt: svm with selected features from LASSO
rankvar = data.frame(as.matrix(coef(cvfit, s = "lambda.min")[[1]]))

topvar = data.frame(Variable = row.names(rankvar), coef = abs(rankvar$X1))
topvar = topvar[which(topvar$coef >0),]
topvar = topvar[-1,]
topvar = as.character(topvar[,1])


train.clean = train.clean[,which(colnames(train.clean) %in% c(topvar,"popularity"))]
test.clean = features_clean[30001:39644,which(colnames(test.clean) %in% topvar)]

model3 <- svm(train.clean$popularity~., train.clean)
res4 <- predict(model3, newdata = as.matrix(test.clean))
fit4 <- signif(res4, digits = 1)
fit5 <- round(res4)

sum(fit5 == final$popularity)/9644 #50,something % 

#Final attempt: svm with selected features from Fisher Scoring
model4 <- svm(train.clean$popularity~., train.clean)
res5 <- predict(model4, newdata = as.matrix(test.clean))
fit6 <- signif(res5, digits = 1)
fit7 <- round(res5)
sum(fit7 == final$popularity)/9644  #51,03691 %

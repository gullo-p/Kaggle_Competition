####################
#CROSS VALIDATION
# Among the inputs we can have the four different models we used (lasso, random forest, knn, svm)

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

#loop for optimization is missing.
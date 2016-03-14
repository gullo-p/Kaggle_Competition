# Random Forest: Tuning
require(caret)
fitControl = trainControl(allowParallel = TRUE)

rfGrid <-  expand.grid(mtry = seq(2,20,2))

set.seed(1234)
rfFit <- train(x = x, y = y,
               method = "ranger",
               trControl = fitControl,
               tuneGrid = rfGrid,
               num.trees = 400
)
rfFit

df <- rfFit$results

qplot(x= df$mtry , y = df$Accuracy , geom = "line" , xlab = "Number of features sampled at each split" , ylab = "Accuracy")

rf <- ranger(formula = y ~ . , data = data, num.trees  = ntree,
             mtry = mtry ,seed = seed, write.forest = TRUE, 
             importance = "impurity")
rf.varimp <- sort(rf$variable.importance)[-62]

p <- ggplot() + geom_point(aes(y = factor(names(rf.varimp),levels = names(rf.varimp)), x = rf.varimp),colour = 'red', size = 3) + labs(y= "Predictors",x="Variable Importance: Gini Index")+ theme(axis.text.y = element_text(size = 10))

p

rf <- randomForest(formula = y ~ . , data = data,ntree = 400,
                   mtry = mtry)
df <- rf$err.rate

qplot(x= 1:400 , y = df[,1] , geom = "point" , xlab = "Number of trees" , ylab = "Out of Bag error")


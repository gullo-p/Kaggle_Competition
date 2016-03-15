# Random Forest: Tuning
library(doMC)
registerDoMC(cores = 2)

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


set.seed(825)
rfGrid <-  expand.grid(interaction.depth = 1:5,
                       n.trees = seq(100,500,50),
                       shrinkage = 0.1,
                       n.minobsinnode = 10)

fitControl = trainControl(allowParallel = TRUE)

# Gradient Boosting: Tuning
set.seed(1234)
gbmFit1 <- train(y = y, x = x,
                 method = "gbm",
                 trControl = fitControl,
                 tuneGrid = rfGrid,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

set.seed(1234)
adaFit1 <- train(y = y, x = x,
                 method = "AdaBag",
                 trControl = fitControl)
adaFit1


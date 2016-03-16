# Random Forest: Tuning
library(doMC)
registerDoMC(cores = 2)

require(caret)
fitControl = trainControl(allowParallel = TRUE)

rfGrid <-  expand.grid(mtry = seq(4,10,2))

set.seed(1234)
rfFit <- train(x = x, y = y,
               method = "ranger",
               trControl = fitControl,
               tuneGrid = rfGrid,
               num.trees = 400
)
rfFit

df <- rfFit$results

qplot(x= df$mtry , y = df$Accuracy , geom = "line" , xlab = "Number of features sampled at each split" , ylab = "Accuracy from Bootstrapping 25 reps")

data <- cbind(y=y,x)
ntree = 300
mtry = 8
seed = 1234

rf <- ranger(formula = y ~ . , data = data, num.trees  = ntree,
             mtry = mtry ,seed = seed, write.forest = TRUE, 
             importance = "impurity")
rf.varimp <- sort(rf$variable.importance)

p <- ggplot() + geom_point(aes(y = factor(names(rf.varimp),levels = names(rf.varimp)), x = rf.varimp),colour = 'red', size = 3) + labs(y= "Predictors",x="Variable Importance: Gini Index")+ theme(axis.text.y = element_text(size = 10))

p

sel.vars <- names(which(rf.varimp > 100))

rf <- randomForest(formula = y ~ . , data = data,ntree = 400,
                   mtry = mtry)
df <- rf$err.rate

qplot(x= 1:400 , y = df[,1] , geom = "point" , xlab = "Number of trees" , ylab = "Out of Bag error")

# Rolling windows optimization

data <- time.data[time.data$flag == 0,]
m = 20000
flag.index = sample(1:nrow(data),m)
train.data <- data[flag.index,]
train.data$flag <- 0
test.data <- data[-flag.index,]
test.data$flag <- 1

data <- rbind(train.data,test.data)
data <- data[order(data$ts),]

stepsizes <- seq(1000, 5000, 1000)
window.size <- 10000

n <- length(stepsizes)
train <- rep(0,n)
test <- rep(0,n)

for(i in 1:n){
  
  rf <- rolling.windows.rf(dataset = data, step.size = stepsizes[i])
  
  train[i] <- rf$train.acc
  test[i] <- rf$test.acc
}

library(reshape2)
df <- data.frame(step.size = stepsizes,train , test)
df.melt <- melt(df,1,variable.name = "type",value.name = "Accuracy")

errPlot <- ggplot(data = df.melt, aes(x = step.size, y = Accuracy, color = type)) + 
  geom_line() + 
  geom_point() +
  xlab("Step size in rolling windows") +
  ylab("Classification Accuracy") +
  theme_bw(base_size = 14, base_family = "Helvetica")

errPlot

# Evolution of errors across windows
data <- time.data[time.data$flag == 0,]
m = 20000
flag.index = sample(1:nrow(data),m)
train.data <- data[flag.index,]
train.data$flag <- 0
test.data <- data[-flag.index,]
test.data$flag <- 1

data <- rbind(train.data,test.data)
data <- data[order(data$ts),]

rf <- rolling.windows.rf(dataset = data)

train <- rf$train.iter
test <- rf$test.iter

df <- data.frame(windows = 1:length(train),train, test)
df.melt <- melt(df,1,variable.name = "type",value.name = "Accuracy")

errPlot1 <- ggplot(data = df.melt, aes(x = windows, y = Accuracy, color = type)) + 
  geom_line() + 
  geom_point() +
  xlab("Iteration on Rolling windows") +
  ylab("Classification Accuracy") +
  theme_bw(base_size = 14, base_family = "Helvetica")

errPlot1


########### Stochastic Gradient Boosting: Tuning GBM
gbmGrid <-  expand.grid(interaction.depth = 1,
                       n.trees = 100,
                       shrinkage = seq(0.1,0.5,0.1),
                       n.minobsinnode = c(10,20,30))

fitControl = trainControl(allowParallel = TRUE)

set.seed(1234)
gbmFit1 <- train(y = y, x = x,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 trControl = fitControl)
gbmFit1

###### AdaBag tuning
set.seed(1234)
adaFit1 <- train(y = y, x = x,
                 method = "AdaBag",
                 trControl = fitControl)
adaFit1
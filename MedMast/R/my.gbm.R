# ----------------------------------------------------------------------------------------------------
# Stochastic Gradient Boosting
# ----------------------------------------------------------------------------------------------------
#' @title Boosting with gbm
#' 
#' @description This function simply uses gbm from 'gbm' package for training the model and testing 
#' it on the given test set (needed for the rolling windows function).
#' 
#' @param x A dataframe containing the features for training.
#' @param y The labels from the training dataset.
#' @param xtest The test dataset for which we want to predict the labels.
#' @return The predicted labels for the test set.
#' @export
#' @import gbm
#' @import caret

my.gbm <- function(x,y,xtest, seed = 1234){
  
  fitControl <- trainControl(method = "none")
  
  y <- factor(y)
  data = cbind(y = y, x)
  
  set.seed(seed)
  gbmFit <- train(y ~ ., data = data,
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE,
                   ## Only a single model can be passed to the
                   ## function when no resampling is used:
                   tuneGrid = data.frame(interaction.depth = 1,
                                         n.trees = 100,
                                         shrinkage = .1,
                                         n.minobsinnode = 10))
  
  pred <- predict(gbmFit, newdata = xtest)
  
  return(pred)
}
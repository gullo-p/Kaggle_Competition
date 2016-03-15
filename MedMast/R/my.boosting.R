# ----------------------------------------------------------------------------------------------------
# Boosting
# ----------------------------------------------------------------------------------------------------
#' @title Boosting with adabag
#' 
#' @description This function simply uses boosting from 'adabag' package for training the model and testing 
#' it on the given test set (needed for the rolling windows function).
#' 
#' @param x A dataframe containing the features for training.
#' @param y The labels from the training dataset.
#' @param xtest The test dataset for which we want to predict the labels.
#' @param mfinal An integer, the number of iterations for which boosting is run or, equivalently, 
#' the number of trees to use.
#' @param seed The initial seed, by default is 1234.
#' @return The predicted labels for the test set.
#' @export
#' @import adabag

my.boosting <- function(x , y , xtest, mfinal, seed = 1234){
  
  require(adabag)
  set.seed(seed)
  y <- factor(y)
  
  data = cbind(y = y, x)
  adaboost <- boosting(y ~ . , data , mfinal=mfinal)
  
  pred <- predict.boosting(adaboost,newdata=xtest)$class
  
  return(pred)
  
}
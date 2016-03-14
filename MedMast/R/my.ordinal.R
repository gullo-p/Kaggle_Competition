# ----------------------------------------------------------------------------------------------------
# Ordinal regression
# ----------------------------------------------------------------------------------------------------
#' @title Ordinal regression
#' 
#' @description This function performs an ordinal regression of the labels over all the other predictors 
#' given as inputs (needed for the rolling windows function).
#' 
#' @param x A dataframe containing the features for training.
#' @param y The labels from the training dataset.
#' @param xtest The test dataset for which we want to predict the labels.
#' @return The predicted labels for the test set.
#' @export
#' @import MASS

my.ordinal <- function(x , y , xtest){

  y <- factor(y,ordered = TRUE)
  data <- cbind(y = y,x)
  
  fit <- polr( y ~ . , data = data, Hess = TRUE)
  
  levels = levels(y)
  
  pred <- factor(predict(fit, xtest, type = "c",s=fit$lambda.min),levels = levels,ordered = TRUE)
  
  return(pred)
}



# ----------------------------------------------------------------------------------------------------
# Svm
# ----------------------------------------------------------------------------------------------------
#' @title Svm with e1071
#' 
#' @description This function performs SVM using 'e1071' package (needed for the rolling windows function).
#' 
#' @param x A dataframe containing the features for training.
#' @param y The labels from the training dataset.
#' @param xtest The test dataset for which we want to predict the labels.
#' @param cost The regularization term for the constraints violation.
#' @param gamma Parameter for the formulation of kernels (needed for all except 'linear').
#' @return The predicted labels for the test set.
#' @export
#' @import e1071

my.svm <- function(x , y , xtest, cost, gamma){
  
  y <- factor(y)

  fit <- svm(x,y,scale = TRUE, type = "C", cost = cost, gamma = gamma)
  
  pred <- predict(fit, newdata = xtest, type = "class")
  
  return(pred)
}
?svm

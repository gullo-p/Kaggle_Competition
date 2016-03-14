# ----------------------------------------------------------------------------------------------------
# K-NN
# ----------------------------------------------------------------------------------------------------
#' @title K-NN with caret 
#' 
#' @description This function performs k-NN from 'caret' package (needed for the rolling windows function).
#' 
#' @param x A dataframe containing the features for training.
#' @param y The labels from the training dataset.
#' @param xtest The test dataset for which we want to predict the labels.
#' @param k An integer, the number of nearest neighbors to use.
#' @return The predicted labels for the test set.
#' @export
#' @import caret

my.knn <- function(x , y , xtest, k){
  
  y <- factor(y)
  
  pred <- knn3Train(train = x, cl = y, k = k, test = xtest, prob = FALSE)
  
  return(pred)
}
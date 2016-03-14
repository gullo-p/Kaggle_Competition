# ----------------------------------------------------------------------------------------------------
# Random Forest
# ----------------------------------------------------------------------------------------------------
#' @title Random forest with ranger
#' 
#' @description This function uses 'ranger' package to perform a random forest 
#' (needed for the rolling windows function).
#' 
#' @param x A dataframe containing the features for training.
#' @param y The labels from the training dataset.
#' @param xtest The test dataset for which we want to predict the labels.
#' @param ntree An integer, the number of trees to use.
#' @param mtry An integer, the depth of each tree.
#' @param seed The initial seed, by default is 1234.
#' @return The predicted labels for the test set.
#' @export
#' @import ranger


my.forest <- function(x , y , xtest, ntree = 300, mtry = 4, seed = 1234){
  
  y <- factor(y)
  data <- cbind(y = y,x)
  
  rf <- ranger(formula = y ~ . , data = data, num.trees  = ntree,
               mtry = mtry ,seed = seed, write.forest = TRUE)
  
  pred <- predict(rf , xtest, seed = seed, verbose = FALSE)$predictions
  
  return(pred)
  
}
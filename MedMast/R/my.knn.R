my.knn <- function(x , y , xtest, k){
  
  require(caret)
  y <- factor(y)
  
  pred <- knn3Train(train = x, cl = y, k = k, test = xtest, prob = FALSE)
  
  return(pred)
}
my.boosting <- function(x , y , xtest, mfinal, seed = 1234){
  
  require(adabag)
  set.seed(seed)
  y <- factor(y)
  
  data = cbind(y = y, x)
  adaboost <- boosting(y ~ . , data , mfinal=mfinal)
  
  pred <- predict.boosting(adaboost,newdata=xtest)$class
  
  return(pred)
  
}
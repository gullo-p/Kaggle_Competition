my.svm <- function(x , y , xtest, cost, gamma){
  
  require(e1071)
  y <- factor(y)

  fit <- svm(x,y,scale = TRUE, type = "C", cost = cost, gamma = gamma)
  
  pred <- predict(fit, newdata = xtest, type = "class")
  
  return(pred)
}
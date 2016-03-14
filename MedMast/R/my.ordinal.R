my.ordinal <- function(x , y , xtest){

  y <- factor(y,ordered = TRUE)
  data <- cbind(y = y,x)
  
  fit <- polr( y ~ . , data = data, Hess = TRUE)
  
  levels = levels(y)
  
  pred <- factor(predict(fit, xtest, type = "c",s=fit$lambda.min),levels = levels,ordered = TRUE)
  
  return(pred)
}
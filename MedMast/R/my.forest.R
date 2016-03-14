my.forest <- function(x , y , xtest, ntree = 300, mtry = 4, seed = 1234){
  
  require(ranger)
  y <- factor(y)
  data <- cbind(y = y,x)
  
  rf <- ranger(formula = y ~ . , data = data, num.trees  = ntree,
               mtry = mtry ,seed = seed, write.forest = TRUE)
  
  pred <- predict(rf , xtest, seed = seed, verbose = FALSE)$predictions
  
  return(pred)
  
}
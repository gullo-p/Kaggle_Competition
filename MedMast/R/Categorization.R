
#transform a feature into a 3-categorical (0, 1 or more than 1)
three.cat <- function(x, lambda1, lambda2){
  
  for(i in 1:length(x)){
    if((x[i] >= 0)&&(x[i] <= lambda1)) x[i] <- 0
    else if((x[i] > lambda1) &&(x[i] <=lambda2)) x[i] <- 1
    else x[i] <- 2
    
  }
  return(x)
}


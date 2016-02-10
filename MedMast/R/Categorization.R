# ----------------------------------------------------------------------------------------------------
# CREATE CATEGORICAL VARIABLES
# ----------------------------------------------------------------------------------------------------
#' This function transforms a feature into a 3-categorical (0, 1, 2 valued) one.
#' @param x The vector containing the feature you wish to categorize.
#' @param lambda1, lambda2 parameters that define the three cuts for setting the values 0, 1 and 2.
#' @return The categorized feature.
three.cat <- function(x, lambda1, lambda2){
  
  for(i in 1:length(x)){
    if((x[i] >= 0)&&(x[i] <= lambda1)) x[i] <- 0
    else if((x[i] > lambda1) &&(x[i] <=lambda2)) x[i] <- 1
    else x[i] <- 2
    
  }
  return(x)
}


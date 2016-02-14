# ----------------------------------------------------------------------------------------------------
# Feature standardization
# ----------------------------------------------------------------------------------------------------
#' @title Feature standardization
#' 
#' @description This function standardizes a feature, either continuous or discrete. 
#' If it is continuous it applies the normal standardization formula (x - sample mean)/(sample sd), 
#' else if it is discrete it returns (x-min(x))/(max(x) - min(x)).
#'  
#' @param x A vector containing the feature to be standardized.
#' @param type A string with values "continuous" (counts included) or "discrete" (e.g. binary, categorical) depending on the type of the feature.
#' @return The standardized variable.
#' @export
#' @import assertthat
#' @examples 
#' #create a vector
#' x <- 1:100
#' x_stand <- standardize(x, type = "continuous") #this will return the standardized feature.

standardize <- function(x,type = "continuous"){
  
  if(!require("assertthat")) install.packages("assertthat"); library(assertthat)
  # test the inputs
  not_empty(x);
  is.string(type); assert_that(type %in% c("continuous", "discrete"))
  
  #standardize the continuous features
  con.stand <- function(x) {
    num <- x - mean(x)
    denom <-sd(x)
    return (num/denom)
  }
  #dicrete standardization
  cat.stand <- function(x){
    num <- x - min(x)
    denom <- max(x)-min(x)
    return(num/denom)
  }
  
  if (type == "continuous"){
    result = con.stand(x)
  }else{
    result = cat.stand(x)
  }
  return(result)
}

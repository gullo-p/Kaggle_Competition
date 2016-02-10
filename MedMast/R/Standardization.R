# ----------------------------------------------------------------------------------------------------
# Feature standardization
# ----------------------------------------------------------------------------------------------------
#' This function standardized a feature, either continuous or discrete. 
#' @param x A vector containing the feature to be standardized.
#' @param type A string with values "continuous" or "discrete" depending on the type of the feature.
#' @return The standardized variable.
#' @export
#' @import assertthat


standardize <- function(x,type = "continuous"){
  
  library(assertthat)
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

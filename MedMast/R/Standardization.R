# Wrap them in a unique function that controls if the input is continuous or categorical
######Standardization of the features

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

# Wrap them in a unique function that controls if the input is continuous or categorical
######Standardization of the features
#standardize the continuous features
standardize <- function(x) {
  num <- x - mean(x)
  denom <-sd(x)
  return (num/denom)
}

#categorical standardization
cat_stand <- function(x){
  num <- x - min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}

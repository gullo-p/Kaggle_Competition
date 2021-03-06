# ----------------------------------------------------------------------------------------------------
# FISHER SCORING FOR FEATURE SELECTION
# ----------------------------------------------------------------------------------------------------
#' @title Fisher scoring for feature selection
#' 
#' @description This function computes the Fisher scoring for each feature 
#' based on the binary version of the popularity label ("non-popular" being 0 and "popular" being 1) 
#' and ranks the Fisher scoring of the features in descending order.
#' 
#' @param features The dataframe containing the features.
#' @param labels The categorical labels (in our case we have 5 categories for popularity) 
#' according to which you want to measure the variability of each feature.
#' @param n The number of features with the highest score you want to select for your final model.
#' @param threshold The value to use as the threshold for converting the categorical labels to binary.
#' @return A dataframe containing the selected features.
#' @export
#' @import assertthat
#' @examples 
#' # create sample dataset
#' features <- matrix(rnorm(200), ncol=2)
#' labels <- c(rep(1, 40), rep(2, 40), rep(3,40), rep(4, 40), rep(5,40))
#' n <- 1
#' threshold <- 2 #categories above 2 will be set to be 1, otherwise 0.
#' # get the variable with the highest fisher scoring
#' fisher.selection(features, labels, n, threshold)


fisher.selection <- function(features,labels,n,threshold){
  if(!require("assertthat")) install.packages("assertthat"); library(assertthat)
  # Test the inputs
  not_empty(features); not_empty(labels);
  assert_that(nrow(features) == length(labels))
  is.count(n); assert_that(n <= ncol(features));
  is.count(threshold);
  # Convert response to binary
  y <- rep(0,length(labels))
  y[labels > threshold] <- 1
  
  # Function to caluclate the variable score
  fisher.score <- function(x,y){
    num <- (mean(x[y == 0]) - mean(x[y == 1]))^2
    denom <- var(x[y == 0]) + var(x[y == 1])
    score <- round(num/denom,4)
    return(score)
  }
  
  fisher.score = apply(features,2,function(x)fisher.score(x,y))
  names(fisher.score) = colnames(features)
  
  top.ranks = fisher.score[order(fisher.score,decreasing = T)]
  top.vars = names(top.ranks[1:n])
  # subset the data with the top variables
  result.frame <- features[,top.vars]
  return(result.frame)
}

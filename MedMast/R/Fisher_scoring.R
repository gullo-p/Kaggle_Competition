# ----------------------------------------------------------------------------------------------------
# FISHER SCORING FOR FEATURE SELECTION
# ----------------------------------------------------------------------------------------------------
#' This function computes the Fisher scoring for each feature 
#' based on a binary output and ranks them in descending order.
#' @param feature The dataframe containing the features.
#' @param label The binary label according to which you want to measure the variability of each feature.
#' @param n The number of features with the highest score you want to select for your final model.
#' @return A dataframe containing the selected features.


fisher.rank <- function(feature,label){
  num <- (mean(feature[label == 0]) - mean(feature[label == 1]))^2
  denom <- var(feature[label == 0]) + var(feature[label == 1])
  rank <- round(num/denom,4)
  return(rank)
}

X = train.clean[,-which(colnames(train.clean) %in% c("popularity","dummy"))]
fisher.score = apply(X,2,function(x)fisher.rank(x,train.clean$dummy))
names(fisher.score) = colnames(X)

top.ranks = fisher.score[order(fisher.score,decreasing = T)]
top.vars = names(top.ranks[1:40])

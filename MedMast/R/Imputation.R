# ----------------------------------------------------------------------------------------------------
# Handle missing values
# ----------------------------------------------------------------------------------------------------
#' @title Handle missing values
#' 
#' @description This function deals with the cleaning and/or imputation of the missing values in the data.
#' 
#' @param features A dataframe containing the dataset you want to clean or impute.
#' @param type A string which can take the value "remove" if you want to remove the nonsense 
#' 0's in the data or "impute" if you want to impute them from the remaining observations.
#' @return The features dataframe modified accordingly.
#' @export
#' @import assertthat
#' @import HotDeckImputation

handle.missing <- function(features,type){
  
  if(!require("HotDeckImputation")) install.packages("HotDeckImputation"); library(HotDeckImputation)
  if(!require("assertthat")) install.packages("assertthat"); library(assertthat)

  # test the inputs
  not_empty(features);
  is.string(type); assert_that(type %in% c("remove", "impute"))
  
  if(type == "remove"){
    # Remove the missing values
    features <- features[-which(features$n_tokens_content == 0),]
    features <- features[-which(features$global_subjectivity == 0),]
    
  }else if(type == "impute"){
    
    index1 <- match("n_tokens_content",colnames(features))
    index2 <- match("global_subjectivity",colnames(features))
    # Recode the missing values
    features[features$n_tokens_content == 0,c(index1:(index1+4),(index1+7))] <- NA
    features[features$global_subjectivity == 0,c(index2:(index2+10)] <- NA
    
    # Hot deck Imputation
    features <- impute.NN_HD(DATA=features,distance="eukl")
  }
  return(features)
}
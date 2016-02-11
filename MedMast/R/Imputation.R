# ----------------------------------------------------------------------------------------------------
# Handle missing values
# ----------------------------------------------------------------------------------------------------
#' This function deals with the cleaning and/or imputation of the missing values in the data.
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
    features <- features[-which(features$n_tokens_content == 0 & features$id <= 30000),]
    features <- features[-which(features$global_subjectivity == 0 & features$id <= 30000),]
    features <- handle.missing(features , type = "impute")
  }else{
    
    # Recode the missing values
    features$n_unique_tokens[features$n_tokens_content == 0] <- NA
    features$n_non_stop_unique_tokens[features$n_tokens_content == 0] <- NA
    features$num_hrefs[features$n_tokens_content == 0] <- NA
    features$num_self_hrefs[features$n_tokens_content == 0] <- NA
    features$average_token_length[features$n_tokens_content == 0] <- NA
    features$n_tokens_content[features$n_tokens_content == 0] <- NA
    
    # Recode the missing values
    features$global_sentiment_polarity[features$global_subjectivity == 0] <- NA
    features$global_rate_positive_words[features$global_subjectivity == 0] <- NA
    features$global_rate_negative_words[features$global_subjectivity == 0] <- NA
    features$rate_positive_words[features$global_subjectivity == 0] <- NA
    features$avg_positive_polarity[features$global_subjectivity == 0] <- NA
    features$avg_negative_polarity[features$global_subjectivity == 0] <- NA
    features$min_positive_polarity[features$global_subjectivity == 0] <- NA
    features$min_negative_polarity[features$global_subjectivity == 0] <- NA
    features$max_positive_polarity[features$global_subjectivity == 0] <- NA
    features$max_negative_polarity[features$global_subjectivity == 0] <- NA
    features$global_subjectivity[features$global_subjectivity == 0] <- NA
    
    # Hot deck Imputation
    imp.features <- impute.NN_HD(DATA=features[,-c(1,2)],distance="eukl")
    
    features <- data.frame(id = features$id, url = features$url, imp.features)
  }
  return(features)
}
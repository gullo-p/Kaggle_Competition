# ----------------------------------------------------------------------------------------------------
# LASSO MODEL FOR FEATURE SELECTION AND PREDICTION
# ----------------------------------------------------------------------------------------------------
#' @title Lasso model for feature selection and prediction.

#' This function uses cross-validated Lasso regression (generalised to multinomial output) 
#' for both multiclass classification and feature selection.
#' 
#' @param train.features A dataframe containing the training dataset (output excluded).
#' @param labels A vector of multi-class labels for the training set.
#' @param type A string with values "select" for performing feature selection or "predict" for 
#' predictions using Lasso (by default it is set to "select").
#' @param test.features A dataframe containing the test dataset (NULL if type = "select").
#' @param dfmax = An integer which, if not NULL, indicates how many features to be 
#' extracted (NULL by default).
#' @return If type = "select" the function returns a dataframe with the extracted features, 
#' else if type = "predict" it returns the vector of predicted labels.
#' @export
#' @import assertthat
#' @import glmnet
#' @examples 
#' # create sample dataset
#' features <- matrix(rnorm(200), ncol=2)
#' labels <- c(rep(1, 40), rep(2, 40), rep(3,40), rep(4, 40), rep(5,40))
#' lasso.model(train.features = features, labels, type = "select", dfmax = 1)

lasso.model <- function(train.features, labels, type = "select", test.features = NULL, dfmax = NULL) {
  if(!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if(!require("glmnet")) install.packages("glmnet"); library(glmnet)
  
  
  # test the inputs
  not_empty(train.features); not_empty(labels);
  is.string(type); assert_that(type %in% c("select", "predict"))
  assert_that(nrow(train.features) == length(labels))
  if (type == "predict") {
    assert_that(not_empty(test.features) &
                  ncol(test.features) == ncol(train.features))
  }else {
    is.count(dfmax); assert_that(dfmax <= ncol(train.features));
  }
  
  X <- as.matrix(train.features)
  y <- as.factor(labels)
  
  if (type == "select") {
    cvfit = cv.glmnet(X, y, family = "multinomial", type.multinomial = "grouped", dfmax = dfmax)
    rankvar = data.frame(as.matrix(coef(cvfit, s = "lambda.min")[[1]]))
    rankvar = data.frame(Variable = row.names(rankvar), coef = abs(rankvar$X1))
    top.vars = subset(rankvar,coef > 0, Variable)
    top.vars = as.character(top.vars$Variable)[-1]
    
    result.frame <- train.features[,top.vars]
    return(result.frame)
  }else{
    cvfit = cv.glmnet(X, y, family = "multinomial")
    pfit = predict(cvfit, newx = test.features, s = "lambda.min", type = "class")
    return(pfit)
  }
}

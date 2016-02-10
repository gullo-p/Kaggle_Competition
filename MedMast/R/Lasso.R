# ----------------------------------------------------------------------------------------------------
# LASSO MODEL FOR FEATURE SELECTION AND PREDICTION
# ----------------------------------------------------------------------------------------------------
#' This function computes the Fisher scoring for each feature
#' based on a binary output and ranks them in descending order.
#' @param feature The dataframe containing the features.
#' @param label The binary label according to which you want to measure the variability of each feature.
#' @param n The number of features with the highest score you want to select for your final model.
#' @param threshold The value to use as the threshold for converting the label to binary.
#' @return A dataframe containing the selected features.

lasso.model = function(train.features, labels, type = "select", test.features = NULL, dfmax = NULL) {
  library(assertthat)
  library(glmnet)
  
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

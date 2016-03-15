#' @title Rolling windows
#' 
#' @description This function fits a desired model with a rolling windows technique, i.e., 
#' the dataset is split in chunks of fixed size, were each chunk starts from the observation
#' which follows the starting observation of the previous chunk 
#' (this is why is called rolling windows).
#' In each split of the data we predict a portion of the test data labels.
#' The final prediction is given then by a majority vote over all the predictions made.
#' 
#' @param dataset A dataset comprising of training and test data
#' @param window.size An integer which establishes the size of each chunk of the rolling windows.
#' @param step.size An integer which defines the distance between the starting observations for two consecutive windows.
#' @param FUN The function to which you want to apply the rolling window technique (can be one among the 
#' other my.functions present in the package).
#' @param ... Additional parameter passed to FUN
#' 
#' @return A dataframe containing the id's and the predicted labels in the right format for submission.
#' @export
#' @import assertthat

rolling.windows <- function(dataset , window.size = 10000, step.size = 1000 ,FUN = my.forest ,...){
  
  
  not_empty(dataset);
  is.count(window.size); is.count(step.size);
  
  # Calculate the number of iterations
  n <- nrow(dataset)
  iter <- round((n - window.size)/step.size,0)
  rwpred <- data.frame(id = dataset$id[dataset$flag == 1])
  
  for(i in 1:iter){
    
    # Subset the data part of the window
    start <- (1+((i-1)*step.size))
    end <- ifelse(i < iter, (window.size + ((i-1)*step.size)),n)
    index <- start:end
    data <- dataset[index,]
    data <- data[order(data$id),]
    x = data[which(data$flag == 0),-which(colnames(data) %in% c("id","url","timedelta","popularity","flag"))]
    y = data$popularity[data$flag == 0]
    xtest = data[which(data$flag == 1),-which(colnames(data) %in% c("id","url","timedelta","popularity","flag"))]
    ytest = data.frame(id = data[which(data$flag == 1),1])
    
    ntrain = length(y)
    ntest = nrow(xtest)
    
    predLabels <- FUN(x,y,xtest,...)
    
    ytest$predicted <- predLabels
    colnames(ytest)[2] <- paste0("X",i)
    rwpred <- merge(rwpred,ytest,sort=T,all.x=T)
    
  }
  # Use the majority vote to arrive at final predictions
  rwpred$prediction <- apply(rwpred[,-1],1,function(x) as.numeric(names(tail(sort(table(x)),1))))
  
  #return the predictions from all windows
  return(rwpred)
}
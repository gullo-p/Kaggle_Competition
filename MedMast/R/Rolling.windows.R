#' @title Rolling windows with random forest
#' 
#' @description This function performs random forest with a rolling windows technique, i.e., 
#' the dataset is split in chunks of fixed size, were each chunk starts from the observation
#' which follows the starting observation of the previous chunk 
#' (this is why is called rolling windows).
#' In each split of the data we predict a portion of the test data labels through a random forest.
#' The final prediction is given then by a majority vote over all the predictions made.
#' 
#' @param dataset A dataset comprising of training and test data
#' @param window.size An integer which establishes the size of each chunk of the rolling windows.
#' @param step.size An integer which defines the distance between the starting observations for two consecutive windows.
#' @param seed The seed you want to set for the iterations of the random forest.
#' @param ntree The number of trees in each call of random forest.
#' @return A dataframe containing the id's and the predicted labels in the right format for submission.
#' @export
#' @import randomForest
#' @import assertthat
 
rolling.windows.rf <- function(dataset , window.size = 10000, step.size = 1000 , seed = 1234, ntree = 300){
  
  
  not_empty(dataset);
  is.count(window.size); is.count(seed); 
  is.count(step.size); is.count(ntree);
 
  # Calculate the number of iterations
  n <- nrow(dataset)
  iter <- round((n - window.size)/step.size,0)
  rfpred <- data.frame(id = dataset$id[dataset$flag == 1])
  
  # Initialize a vector for keeping track of the accuracy at each iteration
  acc <- rep(0,iter)
  
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
    
    # Train and predict using  randomForest
    set.seed(seed)
    rf <- randomForest(x,y,xtest,ntree=ntree)
    
    # Save the accuracy
    acc[i] = sum(as.numeric(y) == as.numeric(rf$predicted))/ntrain
    ytest$predicted <- rf$test$predicted
    colnames(ytest)[2] <- paste0("X",i)
    rfpred <- merge(rfpred,ytest,sort=T,all.x=T)
    
  }
  # Use the majority vote to arrive at final predictions
  rfpred$prediction <- apply(rfpred[,-1],1,function(x) as.numeric(names(tail(sort(table(x)),1))))
  
  return(rfpred[,c("id","prediction")])
  
}
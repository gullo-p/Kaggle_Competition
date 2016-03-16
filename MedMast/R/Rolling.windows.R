#' @title Rolling windows with random forest
#' 
#' @description This function performs tuning of the rolling windows technique, i.e., 
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
#' @return A list containing the overall train and test accuaracies including the window-wise values.
#' @export
#' @import ranger
#' @import assertthat
 
rolling.windows.rf <- function(dataset , window.size = 10000, step.size = 1000 , seed = 1234,mtry = 4, ntree = 300){
  
  
  not_empty(dataset);
  is.count(window.size); is.count(seed);
  is.count(step.size); is.count(ntree);
 
  # Calculate the number of iterations
  n <- nrow(dataset)
  iter <- round((n - window.size)/step.size,0)
  rfpred <- data.frame(id = dataset$id[dataset$flag == 0])
  rftpred <- data.frame(id = dataset$id[dataset$flag == 1])
  
  # Initialize a vector for keeping track of the accuracy at each iteration
  acc <- rep(0,iter)
  tacc <- rep(0,iter)
  
  for(i in 1:iter){
    
    # Subset the data part of the window
    start <- (1+((i-1)*step.size))
    end <- ifelse(i < iter, (window.size + ((i-1)*step.size)),n)
    index <- start:end
    data <- dataset[index,]
    data <- data[order(data$id),]
    x = data[which(data$flag == 0),-which(colnames(data) %in% c("id","url","timedelta","popularity","flag"))]
    y = data[which(data$flag == 0), which(colnames(data) %in% c("id","popularity"))]
    xtest = data[which(data$flag == 1),-which(colnames(data) %in% c("id","url","timedelta","popularity","flag"))]
    ytest = data[which(data$flag == 1), which(colnames(data) %in% c("id","popularity"))]
    
    ntrain = nrow(x)
    ntest = nrow(xtest)
    
    # Train and predict using ranger
    data <- cbind(y = y$popularity,x)
    
    rf <- ranger(formula = y ~ . , data = data, num.trees  = ntree,
                 mtry = mtry ,seed = seed, write.forest = TRUE)
    
    pred <- rf$predictions
    tpred <- predict(rf , xtest, seed = seed, verbose = FALSE)$predictions
    
    # Save the accuracy
    acc[i] = sum(as.numeric(y$popularity) == as.numeric(pred))/ntrain
    tacc[i] = sum(as.numeric(ytest$popularity) == as.numeric(tpred))/ntest
    
    y$predicted <- pred
    colnames(y)[3] <- paste0("X",i)
    rfpred <- merge(rfpred,y[,-2],sort=T,all.x=T)
    
    ytest$predicted <- tpred
    colnames(ytest)[3] <- paste0("X",i)
    rftpred <- merge(rftpred,ytest[,-2],sort=T,all.x=T)
    
  }
  # Use the majority vote to arrive at final predictions
  rfpred$prediction <- apply(rfpred[,-1],1,function(x) as.numeric(names(tail(sort(table(x)),1))))
  labels = dataset[order(dataset$id),which(colnames(dataset) %in% c("flag","popularity"))]
  train.acc <- mean(labels$popularity[labels$flag == 0] == as.numeric(rfpred$prediction))
  
  rftpred$prediction <- apply(rftpred[,-1],1,function(x) as.numeric(names(tail(sort(table(x)),1))))
  test.acc <- mean(labels$popularity[labels$flag == 1] == as.numeric(rftpred$prediction))
  
  return(list(train.acc= train.acc, test.acc = test.acc, train.iter = acc, test.iter = tacc))
  
}
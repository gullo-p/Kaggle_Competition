#Random forest

library(randomForest)



#Best performance so far
rf <- randomForest(x = train[,3:61], y = train[,62], 
                   xtest = test[,3:61],  ntree=100,nPerm=1,mtry=3,
                   proximity=TRUE,importance=TRUE,nodesize = 20)

pred <- rf$test$predicted

submit <- as.data.frame(cbind(c(30001:39644), pred))
names(submit) <- c("id", "popularity")

write.csv(submit, file = "submit6.csv", quote = FALSE, row.names = FALSE)

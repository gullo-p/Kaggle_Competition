#Random forest

library(randomForest)



#Best performance so far
rf <- randomForest(x = train_stand[1:30000,4:63], y = train_stand[1:30000,64], 
                   xtest = test_stand[,4:63],  ntree=500,nPerm=2,mtry=3,
                   proximity=TRUE,importance=TRUE)



submit <- as.data.frame(cbind(c(30001:39644), pred))
names(submit) <- c("id", "popularity")


write.csv(submit, file = "submit6.csv", quote = FALSE, row.names = FALSE)

##############################################################################

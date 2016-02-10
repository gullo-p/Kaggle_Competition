#Random forest

library(randomForest)



#Best performance so far
rf <- randomForest(x = train[,3:61], y = train[,62], 
                   xtest = test[,3:61],  ntree=100,nPerm=1,mtry=3,
                   proximity=TRUE,importance=TRUE,nodesize = 20)

pred <- rf$test$predicted
sum(popularity$popularity == pred)/length(popularity$popularity)



submit <- as.data.frame(cbind(c(30001:39644), pred))
names(submit) <- c("id", "popularity")

#0.5227084

write.csv(submit, file = "submit6.csv", quote = FALSE, row.names = FALSE)

##############################################################################


#Random forest: ntree = 100, nPerm = 10, mtry = 3, proximity = TRUE, importance = TRUE
rf <- randomForest(x = train_stand[1:30000,4:63], y = train_stand[1:30000,64], 
                   xtest = test_stand[,4:63],  ntree=100,nPerm=10,mtry=3,
                   proximity=TRUE,importance=TRUE)

pred <- rf$test$predicted
sum(popularity$popularity == pred)/length(popularity$popularity)

# 1.000:   0.4900456
# 3.000:   0.4996889
# 10.000:  0.5049772
# 30.000:  0.514102


submit <- as.data.frame(cbind(c(30001:39644), pred))
names(submit) <- c("id", "popularity")


##############################################################################

#Random forest: ntree = 100, nPerm = 10
n <- ncol(train_stand) - 1
rf <- randomForest(x = train_stand[1:3000,4:n], y = train_stand[1:3000,(n+1)], 
                   xtest = test_stand[,4:n],  ntree=100,nPerm=10, 
                   proximity=TRUE,importance=TRUE)

pred <- rf$test$predicted
sum(popularity$popularity == pred)/length(popularity$popularity)

# mtry 3: 0.4950228

system.time(rf <- randomForest(x = train_stand[1:3000,4:n], y = train_stand[1:3000,(n+1)], 
                               xtest = test_stand[,4:n],  ntree=100,nPerm=10, 
                               proximity=TRUE,importance=TRUE))

user  system elapsed 
21.902   1.187  23.246 

##############################################################################

#Random forest: ntree = 100, nPerm = 10, mtry = 3
rf <- randomForest(x = train_stand[1:1000,4:63], y = train_stand[1:1000,64], 
                   xtest = test_stand[,4:63],  ntree=100,nPerm=10,mtry=3,
                   proximity=TRUE,importance=FALSE)

pred <- rf$test$predicted
sum(popularity$popularity == pred)/length(popularity$popularity)

# Proximity = TRUE, Importance = True: 0.4926379
# Proximity = FALSE, Importance = True: 0.4909788 
# Proximity = TRUE, Importance = FALSE: 0.4825798


##############################################################################

#Random forest: ntree = 100, nPerm = 10
rf <- randomForest(x = train_stand[1:3000,4:63], y = train_stand[1:3000,64], 
                   xtest = test_stand[,4:63],  ntree=100,nPerm=10,mtry=3,
                   proximity=TRUE,importance=TRUE)

pred <- rf$test$predicted
sum(popularity$popularity == pred)/length(popularity$popularity)

# mtry 3: 0.4926379
# mtry 4: 0.4933637
# mtry 5: 0.4930527
# mtry 6: 0.4924302


##############################################################################



#Random forest: ntree = 100, nPerm = 10, mtry = 3, proximity = TRUE, importance = TRUE
rf <- randomForest(x = train_stand[1:30000,4:63], y = train_stand[1:30000,64], 
                   xtest = test_stand[,4:63],  ntree=500,nPerm=2,mtry=3,
                   proximity=TRUE,importance=TRUE)

pred <- rf$test$predicted
sum(popularity$popularity == pred)/length(popularity$popularity)



submit <- as.data.frame(cbind(c(30001:39644), pred))
names(submit) <- c("id", "popularity")


##############################################################################


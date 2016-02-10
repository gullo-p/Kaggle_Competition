########LASSO REGRESSION
train.clean <- data.frame(features_clean,popularity = as.numeric(dataset$popularity))[1:30000,]
test.clean <- as.matrix(features_clean[-c(1:30000),])

X <- as.matrix(train.clean[,-60])
y <- as.factor(train.clean$popularity)
lasso.model <- glmnet(x = X, y = y ,family="multinomial")
s = min(lasso.model$lambda)
pfit = predict(lasso.model,test.clean,s=s,type="class")

cvfit = cv.glmnet(X, y, family="multinomial", type.multinomial = "grouped", dfmax = 20)

pfit = predict(cvfit, newx = test.clean, s = "lambda.min", type = "class")

#top 20 variable selected with lasso 
rankvar = data.frame(as.matrix(coef(cvfit, s = "lambda.min")[[1]]))

topvar = data.frame(Variable = row.names(rankvar), coef = abs(rankvar$X1))
topvar = topvar[which(topvar$coef >0),]
topvar = topvar[-1,]
topvar = as.character(topvar[,1])
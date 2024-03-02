library(plotmo) 
library(glmnet) 
library(ISLR)
library(caret)

setwd('C:/Users/sumay/OneDrive/New folder/Pictures/Documents/Fall 2020/sta314')

# read in the data
# d.train is the training set
d.train = read.csv('trainingdata.csv')
# d.test are the predictors in the test set
d.test = read.csv('test_predictors.csv')
lambdas = 10^seq(10,-2,length = 100)
y=d.train$y
id = d.test$id
#x = model.matrix(y ~ .,d.train)[,-1]
#ridge.mod = glmnet(x,y,alpha = 0, lambda =grid)

dummies1 <- dummyVars(y ~ ., data = d.train)
dummies2 <- dummyVars(id ~ ., data = d.test)

train_ = predict(dummies1, newdata = d.train)

test_ = predict(dummies2, newdata = d.test)

x = as.matrix(train_)
y_train = y

x_test = as.matrix(test_)
y_test = d.test$id



cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

ridge_reg = glmnet(x, y_train, alpha = 0, lambda = lambdas)


predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)

example_pred = data.frame(cbind(1:6000,predictions_test))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='Ridge.csv',row.names=FALSE)


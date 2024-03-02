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

set.seed(123)
train=sample(1:nrow(d.train),size=0.7*nrow(d.train))

test = (-train)
ytest = y[test]

x = model.matrix(y ~ ., d.train )[,-1]  
y = d.train$y


grid = 10^seq(10,-2,length = 100) 


#fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)
fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)

set.seed(1)
cv.lasso = cv.glmnet(x[train,],y[train],alpha =1, lambda = grid)
best.la = cv.lasso$lambda.min

pred.la = predict(fit.la, s = best.la, newx = x[test,])

mean((pred.la-ytest)^2)

# compare this with linear model including all predictors

predi.lm = predict(fit.la, s = 0, newx = x[test,])
mean((predi.lm-ytest)^2)


example_pred = data.frame(cbind(1:6000,pred.la))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='LassoUpdated1.csv',row.names=FALSE)


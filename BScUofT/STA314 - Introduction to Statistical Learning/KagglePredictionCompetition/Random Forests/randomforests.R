
library(tree)
library(MASS)
setwd('C:/Users/sumay/OneDrive/New folder/Pictures/Documents/Fall 2020/sta314') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

y= d.train$y

y.test=d.test$id
rf._1 = randomForest(y~., data = d.train, subset=train, mtry = 1)
rf._2 = randomForest(y~., data = d.train, subset=train, mtry = 2)
rf._6 = randomForest(y~., data = d.train, subset=train, mtry = 6)

yhat.rf1 = predict(rf._1,newdata=d.test)
yhat.rf2 = predict(rf._2,newdata=d.test)
yhat.rf6 = predict(rf._6,newdata=d.test)

#mean((yhat.tree - y.test)^2)
#mean((yhat.bag - y.test)^2)
mean((yhat.rf1 - y.test)^2)
mean((yhat.rf2 - y.test)^2) #m=2 does the best
mean((yhat.rf6 - y.test)^2)


example_pred = data.frame(cbind(1:6000,yhat.rf2))
names(example_pred) = c('id','y')

write.csv(example_pred,file='randomforests.csv',row.names=FALSE)

install.packages('tree')
install.packages('randomForest')
library(randomForest)
library(tree)
library(MASS)

setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

y= d.train$y

y.test=d.test$id

rf._1 = randomForest(y~., data = d.train, mtry = 1, subset = train)
rf._2 = randomForest(y~., data = d.train, mtry = 2)
rf._6 = randomForest(y~., data = d.train, mtry = 6)
rf._8 = randomForest(y~., data = d.train, mtry = 8)
rf.20 = randomForest(y~., data = d.train, mtry = 20)
rf.50 = randomForest(y~., data = d.train, mtry = 50)
rf.100 =randomForest(y~., data = d.train, mtry = 100)
rf.37 =randomForest(y~., data = d.train, mtry = 37)


yhat.rf1 = predict(rf._1,newdata=d.test)
yhat.rf2 = predict(rf._2,newdata=d.test)
yhat.rf6 = predict(rf._6,newdata=d.test)
yhat.rf100 = predict(rf.100,newdata=d.test)
yhat.rf37 = predict(rf.37,newdata=d.test)

#mean((yhat.tree - y.test)^2)
#mean((yhat.bag - y.test)^2)
mean((yhat.rf1 - y.test)^2)
mean((yhat.rf2 - y.test)^2)
mean((yhat.rf6 - y.test)^2)
mean((yhat.rf100 - y.test)^2)
mean((yhat.rf37 - y.test)^2)

example_pred = data.frame(cbind(1:6000,yhat.rf100))
names(example_pred) = c('id','y')

write.csv(example_pred,file='randomforests100.csv',row.names=FALSE)

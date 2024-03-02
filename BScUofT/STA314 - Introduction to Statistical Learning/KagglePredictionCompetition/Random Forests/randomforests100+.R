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


rf.100 =randomForest(y~., data = d.train, mtry = 100)
rf.101 =randomForest(y~., data = d.train, mtry = 101)
rf.102 =randomForest(y~., data = d.train, mtry = 102)
rf.112 = randomForest(y~., data = d.train, mtry = 112) # doing best
rf.111 = randomForest(y~., data = d.train, mtry = 111)
rf.110 = randomForest(y~., data = d.train, mtry = 110)

yhat.rf112 = predict(rf.112,newdata=d.test)

example_pred = data.frame(cbind(1:6000,yhat.rf112))
names(example_pred) = c('id','y')

write.csv(example_pred,file='randomforests112.csv',row.names=FALSE)
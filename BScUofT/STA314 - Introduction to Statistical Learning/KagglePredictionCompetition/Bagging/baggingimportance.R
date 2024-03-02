setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

install.packages('tree')
install.packages('randomForest')
library(randomForest)
library(tree)
library(MASS)

bag.boston.full = randomForest(y~.,data = d.train, mtry = 112, importance = TRUE)

yhat.tree = predict(bag.boston.full,newdata=d.test)

example_pred = data.frame(cbind(1:6000,yhat.tree))
names(example_pred) = c('id','y')

write.csv(example_pred,file='baggingimportance.csv',row.names=FALSE)
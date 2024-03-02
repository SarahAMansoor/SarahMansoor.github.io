setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

install.packages('tree')
install.packages('randomForest')
library(randomForest)
library(tree)
library(MASS)


tree.train = tree(y~.,data=d.train)
cv.train = cv.tree(tree.train)
best.prune = cv.train$size[which.min(cv.train$dev)]
prune.train = prune.tree(tree.train,best=best.prune)
yhat.tree = predict(prune.train,newdata=d.test)

example_pred = data.frame(cbind(1:6000,yhat.tree))
names(example_pred) = c('id','y')

write.csv(example_pred,file='pruningtrees.csv',row.names=FALSE)
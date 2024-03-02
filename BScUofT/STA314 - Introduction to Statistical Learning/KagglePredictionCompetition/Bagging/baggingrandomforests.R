setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

install.packages('tree')
install.packages('randomForest')
library(randomForest)
library(tree)
library(MASS)

rf.100 =randomForest(y~., data = d.train, mtry = 100)
rf.37 =randomForest(y~., data = d.train, mtry = 37)

rf.112 =randomForest(y~., data = d.train, mtry = 112)

yhat.rf100 = predict(rf.100,newdata=d.test)
yhat.rf37 = predict(rf.37,newdata=d.test)

train = sample(1:nrow(d.train), nrow(d.train)/2)  

bag.train500 = randomForest(y~., data = d.train, mtry = ncol(d.train)-1, ntree = 500) # does better than 112
bag.train100 = randomForest(y~., data = d.train, mtry = ncol(d.train)-1, ntree = 100) # does worse than 500
bag.train600 = randomForest(y~., data = d.train, mtry = ncol(d.train)-1, ntree = 600) # does worse than 500
bag.train550 = randomForest(y~., data = d.train, mtry = ncol(d.train)-1, ntree = 550) # does worse than 500

yhat.bag500 = predict(bag.train500,newdata=d.test)

example_pred = data.frame(cbind(1:6000,yhat.bag500))
names(example_pred) = c('id','y')

write.csv(example_pred,file='baggingrandomforests500.csv',row.names=FALSE)
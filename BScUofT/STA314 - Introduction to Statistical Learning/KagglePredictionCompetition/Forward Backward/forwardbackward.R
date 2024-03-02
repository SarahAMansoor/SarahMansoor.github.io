# here you will need to set your working directory to
# the place where you downloaded the data to
# in this directory you should have the following two files:
# trainingdata.csv
# test_predictors.csv
# those you will download from the kagge website

setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

# read in the data
# d.train is the training set
d.train = read.csv('trainingdata.csv')
# d.test are the predictors in the test set
d.test = read.csv('test_predictors.csv')

y = d.train$y

# libraries needed
install.packages('leaps')
install.packages('ISLR')

library(leaps)
library(MASS)
library(ISLR)

# leaving stepwise for nowwww

modelstart <- lm(y ~ 1, data = d.train)
modelall <- lm(y~., data = d.train)
stepModel <- step(modelstart, direction = 'both', scope = formula(modelall))
summary(stepModel)

predictions <- predict(stepModel, d.test)

example_pred = data.frame(cbind(1:6000,predictions))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='forwardbackward.csv',row.names=FALSE)
library(leaps) # this contains the function regsubsets
library(MASS)
library(ISLR)
setwd('C:/Users/sumay/OneDrive/New folder/Pictures/Documents/Fall 2020/sta314')

# read in the data
# d.train is the training set
d.train = read.csv('trainingdata.csv')
# d.test are the predictors in the test set
d.test = read.csv('test_predictors.csv')

# in the following we will generate predictions using the sample mean
# the following code repeats the sample mean of y 30000 times
# you will need to replace the following by better ways of generating predictions later
#mean_pred = rep(mean(d.train$y),6000)
#mean_pred

# now bring in the right format for submission
# the first column wiht name idshould be the number of the observation
# the second line is your prediction

y = d.train$y
#regfit.best = regsubsets(y~., data = d.train, really.big=TRUE) #default is 8
#summary(regfit.best)

#regfit.best = regsubsets(y~., data = d.train,nvmax = 11)
#coef(regfit.best,20)


regfit.forward = regsubsets(y~ ., data = d.train,method='forward')
# performs forward selection
#coef(regfit.forward,4)
#summary(regfit.forward)
coef(regfit.forward,8)
regfit.backward = regsubsets(y~.,data = d.train,method='backward')
summary(regfit.backward)
coef(regfit.backward,8)

adjr2back = summary(regfit.backward)$adjr2
cpback = summary(regfit.backward)$cp
bicback = summary(regfit.backward)$bic

adjr2for = summary(regfit.forward)$adjr2
cpfor = summary(regfit.forward)$cp
bicfor = summary(regfit.forward)$bic

which.max(adjr2back)
which.min(cpback)
which.min(bicback)

which.max(adjr2for)
which.min(cpfor)
which.min(bicfor)


example_pred = data.frame(cbind(1:6000,mean_pred))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='example.csv',row.names=FALSE)

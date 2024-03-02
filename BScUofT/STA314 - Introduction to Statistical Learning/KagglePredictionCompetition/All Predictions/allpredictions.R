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
d.train
# d.test are the predictors in the test set
d.test = read.csv('test_predictors.csv')
d.test

# generalized additive model

library('mgcv')

y = d.train$y

model_all_predictors <- lm(y ~ ., data = d.train)
summary(model_all_predictors)

predictions <- predict(model_all_predictors, d.test)
predictions


# now bring in the right format for submission
# the first column wiht name idshould be the number of the observation
# the second line is your prediction
example_pred = data.frame(cbind(1:6000,predictions))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='allpredictions.csv',row.names=FALSE)

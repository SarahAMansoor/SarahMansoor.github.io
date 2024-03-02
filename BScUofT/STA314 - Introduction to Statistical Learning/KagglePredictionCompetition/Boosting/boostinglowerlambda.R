install.packages('viridis')
library(viridis)
library(gbm)
library(MASS)

setwd('C:/Users/sumay/OneDrive/New folder/Pictures/Documents/Fall 2020/sta314')

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

y = d.train$y
y.test = d.test$id


boost.values = gbm(y~.,
 data=d.train,
 distribution='gaussian',
 n.trees = 5000,
 interaction.depth = 8,
 shrinkage = 0.001,
 cv.folds = 5
 )

# next line gives results from cross-validation
bi = gbm.perf(boost.values,method="cv")

bi

# predictions 
# note: once we have a model with N trees
# can specify any number of trees for prediction
# as long as it does not exceed N
pr.boo = predict(boost.values,newdata=d.test,n.trees=bi)
mean((pr.boo-y.test)^2)

example_pred = data.frame(cbind(1:6000,pr.boo))
names(example_pred) = c('id','y')

write.csv(example_pred,file='boosting0.001.csv',row.names=FALSE)













setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

y = d.train$y

# important predictors: X18, X23, X27, X30, X31, X44, X47, X48, X59, X61, X69, X72, X83, 
# X90, X92, X95, X110

fit = lm(y ~ poly(X18 + X23 + X27 + X30 + X31 + X44 + X47 + X48 + X59
                  + X61 + X69 + X72 + X83 + X90 + X92 + X95 + X110, degree = 2, 
                  raw = TRUE), data = d.train)
prediction <- predict(fit, d.test)

example_pred = data.frame(cbind(1:6000,prediction))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='polynomialregression.csv',row.names=FALSE)

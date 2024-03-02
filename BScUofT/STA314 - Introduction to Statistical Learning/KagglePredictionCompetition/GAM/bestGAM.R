setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

library('mgcv')

y = d.train$y

model_all_predictors <- lm(y ~ ., data = d.train)
summary(model_all_predictors)

model_1 <- lm(y ~ . - X2, data = d.train)
summary(model_1)
# Residual standard error: 4.125 on 1888 degrees of freedom
# Multiple R-squared:  0.2962,	Adjusted R-squared:  0.2548 
# F-statistic: 7.159 on 111 and 1888 DF,  p-value: < 2.2e-16

model_2 <- lm(y ~ . - X2 - X3 - X4 - X5 - X6 - X7 - X8 - X9 - X10 - X11 - X12 - 
                X13 - X14 - X15 - X16 - X17, data = d.train)
summary(model_2)
# Residual standard error: 4.122 on 1903 degrees of freedom
# Multiple R-squared:  0.2916,	Adjusted R-squared:  0.2559 
# F-statistic: 8.162 on 96 and 1903 DF,  p-value: < 2.2e-16

model_3 <- lm(y ~ . - X2 - X3 - X4 - X5 - X6 - X7 - X8 - X9 - X10 - X11 - X12 - 
                X13 - X14 - X15 - X16 - X17 - X19 - X20 - X21 - X24 - X25 - X26
              - X28 - X29,  data = d.train)
summary(model_3)
# Residual standard error: 4.118 on 1911 degrees of freedom
# Multiple R-squared:   0.29,	Adjusted R-squared:  0.2573 
# F-statistic:  8.87 on 88 and 1911 DF,  p-value: < 2.2e-16

model_4 <- lm(y ~ . - X2 - X3 - X4 - X5 - X6 - X7 - X8 - X9 - X10 - X11 - X12 - 
               X13 - X14 - X15 - X16 - X17 - X19 - X20 - X21 - X24 - X25 - X26
             - X28 - X29 - X32 - X33 - X34 - X35 - X36 - X37 - X38 - X39 - X40
             - X41 - X42 - X43,  data = d.train)
summary(model_4)
# Residual standard error: 4.115 on 1923 degrees of freedom
# Multiple R-squared:  0.2865,	Adjusted R-squared:  0.2583 
# F-statistic: 10.16 on 76 and 1923 DF,  p-value: < 2.2e-16

model_5 <- lm(y ~ . - X2 - X3 - X4 - X5 - X6 - X7 - X8 - X9 - X10 - X11 - X12 - 
                X13 - X14 - X15 - X16 - X17 - X19 - X20 - X21 - X24 - X25 - X26
              - X28 - X29 - X32 - X33 - X34 - X35 - X36 - X37 - X38 - X39 - X40
              - X41 - X42 - X43 - X46 - X49 - X51 - X52 - X54 - X55 - X56 - X57
              - X58 - X59 - X60 - X62 - X63 - X64 - X65 - X66 - X67 - X68 - X70
              - X71 - X73 - X74 - X75 - X76 - X77 - X78 - X79 - X80 - X81 - X81
              - X84 - X85 - X86 - X87 - X88 - X89 - X91 - X93 - X94 - X96 - X97
              - X98 - X99 - X100 - X101 - X102 - X103 - X104 - X105 - X106 - X107
              - X108 - X109 - X111 - X112,  data = d.train)
summary(model_5)
# Residual standard error: 4.115 on 1923 degrees of freedom
# Multiple R-squared:  0.2865,	Adjusted R-squared:  0.2583 
# F-statistic: 10.16 on 76 and 1923 DF,  p-value: < 2.2e-16


model_best <- lm(y ~ . - X2 - X3 - X4 - X5 - X6 - X7 - X8 - X9 - X10 - X11 - X12 - 
                X13 - X14 - X15 - X16 - X17 - X19 - X20 - X21 - X24 - X25 - X26
              - X28 - X29 - X32 - X33 - X34 - X35 - X36 - X37 - X38 - X39 - X40
              - X41 - X42 - X43 - X46 - X49 - X51 - X52 - X54 - X55 - X56 - X57
              - X58 - X59 - X60 - X62 - X63 - X64 - X65 - X66 - X67 - X68 - X70
              - X71 - X73 - X74 - X75 - X76 - X77 - X78 - X79 - X80 - X81 - X81
              - X84 - X85 - X86 - X87 - X88 - X89 - X91 - X93 - X94 - X96 - X97
              - X98 - X99 - X100 - X101 - X102 - X103 - X104 - X105 - X106 - X107
              - X108 - X109 - X111 - X112 - X1- X22 - X45 - X53 - X82,  
              data = d.train )
summary(model_best)

# ^^^ best GAM model

predictions <- predict(model_best, d.test)
predictions


# now bring in the right format for submission
# the first column wiht name idshould be the number of the observation
# the second line is your prediction
example_pred = data.frame(cbind(1:6000,predictions))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='allpredictions.csv',row.names=FALSE)

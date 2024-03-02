setwd('/Users/sarahmansoor/Desktop/Fall 2020/sta314/prediction') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

library(caret)

data_ctrl <- trainControl(method = 'cv', number = 10)

model_caret <- train(y ~ ., 
                     data = d.train,
                     trControl = data_ctrl, 
                     method = 'lm', 
                     na.action = na.pass)
model_caret

data_ctrl <- trainControl(method = 'cv', number = 5)

model_caret$finalModel

model_caret <- train(y ~ . - X2 - X8 - X9 - X10 - X11 - X13 - 
                       X15 - X16 - X19 - X20 - X21 - X24 -X25 - 
                       X26 - X27 - X33 - X35 - X38 - X40 - X41 - 
                       X42 - X46 - X48 - X51 - X52 - X54 - X55 -
                       X58 - X59 - X60 - X61 - X63 - X65 - X66 -
                       X67 - X71 - X76 - X78 - X79 - X81 - X82 -
                       X84 - X87 - X88 - X89 - X91 - X94 - X97 -
                       X98 - X100 - X101 - X102 - X103 - X104 - 
                       X105 - X106 - X108 - X109 - X111 - X34 -
                       X36 - X56 - X64 - X75 - X85 - X99 - X3 -
                       X4 - X5 - X29 - X37 - X14 - X17 - X28 -
                       X31 - X32 - X39 -X57 - X68 - X6 - X7 - 
                       X70 - X86 - X93, 
                     data = d.train,
                     trControl = data_ctrl, 
                     method = 'lm', 
                     na.action = na.pass)
model_caret

predictions <- predict(model_caret, d.test)

example_pred = data.frame(cbind(1:6000,predictions))
names(example_pred) = c('id','y')

write.csv(example_pred,file='crossvalidation.csv',row.names=FALSE)
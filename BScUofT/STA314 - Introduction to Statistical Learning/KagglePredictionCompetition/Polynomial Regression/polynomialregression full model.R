setwd('C:/Users/sumay/OneDrive/New folder/Pictures/Documents/Fall 2020/sta314') 

d.train = read.csv('trainingdata.csv')
d.test = read.csv('test_predictors.csv')

y = d.train$y

# important predictors: X18, X23, X27, X30, X31, X44, X47, X48, X59, X61, X69, X72, X83, 
# X90, X92, X95, X110
d.train[1:1,]
totalsum <-NULL
#d.train[,1]
head(d.train)
for(i in 2:ncol(d.train))
{
  #lm(y ~ poly(d.train[i], degree=2, raw=T))  
  #totalsum[i]<-poly(d.train[i], degree=2, raw=T)
  totalsum[i] = d.train[,i]
}

fit = lm(y ~ poly(X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+
                    X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27
                  +X28+X29+X30+X31+X32+X33+X34+X35+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50
                  +X51+X52+X53+X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+
                    +X71+X72+X73+X74+X75+X76+X77+X78+X79+X80+X81+X82+X83+X84+X85+X86+X87+X88+X89+X90+
                    X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+
                    X112, degree = 2, raw = TRUE), data = d.train)
prediction <- predict(fit, d.test)
summary(fit)
y_test = d.test$id
mean((prediction-y_test)^2)
example_pred = data.frame(cbind(1:6000,prediction))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='polynomial.csv',row.names=FALSE)

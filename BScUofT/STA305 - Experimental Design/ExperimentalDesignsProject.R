# CASE STUDY 1

attach(project_data_mercuryinfish)

# ques 1.a

require(ggplot2)
ggplot(project_data_mercuryinfish, aes(x = project_data_mercuryinfish$Type, y =  project_data_mercuryinfish$Mercury
                                       , fill = project_data_mercuryinfish$Dam)) + geom_boxplot()

attach(project_data_mercuryinfish_edited)

# setup

y <- Mercury_edited
a <- c(rep("Type 1", 6), rep("Type 2", 23), rep("Type 3", 17), rep("Type 1", 11), 
       rep("Type 2", 29), rep("Type 3", 26))
b <- c(rep("No Dam", 46), rep("Dam", 66))
lakes <- data.frame(mercury = y, type = a, dam = b)

ggplot(lakes, aes(x = type, y - mercury, fill = dam)) + geom_boxplot()

0
0
0

# ques. 1.c

# H0mercury: mue = 0.5 vs Hamercury: mue > 0.5

t.test(y, data = lakes, mu = 0.5, alternative = 'greater', paired = FALSE, var.equal = FALSE)

#
#One Sample t-test
#
#data:  y
#t = -2.1223, df = 111, p-value = 0.982
#alternative hypothesis: true mean is greater than 0.5
#95 percent confidence interval:
#  0.4049731       Inf
#sample estimates:
#  mean of x 
#0.4466607 

# we fail to reject H0 since the p-value is 0.2142 > 0.05 and the confidence interval contains 0.5 and
# goes above 0.5189 ... state more

0
0
0


# ques. 1. d

# H0: beta_dam = 0 vs. Ha: beta_dam /= 0

model_1 <- lm(mercury ~ dam, data = lakes)
anova(model1)
#Analysis of Variance Table
#
#Response: mercury
#           Df Sum Sq  Mean Sq F value  Pr(>F)  
#dam         1 0.2870 0.287047  3.7427 0.05549 .
#Residuals 115 8.8198 0.076694                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# We cannot reject the null hypothesis and say that there is an effect on the dam since
# p-value = 0.05549 > 0.05... say more

# post hoc analysis

#bonf. 

t.test(y, data = lakes, paired = FALSE, var.equal = FALSE, adj = "bonf")

#tukeys


# ^ not working

0
0
0


# ques. 1.e

# Ho: mue_1 = mue_2 = mue_3 vs Ha: At least two aren't equal

model_2 <- lm(mercury ~ type, data = lakes)
anova(model_2)
#Analysis of Variance Table
#
#Response: mercury
#            Df Sum Sq  Mean Sq F value Pr(>F)  
#type        2 0.6212 0.310616  4.6819 0.0112 *
#Residuals 109 7.2315 0.066344                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# since we reject H0 we must conduct a post-hoc test to see how. 

TukeyHSD(aov(model_2), conf.level = 0.95)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = model_2)
#
#$type
#                   diff         lwr        upr     p adj
#Type 2-Type 1  0.21655543  0.04556431 0.38754655 0.0090547
#Type 3-Type 1  0.13544049 -0.03990398 0.31078497 0.1629587
#Type 3-Type 2 -0.08111494 -0.20726894 0.04503907 0.2819582

tukeysCIs = TukeyHSD(aov(model_2), factor = a, conf.level = 0.95)
plot(tukeysCIs)

L <- matrix(c(
  1, -1, 0,
  0, 1, -1,
  1, 0, -1), byrow = TRUE, nrow = 3)
require(multcomp)
summary(glht(model_2, L), test = adjusted("bonferroni"))



0
0
0

# ques 1.f 

model_3 <- lm(mercury ~ dam * type, data = lakes)
anova(model_3)
#Analysis of Variance Table
#
#Response: mercury
#           Df Sum Sq Mean Sq F value  Pr(>F)  
#dam         1 0.4176 0.41762  6.7253 0.01085 *
#type        2 0.5593 0.27965  4.5034 0.01327 *
#dam:type    2 0.2935 0.14674  2.3630 0.09908 .
#Residuals 106 6.5823 0.06210                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(aov(model_3), conf.level = 0.95)

0
0
0

# ques 1. g

modelCR <- lm(mercury ~ type, data = lakes)
anova(modelCR)
#Analysis of Variance Table
#
#Response: mercury
#           Df Sum Sq  Mean Sq F value Pr(>F)  
#type        2 0.6212 0.310616  4.6819 0.0112 *
#Residuals 109 7.2315 0.066344                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

modelRCB <- lm(mercury ~ dam + type, data = lakes)
anova(modelRCB)
#Analysis of Variance Table
#
#Response: mercury
#           Df Sum Sq Mean Sq F value  Pr(>F)  
#dam         1 0.4176 0.41762  6.5597 0.01181 *
#type        2 0.5593 0.27965  4.3926 0.01465 *
#Residuals 108 6.8758 0.06366                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


0
0
0
0
0
0
0
0
0




# case study 2

attach(CASE_STUDY_2_DATA)

# ques 2.f

# ancova assumptions: guass-markov conditions, linear relationship b/w response Y and covariate x, homogeneity of regression slops: 
# slopes of regression lines in diff treatments are the same. 
# need to make sure the treatments regression lines are parallel. 

y <- y_II
a <- factor(rep(c("Vitamin A", "Vitamin B", "Vitamin C", "Vitamin D"), each = 5))
x <- x_II
animals <- data.frame(weight_gain = y, vitamin = a, caloric = x)
# plot of weight gain with caloric intake
plot(weight_gain ~ caloric, data = animals, main = "Plot of Weight Gain and Caloric Intake", xlab = "Caloric Intake", 
     ylab = "Weight Gain")

require(ggplot2)

qplot(caloric, weight_gain, data = animals, colour = vitamin, main = "Lease Squares Regression Lines for each Group"
      , xlab = "Caloric Intake", ylab = "Weight Gain") + geom_smooth(method = "lm", se = FALSE)

homo <- aov(weight_gain ~ caloric * vitamin, data = animals)
summary(homo)
#                Df Sum Sq Mean Sq F value Pr(>F)  
#caloric          1  391.1   391.1   4.430 0.0571 .
#vitamin          3 1501.0   500.3   5.667 0.0118 *
#caloric:vitamin  3  119.1    39.7   0.449 0.7223  
#Residuals       12 1059.6    88.3                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

y1 <- y[0:5]
y2 <- y[6:10]
y3 <- y[11:15]
y4 <- y[16:20]

x1 <- x[0:5]
x2 <- x[6:10]
x3 <- x[11:15]
x4 <- x[16:20]

var.test(x1, y1)
var.test(x2, y2)
var.test(x3, y3)
var.test(x4, y4)

var.test(weight_gain ~  vitamin, data = animals)

0
0
0

# ques 2.g

# Ho: b1 = b2 = b3 = 0 vs. Ha: at least one not equal to 0, with alpha = 0.05

model1 <- lm(weight_gain ~ vitamin, data = animals)
anova(model1)
#Analysis of Variance Table
#
#Response: weight_gain
#          Df Sum Sq Mean Sq F value Pr(>F)
#vitamin    3  802.0  267.33  1.8853 0.1728
#Residuals 16 2268.8  141.80 

# FTR Ho

0
0
0

# ques 2. h

# Ho: b2 = b3 = b4 = 0 vs Ha: at least one not 0

model2 <- lm(weight_gain ~ caloric + vitamin, data = animals)
anova(model2)
#Analysis of Variance Table
#
#Response: weight_gain
#          Df  Sum Sq Mean Sq F value   Pr(>F)   
#caloric    1  391.13  391.13  4.9778 0.041361 * 
#vitamin    3 1501.05  500.35  6.3678 0.005352 **
#Residuals 15 1178.62   78.57                    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#look at p-value of vitamin

# reject Ho, different results when covariate is taken into account

0
0
0


# ques 2.i

model2 <- lm(weight_gain ~ caloric + vitamin, data = animals)
summary(model2)
#
#Call:
#lm(formula = weight_gain ~ caloric + vitamin, data = animals)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-14.5792  -4.0661  -0.3448   5.9484  15.3271 
#
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)       -9.2859    19.8073  -0.469  0.64595   
#caloric            1.6354     0.4391   3.725  0.00203 **
#  vitaminVitamin B  -4.2917     5.6172  -0.764  0.45671   
#vitaminVitamin C  -2.0521     5.7213  -0.359  0.72483   
#vitaminVitamin D -24.0126     6.1943  -3.877  0.00149 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 8.864 on 15 degrees of freedom
#Multiple R-squared:  0.6162,	Adjusted R-squared:  0.5138 
#F-statistic:  6.02 on 4 and 15 DF,  p-value: 0.004273

# look at p-value of caloric

0
0
0

# ques 2.j

# Ho: b2 = b3 = b4 = b5 = b6 = b7 = 0 vs Ha: at least one not 0

model3 <- lm(weight_gain ~ caloric * vitamin, data = animals)
anova(model3)
#Analysis of Variance Table
#
#Response: weight_gain
#                 Df  Sum Sq Mean Sq F value  Pr(>F)  
#caloric          1  391.13  391.13  4.4297 0.05707 .
#vitamin          3 1501.05  500.35  5.6667 0.01182 *
#caloric:vitamin  3  119.06   39.69  0.4495 0.72229  
#Residuals       12 1059.56   88.30                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# here vitamin seems to have an effect, but caloric intake does not. and caloric intake with 
# vitamin does not either.

tukeysCIs = TukeyHSD(aov(model2), factor = a, conf.level = 0.95)
plot(tukeysCIs)



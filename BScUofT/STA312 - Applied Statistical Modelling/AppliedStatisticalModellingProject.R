attach(lifetime)

# question 3 a
data_proj <- stack(lifetime)
X <- data_proj$values
expected_value <- mean(X)
expected_value
#[1] 68.80698
n <- 1000
variance_proj <- mean((X - expected_value)^2) / n
variance_proj
#[1] 4.673983
conf_proj <- c(expected_value - 1.96 * sqrt(variance_proj), expected_value + 1.96 * sqrt(variance_proj))
conf_proj
#[1] 64.56958 73.04438

# question 3 b

n <- 1000
median(X) #sample median
# [1] 48.41656
kurtosis_s = sum((X-mean(X))^4)/n/ var(X)^2 
kurtosis_s # sample kurtosis
# [1] 8.17837

B <- 200
bootstrap.statM <- NULL
bootstrap.statK <- NULL
for (b in 1:B){
  i <- sample(1:n, size = n, replace = TRUE)
  X.boot <- X[i]
  M <- median(X.boot)
  fourth.moment <- sum((X.boot - mean(X.boot))^4)/n
  K <- fourth.moment/var(X.boot)^2
  bootstrap.statM <- c(bootstrap.statM, M)
  bootstrap.statK <- c(bootstrap.statK, K)
}

M <- mean(bootstrap.statM)
K <- mean(bootstrap.statK)
M
#[1] 47.97574
K
#[1] 8.041159

# 95% CI for median and kurtosis
alpha <- 0.05

lowerM <- M - qnorm(1 - alpha/2)*sd(bootstrap.statM)
upperM <- M + qnorm(1 - alpha/2)*sd(bootstrap.statM)
lowerM
upperM

#(43.305033, 52.64615)

lowerK <- K - qnorm(1 - alpha/2)*sd(bootstrap.statK)
upperK <- K + qnorm(1 - alpha/2)*sd(bootstrap.statK)
lowerK
upperK

# (5.662382, 10.41994)

# Normal Bootstrap CI for median and kurtosis
lowerM <- median(X) - qnorm(1 - alpha/2)*sd(bootstrap.statM)
upperM <- median(X) + qnorm(1 - alpha/2)*sd(bootstrap.statM)

lowerM
#[1] 43.87595
upperM
#[1] 52.95716

lowerK <- kurtosis_s - qnorm(1 - alpha/2)*sd(bootstrap.statK)
upperK <- kurtosis_s + qnorm(1 - alpha/2)*sd(bootstrap.statK)

lowerK
#[1] 5.685464
upperK
#[1] 10.67128

par(mfrow = c(1,2))
hist(bootstrap.statK, breaks = floor(sqrt(B)), freq = FALSE, main = 'Histogram of Bootstrapped Kurtosis Estimate', xlab = 'Kurtosis Estimates')
hist(bootstrap.statM, breaks = floor(sqrt(B)), freq = FALSE, main = 'Histogram of Bootstrapped Median Estimate', xlab = 'Median Estimates')


# ques 3.c

# H0: X ~ exp(lambda) vs. Ha: X /~ exp(lambda) alpha = 0.05

hist(X, main = 'Histogram of Lifetime Data')

library("fitdistrplus")

fw <- fitdist(X, 'exp')

summary(fw)

#> summary(fw)
#Fitting of the distribution ' exp ' by maximum likelihood 
#Parameters : 
#  estimate   Std. Error
#rate 0.01453341 0.0004573987
#Loglikelihood:  -5231.305   AIC:  10464.61   BIC:  10469.52 

ks.test(X, 'pexp', rate = 0.01453341)
#> ks.test(X, 'pexp', rate = 0.01453341)
#
#One-sample Kolmogorov-Smirnov test
#
#data:  X
#D = 0.017823, p-value = 0.9085
#alternative hypothesis: two-sided

# based on this p-value > 0.05 so we can't reject

# ques 1.d

n <- 1000
Y <- rexp(n, rate = 0.01453341)
median(Y)
#[1] 51.46687
kurtosis_t = sum((Y-mean(Y))^4)/n/ var(Y)^2
kurtosis_t
#[1] 9.192094

n <- 1000
Y <- rexp(n, rate = 0.01453341)
B <- 200
bootstrap.statMT <- NULL
bootstrap.statKT <- NULL
for (i in 1:B){
  boot.s <- NULL
  boot.s <- sample(Y, replace = TRUE)
  M_T <- median(boot.s)
  fourth.moment_T <- sum((boot.s - mean(boot.s))^4)/n
  K_T <- fourth.moment_T/var(boot.s)^2
  bootstrap.statMT <- c(bootstrap.statMT, M_T)
  bootstrap.statKT <- c(bootstrap.statKT, K_T)
}

M_T <- mean(bootstrap.statMT)
K_T <- mean(bootstrap.statKT)
M_T
#[1] 47.81113
K_T
#[1] 9.201635

# 95% CI for median and kurtosis
alpha <- 0.05

# Normal Bootstrap CI for median and kurtosis
lowerM_T <- median(Y) - qnorm(1 - alpha/2)*sd(bootstrap.statMT)
upperM_T <- median(Y) + qnorm(1 - alpha/2)*sd(bootstrap.statMT)

lowerM_T # lower exp bootstrapped median 
#[1] 47.31662
upperM_T # upper exp bootstrapped median 
#[1] 56.14983

lowerK_T <- kurtosis_t - qnorm(1 - alpha/2)*sd(bootstrap.statKT)
upperK_T <- kurtosis_t + qnorm(1 - alpha/2)*sd(bootstrap.statKT)

lowerK_T # lower exp bootstrapped kurtosis
#[1] 6.64921
upperK_T # upperr exp bootstrapped kurtosis
#[1] 11.66061

par(mfrow = c(1,2))

hist(X, main = 'Histogram of Lifetime Data')

hist(Y, main = 'Histogram of Y~exp(0.01453341)')

par(mfrow = c(1,2))

hist(bootstrap.statK, breaks = floor(sqrt(B)), freq = FALSE, 
     main = 'Histogram of Bootstrapped Kurtosis Estimate', xlab = 'Kurtosis Estimates')
hist(bootstrap.statKT, breaks = floor(sqrt(B)), freq = FALSE, 
     main = 'Histogram of Bootstrapped Kurtosis Estimate with Y~exp(0.01453341)', xlab = 'Kurtosis Estimates')

hist(bootstrap.statM, breaks = floor(sqrt(B)), freq = FALSE, 
     main = 'Histogram of Bootstrapped Median Estimate', xlab = 'Median Estimates')
hist(bootstrap.statMT, breaks = floor(sqrt(B)), freq = FALSE, 
     main = 'Histogram of Bootstrapped Median Estimate with Y~exp(0.01453341)', xlab = 'Median Estimates')

# ques 1.e

# values

expected_value
#[1] 68.80698
variance_proj
#[1] 4.673983
conf_proj
#[1] 64.56958 73.04438

median(X) #sample median
# [1] 48.41656
kurtosis_s # sample kurtosis
# [1] 8.17837

lowerM
#[1] 43.87595
upperM
#[1] 52.95716
lowerK
#[1] 5.685464
upperK
#[1] 10.67128

summary(fw)
#Fitting of the distribution ' exp ' by maximum likelihood 
#Parameters : 
#  estimate   Std. Error
#rate 0.01453341 0.0004573987
#Loglikelihood:  -5231.305   AIC:  10464.61   BIC:  10469.52
Y <- rexp(n, rate = 0.01453341)
median(Y) # theoretical median
#[1] 51.46687
kurtosis_t # theoretical kurtosis
#[1] 6.75745

lowerM_T # lower exp bootstrapped median 
#[1] 47.31662
upperM_T # upper exp bootstrapped median 
#[1] 56.14983
lowerK_T # lower exp bootstrapped kurtosis
#[1] 6.64921
upperK_T # upperr exp bootstrapped kurtosis
#[1] 11.66061




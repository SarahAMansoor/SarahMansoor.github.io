rm(list=ls())

# install package
install.packages("TULIP")
library(TULIP)

# number of Monte Carlo runs
nrep<-100

# sample size
n<-50

# number of variables/predictors
p = 1000

# number of non-zero mean differences
s = 10

# pairwise correlation coeff
rho = 0.5

# set seed
set.seed(123)

# gererating the data
# means
mu1 = rep(0L, p)
mu2 = rep(0L, p)

# covariance matrix
sigma = (1- rho)*diag(1, p, p) + rho*rep(p, p)

# Three different statistics to estimate the mean, and we want to compare performance
T1<-ROAD(x,y,standardize=FALSE,lambda=NULL,eps=1e-7)
  
T2<-ROAD(x,y,standardize=FALSE,lambda=NULL,eps=1e-7)
  
# T3<-rep(NA, nrep)


for (s in 1:nrep){
  # The random seed for the sth Monte Carlo run
  set.seed(2018+100*s)	
  #### y1,..., yn are iid generated from N(2,1)
  y<-rnorm(n,mu,sigma)
  
  #### y1,..., yn are iid generated from centred F_{3,5}
  # y<-rf(n, 3, 5)
  
  #### T1 (sample mean) calculated from the sth Monte Carlo run
  T1[s]<-mean(y)
  #### T2	(20% trimmed mean) calculated from the sth Monte Carlo run
  T2[s]<-mean(y, trim=0.1)
  
  #### T3 
  T3[s]<-median(y)
}

#### Results for T1
# Estimated mean for T1
T1estimatedmean<-mean(T1)

# Estimated standard deviation for T1
T1estimatedsd<-sd(T1)

# standard error for estiamted mean for T1
seT1estimatedmean<-T1estimatedsd/sqrt(nrep)

# Estimated bias for T1
T1estimatedbias<-T1estimatedmean-mu


# Estimated MSE for T1
T1estimatedMSE<-T1estimatedsd^2+T1estimatedbias^2


#### Results for T2
# Estimated mean for T2
T2estimatedmean<-mean(T2)

# Estimated standard deviation for T2
T2estimatedsd<-sd(T2)

# standard error for estiamted mean for T2
seT2estimatedmean<-T2estimatedsd/sqrt(nrep)

# Estimated bias for T2
T2estimatedbias<-T2estimatedmean-mu

# Estimated MSE for T2
T2estimatedMSE<-T2estimatedsd^2+T2estimatedbias^2


#### Results for T3
# Estimated mean for T3
T3estimatedmean<-mean(T3)

# Estimated standard deviation for T3
T3estimatedsd<-sd(T3)

# standard error for estiamted mean for T3
seT3estimatedmean<-T3estimatedsd/sqrt(nrep)

# Estimated bias for T3
T3estimatedbias<-T3estimatedmean-mu

# Estimated MSE for T3
T3estimatedMSE<-T3estimatedsd^2+T3estimatedbias^2





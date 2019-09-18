#Creating a 20*10 matrix
matX <- matrix(rnorm(20*10),ncol=20,nrow=10)

#creating the beta
beta <- c(rep(1,5),rep(0,15))
beta

#Randomly generating the y
y <- matX%*%beta + rnorm(10,sd=0.1)

#Perform a regression
mod <- lm(y~matX)
summary(mod)
#As we can expect the normal linear regression failed because too much beta

#Let's try the ridge regression
library(MASS)
lm.ridge(y~matX-1,lambda=0.1) #The -1 mean that we try to explain y without an intercept

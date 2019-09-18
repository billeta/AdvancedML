#####LINEAR REGRESSION##

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

#####RIDGE REGRESSION#####

#Let's try the ridge regression
library(MASS)
lm.ridge(y~matX-1,lambda=0.1) #The -1 mean that we try to explain y without an intercept

#we can do it with glmnet library as well
library(glmnet)
model <- glmnet(matX,y,alpha=0,lambda = 0.1) #lambda is the tuning parameter and we want to find the best one (use cross validation)

#this is the beta of the model
as.vector(model$beta)


#Lets see how ridge regression fails with correlation between variables
set.seed(10)
#generating correlated variables
x1 <- rnorm(20)
x2 <- rnorm(20,mean=x1,sd=.01)
#Checking correlation
cor(x1,x2)

#Creating the model
y <- 3+x1+x2+rnorm(20)
#Run the model
lm(y~x1+x2)$coef

#Checking the estimator
lm.ridge(y~x1+x2,lambda=1)
#!!!!! Careful correlated data give back bad estimator the x1 and x2 harshly different!


#lets try with the breath cancer dataset
load("bcTCGA.RData")
#Checking the dim of the dataset
dim(X)

#We try to find the best beta to choose using a cross validation (10 folds by default)
tune <- cv.glmnet(X,y,alpha=0)
plot(tune)

#performing ridge regretion using the best value of beta
ridge <- glmnet(X,y,lambda=tune$lambda.1se,alpha=0)

#Checking how many parameters are relevant (>0.003)
length(which(abs(ridge$beta[,1])>0.003))

#Computing the MSEP
mean((predict(ridge,newx = X)-y)**2)

#looking at our new parameters
head(ridge$beta)


######Lasso Regression#####
#We try to compute the best beta iwh the cross validation
tune.lasso <- cv.glmnet(X,y,alpha=1)
plot(tune.lasso)

#We can then run a Lasso regression
lasso <- glmnet(X,y,lambda=tune.lasso$lambda.1se,alpha=1)

#We can then see which genes will be selected by the Lasso model
colnames(X)[nonzeroCoef(lasso$beta)]
#As we can see we've reduce the number of genes to 51

#Let's finish by computing the MSEP
mean((predict(lasso,newx = X)-y)**2)

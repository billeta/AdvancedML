#####Big Lasso example#####

#Loading the dependencies
library(biglasso)

#loading our dataset
data(colon)

#Getting the explanatory variable
X <- colon$X

#The response variable
y <- colon$y

#Checking the dim of the dataset
dim(X)

#Changing the type of the dataset X into a big matrix (big memory)
X.bm <- as.big.matrix(X)
str(X.bm) #It will be store on the hard drive instead of the RAM

#Fitting a Lasso regression on the big matrix X
fit <- biglasso(X.bm, y, family = "binomial") #Binomial because our response variable y is a binary variable
#Plotting what we get
plot(fit)

#Performing a cross validation with 10 folds
cvfit <- cv.biglasso(X.bm, y, seed = 1234, family = "binomial", nfolds = 10, ncores = 4)

par(mfrow = c(2, 2), mar = c(3.5, 3.5, 3, 1) ,mgp = c(2.5, 0.5, 0))
plot(cvfit, type = "all")

summary(cvfit)

plot(cvfit$fit)
abline(v = log(cvfit$lambda.min), col = 2, lty = 2)

coefs <- as.matrix(coef(cvfit))
coefs[coefs != 0, ]

as.vector(predict(cvfit, X = X.bm, type = "class"))

predict(cvfit, type = "nvars")

predict(cvfit, type = "vars")
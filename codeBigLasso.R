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
#beta hat represent the importance of every variables
#We try to look at the value of the beta hat for each value of lambda

#Performing a cross validation with 10 folds to choose the tuning parameter (it computed in a parallel way to be more efficient)
cvfit <- cv.biglasso(X.bm, y, seed = 1234, family = "binomial", nfolds = 10, ncores = 4)

#Setting the Plots area to be able to plot everything we need in the same windows
par(mfrow = c(2, 2), mar = c(3.5, 3.5, 3, 1) ,mgp = c(2.5, 0.5, 0))
#Plotting the results of the cross-validation
plot(cvfit, type = "all")
#Many king of predictor to look at the best lambda that will select the more efficient number of selected variables

#Set back the usual plot area
par(mfrow = c(1,1))


#Look at the cross validation's summary
summary(cvfit)

#Plot the big lasso model with the optimal lambda
plot(cvfit$fit)
abline(v = log(cvfit$lambda.min), col = 2, lty = 2)
#The selected beta hat are the one cut by the red dotted line (optimal lambda in term of prediction error)

#Get the coefficient of each variables in the model
coefs <- as.matrix(coef(cvfit))
#keeping only the one that are significant (greater than 0)
coefs[coefs != 0, ]

#Then we try to predict output
as.vector(predict(cvfit, X = X.bm, type = "class")) #We can replace class by response to get the actual probabilities

#Let's check if the prediction is good or not
length(which(as.vector(predict(cvfit, X = X.bm, type = "class")) == y))
#We have predicted well 57 patients over 62, that is 92% of the patients

#Look at the number of variables that is useful
predict(cvfit, type = "nvars")

#Checking the vars
predict(cvfit, type = "vars")

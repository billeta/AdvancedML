#Dependencies
library(bigstatsr)

#creating the .rds
system.time(MNIST <- big_read("MNIST10000.csv",select=1:784,type="integer"))

#Looking at the dataset
MNIST <- big_attach("MNIST10000.rds")
class(MNIST)


set.seed(1)
#Explanatory variable
X <- big_attachExtdata()
#number of row
n <- nrow(X)
#Response variables
y <- rnorm(n)
#Covariance matrix
covar <- matrix(rnorm(n * 3), n)

#look at the dim of the dataset
dim(X)

#We are going to do a volcano plot by creating an univariate linear model
test <- big_univLinReg(X, y, covar = covar)
plot(test, type = "Volcano")

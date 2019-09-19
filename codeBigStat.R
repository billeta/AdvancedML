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

X1 <- X[, 1]

#We are going to do a volcano plot by creating an univariate linear model
test <- big_univLinReg(X, y, covar = covar)
plot(test, type = "Volcano")
#It as done one simple linear model each time for every variable
#The value located on the top on the graph are significant because they have a small p-value
#They also affect well the response variable because the estimate are not close to 0

#Getting the p value from the test
test$p.value <- predict(test, log10 = FALSE)
str(test)

summary(lm(y ~ X1 + covar))$coefficients[2, ]


#####Sparse Linear Regression#####

set.seed(1)

# simulating some data
N <- 230
M <- 730
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y <- rowSums(X[, 1:5]) + rnorm(N)

#Computing the covariance matrix
covar <- matrix(rnorm(N * 3), N)

#Setting train and test subset
ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)

#Running a sparse linear regression on the train set
test <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                     covar.train = covar[ind.train, ])

#Using it to predict output of the test
preds <- predict(test, X, ind.row = ind.test, covar.row = covar[ind.test, ])

#Plotting the predictions
plot(preds, y[ind.test], pch = 20); abline(0, 1, col = "red")
#They are close to the red line, the model predict well


#Simulating again some new data
set.seed(2)
# simulating some data
N <- 230
M <- 730
#Randomly generating
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
#Response variable
y01 <- as.numeric((rowSums(X[, 1:5]) + 2 * rnorm(N)) > 0)
#Creating the covariance matrix
covar <- matrix(rnorm(N * 3), N)

#Creating our train and test dataset
ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)

#Performing a spaarse logistic model on the train dataset
test <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                     covar.train = covar[ind.train, ])

#Compute the prediction on the test data using our model
preds <- predict(test, X, ind.row = ind.test, covar.row = covar[ind.test, ])

#Dependencies
library(ggplot2)

#Plotting the results in order to look at the model performance
qplot(preds, fill = as.logical(y01[ind.test]), geom = "density", alpha = I(0.4)) +
  labs(fill = "Case?") +
  theme_bigstatsr()


AUC(preds, y01[ind.test])
?AUC



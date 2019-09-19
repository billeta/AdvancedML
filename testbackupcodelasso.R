#####DEALING WITH BIG DATA USING BIG LASSO LIBRARY#####


#Loading dependencies
library(biglasso)

#Generating the bin and desc file
X <- setupX(filename = "X_3000_1340000_200_logistic.txt")

#Getting the desc
dataXdesc<-dget("X_3000_1340000_200_logistic.desc")
system.time(dataX <- attach.big.matrix(dataXdesc))

#get the type
str(dataX)

#Check the dim
dim(dataX)

#Look at some data
dataX[1:10,1:9]

#Response variable
y <- as.matrix(read.table("y_3000_1340000_200_logistic.txt", header = F))

#Look at the characteristic of the response variable
class(y)
dim(y)
table(y)

#Running a lasso on the HUGE dataset
time <- system.time(
  fit <- biglasso(dataX, y, family = "binomial", screen = "SSR-Slores",ncores = 6)
)
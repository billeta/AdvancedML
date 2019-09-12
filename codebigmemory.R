#We load the library
library(bigmemory)

#Create the bin and desc file
#x <- read.big.matrix("Netflix.txt", sep = ",", type = "integer",
#                     backingfile = "Netflix.bin", descriptor = "Netflix.desc",
#                     shared = TRUE, col.names = c("movie", "customer", "rating","year", "month"))
#


#We need to change the dirname of the .desc
dataXdesc <- dget("Netflix.desc")
system.time(dataX <- attach.big.matrix(dataXdesc))

dataX[1:3,1:4]

#check the dim
dim(dataX)

summary(dataX[,1:4])

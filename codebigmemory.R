#We load the library
library(bigmemory)
library(biglm)
library(biganalytics)

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

#Summary of the four columns
summary(dataX[,1:4])

#Range of column 1
range(dataX[,1])

#we might be interested in customer 2442’s
#movies which were rated 2 or worse during February through October of 2004
these <- mwhich(dataX, c("customer", "year", "month", "rating"), list(2442, 2004,
                                                                  c(2, 10), 2), list("eq", "eq", c("ge", "le"), "le"), "AND")
dataX[these, ]

#We provide the movie titles to place these ratings in context
mnames <- read.csv("movie_titles.csv", header = FALSE)
names(mnames) <- c("movie", "year", "Name of Movie")
mnames[mnames[, 1] %in% unique(dataX[these, 1]), c(1, 3)]

#We'll try to run a bigglm on the netflix dataset
model = biglm.big.matrix(rating ~ year, data = dataX, fc = "year")
print(summary(model)$mat)

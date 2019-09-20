#https://rpubs.com/raviolli77/352956

wdbc <- read.table("wdbc.txt", sep = ",")
wdbc[,2] <- as.factor(wdbc[,2])
levels(wdbc[,2]) <- c(0, 1)
#M = Malignant
#B = Benigm
wdbc[,2]

#Basically use python program to replace this response variable



#Dependencies
library(bigmemory)

#Create a bigmemory matrix
x <- read.big.matrix("wdbc.txt", sep = ",", type = "integer",
                                          backingfile = "wdbc.bin", descriptor = "wdbc.desc",
                                          shared = TRUE, col.names = c('id_number', 'diagnosis', 'radius_mean', 
                                                                       'texture_mean', 'perimeter_mean', 'area_mean', 
                                                                       'smoothness_mean', 'compactness_mean', 
                                                                       'concavity_mean','concave_points_mean', 
                                                                       'symmetry_mean', 'fractal_dimension_mean',
                                                                       'radius_se', 'texture_se', 'perimeter_se', 
                                                                       'area_se', 'smoothness_se', 'compactness_se', 
                                                                       'concavity_se', 'concave_points_se', 
                                                                       'symmetry_se', 'fractal_dimension_se', 
                                                                       'radius_worst', 'texture_worst', 
                                                                       'perimeter_worst', 'area_worst', 
                                                                       'smoothness_worst', 'compactness_worst', 
                                                                       'concavity_worst', 'concave_points_worst', 
                                                                       'symmetry_worst', 'fractal_dimension_worst'))


#get the .desc file in order to work with our dataset
wdbcDesc <- dget("wdbc.desc")
wdbc <- attach.big.matrix(wdbcDesc)

#Check the date starting by the head
head(wdbc)

#Looking at the dim
dim(wdbc)
#We can see that we have a dataset of 569 rows and 32 columns
#We'll consider it as tall data even though there is not that much column
#We'll work as it was a real huge dataset with a lot of rows

#Let's check the summary of our dataset
summary(wdbc[,1:4])

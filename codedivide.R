library(parallel)
cores <- detectCores() ## How many cores do I have?
print(cores)

library(biglm)
one.simu <- function(i){
  ## draw data
  n <- 1000; p <- 500
  x <- matrix(rnorm(n*p),n,p) ; y <- rnorm(n)
  ## return ridge's coefficients
  train <- 1:floor(n/2)
  test <- setdiff(1:n,train)
  ridge <- lm.ridge(y~x+0,lambda=1,subset=train)
  err <- (y[test] - x[test, ] %*% ridge$coef )^2
  return(list(err = mean(err), sd = sd(err)))
}

library(MASS)

out <- mclapply(1:8, one.simu, mc.cores=cores)
head(do.call(rbind, out))

#Divide and combine
readchunk <- function(X, g, size.chunk) {
  rows <- ((g - 1) * size.chunk + 1):(g * size.chunk)
  chunk <- X[rows,]
}
cpc <- function(X, Y, ng = 1) {
  readchunk <- function(X, g, size.chunk) {
    rows <- ((g - 1) * size.chunk + 1):(g * size.chunk)
    chunk <- X[rows,]
  }
  res <- foreach(g = 1:ng, .combine = "+") %dopar% {
    size.chunk <- nrow(X) / ng
    chunk.X <- readchunk(X, g, size.chunk)
    chunk.Y <- readchunk(Y, g, size.chunk)
    term <- t(chunk.X) %*% chunk.Y
  }
  return(res)
}

set.seed(1)
n <- 10000
p <- 4
q <- 3
X <- matrix(rnorm(n*p),ncol=4,nrow=n)
Y <- matrix(rnorm(n*q),ncol=3,nrow=n)
res <- crossprod(X,Y)
res

library(doSNOW)
cl <- makeCluster(4)
registerDoSNOW(cl)
res.cpc <- cpc(X,Y,ng=10)
stopCluster(cl)
res.cpc

##7.3
dataX <- attach.big.matrix(dget("Xda.desc"))
Xdes <- describe(dataX)
Xdes

dim(dataX)

XtX.big <- function(X.des, ng = 1) {
  readchunk <- function(X, g, size.chunk) {
    rows <- ((g - 1) * size.chunk + 1):(g * size.chunk)
    chunk <- X[rows,]
  }
  res <- foreach(g = 1:ng, .packages = c("bigmemory"), .combine = "+") %dopar% {
    X <- attach.big.matrix(X.des)
    size.chunk <- nrow(X) / ng
    chunk.X <- readchunk(X, g, size.chunk)
    term <- t(chunk.X) %*% chunk.X
  }
  return(res)
}

library(doSNOW)
cl <- makeCluster(4)
registerDoSNOW(cl)
cl <- makeCluster(4)
res.big <- XtX.big(Xdes,ng=10)
stopCluster(cl)
summary(res.big[,101])

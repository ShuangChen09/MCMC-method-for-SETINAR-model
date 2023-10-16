## Binary Sparse Operator ##
bto <- function(alpha, x) {
  if (x > 0) {
    result1 <- 0
    for (i in 1:x) {
      b <- rbinom(1, 1, alpha)
      result1 <- result1 + b
    }
    result <- result1
  } else if (x == 0) {
    result <- 0
  }
  return(result)
}


## Define SETINAR Model ##
setinar <- function(length, alpha1, alpha2, lambda, r) {
  x <- rep(0, length + 1)
  for (t in 2:(length + 1)) {
    if (x[t - 1] <= r) {
      x[t] <- bto(alpha1, x[t - 1]) + rpois(1, lambda)
    } else {
      x[t] <- bto(alpha2, x[t - 1]) + rpois(1, lambda)
    }
  }
  return(x[2:(length + 1)])
}


## data generation ##
set.seed(8888)
##scenario1##
data1_100=setinar(100,0.2,0.1,3,4)
data1_300=setinar(300,0.2,0.1,3,4)
data1_500=setinar(500,0.2,0.1,3,4)

##scenario2##
data2_100=setinar(100,0.8,0.1,7,21)
data2_300=setinar(300,0.8,0.1,7,21)
data2_500=setinar(500,0.8,0.1,7,21)

#Data Mining hw 14

library(doMC)
registerDoMC()

source('~/Dropbox/Tarleton/data_mining/generic_functions/dataset_ops.R')

#1 Use gradient descent to minimize the function f(x, y) = x^2 + y^2 + y
#  on the open disk x^2 + y^2 < 1. This is essentially the example on 
#  p. 7 of the Lagrange multipliers slides, except that we are only 
#  interested in interior points on this problem. 

#  (Hints: Start by randomly selecting a vector (x, y) in the disk, and 
#   then apply gradient descent. If the vector excapes the disk during the 
#   algorithm, randomly select a new vector. Store vectors that converge 
#   to a minimum in a matrix, and repeat the entire process a large number 
#   of times to ensure that there is only one local minimum inside the disk. 
#   You may need to take some care with the learning rate to prevent vectors 
#   from escaping the disk too often.)

f <- function(x){
  return( sum(x^2) + x[2])
}

grad_f <- function(x){
   return(c(2*x[1], 2*x[2] + 1))
}

params <- function(x){
  if( sum(x^2) < 1){
  	return(TRUE)
  }else{
  	return(FALSE)
  }
}

initial_guess <- function(){
  x <- runif(2, -1, 1)
  return(x)
}

grad_descent <- function(f, grad_f, params, initial_guess, x0, eps, tol, niter){
  i = 1; NOT_ZERO = TRUE
  x = x0
  while((i < niter) && (NOT_ZERO)){
    i = i + 1
    xo = x
    x = xo - eps * grad_f(x)
    if(f(x) < f(xo)){
    	  eps = eps * 1.1
    }else{
    	  eps = eps * 0.5
    	}
    if(params(x)){
      if(abs(sum(grad_f(x)^2)) < tol){
        NOT_ZERO = FALSE
      }
    }else{
      x <- initial_guess()
      eps = 0.75 * eps
      i = 1
    }
  }
  return(list(grad = grad_f(x), x = x, dx = eps, 
              nit = i, converged = !NOT_ZERO))
}

######################################################################
# Initialize matrix of initial guesses and compute gradient descent  #
######################################################################
n_guesses = 300
x_m <- matrix(runif(n_guesses * 2, -1, 1), nrow = n_guesses, ncol = 2)
vals <- matrix(,nrow=n_guesses,ncol=2)

vals <- foreach(i=1:n_guesses, .combine=rbind) %dopar% {
  grad_descent(f, grad_f, params, initial_guess, x_m[i,], 0.9, 1.0e-17, 10000)$x
}

dx <- paste('TOTAL VARIATION IN COMPUTED CRITICAL POINT', 
            '\n variation in x-values -> ', 
            toString(max(vals[,1]) - min(vals[,1])),
            '\n','variation in y-values -> ',
             toString(max(vals[,2]) - min(vals[,2])))
writeLines(dx)
print(apply(vals, 1, grad_f))

#2 Compare the merits of using a 2-layer neural network (with 1 hidden layer) 
#  and a multi-layer neural network to the wdbc data set. Let's say you only 
#  have 10 minutes to train a network with nnet or mlp. Which approach produces 
#  the model with the higher classification accuracy? It may be interesting to 
#  compare the two packages for a variety of time frames and to consider 
#  different network topologies for mlp, e.g., size=c(2, 4) vs. size=c(2, 2, 2).

library(nnet)
library(RSNNS)

source('~/Dropbox/Tarleton/data_mining/class_notes/extras.R')
wdbc <- read.csv('~/Dropbox/Tarleton/data_mining/dfiles/wdbc.data', 
                  header=F, sep=',')
wdbc <- wdbc[,-1]
splitset <- splitdata(wdbc,0.7,F)
train <- splitset$train

## Consider first merely increasing size
len = 30


# 1   2     4      8        16
#    11     22     44       88 
#          1111   2222     4444
#               11111111 22222222
#                    1111111111111111

accnnet <- rep(-1,len)
nnet_times <- rep(-1, len)

for(i in 1:len){
  t_0 <- proc.time()
  model <- nnet(V2~., wdbc[train,], size=i,trace=F)
  t_1 <- proc.time()
  nnet_times[i] <- t_1[3] - t_0[3]
  predvals <- predict(model,wdbc[-train,],type='class')
  accnnet[i] <- confmatrix(wdbc$V2[-train], predvals)$accuracy
}

normwdbc <- standardize(wdbc, 2:ncol(wdbc))
train_vals <- normwdbc[train,-1]
train_targ <- decodeClassLabels(normwdbc[train,1])
test_vals <- normwdbc[-train,-1]
test_targ <- decodeClassLabels(normwdbc[-train,1])

num = 6
accmlpnet <- matrix(-1, num, num)
mlp_times <- matrix(-1, num, num)

for(i in 1:num){

  for(j in 1:num){
    if(j >= i){
      reps <- 2^(i - 1)
      vec <- rep(2^(j - i), reps)
      t_0 <- proc.time()
      model <- mlp(train_vals, 
                   train_targ, 
                   size=vec,  
                   maxit = 50, 
                   learnFuncParams = c(0.1),
                   inputsTest = test_vals,
                   targetsTest = test_targ,
                   linout = TRUE)
      t_1 <- proc.time()
      mlp_times[i,j] <- t_1[3] - t_0[3]
      idx <- (model$fittedTestValues[,1] >= 0.5) * 1
      predvals <- idx
      predvals[idx == 1] <- 'B'
      predvals[idx == 0] <- 'M'
      accmlpnet[i,j] <- confmatrix(wdbc$V2[-train], predvals)$accuracy
    }
  }
}
mlp_times


plot(1:length(accnnet), accnnet, type = 'p', pch = 16, cex = 0.5 + nnet_times, xlab='# neurons', ylab='accuracy ')

plot(2^(c(1:(num-1))), accmlpnet[2,2:num], 
type = 'p', pch = 1, cex = mlp_times[2,2:num], xlim = c(0,2^(num - 1)) ,ylim=c(0.2,1.0),
xlab='total # neurons', ylab='accuracy ',col='black')

lines(2^(c(2:(num-1))), accmlpnet[3,3:num],
type = 'p', pch = 2, cex = mlp_times[3,3:num],col='blue')

lines(2^(c(3:(num-1))), accmlpnet[4,4:num],
type = 'p', pch = 4, cex = mlp_times[4,4:num],col='orange')

lines(2^(c(4:(num-1))), accmlpnet[5,5:num],
type = 'p', pch = 5, cex = mlp_times[5,5:num],col='green')
legend(0, 0.4, c('2 hidden layers', '3 hidden layers', '4 hidden layers', '5 hidden layers'), col = c('black', 'blue', 'orange', 'green'), 
pch = c(1, 2, 4, 5))

plot(2^(c(1:4)), accmlpnet[2,2:num], 
type = 'p', pch = 1, cex = mlp_times[2,2:num], xlim = c(0,2^(num - 1)),
xlab='total # neurons', ylab='accuracy ', main = '2 hidden layers')

plot(2^(c(2:4)), accmlpnet[3,3:num],
type = 'p', pch = 16, cex = mlp_times[3,3:num], 
xlab='total # neurons', ylab='accuracy ', main = '3 hidden layers')

plot(2^(c(3:4)), accmlpnet[4,4:num], 
type = 'p', pch = 16, cex = mlp_times[4,4:num], 
xlab='total # neurons', ylab='accuracy ', main = '4 hidden layers')

plot(2^(c(4:4)), accmlpnet[5,5:num], 
type = 'p', pch = 16, cex = mlp_times[5,5:num], 
xlab='total # neurons', ylab='accuracy ', main = '5 hidden layers')

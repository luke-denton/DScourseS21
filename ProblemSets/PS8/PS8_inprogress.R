library(tidyverse)
library(nloptr)

#### Q4

# set the seed
set.seed(100)

# form the matrix
N <- 100000
K <- 10
sigma <- 0.5
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
X[,1] <- 1 # first column of X should be all ones
# epsilon
eps <- rnorm(N,mean=0,sd=sigma)
#beta
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y <- X%*%beta + eps
###############################################################################

#### Q5

# compute betahat(OLS)
betahat_ols <- as.vector(solve(crossprod(X))%*%crossprod(X,Y))
comparison <- tibble(truth=beta, estimates=betahat_ols)
# our estimates are very close to the values of beta

###############################################################################

#### Q6

# step rate and iterations
alpha <- 0.0000003
M <- 5000

# Gradient of our objective function, provided by the matrix algebra of Dr Ransom
gradient <- function(beta,y,X) {
  return (as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# vector to contain all betas for all steps
beta.All <- matrix("numeric",length(beta),M)

iter  <- 1
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

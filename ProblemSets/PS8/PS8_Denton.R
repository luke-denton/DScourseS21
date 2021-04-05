library(tidyverse)
library(nloptr)
library(modelsummary)

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
gradient <- function(beta,Y,X) {
  return (as.vector(-2*t(X)%*%(Y-X%*%beta)) )
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
print(iter)
print(paste("The minimum of f(beta,Y,X) is ", beta, sep = ""))
beta

################################################################################

#### Q7

# Our objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}

# Gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

# Optimize!
result_lbfgs <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result_lbfgs)

# Nelder-Mead
## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result_nmead <- nloptr( x0=beta0,eval_f=objfun,opts=options,Y=Y,X=X)
print(result_nmead)
# LBFGS and Nelder-Mead tests produced similar estimates, with Nelder-Mead being
# a little more off track from the true values of beta

################################################################################

#### Q8

# Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

# Gradient
gradient <- function(theta ,Y,X) {
  grad <- as.vector( rep (0, length (theta )))
  beta <- theta[1:( length ( theta) -1)]
  sig <- theta[ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig
                                                                       ^3)
  return ( grad )
}

# initial values
theta0 <- runif(dim(X)[2]+1)

# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

# Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

################################################################################

##### Q9

easymode <- lm(Y ~ X -1)

modelsummary(easymode, output = "latex")














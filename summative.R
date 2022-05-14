# Question 1
#a)
my_arma = function(n, sigsq=1, mu=0, phi=numeric(0), theta=numeric(0), inits=NULL) {
  p = length(phi)
  q = length(theta)
  # Check that the process is stationary
  if(p>0) {
    roots = polyroot(c(1, -phi))
    if(all(Mod(roots)>1)) message("Process is stationary.")
    else message("Process is not stationary.")
  }
  # Check that the process is invertible
  if(q>0) {
    roots = polyroot(c(1, theta))
    if(all(Mod(roots)>1)) message("Process is invertible.")
    else message("Process is not invertible.")
  }
  # Establish initial values
  maxpq = max(c(p, q))
  if(!is.null(inits)) {
    if(length(inits)!=maxpq) stop("If initial values are supplied,
there must be max(p, q) of them.")
  } else {
    # Note: sets x[1], ..., x[maxpq] = 0
    inits = rep(0, maxpq)
  }
  # Simulate from model
  x = numeric(n)
  x[1:maxpq] = inits
  eps = rnorm(n, 0, sqrt(sigsq))
  for(t in (maxpq+1):n) {
    x[t] = sum(phi * x[(t-1):(t-p)]) + sum(c(1, theta) * eps[t:(t-q)])
  }
  return(x+mu)
}

n = 1000
sigsq = 0.5
mu = 0
phi = c(0.8, 0.1)
theta = -0.6
## Sample from process
x = my_arma(n, sigsq, mu, phi, theta)


my_arima = function(n, sigsq=1, mu=0, phi=numeric(0), theta=numeric(0), d){
  x = my_arma(n, sigsq, mu, phi, theta)
  y = numeric(length(x))
  for (i in 1:d){
  y[1] = 0
  for(t in 2:length(x)){
    y[t] = x[t] + y[t-1]
  }
  x=y
  }
  return(y)
}

# part b
set.seed(3)
output1 = my_arima(n, sigsq=1, mu=0, phi, theta, 1)
output2 = my_arima(n, sigsq=1, mu=0, phi, theta, 1)
output3 = my_arima(n, sigsq=1, mu=0, phi, theta, 1)

plot(output1, type = "l", ylim = c(-160,150), xlab = "Time", ylab = "Outputs")
lines(output2, col = "red")
lines(output3, col = "blue")


plot(diff(output1), type = "l")
lines(diff(output2), col = "red")
lines(diff(output3), col = "blue")

# Question 2 
# a)
par_est_func = function(x, p){
  lag = p + 1
  rho = acf(x, lag.max = lag -1, plot = FALSE)
  #Remove the first element
  rho_minus_1 = rho$acf[-1]
  # remove the last element of rho 
  rho_new = rho$acf[-lag]
  #Construct the capital rho matrix
  capital_rho = toeplitz(rho_new)
  #Find gammas
  gammas = acf(x, plot=FALSE, type="covariance", lag.max = p)
  # Find gamma zero 
  gamma_zero = gammas$acf[1]
  # Find phi
  phi = solve(capital_rho)%*%rho_minus_1[1:p]
  # Find sigma 
  sigma_sq = gamma_zero*(1 - t(rho_minus_1)%*%solve(capital_rho)%*%rho_minus_1)
  my_list = list("phi"= phi, "sigma squared" = sigma_sq)
  return(my_list)
}
par_est_func(x,2)


# b)
# Using the function for question 3a and setting p = 3 we get:
n = 1000
sigsq = 0.5
mu = 0
phi = c(0.8, 0.1, -0.4)
f = my_arma(n, sigsq, mu, phi)
par_est_func(f,3)

# The values I get using my function are very similar to the ones I used to simulate my 
# time series 

# Trying one more example where p = 4:
n = 1000
sigsq = 1.3
mu = 0
phi = c(0.8, 0.1, -0.4, 0.3)
g = my_arma(n, sigsq, mu, phi)
par_est_func(g,4)






getAnywhere(ar.yw.default)
library(itsmr)
yw(x,2)


#Question 4
# a)
install.packages("MASS")
library(MASS)

my_DLM = function(f, G, W, V, n, m_0, C_0){
  theta = matrix(ncol = 2, nrow = n+1, byrow = TRUE)
  theta[1,] = as.vector(mvrnorm(1, m_0, C_0))
  for (t in 2:(n+1)){
      theta[t,] = as.vector(G%*%theta[(t-1),]) + mvrnorm(1, m_0, W)
  }
  y = numeric(n)
  for(row in 2:(n+1)){
    y[row] = f%*%theta[row,] +  rnorm(1, 0, sqrt(V))
  }
    y = ts(y)
    plot(y)
}


# b) 
m_0 = t(c(0,0))
C_0 = 10*diag(2)
W = diag(c(0.7, 0.05))
V = 35
n = 100
G = matrix(c(1,1,0,1),  nrow = 2, byrow = 2)
f = matrix(c(1,0), nrow = 1, ncol = 2)

my_DLM(f, G, W, V, n, m_0, C_0)

# plot of the second differences 
my_DLM = function(f, G, W, V, n, m_0, C_0){
  theta = matrix(ncol = 2, nrow = n+1, byrow = TRUE)
  theta[1,] = as.vector(mvrnorm(1, m_0, C_0))
  for (t in 2:(n+1)){
    theta[t,] = as.vector(G%*%theta[(t-1),]) + mvrnorm(1, m_0, W)
  }
  y = numeric(n)
  for(row in 2:(n+1)){
    y[row] = f%*%theta[row,] +  rnorm(1, 0, sqrt(V))
  }
  y = ts(y)
  plot(diff(y,2))
}

my_DLM(f, G, W, V, n, m_0, C_0)

# Find the mean and autocovariances 
my_DLM_val = function(f, G, W, V, n, m_0, C_0){
  theta = matrix(ncol = 2, nrow = n+1, byrow = TRUE)
  theta[1,] = as.vector(mvrnorm(1, m_0, C_0))
  for (t in 2:(n+1)){
    theta[t,] = as.vector(G%*%theta[(t-1),]) + mvrnorm(1, m_0, W)
  }
  y = numeric(n)
  for(row in 2:(n+1)){
    y[row] = f%*%theta[row,] +  rnorm(1, 0, sqrt(V))
  }
  y = ts(y)
  return(diff(y,2))
}

my_DLM_val(f, G, W, V, n, m_0, C_0)

second_dif = my_DLM_val(f, G, W, V, n, m_0, C_0)


# Find the autocovariance 
autocovs = acf(second_dif, lag.max = 5, type = "covariance", plot = FALSE)
autocovs_values = autocovs$acf
autocovs_values

# Find the sample mean 
mean(second_dif)


library(dlm)
getAnywhere(dlm)

# Question 6 
my_varma = function(n, sigma=1, mu=0, A=numeric(0), M=numeric(0)) {
  p = length(A)
  q = length(M)
  m = length(mu)
  # Establish initial values
  maxpq = max(c(p, q))
  inits = rep(0, maxpq)
  # Simulate from model
  x = matrix(nrow = n, ncol = m)
  x[1:maxpq,] = inits
  # simulate from a multivariate normal to get the errors
  e = mvrnorm(n,rep(0, m),sigma)
  for(t in (maxpq +1):n) {
    ar = rep(0,m)
    for(i in 1:p){
      ar = ar + A[[i]]%*%x[t-i,]
    }
    ma = rep(0, m)
    for(j in 1:q){
      ma = ma + M[[j]]%*%e[t-j,]
    }
    x[t,] = ar + ma + e[t,]
  }
  x = ts(x)
  return(x)
}

# b)

phi1 = matrix(c(0.8, -0.1, 0.1,0.85), nrow = 2, byrow = TRUE)
A = list(phi1)
theta1 = matrix(c(0.6,0.1,0.15,0.5), nrow = 2, byrow = TRUE)
theta2 = matrix(c(3, 0, -0.1, 0.2), nrow = 2, byrow = TRUE)
M = list(theta1, theta2)
sigma = matrix(c(1, 0.4, 0.4, 1.2), nrow = 2, byrow = TRUE)
mu = c(0,0)
n = 100

varma = my_varma(n, sigma,mu, A, M)

# Find the mean
apply(varma, 2, mean)
# Find the variance 
var((varma))

# Find the covariance
varma_matrix = as.matrix(varma)
cov(varma_matrix[-1,], varma_matrix[-100,])


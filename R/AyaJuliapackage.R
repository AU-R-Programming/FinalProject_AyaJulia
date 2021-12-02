#' @title Linear Regression
#' @description Linear regression is used to predict the value of an outcome variable Y based on one or more input predictor variables X.
#' @param y A binary \code{vector} representing the response or output.
#' @param X A \code{matrix} of regressors or inputs.
#' @param init.val A \code{numeric} vector used for starting values (by default a vector of zeros).
#' @return A \code{vector} containing the estimated weights and bias
#' @author AyaJulia
#' @importFrom stats runif
#' @export
#' @examples
#'
#'

set.seed(123)

#I created random values for x and y to test the functions
x <- matrix(rnorm(400), ncol = 4)
y <- rnorm(100)
alpha <- 0.05

my_lm <- function(y, x, alpha = 0.05) {

  # Define parameters
  n <- length(y)
  X <- cbind(rep(1, n), x)
  y_mean <- mean(y)

  #Set initial beta
  beta_zero <- c(y_mean)

  for (i in 2:ncol(X)){
    beta_zero[i] <- cov(y, X[,i])/var(X[,i])
  }

  #Define loss function
  loss_func <- function(y, X, par){
    H <- t(y - X%*%par) %*% (y - X%*%par)
    return(H)
  }

  # Estimate beta through loss function
  beta.hat <- optim(par = beta_zero, fn = loss_func, X = X, y = y, method = "BFGS")

  #Calculating R squared
  y_hat <- X %*% as.matrix(beta.hat$par)
  sse <- sum((y - y_hat)^2)
  sst <- sum((y - y_mean)^2)

  R_squared <- 1-(sse/sst)

  #Calculating Cp
  resid <- y - X%*%as.matrix(beta.hat$par)
  p <- dim(X)[2]
  df <- n - p
  sigma2.hat <- (1/df)*t(resid)%*%resid

  Cp <- sse - 2*p*sigma2.hat

  #Estimating the variance beta
  var.beta <- sigma2.hat*optim(t(X)%*%X)

  #Calculating confidence intervals
  quant <- 1 - alpha/2
  ci.beta <-c(beta.hat-qnorm(p=quant)*sqrt(var.beta), beta.hat+ qnorm(p=quant)*sqrt(var.beta))


  # Return all estimated values
  return(list(beta = c(beta.hat$par), sigma2 = sigma2.hat, Cp = Cp, R2 = R_squared, ci = ci.beta))



}

plot(y ~ X[,2], data = , main="Least square error regression for x")
abline(a = beta.hat$par[1], b = beta.hat$par[2], col = "red")


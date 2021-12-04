#' @title Linear Regression
#' @description Linear regression is used to predict the value of an outcome variable Y based on one or more input predictor variables X.
#' @param y A binary \code{vector} representing the response or output.
#' @param X A \code{matrix} of regressors or inputs.
#' @param alpha A \code{numeric} for setting the confidence level.
#' @return A \code{dataframe} containing the beta estimator, sigma squared, Cp, R squared, confidence intervals, F stattistics, p-value, residuals, and predictor
#' @author AyaJulia
#' @importFrom stats runif
#' @export
#' @examples
#'variables <- matrix(rnorm(400), ncol = 4)
#'response <- rnorm(100)
#'alpha <- 0.05
#'
#'my_lm(response, variables, alpha)
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

  Cp <- sse + 2*p*sigma2.hat

  #Estimate of the variance of the estimated beta
  var.beta <- diag(as.numeric(sigma2.hat) * solve(t(X) %*% X))

  #Calculating confidence intervals
  quant <- 1 - alpha/2
  ci.beta <- c(as.matrix(beta.hat$par) - qnorm(p = quant)*sqrt(var.beta), as.matrix(beta.hat$par)+ qnorm(p = quant)*sqrt(var.beta))

  #Calculating F statistics
  dfm <- p-1
  dfe <- n - p
  ssm <- sum((y_hat - y_mean)^2)
  msm <- (ssm/dfm)
  mse <- (sse/dfe)
  f_stats <- (msm/mse)
  p_value <- (1-pf(f_stats,n-1, df))


  # Return all estimated values
  return(list(beta = c(beta.hat$par), sigma2 = sigma2.hat, Cp = Cp, R2 = R_squared, ci = ci.beta, f_statistic = f_stats, p_value = p_value, residuals = c(resid), predictor = c(y_hat)))

}

#' @title Plot Residuals
#' @description This function will plot the residuals for the my_lm function.
#' @param y A \code{vector} representing the response.
#' @param residuals A \code{vector} representing the residuals.
#' @return A \code{graph} with the residuals.
#' @author AyaJulia
#' @importFrom stats runif
#' @export
#' @examples
#'variables <- matrix(rnorm(400), ncol = 4)
#'response <- rnorm(100)
#'alpha <- 0.05
#'
#'result <- my_lm(response, variables, alpha)
#'plot_residuals(y, result$residuals)
plot_residual <- function(y, residuals){
  df_resid <- data.frame(y = residuals)
  plot_resid <- ggplot(df_resid, aes(sample = y))
  return (plot_resid + stat_qq() + stat_qq_line() + ggtitle("QQ Plot Residuals"))

}

#' @title Plot Histogram of Residuals
#' @description This function will plot the histogram for the residuals for the my_lm function.
#' @param residuals A \code{vector} representing the residuals.
#' @return A \code{graph} with the residuals.
#' @author AyaJulia
#' @importFrom stats runif
#' @export
#' @examples
#'variables <- matrix(rnorm(400), ncol = 4)
#'response <- rnorm(100)
#'alpha <- 0.05
#'
#'result <- my_lm(response, variables, alpha)
#'plot_histogram_residuals(result$residuals)
plot_histogram_residuals <- function(residuals){
  plot_hist <- ggplot() + aes(residuals)+ geom_histogram(binwidth=1, colour="black", fill="white")
  return(plot_hist)
}


#' @title Plot Residuals versus fitted values
#' @description This function will plot the residuals x the fitted values for the residuals for the my_lm function.
#' @param predictor A \code{vector} representing the predictor.
#' @param residuals A \code{vector} representing the residuals.
#' @return A \code{graph} with the residuals.
#' @author AyaJulia
#' @importFrom stats runif
#' @export
#' @examples
#'variables <- matrix(rnorm(400), ncol = 4)
#'response <- rnorm(100)
#'alpha <- 0.05
#'
#'result <- my_lm(response, variables, alpha)
#'plot_residuals_fitted(result$predictor,result$residuals)
plot_residuals_fitted <- function(predictor,residuals){
  plot_res_fit <- ggplot() + aes (x = residuals, y = predictor)+geom_point()
  return(plot_res_fit)
}

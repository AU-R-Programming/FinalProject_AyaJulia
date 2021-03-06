% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AyaJuliapackage_final.R
\name{my_lm}
\alias{my_lm}
\title{Linear Regression}
\usage{
my_lm(y, x, alpha = 0.05)
}
\arguments{
\item{y}{A binary \code{vector} representing the response or output.}

\item{alpha}{A \code{numeric} for setting the confidence level.}

\item{X}{A \code{matrix} of regressors or inputs.}
}
\value{
A \code{dataframe} containing the beta estimator, sigma squared, Cp, R squared, confidence intervals, F stattistics, p-value, residuals, and predictor
}
\description{
Linear regression is used to predict the value of an outcome variable Y based on one or more input predictor variables X.
}
\examples{
variables <- matrix(rnorm(400), ncol = 4)
response <- rnorm(100)
alpha <- 0.05

my_lm(response, variables, alpha)
}
\author{
AyaJulia
}

#' @title Linear Regression
#'
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
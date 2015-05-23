#' Normalise a data frame or matrix
#'
#' This function normalise a data frame or matrix.
#'
#' @param x a data frame or matrix.
#' @param method 'range': normalise to values between 0 and 1.
#'
#' @examples
#' ## Normalise a matrix
#' x <- matrix(rnorm(100), nrow = 10)
#' x_norm <- normalise(x)
#' print(summary(x))
#' print(summary(x_norm))
#'
#' @export

normalise <- function(x, method = c("center", "scale")) {
  pp <- preProcess(x, method)
  return(predict(pp, x))
}

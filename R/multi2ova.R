#' Convert multi-class targets into One-vs-All targets
#'
#' This function determines the best combination of predictors.
#'
#' @param y target (multi-class factor)
#'
#' @examples
#' ## T.B.A.
#'
#' @export


multi2ova <- function(y) {

  y <- data.frame(y)
  colnames(y) <- "class_"
  y_ova <- dummy.data.frame(y)
  return(y_ova)

}

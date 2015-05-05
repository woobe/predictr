#' Remove highly correlated predictors
#'
#' This function finds and removes attributes that are highly corrected.
#'
#' @param x a data frame or matrix of predictors (x).
#' @param cutoff cutoff correlation value (ideally >0.75)
#'
#' @examples
#' ## T.B.A.
#'
#' @export

remove_highcorr <- function(x, cutoff = 0.75) {

  ## Calculate Correlation Matrix
  mat_corr <- cor(x)

  ## Find High Corr
  high_corr <- findCorrelation(mat_corr, cutoff = cutoff)

  ## Remove columns if needed
  if (length(high_corr) >= 1) x <- x[, -high_corr]

  ## Return
  return(x)

}

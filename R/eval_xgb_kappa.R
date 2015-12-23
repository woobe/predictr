#' Custom Evaluation Functions for xgboost
#'
#' This function loads various xgboost evaluation function for xgboost
#'
#' @param preds predictions
#' @param dtrain DMatrix of the reference data
#'
#' @examples
#' eval_xgb_rmpse(preds, dtrain)
#' eval_xgb_kappa(preds, dtrain)
#'
#' @export

# QuadWeightedKappa
eval_xgb_kappa <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  err <- round(ScoreQuadraticWeightedKappa(as.integer(labels),as.integer(preds)), 5)
  return(list(metric = "kappa", value = err))

}


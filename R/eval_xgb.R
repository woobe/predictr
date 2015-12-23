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


# ------------------------------------------------------------------------------

# RMSPE
eval_xgb_rmpse <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- round(sqrt(mean((epreds/elab-1)^2)), 5)
  return(list(metric = "RMPSE", value = err))

}


# ------------------------------------------------------------------------------

# QuadWeightedKappa
eval_xgb_kappa <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  err <- round(ScoreQuadraticWeightedKappa(as.integer(labels),as.integer(preds)), 5)
  return(list(metric = "kappa", value = err))

}

# ------------------------------------------------------------------------------




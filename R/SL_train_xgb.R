#' Super Learner Training - xgboost
#'
#' This function splits data into training/tuning/holdation
#'
#' @param x predictors
#' @param ratio split ratio i.e. train:tune:hold = 80:10:10
#'
#' @examples
#' a <- SL_train_reg_xgb(x, y)
#'
#' @export

SL_train_reg_xgb <- function(...) {

  # Not run
  if (FALSE) {

    # Libraries
    library(mlbench)
    library(caret)
    library(xgboost)

    # Load data
    data(BostonHousing)
    d_train <- BostonHousing[1:400,]
    d_tune <- BostonHousing[401:450,]
    d_hold <- BostonHousing[451:500,]
    d_test <- BostonHousing[501:506,]
    rm(BostonHousing)
    features <- colnames(d_train)[-ncol(d_train)]
    target <- colnames(d_train)[ncol(d_train)]

    # Fixed folds
    set.seed(1234)
    folds <- createFolds(d_train$medv, k = 5)

    # eval functions for xgboost
    rmpse <- function(preds, dtrain) {
      labels <- getinfo(dtrain, "label")
      elab<-exp(as.numeric(labels))-1
      epreds<-exp(as.numeric(preds))-1
      err <- round(sqrt(mean((epreds/elab-1)^2)), 5)
      return(list(metric = "RMPSE", value = err))
    }


  }

  # --------------------------------------------------------------------------

  # Core Parameters

  # --------------------------------------------------------------------------

  # Create DMatrix for xgboost
  dtrainFull <- xgb.DMatrix(data = data.matrix(d_train[, features]), label = d_train[, target], missing = NA)
  dtune <- xgb.DMatrix(data = data.matrix(d_tune[, features]), label = d_tune[, target], missing = NA)
  dhold <- xgb.DMatrix(data = data.matrix(d_hold[, features]), label = d_hold[, target], missing = NA)
  dtest <- xgb.DMatrix(data = data.matrix(d_test[, features]), missing = NA)

  # --------------------------------------------------------------------------

  # Main CV Traininig Loop

  for (n_fold in 1:length(folds)) {

    # Display
    cat("[SL]: Training CV Fold", n_fold, "...\n")

    # Extract row number
    row_train <- as.integer(unlist(folds[-n_fold]))
    row_valid <- as.integer(unlist(folds[n_fold]))

    # Create temporary DMatrix for xgboost
    dtrain <- xgb.DMatrix(data = data.matrix(d_train[row_train, features]), label = d_train[row_train, target], missing = NA)
    dvalid <- xgb.DMatrix(data = data.matrix(d_train[row_valid, features]), label = d_train[row_valid, target], missing = NA)

    # ----------------------------------------------------------------------------

    # Set up param
    param <- list(  objective           = "reg:linear",
                    booster             = "gbtree",
                    eta                 = 0.001,
                    max_depth           = 8,
                    subsample           = 0.667,
                    colsample_bytree    = 0.667,
                    eval_metric         = "rmse"
    )

    # --------------------------------------------------------------------------

    # Train xgb model
    watchlist <-list(tune = dtune, hold = dhold, train = dtrain, valid = dvalid)
    suppressWarnings(
      model <- xgb.train(  params            = param,
                           data              = dtrain,
                           nrounds           = 10001,
                           verbose           = 1,
                           print.every.n     = 100L,
                           early.stop.round  = 1000L,
                           watchlist         = watchlist)
    )


    # --------------------------------------------------------------------------

    # Use model

    # --------------------------------------------------------------------------

    # Store results

    # --------------------------------------------------------------------------

    # Stats

    # --------------------------------------------------------------------------


  } # End of CV loop

  # --------------------------------------------------------------------------

} # End of Function




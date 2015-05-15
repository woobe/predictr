#' Quick paramemters tuning using caret
#'
#' Quick paramemters tuning using caret
#'
#' @param x predictors
#' @param y targets
#'
#' @examples
#' ## T.B.A.
#'
#' @export

quick_tune <- function(x, y,
                       method = c("rf"),
                       n_samp = 1000,
                       n_cv_fold = 3,
                       n_cv_repeat = 3,
                       n_run = 25,
                       n_tuneLength = 5,
                       custom_tuneGrid = NULL,
                       n_core = 7) {

  ## Determine p for sampling
  n_samp <- min(c(nrow(x), n_samp))
  p_samp <- n_samp / nrow(x)

  ## Mini train
  mini_train <- function(x, y, method, p_samp, n_cv_fold, n_cv_repeat, n_tuneLength, custom_tuneGrid) {

    ## Resampling
    row_samp <- createDataPartition(y, p = p_samp, list = FALSE)

    ## Ctrl
    ctrl <- trainControl(method = "repeatedcv",
                         number = n_cv_fold,
                         repeats = n_cv_repeat,
                         allowParallel = FALSE)

    ## Train caret
    if (is.null(custom_tuneGrid)) {
      model <- train(x, y,
                     method = method,
                     trControl = ctrl,
                     tuneLength = n_tuneLength)
    } else {
      model <- train(x, y,
                     method = method,
                     trControl = ctrl,
                     tuneGrid = custom_tuneGrid)
    }

    ## Return
    return(model$finalModel$tuneValue)

  }

  ## doParallel
  cl <- makePSOCKcluster(n_core)
  registerDoParallel(cl)

  ## Foreach
  tune_value <- foreach(n = 1:n_run,
                        .combine = rbind,
                        .multicombine = TRUE,
                        .errorhandling = "remove",
                        .packages = "caret") %dopar%
    mini_train(x, y, method, p_samp, n_cv_fold, n_cv_repeat, n_tuneLength, custom_tuneGrid)

  ## doParallel
  stopCluster(cl)

  ## Return mode
  tune_output <- data.frame(t(apply(tune_value, 2, get_mode)))
  colnames(tune_output) <- colnames(tune_value)
  return(tune_output)

}


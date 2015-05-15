#' Feature selection using xgboost
#'
#' Feature selection using xgboost
#'
#' @param x predictors
#' @param y target
#'
#' @examples
#' ## T.B.A.
#'
#' @import xgboost
#'
#' @export

select_fea_xgb <- function(x, y,
                           n_p = 1,
                           t_obj = "multi:softprob",
                           t_eval = "mlogloss",
                           n_class = NULL,  ## you need to provide this
                           n_depth = 9,
                           n_eta = 0.1,
                           n_subsamp = 0.9,
                           n_core = 8,
                           n_round = 100,
                           n_cv_fold = 3,
                           n_final_cv_fold = 10,
                           n_size = "auto",
                           n_repeat = 3,
                           verbose = TRUE) {

  ## ===========================================================================
  ## Core Parameters and Data Resampling
  ## ===========================================================================

  ## Resampling
  row_samp <- createDataPartition(y, p = n_p, list = FALSE)

  ## Set param
  param <- list("objective" = t_obj,
                "eval_metric" = t_eval,
                "num_class" = n_class,
                "max_depth" = n_depth,
                "eta" = n_eta,
                "sub_sample" = n_subsamp,
                "nthread" = n_core)


  ## ===========================================================================
  ## Obtain Feature Importance Score
  ## ===========================================================================

  ## Train first model
  cat("[predictr]: Training xgboost models to calculate feature importance ...\n")

  ## Empty Shell
  imp_all <- c()

  ## Loop
  for (n_model in 1:n_repeat) {

    cat("[predictr]: Training Model", n_model, "/", n_repeat, "...")
    model <- xgboost(param = param,
                     data = x[row_samp, ],
                     label = y[row_samp],
                     nrounds = n_round,
                     verbose = FALSE)

    cat(" Summarising ...")
    imp_temp <- data.frame(xgb.importance(colnames(x), model = model))
    imp_all <- rbind(imp_all, imp_temp)

    cat(" Done!\n")
  }

  ## Averaging Importance Score
  imp <- imp_all %>% group_by(Feature) %>% summarise(Gain = mean(Gain))
  imp <- imp[order(imp$Gain, decreasing = TRUE), ]

  ## Display if needed
  if (verbose) {

    ## Print top 10
    print(head(imp, 10))

    ## ggplot2
    p <- ggplot(imp, aes(x = reorder(Feature, Gain), y = Gain)) +
      geom_bar(stat="identity", fill="#53cfff") +
      coord_flip() +
      theme_light(base_size=20) +
      xlab("Importance") +
      ylab("") +
      ggtitle("XGBOOST Feature Importance\n") +
      theme(plot.title=element_text(size=18))
    print(p)

  }


  ## ===========================================================================
  ## Grid Search (Prototype)
  ## ===========================================================================

  ## Define test size for CV models
  if (n_size == "auto") {
    col_cv <- c(2, round(ncol(x) * seq(0.1, 0.9, 0.1)), ncol(x))
  } else {
    col_cv <- n_size
  }

  ## First Loop
  for (n_col in col_cv) {

    ## Display
    cat("[predictr]: Now training ", n_cv_fold, "-fold Cross-Validation with top ", n_col, " features ...\n", sep ="")

    ## Extract features
    fea_temp <- imp[1:n_col, ]$Feature

    ## Empty Shell
    df_temp_all <- c()

    ## Repeat
    for (n_model in 1:n_repeat) {

      ## Display
      cat("[predictr]: Training CV model", n_model, "/", n_repeat, "...\n")

      ## Train xgb.cv model
      model_cv <- xgb.cv(param = param,
                         data = x[row_samp, ],
                         label = y[row_samp],
                         nfold = n_cv_fold,
                         nrounds = n_round,
                         showsd = FALSE,
                         verbose = FALSE)

      ## Create df for results summary
      df_temp <- data.frame(n_feature = n_col,
                            best_cv_test = min(as.matrix(model_cv)[, 2]),
                            best_round = which(as.matrix(model_cv)[, 2] == min(as.matrix(model_cv)[, 2])))
      df_temp_all <- rbind(df_temp_all, df_temp)

    }

    ## Simple Average
    df_temp <- data.frame(n_feature = n_col,
                          best_cv_test = mean(df_temp_all$best_cv_test),
                          best_round = round(mean(df_temp_all$best_round)))

    ## Display
    cat("Average Best CV Score:", df_temp$best_cv_test, "\nAverage Best Round   :", df_temp$best_round, "\n\n")

    ## Store
    if (n_col == 2) df_result <- df_temp else df_result <- rbind(df_result, df_temp)

  }

  ## Sort results
  df_result <- df_result[order(df_result$best_cv_test), ]

  ## Display
  if (verbose) print(df_result)

  ## ===========================================================================
  ## Return Best Combination
  ## ===========================================================================

  ## Extract Best Features
  best_n <- df_result[1, ]$n_feature
  best_fea <- imp[1:best_n, ]$Feature

  ## Display
  if (verbose) {
    cat("\nChosen Features:\n")
    print(best_fea)
  }

  ## Return
  return(best_fea)

}

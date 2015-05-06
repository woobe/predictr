#' Feature Selection using Caret's Random Forest Approach
#'
#' This function determines the best combination of predictors.
#'
#' @param x a data frame or matrix of predictors (x).
#' @param y target (numeric or factor)
#'
#' @examples
#' ## T.B.A.
#'
#' @export


select_features <- function(x, y,
                            n_core = 8,
                            n_cv_fold = 10,
                            n_cv_repeat = 1,
                            n_run = 20,
                            n_maxsamp = 1000,
                            n_size = c(2:10),
                            n_cutoff = 0.6,
                            verbose = TRUE)
{

  ## Set up
  ctrl <- rfeControl(functions = rfFuncs,
                     method="repeatedcv",
                     number = n_cv_fold,
                     repeats = n_cv_repeat,
                     allowParallel = FALSE)

  ## Determine p for sampling
  max_samp <- min(c(nrow(x), n_maxsamp))
  p_samp <- max_samp / nrow(x)

  ## Create multiple resampling folds
  fold_samp <- createDataPartition(y, times = n_run, p = p_samp)

  ## batch RFE function
  batch_rfe <- function(x, y, n_size, ctrl, fold_samp, n_fold) {

    ## extract samp
    row_samp <- as.integer(unlist(fold_samp[n_fold]))

    ## Run RFE
    model_rfe <- rfe(x[row_samp, ],
                     y[row_samp],
                     sizes = n_size,
                     rfeControl = ctrl)

    ## Return results
    results <- matrix(0, nrow = 1, ncol = ncol(x))
    colnames(results) <- colnames(x)
    col_use <- which(colnames(x) %in% predictors(model_rfe))
    results[1, col_use] <- 1
    return(results)

  }

  ## Display
  cat("[predictr]: Running {caret} RFE ...\n")

  ## Register
  cl <- makePSOCKcluster(n_core)
  registerDoParallel(cl)

  ## Use foreach for batch RFE
  results <- foreach(n_fold = 1:n_run,
                     .combine = rbind,
                     .multicombine = TRUE,
                     .errorhandling = "remove",
                     .packages = c("caret", "randomForest")) %dopar%
    batch_rfe(x, y, n_size, ctrl, fold_samp, n_fold)

  ## Stop
  stopCluster(cl)

  ## Summarise
  df_summary <- data.frame(Variables = colnames(x), N_Chosen = colSums(results), P_Chosen = colSums(results) / n_run)
  opt_var <- as.character(df_summary[which(df_summary$P_Chosen >= n_cutoff), ]$Variables)

  ## print summary
  cat("[predictr]:", length(opt_var), "out of", ncol(x), "predictors have been chosen for best OOB performance.\n")


  ## Print more
  if (verbose) {

    cat("\n[predictr]: Summary of RFE:\n")
    print(df_summary)

    cat("\n[predictr]: Chosen Variables:\n")
    print(df_summary[which(df_summary$P_Chosen >= n_cutoff), ])

  }

  ## Return
  return(opt_var)

}


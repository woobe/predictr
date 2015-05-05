#' Feature Selection using Caret's Random Forest Approach
#'
#' This function determines the best combination of predictors.
#'
#' @param x a data frame or matrix of predictors (x).
#' @param y target (numeric or factor)
#' @param n_core number of CPU cores (set it to 1 for single core)
#' @param n_fold number of cross-validation folds
#' @param n_repeat number of repeated CV
#' @param n_size number of predictors to be tested
#' @param verbose display results (TRUE)
#' #'
#' @examples
#' ## T.B.A.
#'
#' @export


select_features <- function(x, y,
                            n_core = 4,
                            n_fold = 10,
                            n_repeat = 1,
                            n_size = c(2, 4, 6, 8),
                            verbose = TRUE)
{

  ## Set up
  ctrl <- rfeControl(functions = rfFuncs,
                     method="repeatedcv",
                     number = n_fold,
                     repeats = n_repeat)

  ## Register
  cl <- makePSOCKcluster(n_core)
  registerDoParallel(cl)

  ## Use rfe
  results <- rfe(x, y,
                 sizes = n_size,
                 rfeControl = ctrl)

  ## Stop
  stopCluster(cl)

  ## Print
  if (verbose) {

    # summarize the results
    print(results)

    # list the chosen features
    predictors(results)

  }

  ## Return
  return(results)

}

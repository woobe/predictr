#' Spliting data into training/tuning/holdation based on predictors (x)
#'
#' This function splits data into training/tuning/holdation
#'
#' @param x predictors
#' @param ratio split ratio i.e. train:tune:hold = 80:10:10
#'
#' @examples
#' row_samp <- split_by_x(x)
#'
#' @export

split_by_x <- function(x, ratio = c(8,1,1)) {

  # Run PCA
  n_comp <- min(c(10, ncol(x)))
  m_pca <- preProcess(x, method = "pca", pcaComp = n_comp)
  x_pca <- predict(m_pca, x)

  # Compress into 1D
  features <- paste0("PC", 1:n_comp)
  x_pca_mean <- rowMeans(x_pca[, features])

  # Use x_pca_mean to split
  fold_train <- ratio[1]
  fold_tune <- ratio[2]
  fold_hold <- ratio[3]
  fold_total <- sum(ratio)
  folds <- createFolds(x_pca_mean, k = fold_total, list = TRUE)

  # Split
  start_tune <- 1;               end_tune <- fold_tune
  start_hold <- end_tune + 1;  end_hold <- end_tune + fold_hold
  start_train <- end_hold + 1; end_train <- fold_total

  row_tune <- as.integer(unlist(folds[start_tune:end_tune]))
  row_hold <- as.integer(unlist(folds[start_hold:end_hold]))
  row_train <- as.integer(unlist(folds[start_train:end_train]))

  # Return
  output <- list(row_train = row_train,
                 row_tune = row_tune,
                 row_hold = row_hold)
  return(output)

}





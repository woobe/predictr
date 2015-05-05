#' Writing data to CSV file in batch mode
#'
#' This function is especially useful for writing large files.
#'
#' @param data the tabular object (matrix or data frame)
#' @param file CSV filename
#' @param apd append existing file (TRUE or FALSE)
#' @param batch_size number of rows to be written for each batch
#' @param pause seconds timeout between batch
#'
#' @examples
#' ## T.B.A.
#'
#' @export

batch_write <- function(data, file, apd = FALSE, batch_size = 10000, pause = 0) {

  n_size <- nrow(data)
  n_batch <- ceiling(n_size / batch_size)

  for (nn_batch in 1:n_batch) {

    ## Locate
    n_start <- (nn_batch - 1) * batch_size + 1
    n_end <- nn_batch * batch_size
    n_end <- min(c(n_end, n_size))

    ## Display
    if (nn_batch == 1) {
      cat("Writing batch:", nn_batch, "/", n_batch, "=== rows:", n_start, "to", n_end, "...\n")
    } else if (nn_batch %% 100 == 0) {
      cat("Writing batch:", nn_batch, "/", n_batch, "=== rows:", n_start, "to", n_end, "...\n")
    }

    ## If
    if (nn_batch == 1 & apd == FALSE) {
      write.table(x = data[n_start:n_end, ],
                  file = file,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  append = FALSE)
    } else {
      write.table(x = data[n_start:n_end, ],
                  file = file,
                  sep = ",",
                  row.names = FALSE,
                  col.names = FALSE,
                  append = TRUE)
    }

    ## Display
    Sys.sleep(pause)

  }

}

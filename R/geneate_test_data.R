#' Generate a fake metaproteomic/proteomic data matrix, with grouping information
#'
#' @param seed
#'
#' @return a list with a matrix, of 200 rows and 30 columns, and a meta table, with two columns, sample name and grouping
#' @export
#'
#' @examples
#'
#' test_data <- generate_test_data()
#'
#'
generate_test_data <- function(seed = 1981){

  set.seed(seed)

  matrix_test1 <- matrix(runif(2000), nrow = 200)
  matrix_test2 <- matrix(runif(2000)+ runif(2000), nrow = 200)
  matrix_test3 <- matrix(runif(2000)+ 2*runif(2000), nrow = 200)

  matrix_test <- cbind(matrix_test1,matrix_test2, matrix_test3)

  rownames(matrix_test) <- paste("COG", 1:200, sep = "_")
  colnames(matrix_test) <- paste("sample", 1:30, sep = "_")

  factor_test <- c(rep("A",10),rep("B",10),rep("C",10))
  meta_test <- data.frame(samplename = colnames(matrix_test), grouping = factor_test)

  # Randomize the order
  meta_test <- meta_test[sample(1:nrow(meta_test)),]
  rownames(meta_test) <- NULL

  return(list(matrix =matrix_test,
              meta =  meta_test))

}




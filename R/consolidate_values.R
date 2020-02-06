#' Consolidate vectors with missing values
#'
#' Given two vectors, each of which may have some missing values, consolidate
#' into one vector with the minimum number of missing values.
#'
#' @param col1 one vector
#' @param col2 second vector
#' @param missing what a "missing" value looks like, defaults to "". Could also
#'                be NA, NULL, etc.
#'
#' @return col - a vector with consolidated values
#'
#' @export

consolidate_values <- function(col1, col2, missing = ""){
  col1[is.na(col1)] <- missing
  col2[is.na(col2)] <- missing
  col <- NULL
  for (i in 1:length(col1)){
    if (col1[i] != missing) {
      col <- c(col, col1[i])
    } else if (col2[i] != missing) {
      col <- c(col, col2[i])
    } else {
      col <- c(col, missing)
    }
  }
return(col)
}

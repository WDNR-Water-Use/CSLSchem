#' Define groundwater gradient
#'
#' Given a value and a threshold, define whether groundwater is upgradient,
#' downgradient, or undetermined.
#'
#' @param value mean difference between groundwater and lake levels (positive =
#'              groundwater is higher)
#' @param thrshold value to use to evaluate threshold, defaults to 0.01 (i.e.,
#'                 the precision of HOBO water level loggers)
#'
#' @return gradient - a string noting gradient (upgradient, downgradient, or "")
#'
#' @export

define_gradient <- function(values, threshold = 0.01) {
  gradient <- NULL
  for (value in values){
    if (value > threshold) {
      gradient <- c(gradient, "upgradient")
    } else if (value < -threshold) {
      gradient <- c(gradient, "downgradient")
    } else {
      gradient <- c(gradient, "")
    }
  }
  return(gradient)
}

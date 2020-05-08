#' mg/L to meq/L conversion table
#'
#' @references Schwartz, F.W. and H. Zhang. 2003. Fundamentals of Groundwater. John Wiley & Sons, New York, NY. Table 16.4.
#'
#' @docType data
#'
#' @usage data(mgTOmeq)
#'
#' @format A data frame with the following columns:
#' \describe{
#'  \item{description}{dnr parameter description}
#'  \item{mgTOmeq}{multiply value in mg/L by this value to yield meq/L}
#'  \item{mgTOmmol}{multiply value in mg/L by this value to yield molarity}
#'  }
#'
"mgTOmeq"

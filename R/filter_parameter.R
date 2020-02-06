#' Filter SWIMS by parameter
#'
#' Subsets SWIMS dataset for a CSLS lake to only measurements for a parameter of
#' interest.
#'
#' @param SWIMS data frame with SWIMS information for lakes of interest
#' @param parameter - parameter to subset by, must exactly match DNR description
#'                    (e.g., "ALUMINUM,TOTAL RECOVERABLE")
#' @param plotting_name - name of parameter to use for plotting, e.g., "Total
#'                        Recoverable Alumninum". Units will be added in based
#'                        on SWIMS field.
#'
#' @return SWIMS_parameter, same data frame, with only the results for the
#'         parameter of interest. Also includes the given plotting name ("name").
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate
#'
#' @export

filter_parameter <- function(SWIMS, parameter, plotting_name){
  df <- SWIMS %>%
        filter(.data$description == !!parameter) %>%
        mutate(result = as.numeric(.data$result),
               name = !!plotting_name)

  # Note hypolimnion
  df$site_type[df$site_type == "lake" & (df$depth1_m > 5 | df$depth2_m > 5)] <- "lake_bottom"

  return(df)
}

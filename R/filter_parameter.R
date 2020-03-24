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
#' @param no_blanks logical defaults to TRUE to exclude blank samples
#' @param no_dups logical defaults to TRUE to exclude duplicate samples
#' @param no_age logical defaults to FALSE to include samples that were analyzed
#'               past the holding date.
#'
#' @return SWIMS_parameter, same data frame, with only the results for the
#'         parameter of interest. Also includes the given plotting name ("name").
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate
#'
#' @export

filter_parameter <- function(SWIMS, parameter, plotting_name,
                             no_blanks = TRUE, no_dups = TRUE,
                             no_age = FALSE){
  df <- SWIMS %>%
        filter(.data$description == !!parameter) %>%
        mutate(result = as.numeric(.data$result),
               name = !!plotting_name)

  if (no_blanks) {
    df <- df %>%
          filter(.data$flag != "BLANK")
  }
  if (no_dups) {
    df <- df %>%
          filter(.data$flag != "DUPLICATE")
  }

  # Note hypolimnion
  if (!is.null(df$site_type[df$site_type == "lake" & (df$depth1_m > 5 | df$depth2_m > 5)])) {
    df$site_type[df$site_type == "lake" & (df$depth1_m > 5 | df$depth2_m > 5)] <- "lake_bottom"
  }

  return(df)
}

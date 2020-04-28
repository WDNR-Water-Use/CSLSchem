#' Filter to lake surface temperature
#'
#' Filters large dataset of water chemistry parameters to only lake surface
#' temperature measurements.
#'
#' @param chem_df data frame with water chemistry information for all sites.
#'                Defaults to CSLSdata::water_chem.
#' @param use_HOBO logical defaults to TRUE to use HOBO data for temperature
#'                 measurements. if false, uses field profile data.
#'
#' @return lst, same data frame as chem_df, but subset to only lake surface
#'         temperature measurements.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter group_by mutate
#'
#' @export

filter_lst <- function(chem_df = CSLSdata::water_chem,
                       use_HOBO = TRUE){

  # Grab HOBO or field temperature profile data
  if (use_HOBO){
    ltmp <- filter_parameter(chem_df, "TEMPERATURE HOBO")
  } else {
    ltmp <- filter_parameter(chem_df, "TEMPERATURE FIELD")
  }

  # Keep only shallowest measurement for each date/time
  lst <- ltmp %>%
         filter(!is.na(.data$result)) %>%
         group_by(.data$lake, .data$date) %>%
         mutate(min_depth = min(.data$depth1_m)) %>%
         ungroup() %>%
         filter(.data$depth1_m == .data$min_depth)

  # Remove extraneous columns
  lst$name      <- NULL
  lst$min_depth <- NULL

  return(lst)
}

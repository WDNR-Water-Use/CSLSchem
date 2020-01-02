#' Ice On and Off Dates
#'
#' Estimates ice on and ice off based on lake surface temperature. Assumes "ice
#' on" is the first day between Sept and May that surface water temperature goes
#' below 4 degres C and "ice off" is the last day between Sept and May that
#' surface water temperature is below 4 degrees C.
#'
#' @param lst a data frame with date and ltmp of lake surface temperature
#'            measurements.
#' @param ice_tmp temperature below which assume ice on, default to 4 deg C
#'
#' @return ice, a list with "ice_on" and "ice_off" dates.
#'
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#'
#' @export

find_ice_on_off <- function(lst, ice_tmp = 4) {
  # Find start water year
  start_year  <- year(lst$date[1])
  start_month <- month(lst$date[1])
  start_WY    <- mdy(sprintf("10/1/%s", start_year))
  if (start_month < 10){
    start_WY <- start_WY - years(1)
  }

  # Find last water year
  final_year <- year(lst$date[nrow(lst)])
  final_month <- month(lst$date[nrow(lst)])
  final_WY <- mdy(sprintf("9/30/%s", final_year))
  if (final_month >= 10){
    final_WY <- final_WY + years(1)
  }

  # Find number of water years
  nWY <- interval(start_WY, final_WY) %>%
         int_length() %>%
         NISTunits::NISTsecTOyear() %>%
         round()

  # Find ice on/off each water year
  ice_on  <- NA
  ice_off <- NA
  for (i in 1:nWY){
    start_date <- start_WY + years(i-1)
    end_date   <- start_WY + years(i)
    lst_WY     <- lst %>%
                  filter(.data$date >= start_date,
                         .data$date < end_date) %>%
                  arrange(.data$date)
    # Only note dates if water tmp dips below ice tmp during this WY
    if (any(lst_WY$ltmp <= ice_tmp)) {
      below_ice_tmp <- lst_WY[lst_WY$ltmp <= ice_tmp,]
      ice_on[i]     <- below_ice_tmp$date[1]
      ice_off[i]    <- below_ice_tmp$date[nrow(below_ice_tmp)]
    }
  }
  ice_on  <- as_datetime(ice_on)
  ice_off <- as_datetime(ice_off)

  return(list(ice_on = ice_on, ice_off = ice_off))
}

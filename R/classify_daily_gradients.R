#' Calculate groundwater gradients relative to lakes
#'
#' This function determines whether a well is upgradient, downgradient, or
#' unknown at all recorded dates for all wells associated with the lakes of
#' interest. Uses the given duration and threshold values to classify wells
#' (median difference over given duration must be greater in magnitude than the
#' threshold value).
#'
#' @param lakes lakes of interest, e.g., c("Pleasant", "Long", "Plainfield")
#' @param threshold minimum difference (m) between water levels in order to
#'                  assess gradient, defaults to 0.01m (i.e., the precision of
#'                  HOBO water level loggers)
#' @param duration window (days) to use for calculating median difference
#'                 between groundwater and lake levels, defaults to 30 days.
#'
#' @return gw_gradients, a data frame with the following columns:
#' \item{date}{date of record, POSIXct}
#' \item{site_id}{site id of groundwater well}
#' \item{gradient}{whether well should be considered "upgradient",
#'                 "downgradient", or unknown ("") on that day}
#'
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter select mutate
#'
#' @export

classify_daily_gradients <- function(lakes = c("Pleasant", "Long", "Plainfield"),
                                      threshold = 0.01, duration = 30){

  water_levels          <- calculate_level_diff(lakes)
  water_levels$gradient <- ""

  this_date <- min(water_levels$date)
  end_date  <- max(water_levels$date)
  while (this_date <= end_date){
    this_window    <- interval(this_date - days(duration), this_date)
    these_levels   <- water_levels %>%
                      filter(.data$date %within% this_window)
    median_diffs   <- these_levels %>%
                      group_by(site_id) %>%
                      summarise(median_diff = median(diff_m, na.rm = TRUE)) %>%
                      ungroup()
    gradients      <- median_diffs %>%
                      mutate(date = this_date,
                             gradient = define_gradient(.data$median_diff,
                                                        threshold)) %>%
                      select(.data$date, .data$site_id, .data$gradient)
    water_levels   <- merge(water_levels,
                            gradients,
                            by = c("date", "site_id"),
                            all.x = TRUE) %>%
                      mutate(gradient = consolidate_values(.data$gradient.x,
                                                           .data$gradient.y)) %>%
                      select(.data$date,
                             .data$site_id,
                             .data$diff_m,
                             .data$gradient)
    this_date      <- this_date + days(1)
  }

  gw_gradients  <- water_levels %>%
                   mutate(gradient = as.factor(.data$gradient)) %>%
                   select(.data$date, .data$site_id, .data$gradient)

  return(gw_gradients)
}

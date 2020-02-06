#' Classify SWIMS results
#'
#' This function
#'
#' @param lakes lakes of interest, e.g., c("Pleasant", "Long", "Plainfield")
#' @param precip name of precip site, defaults to "Hancock".
#' @param threshold minimum difference (m) between water levels in order to
#'                  assess gradient, defaults to 0.01m (i.e., the precision of
#'                  HOBO water level loggers)
#' @param duration window (days) to use for calculating median difference
#'                 between groundwater and lake levels, defaults to 30 days.
#'
#' @return SWIMS, a data frame with all columns included in the CSLSdata frame
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select mutate
#' @importFrom lubridate floor_date
#'
#' @export
classify_SWIMS_results <- function(lakes = c("Pleasant", "Long", "Plainfield"),
                                   precip = "Hancock",
                                   threshold = 0.01, duration = 30) {
  gw_gradients <- classify_daily_gradients(lakes, threshold, duration)
  SWIMS        <- NULL
  for (lake in lakes){
    this_SWIMS            <- CSLSdata::SWIMS[[lake]]
    SWIMS_cols            <- colnames(this_SWIMS)
    this_SWIMS$floor_date <- floor_date(this_SWIMS$date, unit = "day")
    dictionary            <- CSLSdata::dictionary[[lake]]
    dictionary            <- dictionary %>%
                             select(site_id = .data$site_id,
                                    static_class = .data$static_iso_class)
    this_SWIMS           <- merge(this_SWIMS,
                                  gw_gradients,
                                  by.x = c("floor_date", "site_id"),
                                  by.y = c("date", "site_id"),
                                  all.x = TRUE)
    this_SWIMS           <- merge(this_SWIMS,
                                  dictionary,
                                  by = "site_id",
                                  all.x = TRUE)
    this_SWIMS$site_type <- consolidate_values(as.character(this_SWIMS$gradient),
                                               as.character(this_SWIMS$static_class))
    this_SWIMS           <- this_SWIMS %>%
                            mutate(lake = !!lake) %>%
                            select(c("lake", "site_type", SWIMS_cols))
    SWIMS                <- rbind(SWIMS, this_SWIMS)
  }

  SWIMS$lake[SWIMS$site_type == "precipitation"] <- precip
  lakes      <- c(precip, lakes)
  SWIMS$lake <- factor(SWIMS$lake, levels = lakes)
  SWIMS      <- SWIMS[!duplicated(SWIMS),]

  return(SWIMS)
}

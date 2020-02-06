#' Calculate difference between groundwater and lake levels
#'
#' For given list of lakes, calculates the difference between groundwater levels
#' and lake levels (positive = groundwater is higher).
#'
#' @param lakes vector with lakes of interest, defaults to c("Pleasant", "Long",
#'              "Plainfield")
#'
#' @return water_levels, a data frame with the following columns:
#' \item{date}{date of observation, POSIXct}
#' \item{site_id}{site id of the groundwater well}
#' \item{diff_m}{difference between groundwater levels and lake levels (m)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select
#'
#' @export

calculate_level_diff <- function(lakes = c("Pleasant", "Long", "Plainfield")) {
  water_levels <- NULL
  for (lake in lakes){
    lake_levels  <- CSLSdata::lake_levels[[lake]]
    gw_levels    <- CSLSdata::gw_levels[[lake]]
    df           <- merge(gw_levels, lake_levels,
                          by = "date", all.x = TRUE)
    df$diff_m    <- df$level_m.x - df$level_m.y
    df           <- df %>% select(.data$date, .data$site_id, .data$diff_m)
    water_levels <- rbind(water_levels, df)
  }
  return(water_levels)
}

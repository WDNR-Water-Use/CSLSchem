#' Define upgradient and downgradient wells
#'
#' This function takes an inputs SWIMS data frame and classifies each
#' measurement as associated with the "lake", an "upgradent" well, or a
#' "downgradient" well based on the previous 30 days of lake and groundwater
#' measurements.
#'
#' @param lake - lake of interest from the CSLS (i.e., "Pleasant", "Plainfield",
#'               or "Long")
#' @param SWIMS - data frame with SWIMS info
#' @param threshold - defaults to 0.01 (m) to indicate that the difference in
#'                    lake levels and groundwater levels over the previous month
#'                    should be greater than 1cm in order to classify a well as
#'                    upgradient.
#' @inheritParams filter_parameter
#'
#' @return SWIMS - same as inputs SWIMS data frame, but with a column for
#'                 "site_type" which is either "lake", "upgradient" or
#'                 "downgradient".
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter select
#' @importFrom NISTunits NISTftTOmeter
#' @importFrom stringr str_split
#' @importFrom stats median
#' @import lubridate
#' @import cslsdata
#'
#' @export

define_lake_wells <- function(lake, SWIMS, static_gw = FALSE, threshold = 0.01){
  dictionary  <- cslsdata::dictionary[[lake]]
  lake_levels <- cslsdata::lake_levels[[lake]]
  gw_levels   <- cslsdata::gw_levels[[lake]]


  # GW Sites
  gw_sites <- dictionary %>%
              filter(.data$obs_type == "GW",
                     .data$static_iso_class != "invalid")

  # All GW-LK Differences
  for (i in 1:nrow(gw_levels)) {
    gw    <- gw_levels$level_m[i]
    lk    <- lake_levels$level_m[lake_levels$date == gw_levels$date[i]]
    if (length(lk) > 0) {
      gw_levels$diff_m[i] <- gw - lk
    } else {
      gw_levels$diff_m[i] <- NA
    }
  }

  # Assign Site Type
  for (i in 1:nrow(SWIMS)) {
    if (SWIMS$station_id[i] %in% gw_sites$SWIMS_station_id) {
      # GROUNDWATER
      if (static_gw) {
        SWIMS$site_type[i] <- gw_sites %>%
                              filter(.data$SWIMS_station_id == SWIMS$station_id[i]) %>%
                              select(.data$static_iso_class) %>%
                              unlist() %>%
                              as.character()
      } else {
        this_USGS_id <- gw_sites %>%
                        filter(.data$SWIMS_station_id == SWIMS$station_id[i]) %>%
                        select(.data$USGS_id) %>%
                        as.numeric()
        this_well    <- gw_levels %>%
                        filter(.data$site_no == this_USGS_id,
                               .data$date <= SWIMS$date[i],
                               .data$date >= (SWIMS$date[i] %m-% months(1)))
        if (length(this_well$diff_m) > 0) {
          # Use lake-gw water level difference, if have
          if (abs(median(this_well$diff_m, na.rm = TRUE)) < threshold) {
            SWIMS$site_type[i] <- NA
          } else if (mean(this_well$diff_m, na.rm = TRUE) > 0) {
            SWIMS$site_type[i] <- "upgradient"
          } else {
            SWIMS$site_type[i] <- "downgradient"
          }
        } else {
          # Use static classification, if don't have lake-gw diff
          SWIMS$site_type[i] <- dictionary %>%
                                filter(.data$SWIMS_station_id ==
                                         SWIMS$station_id[i]) %>%
                                select(.data$static_iso_class) %>%
                                unlist() %>%
                                as.character()
        }
      }

    } else if (!is.na(SWIMS$wbic[i]) &
               SWIMS$wbic[i] %in% dictionary$WBIC) {
      # LAKE
      depths    <- str_split(SWIMS$header_labslip_depth[i], " ") %>% unlist
      units     <- depths[length(depths)]
      deepest   <- depths[length(depths) - 1]
      if (tolower(units) %in% c("feet", "foot", "ft")){
        deepest <- NISTftTOmeter(as.numeric(deepest))
      } else if (tolower(units) %in% c("meters", "meter", "m")){
      } else {
      }
      if (length(deepest) == 0){
        SWIMS$site_type[i] <- "lake_surface"
      } else if (deepest < 3) {
        SWIMS$site_type[i] <- "lake_surface"
      } else {
        SWIMS$site_type[i] <- "lake_bottom"
      }
    }
  }
  return(SWIMS)
}

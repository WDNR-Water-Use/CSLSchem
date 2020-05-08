#' Interpolate water chemistry values
#'
#' Interpolates water chemistry values for consistent comparisons across site
#' types. Can interpolate to a daily time step or a monthly time step. If
#' monthly, assumes end-of-month values are desired.
#'
#' @param df data frame with water chemistry information for tracer/solute.
#'           Includes:
#'   * **date:** date of chem measurement
#'   * **lake:** lake associated with measurement
#'   * **site_type:** type of site (e.g., lake, precipitaiton, upgradient)
#'   * **result:** value of measurement
#' @param dt unit of time to use for interpolation. Defaults to "month", can
#'           also be "day".
#'
#' @return df, a data frame with interpolated values.
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr arrange filter mutate
#' @importFrom zoo read.zoo na.approx
#'
#' @export

interpolate_chem_values <- function(df, dt = "month"){
  # Define start, end, and number of timesteps in desired interpolation dataset
  start_tracer <- floor_date(min(df$date), unit = "month")
  end_tracer   <- floor_date(max(df$date), unit = "month") + months(1) - days(1)
  nt           <- round(time_length(interval(start_tracer, end_tracer), unit = dt))

  # Define dates for interpolation
  dates        <- NULL
  for (i in 1:nt) {
    if (dt == "month") {
      dates <- c(dates, start_tracer + months(i-1))
    } else if (dt == "day") {
      dates <- c(dates, start_tracer + days(i-1))
    }
  }
  # Convert dates to datetime format
  # If monthly, make dates end-of-month
  if (dt == "month") {
    dates <- data.frame(date = as_datetime(dates) + months(1) - days(1))
  } else {
    dates <- data.frame(date = as_datetime(dates))
  }

  # Interpolate values for each site type at each lake
  interpreted <- NULL
  for (lake in unique(df$lake)) {
    for (site_type in unique(filter(df, .data$lake == !!lake)$site_type)){
      subset        <- df %>%
                       arrange(.data$date) %>%
                       filter(.data$lake == !!lake,
                              .data$site_type == !!site_type)
      subset        <- merge(subset, dates, all.x = TRUE, all.y = TRUE) %>%
                       mutate(lake = !!lake,
                              site_type = !!site_type)
      zoo_subset    <- read.zoo(select(subset, c("date", "result")),
                                index.name = "date")
      zoo_subset    <- data.frame(result = na.approx(zoo_subset, rule= 2))
      subset$result <- zoo_subset$result
      subset$lake   <- lake
      subset$site_type <- site_type
      interpreted   <- rbind(interpreted, subset)
    }
  }

  # Limit final data frame to desired dates (only affects monthly interpolation)
  df <- interpreted %>%
        filter(.data$date %in% dates$date) %>%
        group_by(date = floor_date(.data$date, unit = "month"),
                 lake = .data$lake,
                 site_type = .data$site_type) %>%
        summarise(result = mean(.data$result, na.rm = TRUE)) %>%
        ungroup()



  return(df)
}

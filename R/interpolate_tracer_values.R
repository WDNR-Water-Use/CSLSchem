#' Interpolate tracer values
#'
#' Interpolates tracer values to the last day of each month for consistent
#' comparisons across site types.
#'
#' @param tracer data frame with water chemistry information for tracer
#'
#' @return tracer, a data frame with interpolated end-of-month tracer values.
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr arrange filter mutate
#' @importFrom zoo read.zoo na.approx
#'
#' @export

interpolate_tracer_values <- function(tracer){
  # Create data frame of desired end-of-month dates & merge with tracer data
  start_tracer <- floor_date(min(tracer$date), unit = "month")
  end_tracer   <- floor_date(max(tracer$date), unit = "month") + months(1) - days(1)
  nmonths      <- round(time_length(interval(start_tracer, end_tracer), unit = "month"))

  # Define end-of-month
  dates        <- NULL
  for (i in 1:nmonths) {
    dates <- c(dates, start_tracer + months(i-1))
  }
  dates <- data.frame(date = as_datetime(dates) + months(1) - days(1))

  # Interpolate tracer values for each site type at each lake
  interpreted <- NULL
  for (lake in unique(tracer$lake)) {
    for (site_type in unique(filter(tracer, .data$lake == !!lake)$site_type)){
      subset        <- tracer %>%
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

  # Limit final data frame to end-of-month dates
  tracer <- interpreted %>%
            filter(.data$date %in% dates$date)

  return(tracer)
}

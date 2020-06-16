#' Add evaporation isotopes to dataset
#'
#' Adds evaporation stable isotope measurements, calculated with
#' "calculate_Cevap" in Cevap_functions.R.
#'
#' @param tracer data frame with water chemistry information for stable isotope.
#' @param weather data frame with monthly weather
#' @param chem_df data frame with water chemistry information for this analysis,
#'                for getting lake surface temperature.
#' @param start_date start date of analysis.
#' @param end_date end date of analysis.
#' @param chem_tracer name of stable isotope, "d18O" or "d2H".
#' @param seasonal_correction logical defaults to TRUE to incorporate seasonal
#'                            correction factors for d18O_atm based on
#'                            difference between calculated and reported
#'                            d18O_atm values in Krabbenhoft et al. (1990).
#'
#' @return Cevap, new data frame with same columns as tracer, but with only
#'         calculated evaporation values.
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter summarise mutate select
#' @importFrom lubridate month
#'
#' @export

add_evap_isotopes <- function(tracer, weather, chem_df, start_date, end_date,
                              chem_tracer, seasonal_correction){

  # Filter to require inputs, ensure dates will match (end-of-month)
  Cpcpn <- tracer %>%
           filter(.data$site_type == "precipitation") %>%
           select(date = .data$date,
                  lake = .data$lake,
                  Cpcpn = .data$result)
  Clake <- tracer %>%
           filter(.data$site_type == "lake")  %>%
           select(date = .data$date,
                  lake = .data$lake,
                  Clake = .data$result)
  air   <- weather %>%
           mutate(date = .data$date + months(1) - days(1)) %>%
           select(.data$date, .data$atmp_C, .data$RH_pct) %>%
           unique()
  water <- filter_lst(chem_df) %>%
           filter(.data$date >= start_date,
                  .data$date <= end_date) %>%
           group_by(lake = .data$lake,
                    date = floor_date(.data$date, unit = "month") +
                           months(1) - days(1)) %>%
           summarise(ltmp_C = mean(.data$result, na.rm = TRUE)) %>%
           ungroup()

  # Merge inputs
  Cevap <- merge(Clake, Cpcpn)
  Cevap <- merge(Cevap, air, by = "date", all.x = TRUE)
  Cevap <- merge(Cevap, water, all.x = TRUE)

  # Calculate evaporation stable isotope values for each lake & month
  Cevap <- Cevap %>%
           group_by(.data$date, .data$lake) %>%
           mutate(site_type = "evaporation",
                  result = calculate_Cevap(.data$atmp_C,
                                           .data$ltmp_C,
                                           .data$RH_pct,
                                           .data$Cpcpn,
                                           .data$Clake,
                                           chem_tracer,
                                           month(.data$date),
                                           seasonal_correction)) %>%
           ungroup() %>%
           select(.data$date, .data$lake, .data$site_type, .data$result)

  return(Cevap)
}

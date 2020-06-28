#' Calculate water balance
#'
#' Calculates the water balance using desired tracer, e.g., stable isotopes or
#' conservative anion/cation.
#'
#' @param parameter name of water chemistry parameter to grab concentration
#'                  information for. Must match the "description" field of
#'                  CSLSdata::water_chem for the desired parameter. Defaults to
#'                  "d18O". Options include "d18O", "d2H", "CALCIUM TOTAL
#'                  RECOVERABLE", "MAGNESIUM TOTAL RECOVERABLE", "CHLORIDE", and
#'                  "SODIUM TOTAL RECOVERABLE".
#' @param start_date start date of analysis. Defaults to start of WY2019
#'                   ("2018-10-01").
#' @param end_date end date of analysis. Defaults to end of WY2019
#'                   ("2019-09-30").
#' @param dt desired time step of inputs (e.g., "day" or "month")
#' @param no_ice logical defaults to TRUE to ignore ice formation.
#' @param C_evap concentraion in evaporation, defaults to zero
#' @param C_ice concentration in ice, defaults to zero.
#'
#' @return lake_inputs
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom zoo read.zoo na.approx
#' @importFrom reshape2 melt dcast
#' @importFrom CSLSevap CSLS_daily_met
#' @import dplyr
#' @import lubridate
#'
#' @export

process_balance_inputs <- function(parameter = "d18O",
                                   start_date = as_datetime("2018-10-01"),
                                   end_date = as_datetime("2019-09-30"),
                                   dt = "day",
                                   no_ice = TRUE,
                                   C_evap = 0,
                                   C_ice = 0){

  # Lake Volume & Area ---------------------------------------------------------
  # date, lake, level_m, area_m2, vol_m3, and dV_m3
  lake_levels  <- process_levels(start_date - months(1), end_date, dt)

  # Lake Weather ---------------------------------------------------------------
  # Uses CSLSevap to calculate lake evaporation
  # lake, date, day, P_mm, E_mm, atmp_C, RH_pct, irr_factor
  lake_weather <- process_weather(start_date- months(1), end_date, dt)

  # Lake Temperature -----------------------------------------------------------
  # date, lake, ltmp_bot_C, ltmp_surf_C
  lake_temp    <- process_lake_temp(start_date - months(1), end_date, dt)

  # Lake Chemistry -------------------------------------------------------------
  # lake, date, C_lake, C_pcpn, C_GWin
  lake_chem    <- process_chem(parameter, start_date - months(1), end_date, dt)

  # Lake Ice -------------------------------------------------------------------
  # date, I_mm, C_ice
  lake_ice <- NULL
  if (dt == "day") {
    dates    <-  seq(start_date - months(1), end_date, by = "1 day")
  } else if (dt == "month") {
    dates    <-  seq(start_date - months(1), end_date, by = "1 month")
  }
  for (i in 1:length(dates)) {
    this_ice <- data.frame(date = dates[i],
                           I_mm = calculate_ice_thickness(dates[i]))
    lake_ice <- rbind(lake_ice, this_ice)
  }
  lake_ice$C_ice <- C_ice

  # If not accounting for ice, set all I_mm to 0
  if (no_ice) {
    lake_ice$I_mm <- 0
  }

  # Lake Evaporation Chemistry -------------------------------------------------
  if (parameter %in% c("d18O", "d2H")) {
    monthly_weather <- process_weather(start_date - months(1), end_date, dt = "month")
    monthly_temp    <- process_lake_temp(start_date - months(1), end_date, dt = "month")
    monthly_chem    <- process_chem(parameter,  start_date - months(1), end_date, dt = "month")
    lake_Cevap      <- monthly_weather %>%
                       full_join(monthly_temp, by = c("lake", "date")) %>%
                       full_join(monthly_chem, by = c("lake", "date"))
    lake_Cevap      <- lake_Cevap %>%
                       mutate(C_evap = calculate_Cevap(atmp = .data$atmp_C,
                                                       ltmp = .data$ltmp_surf_C,
                                                       RH = .data$RH_pct,
                                                       Cpcpn = .data$C_pcpn,
                                                       Clake = .data$C_lake,
                                                       parameter = parameter)) %>%
                       select(.data$lake, .data$date, .data$C_evap)
    if (dt == "day") {
      lake_Cevap <- lake_Cevap %>%
                    mutate(date = .data$date + days(14))
      lake_Cevap <- interpolate_values(lake_Cevap,
                                       group_vars = "lake",
                                       val_var = "C_evap",
                                       start_date,
                                       end_date)
    }
  } else {
    lake_Cevap   <- lake_levels %>%
                    mutate(C_evap = !!C_evap) %>%
                    select(.data$lake, .data$date, .data$C_evap)
  }

  # Join Lake Inputs -----------------------------------------------------------
  # Join lake inputs
  lake_inputs <- lake_weather %>%
                 full_join(lake_levels, by = c("lake", "date")) %>%
                 full_join(lake_temp, by = c("lake", "date")) %>%
                 full_join(lake_ice, by = "date")  %>%
                 full_join(lake_chem, by = c("lake", "date")) %>%
                 full_join(lake_Cevap, by = c("lake", "date"))

  # Convert depths to volumes
  lake_inputs <- lake_inputs %>%
                 group_by(.data$lake) %>%
                 mutate(P_m3 = .data$P_mm*.data$area_m2/1000,
                        E_m3 = .data$E_mm*.data$area_m2/1000,
                        I_m3 = .data$I_mm*.data$area_m2/1000,
                        dC_lake = .data$C_lake - lag(.data$C_lake)) %>%
                 ungroup() %>%
                 select(.data$lake, .data$date, .data$day, .data$atmp_C,
                        .data$RH_pct, .data$irr_factor, .data$ltmp_bot_C,
                        .data$ltmp_surf_C, .data$area_m2, .data$vol_m3,
                        .data$dV_m3, .data$P_mm, .data$E_mm, .data$I_mm,
                        .data$P_m3, .data$E_m3, .data$I_m3, .data$C_lake,
                        .data$dC_lake, .data$C_pcpn, .data$C_GWin, .data$C_evap,
                        .data$C_ice) %>%
                 filter(.data$date >= start_date,
                        .data$date <= end_date)

  # Fix lake levels
  lake_inputs$lake <- factor(lake_inputs$lake,
                             levels = c("Pleasant", "Long", "Plainfield"))

  return(lake_inputs)
}

#' Optimize water balance
#'
#' Calculates RMSE of measured lake concentrations with those calculated with
#' the dynamic lake model (daily timestep) for use in optimization procedure.
#' Assumes use of two time periods for groundwater inflow calculcations (two
#' periods may have different residence times).
#'
#'@param t_res vector with residence times for first time period (t_res[1]) and
#'             second time period (t_res[2]).
#' @param t_switch date at which second time period begins
#' @param df daily inputs for water balance calculations incl. preciptation (vol
#'           & concentration), evaporation (vol & concentration), ice formation
#'           (vol & concentration), and lake volume.
#' @param lake current lake to evaluate
#' @param parameter parameter used for calculating water balance, usually "d18O"
#'
#' @return RMSE, root mean square error of match with lake measurements of
#'         parameter.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate select inner_join
#' @importFrom lubridate floor_date as_datetime
#' @importFrom NISTunits NISTdayTOsec
#'
#' @export

dynamic_lake_water_balance <- function(t_res, t_switch, df, lake, parameter) {
  t1       <- t_res[1]
  t2       <- t_res[2]

  # Get actual measurements
  lake_meas <- filter_parameter(CSLSdata::water_chem, parameter) %>%
               filter(.data$site_type == "lake",
                      .data$lake == !!lake) %>%
               mutate(date = floor_date(.data$date, unit = "day"),
                      C_lake_meas = .data$result) %>%
               select(.data$date, .data$C_lake_meas)

  # Identify starting lake concentration, start/end dates
  C_lake0   <- lake_meas$C_lake_meas[which.min(lake_meas$date)]
  t0        <- lake_meas$date[which.min(lake_meas$date)]
  tf        <- lake_meas$date[which.max(lake_meas$date)]
  df        <- df %>%
               filter(.data$lake == !!lake,
                      .data$date >= t0,
                      .data$date <= tf) %>%
               mutate(C_lake_calc = C_lake0)

  # Calculate GWin based on residence time
  df$res_time                      <- NA
  df$res_time[df$date < t_switch]  <- t1
  df$res_time[df$date >= t_switch] <- t2
  df$GWin_m3                       <- (df$vol_m3 - df$res_time*df$P_m3)/df$res_time

  # Calculate lake concentration
  for (i in 2:nrow(df)) {
    V    <- df$vol_m3[i]
    C_L1 <- df$C_lake_calc[i-1]
    G    <- df$GWin_m3[i]
    C_G  <- df$C_GWin[i]
    P    <- df$P_m3[i]
    C_P  <- df$C_pcpn[i]
    E    <- df$E_m3[i]
    C_E  <- df$C_evap[i]
    I    <- df$I_m3[i] - df$I_m3[i-1]
    C_I  <- df$C_ice[i]
    df$C_lake_calc[i] <- C_L1 + (1/V)*(G*(C_G - C_L1)+
                                         P*(C_P - C_L1) +
                                         E*(C_L1 - C_E))
  }
  df <- df %>% left_join(lake_meas, by = "date")

  return(df)
}

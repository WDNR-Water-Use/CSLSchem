#' Optimize water balance
#'
#' Calculates RMSE of measured lake concentrations with those calculated with
#' the dynamic lake model (daily timestep) for use in optimization procedure.
#' Assumes use of two time periods for groundwater inflow calculcations (two
#' periods may have different residence times).
#'
#' @param par vector with t1 (first residence time) and t2 (second residence time)
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
#'
#' @export

optimize_water_balance <- function(par, t_switch, df, lake, parameter) {
  df <- dynamic_lake_water_balance(par, t_switch, df, lake, parameter)

  # Evaluate fit
  fit  <- df %>%
          filter(!is.na(.data$C_lake_meas)) %>%
          mutate(sim = .data$C_lake_calc,
                 obs = .data$C_lake_meas,
                 SE = (.data$sim - .data$obs)^2) %>%
          select(.data$date, .data$sim, .data$obs, .data$SE)

  RMSE <- sqrt(mean(fit$SE))

  return(RMSE)
}

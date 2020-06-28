#' Calculate water balance
#'
#' Calculates the water balance using a stable isotope tracer using either a) a
#' direct approach, where GWin is directly calculated from measured and
#' interpolated values, or b) an indirect approach, where a dynamic lake model
#' with a daily time step is set up and GW residence time is solved for using
#' optimization.
#'
#' @param parameter parameter to use for water balance calculations, defaults to "d18O".
#' @param start_date start date of calculations
#' @param end_date end date of calculations
#' @param dt desired final time step ("day", "month", or "annual")
#' @param method approach for calculating water balance, either "direct",
#'               "indirect_calc", or "indirect_load". Defaults to
#'               "indirect_load".
#'
#' @return lake_fluxes
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom lubridate as_datetime floor_date
#' @importFrom stats optim
#'
#' @export

calculate_water_balance <- function(parameter = "d18O",
                                    start_date = as_datetime("2018-08-22"),
                                    end_date = as_datetime("2019-11-05"),
                                    dt = "day",
                                    method = "indirect_load"){

  if (method == "direct") {
    # Directly calculate GWin --------------------------------------------------
    lake_inputs <- process_balance_inputs(parameter, start_date, end_date,
                                          dt = "month", no_ice = FALSE)
    if (dt == "annual") {
      lake_inputs <- lake_inputs %>%
                     group_by(.data$lake) %>%
                     mutate(C_evap = .data$C_evap*.data$E_m3/
                                     sum(.data$E_m3[!is.na(.data$C_evap)]),
                            C_pcpn = .data$C_pcpn*.data$P_m3/
                                     sum(.data$P_m3[!is.na(.data$C_pcpn)])) %>%
                     summarise(P_m3 = sum(.data$P_m3, na.rm = TRUE),
                               E_m3 = sum(.data$E_m3, na.rm = TRUE),
                               dV_m3 = sum(.data$dV_m3, na.rm = TRUE),
                               vol_m3 = mean(.data$vol_m3, na.rm = TRUE),
                               area_m2 = mean(.data$area_m2, na.rm = TRUE),
                               C_lake = mean(.data$C_lake, na.rm = TRUE),
                               C_GWin = mean(.data$C_GWin, na.rm = TRUE),
                               C_evap = sum(.data$C_evap, na.rm = TRUE),
                               C_pcpn = sum(.data$C_pcpn, na.rm = TRUE),
                               dC_lake = sum(.data$dC_lake, na.rm = TRUE))
      lake_inputs$date <- interval(start_date, end_date)
    }
    lake_fluxes <- lake_inputs %>%
                   group_by(.data$lake) %>%
                   mutate(GWin_m3 = (.data$P_m3*(.data$C_pcpn - .data$C_lake) +
                                       .data$E_m3*(.data$C_lake - .data$C_evap) -
                                       .data$vol_m3*.data$dC_lake)/
                                    (.data$C_lake - .data$C_GWin)) %>%
                   ungroup()
  } else if (method == "indirect_calc"){
    # Indirectly calculate C_lake ----------------------------------------------
    lake_inputs <- process_balance_inputs(parameter, start_date, end_date,
                                          dt = "day", no_ice = FALSE)
    t_start     <- as_datetime("2019-04-01")
    t_end       <- as_datetime("2019-07-01")
    params      <- NULL
    for (t_switch in seq(t_start, t_end, by = "1 day")) {
      t_switch <- as_datetime(t_switch)
      for (lake in unique(lake_inputs$lake)) {
        optimization <- optim(par = c(300, 300),
                              fn = optimize_water_balance,
                              df = lake_inputs,
                              lake = lake,
                              parameter = parameter,
                              t_switch = t_switch,
                              method = "L-BFGS-B",
                              lower = c(50, 50),
                              upper = c(2*365.25, 2*365.25))
        this_lake    <- data.frame(lake = lake,
                                   t1 = optimization$par[1],
                                   t2 = optimization$par[2],
                                   t_switch = t_switch,
                                   RMSE = optimization$value)
        params       <- rbind(params, this_lake)
      }
    }
    # Limit to best fit
    params <- params %>%
              group_by(.data$lake) %>%
              mutate(best_RMSE = min(.data$RMSE)) %>%
              ungroup() %>%
              filter(.data$RMSE == .data$best_RMSE) %>%
              mutate(t1 = round(.data$t1),
                     t2 = round(.data$t2)) %>%
              select(.data$lake, .data$t1, .data$t2, .data$t_switch, .data$RMSE)
    lake_fluxes <- NULL
    for (i in 1:nrow(params)) {
      this_lake <- dynamic_lake_water_balance(c(params$t1[i], params$t2[i]),
                                              lake_inputs,
                                              as.character(params$lake[i]),
                                              parameter,
                                              params$t_switch[i])
      lake_fluxes <- rbind(lake_fluxes, this_lake)
    }
    lake_fluxes$lake <- factor(lake_fluxes$lake,
                               levels = c("Pleasant", "Long", "Plainfield"))

  } else if (method == "indirect_load") {
    lake_fluxes <- CSLSchem::lake_fluxes
  }


  # Summarize by larger timesteps, if needed -----------------------------------
  if (method %in% c("indirect_calc", "indirect_load")) {
    lake_fluxes <- lake_fluxes %>%
                   filter(.data$date >= start_date,
                          .data$date <= end_date)
    if (dt == "month") {
      lake_fluxes <- lake_fluxes %>%
                     group_by(lake = .data$lake,
                              date = floor_date(.data$date, unit = "month")) %>%
                     summarise(atmp_C = mean(.data$atmp_C),
                               RH_pct = mean(.data$RH_pct),
                               irr_factor = mean(.data$irr_factor),
                               ltmp_bot_C = mean(.data$ltmp_bot_C),
                               ltmp_surf_C = mean(.data$ltmp_surf_C),
                               area_m2 = mean(.data$area_m2),
                               vol_m3 = mean(.data$vol_m3),
                               C_lake = mean(.data$C_lake),
                               C_pcpn = mean(.data$C_pcpn),
                               C_GWin = mean(.data$C_GWin),
                               C_evap = mean(.data$C_evap),
                               C_lake_calc = mean(.data$C_lake_calc),
                               P_m3 = sum(.data$P_m3),
                               E_m3 = sum(.data$E_m3),
                               GWin_m3 = sum(.data$GWin_m3),
                               dV_m3 = sum(.data$dV_m3)) %>%
                     ungroup()
    } else if (dt == "annual") {
      lake_fluxes <- lake_fluxes %>%
                     group_by(lake = .data$lake) %>%
                     summarise(atmp_C = mean(.data$atmp_C, na.rm = TRUE),
                               RH_pct = mean(.data$RH_pct, na.rm = TRUE),
                               irr_factor = mean(.data$irr_factor, na.rm = TRUE),
                               ltmp_bot_C = mean(.data$ltmp_bot_C, na.rm = TRUE),
                               ltmp_surf_C = mean(.data$ltmp_surf_C, na.rm = TRUE),
                               area_m2 = mean(.data$area_m2, na.rm = TRUE),
                               vol_m3 = mean(.data$vol_m3, na.rm = TRUE),
                               C_lake = mean(.data$C_lake, na.rm = TRUE),
                               C_pcpn = mean(.data$C_pcpn, na.rm = TRUE),
                               C_GWin = mean(.data$C_GWin, na.rm = TRUE),
                               C_evap = mean(.data$C_evap, na.rm = TRUE),
                               C_lake_calc = mean(.data$C_lake_calc, na.rm = TRUE),
                               P_m3 = sum(.data$P_m3, na.rm = TRUE),
                               E_m3 = sum(.data$E_m3, na.rm = TRUE),
                               GWin_m3 = sum(.data$GWin_m3, na.rm = TRUE),
                               dV_m3 = sum(.data$dV_m3, na.rm = TRUE)) %>%
                     ungroup()
    }
  }
  # GWout ----------------------------------------------------------------------
  lake_fluxes$GWout_m3 <- lake_fluxes$P_m3 + lake_fluxes$GWin_m3 -
                          lake_fluxes$E_m3 - lake_fluxes$dV_m3

  # Fluxes as percentages ------------------------------------------------------
  lake_fluxes <- lake_fluxes %>%
                 mutate(P_pct = .data$P_m3/(.data$P_m3 + .data$GWin_m3),
                        GWin_pct = .data$GWin_m3/(.data$P_m3 + .data$GWin_m3),
                        E_pct = .data$E_m3/(.data$P_m3 + .data$GWin_m3),
                        GWout_pct = .data$GWout_m3/(.data$P_m3 + .data$GWin_m3),
                        dV_pct = .data$dV_m3/(.data$P_m3 + .data$GWin_m3))

  # If annual, calculate residence time ----------------------------------------
  if (dt == "annual") {
    lake_fluxes$res_time <- lake_fluxes$vol_m3/
                            (lake_fluxes$P_m3 + lake_fluxes$GWin_m3)
  }

  return(lake_fluxes)
}

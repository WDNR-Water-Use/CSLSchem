#' Calculate solute mass balance
#'
#' Calculates the solute mass balance.
#'
#' @param start_date start date of analysis. Defaults to start of WY2019
#'                   ("2018-10-01").
#' @param end_date end date of analysis. Defaults to end of WY2019
#'                   ("2019-09-30").
#' @param method "load" or "calc", defaults to "load".
#' @param lakes name of all lakes in CSLS in the order of desired levels.
#'              Defaults to c("Pleasant", "Long", "Plainfield")
#' @param annual logical defaults to "FALSE" to return daily values, switch to
#'               "TRUE" for annual summary of uptake (Ca) or
#'               sedimentation/release and GW inflow (TP).
#' @param parameter description of parameter to use dynamic lake model on. Can
#'                  be either "CALCIUM TOTAL RECOVERABLE" or "PHOSPHORUS TOTAL".
#'
#' @return mass_fluxes
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr group_by left_join select summarise mutate filter bind_rows
#' @importFrom lubridate as_datetime
#' @importFrom usethis use_data
#'
#' @export

calculate_solute_dynamics <- function(start_date = as_datetime("2018-10-01"),
                                      end_date = as_datetime("2019-09-30"),
                                      method = "load",
                                      lakes = c("Pleasant", "Long", "Plainfield"),
                                      annual = FALSE,
                                      parameter = "CALCIUM TOTAL RECOVERABLE"){
  if (method == "calc") {
    lake_solutes <- NULL
    for (lake in lakes) {
      # Get actual measurements
      lake_meas <- filter_parameter(CSLSdata::water_chem, parameter) %>%
                   filter(.data$site_type == "lake",
                          .data$lake == !!lake) %>%
                   mutate(date = floor_date(.data$date, unit = "day"),
                          C_lake_meas = .data$result) %>%
                   select(.data$date, .data$C_lake_meas)
      t0        <- lake_meas$date[which.min(lake_meas$date)]
      tf        <- lake_meas$date[which.max(lake_meas$date)]

      # Get inputs
      lake_inputs <- process_balance_inputs(parameter = parameter,
                                            start_date = t0,
                                            end_date = tf,
                                            no_ice = FALSE) %>%
                     filter(!is.na(.data$P_m3),
                            .data$lake == !!lake)
      if ((min(lake_inputs$date) - days(1)) %in% lake_meas$date) {
        C_lake0 <- lake_meas$C_lake_meas[lake_meas$date ==
                                           min(lake_inputs$date) - days(1)]
        I0      <- lake_inputs$I_m3[lake_inputs$date ==
                                      min(lake_inputs$date)]
        add_day <- data.frame(date = min(lake_inputs$date) - days(1),
                              lake = lake,
                              I_m3 = I0,
                              C_lake = C_lake0)
        lake_inputs <- bind_rows(lake_inputs, add_day)
      }

      lake_inputs <- lake_inputs %>% arrange(.data$date)

      # Get groundwater
      lake_fluxes <- CSLSchem::lake_fluxes %>%
                     filter(.data$lake == !!lake) %>%
                     select(.data$date, .data$lake, .data$res_time)
      t1          <- lake_fluxes$res_time[which.min(lake_fluxes$date)]
      t2          <- lake_fluxes$res_time[which.max(lake_fluxes$date)]
      t_switch    <- min(lake_fluxes$date[lake_fluxes$res_time == t2])

      lake_inputs$res_time <- NA
      lake_inputs$res_time[lake_inputs$date < t_switch]  <- t1
      lake_inputs$res_time[lake_inputs$date >= t_switch] <- t2
      lake_inputs$GWin_m3 <- (lake_inputs$vol_m3 -
                                lake_inputs$res_time*lake_inputs$P_m3)/
                              lake_inputs$res_time
      lake_inputs$C_GWin  <- mean(lake_inputs$C_GWin, na.rm = TRUE)

      # Merge inputs w/measurements
      df <- left_join(lake_inputs, lake_meas, by = "date")

      if (parameter == "CALCIUM TOTAL RECOVERABLE") {
        par   <- 0.5
        lower <- 0
        upper <- 2
      } else if (parameter == "PHOSPHORUS TOTAL") {
        par   <- c(3.08, 0.047, 0, 0.000595, 0.08)
        lower <- c(0, 0.005, 0, 0.00005, 0)
        upper <- c(100, 0.5, 2, 0.05, 2)
      }

      optimization <- optim(par = par,
                            fn = optimize_solutes,
                            df = df,
                            parameter = parameter,
                            method = "L-BFGS-B",
                            lower = lower,
                            upper = upper)

      if (parameter == "CALCIUM TOTAL RECOVERABLE") {
        this_lake <- dynamic_lake_calcium(optimization$par, df)
      } else if (parameter == "PHOSPHORUS TOTAL") {
        this_lake <- dynamic_lake_phosphorus(optimization$par, df)
      }
      lake_solutes <- rbind(lake_solutes, this_lake)
    }
    if (parameter == "CALCIUM TOTAL RECOVERABLE") {
      lake_Ca <- lake_solutes
      use_data(lake_Ca, overwrite = TRUE, compress = "xz")
    }else if (parameter == "PHOSPHORUS TOTAL") {
      lake_TP <- lake_solutes
      use_data(lake_TP, overwrite = TRUE, compress = "xz")
    }

  } else if (method == "load") {
    if (parameter == "CALCIUM TOTAL RECOVERABLE") {
      lake_solutes <- CSLSchem::lake_Ca
    } else if (parameter == "PHOSPHORUS TOTAL") {
      lake_solutes <- CSLSchem::lake_TP
    }
  }

  lake_solutes <- lake_solutes %>%
                  filter(.data$date >= start_date,
                         .data$date <= end_date)

  if (annual) {
    if (parameter == "CALCIUM TOTAL RECOVERABLE") {
      lake_solutes <- lake_solutes %>%
                      group_by(.data$lake) %>%
                      summarise(Uptake = sum(.data$Uptake, na.rm = TRUE)) %>%
                      ungroup()
    } else if (parameter == "PHOSPHORUS TOTAL") {
      lake_solutes <- lake_solutes %>%
                      group_by(.data$lake) %>%
                      summarise(Sed = sum(.data$sed, na.rm = TRUE),
                                Rel = sum(.data$rel, na.rm = TRUE),
                                GW  = sum(.data$GWin_m3*.data$C_GWin, na.rm = TRUE)) %>%
                      ungroup()
    }
  }

  lake_solutes$lake <- factor(lake_solutes$lake, levels = lakes)

  return(lake_solutes)
}

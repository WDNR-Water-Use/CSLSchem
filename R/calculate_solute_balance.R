#' Calculate solute mass balance
#'
#' Calculates the solute mass balance.
#'
#' @param parameters names of water chemsitry parameters to analyze mass balance
#'                   for. Defaults to c("CALCIUM TOTAL RECOVERABLE",
#'                   "CONDUCTIVITY, UMHOS/CM @ 25C", "CHLORIDE", "MAGNESIUM
#'                   TOTAL RECOVERABLE", "NITROGEN NH3-N DISS", "NITROGEN
#'                   NO3+NO2 DISS (AS N)", "NITROGEN TOTAL", "PH LAB",
#'                   "POTASSIUM TOTAL RECOVERABLE", "SODIUM TOTAL RECOVERABLE",
#'                   "SULFATE TOTAL").
#' @param start_date start date of analysis. Defaults to start of WY2019
#'                   ("2018-10-01").
#' @param end_date end date of analysis. Defaults to end of WY2019
#'                   ("2019-09-30").
#'
#' @return mass_fluxes
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom dplyr group_by select full_join summarise mutate
#' @importFrom reshape2 melt
#' @importFrom lubridate as_datetime
#'
#' @export

calculate_solute_balance <- function(parameters = c("CALCIUM TOTAL RECOVERABLE",
                                                    "CONDUCTIVITY, UMHOS/CM @ 25C",
                                                    "CHLORIDE",
                                                    "MAGNESIUM TOTAL RECOVERABLE",
                                                    "NITROGEN NH3-N DISS",
                                                    "NITROGEN NO3+NO2 DISS (AS N)",
                                                    "NITROGEN TOTAL",
                                                    "PH LAB",
                                                    "POTASSIUM TOTAL RECOVERABLE",
                                                    "SODIUM TOTAL RECOVERABLE",
                                                    "SULFATE TOTAL"),
                                     start_date = as_datetime("2018-10-01"),
                                     end_date = as_datetime("2019-09-30")){

  # WATER FLUXES ---------------------------------------------------------------
  water_fluxes <- calculate_water_balance(dt = "day",
                                          start_date = start_date,
                                          end_date = end_date) %>%
                  select(.data$date, .data$lake, .data$P_m3, .data$vol_m3,
                         .data$GWin_m3, .data$GWout_m3)

  # SOLUTE CONCENTRATIONS ------------------------------------------------------
  water_chem <- NULL
  for (parameter in parameters) {
    this_chem <- process_chem(parameter,
                              start_date,
                              end_date,
                              dt = "day")
    if (parameter == "PH LAB") {
      this_chem[,3:5] <- (10^-(this_chem[,3:5]))*1000 # to make mass conversions work later
    }
    this_chem$parameter <- parameter
    water_chem          <- rbind(water_chem, this_chem)
  }

  # SOLUTE MASSES --------------------------------------------------------------
  # Merge with water fluxes
  mass_fluxes <- full_join(water_fluxes,
                            water_chem,
                            by = c("date", "lake"))

  # Annual summary
  mass_fluxes <- mass_fluxes %>%
                 mutate(M_lake = .data$C_lake*(.data$vol_m3*1000)/(1000*1000)) %>%
                 group_by(.data$lake, .data$parameter) %>%
                 summarise(P_m3 = sum(.data$P_m3, na.rm = TRUE),
                           GWin_m3 = sum(.data$GWin_m3, na.rm = TRUE),
                           GWout_m3 = sum(.data$GWout_m3, na.rm = TRUE),
                           C_pcpn = mean(.data$C_pcpn, na.rm = TRUE),
                           C_GWin = mean(.data$C_GWin, na.rm = TRUE),
                           C_lake = mean(.data$C_lake, na.rm = TRUE),
                           dM_lake = .data$M_lake[.data$date == max(.data$date[!is.na(.data$M_lake)])] -
                                     .data$M_lake[.data$date == min(.data$date[!is.na(.data$M_lake)])]) %>%
                 ungroup()

  # Calculate Masses
  # (mg/L) * (m^3) * (1000 L/m^3) / (1000 mg/g * 1000 g/kg)
  mass_fluxes <- mass_fluxes %>%
                 mutate(P = .data$C_pcpn*(.data$P_m3*1000)/(1000*1000),
                        GWin = .data$C_GWin*(.data$GWin_m3*1000)/(1000*1000),
                        GWout = .data$C_lake*(.data$GWout_m3*1000)/(1000*1000),
                        dLake = .data$dM_lake,
                        IO = .data$P + .data$GWin - .data$GWout,
                        Up = .data$IO - .data$dLake) %>%
                 select(.data$lake, .data$parameter, .data$P, .data$GWin,
                        .data$GWout, .data$IO, .data$dLake, .data$Up)

  colnames(mass_fluxes) <- c("lake", "parameter", "P", "GWin", "GWout", "I-O",
                             paste0('\u0394', " Lake"), "Up")

  mass_fluxes           <- melt(mass_fluxes, id.vars = c("lake", "parameter"))
  colnames(mass_fluxes) <- c("lake", "parameter", "site_type", "mass_kg")

  return(mass_fluxes)
}

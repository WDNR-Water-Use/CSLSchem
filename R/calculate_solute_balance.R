#' Calculate solute mass balance
#'
#' Calculates the solute mass balance.
#'
#' @param desired_lakes vector of lakes to analyze, defaults to
#'                      c("Pleasant", "Long", "Plainfield")
#' @param chem_df data frame with water chemistry information for all sites.
#'                Defaults to CSLSdata::water_chem.
#' @param chem_tracer name of water chemistry parameter to use as a tracer.
#'                    Must match the "description" field of CSLSdata::water_chem
#'                    for the desired parameter. Defaults to "d18O". Options
#'                    include "d18O", "d2H", "CALCIUM TOTAL RECOVERABLE",
#'                    "MAGNESIUM TOTAL RECOVERABLE", "CHLORIDE", and
#'                    "SODIUM TOTAL RECOVERABLE".
#' @param solutes names of water chemsitry parameters to analyze mass balance
#'                for. Defaults to c("CALCIUM TOTAL RECOVERABLE", "CHLORIDE",
#'                "MAGNESIUM TOTAL RECOVERABLE", "NITROGEN NH3-N DISS", "SODIUM
#'                TOTAL RECOVERABLE", "SULFATE TOTAL").
#' @param start_date start date of analysis. Defaults to start of WY2019
#'                   ("2018-10-01").
#' @param end_date end date of analysis. Defaults to end of WY2019
#'                   ("2019-09-30").
#'
#' @return mass_fluxes
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom reshape2 melt dcast
#' @importFrom stringr str_replace
#' @import dplyr
#' @import lubridate
#'
#' @export

calculate_solute_balance <- function(desired_lakes = c("Pleasant", "Long", "Plainfield"),
                                     chem_df = CSLSdata::water_chem,
                                     chem_tracer = "d18O",
                                     solutes = c("CALCIUM TOTAL RECOVERABLE",
                                                 "CHLORIDE",
                                                 "MAGNESIUM TOTAL RECOVERABLE",
                                                 "NITROGEN NH3-N DISS",
                                                 "SODIUM TOTAL RECOVERABLE",
                                                 "SULFATE TOTAL"),
                                     start_date = "10-01-2018",
                                     end_date = "09-30-2019"){

  # WATER FLUXES ---------------------------------------------------------------
  # Calculate water balance
  water_fluxes <- calculate_water_balance(desired_lakes, chem_df, chem_tracer,
                                          "09-01-2018", end_date)

  # Pull out fluxes of interest
  water_fluxes <- water_fluxes %>%
                  select(.data$date, .data$lake, .data$P_m3, .data$mean_vol_m3,
                         .data$GWin_m3, .data$GWout_m3)

  # Rename fluxes to match site_types in water_chem data frame
  water_cols   <- c("P_m3", "mean_vol_m3", "GWin_m3", "GWout_m3")
  water_names  <- c("precipitation", "lake", "upgradient", "downgradient")
  water_fluxes <- melt(water_fluxes, id.vars = c("date", "lake"))
  colnames(water_fluxes) <- c("date", "lake", "site_type", "vol_m3")
  for (i in 1:length(water_cols)) {
    water_fluxes$site_type <- str_replace(water_fluxes$site_type,
                                          water_cols[i],
                                          water_names[i])
  }

  # SOLUTE CONCENTRATIONS ------------------------------------------------------
  # Convert dates to datetime
  start_date <- as_datetime(mdy(start_date))
  end_date   <- as_datetime(mdy(end_date))

  # Get solutes of interest, interpolate to monthly values
  C_solutes <- NULL
  for (solute in solutes) {
    C_solute <- filter_parameter(chem_df, solute)
    C_solute <- C_solute %>%
                filter(.data$date >= start_date - months(1),
                       .data$date <= end_date,
                       .data$site_type %in% c("precipitation",
                                              "lake",
                                              "upgradient"))

    units <- unique(C_solute$units)[!is.na(unique(C_solute$units))]
    if (length(units) > 1 ) {
      warning("One or more solutes has units other than MG/L")
    }
    C_solute <- interpolate_chem_values(C_solute, dt = "day") %>%
                 mutate(parameter = solute,
                        units = units) %>%
                 select(.data$date, .data$lake, .data$parameter,
                        .data$site_type, .data$result, .data$units)
    C_solutes <- rbind(C_solutes, C_solute)
  }

  # Map precip to each lake
  for (lake in desired_lakes) {
    Cpcpn     <- C_solutes %>%
                 filter(.data$lake == "Precip",
                        .data$site_type == "precipitation") %>%
                 mutate(lake = !!lake)
    C_solutes <- rbind(C_solutes, Cpcpn)
  }
  C_solutes    <- C_solutes %>% filter(.data$lake != "Precip")
  C_downgr     <- C_solutes %>%
                  filter(.data$site_type == "lake") %>%
                  mutate(site_type = "downgradient")
  C_solutes    <- rbind(C_solutes, C_downgr)

  # SOLUTE MASSES --------------------------------------------------------------
  # Merge with water fluxes, calcualate mass
  mass_fluxes <- merge(C_solutes,
                       water_fluxes,
                       by = c("date", "lake", "site_type"))
  # (mg/L) * (m^3) * (1000 L/m^3) / (1000 mg/g * 1000 g/kg)
  mass_fluxes$mass_kg <- mass_fluxes$result*(mass_fluxes$vol_m3*1000)/(1000*1000)
  mass_fluxes <- mass_fluxes %>%
                 arrange(.data$date) %>%
                 group_by(.data$lake, .data$site_type, .data$parameter) %>%
                 mutate(mass_kg = ifelse(.data$site_type == "lake",
                                         .data$mass_kg - lag(.data$mass_kg),
                                         .data$mass_kg)) %>%
                 ungroup() %>%
                 mutate(site_type = ifelse(.data$site_type == "lake",
                                           "delta_lake", .data$site_type)) %>%
                 filter(.data$date >= start_date,
                        .data$date <= end_date) %>%
                 select(.data$date, .data$lake, .data$site_type,
                        .data$parameter, .data$mass_kg)
  mass_IO <- mass_fluxes %>%
             filter(.data$site_type %in% c("precipitation", "upgradient",
                                           "downgradient")) %>%
             mutate(mass_kg = ifelse(.data$site_type == "downgradient",
                                     -.data$mass_kg,
                                     .data$mass_kg)) %>%
             group_by(.data$date, .data$lake, .data$parameter) %>%
             summarise(mass_kg = sum(.data$mass_kg),
                       site_type = "in_out") %>%
             ungroup()
  mass_fluxes <- rbind(mass_fluxes, mass_IO)

  mass_fluxes <- mass_fluxes %>%
                 group_by(.data$lake, .data$parameter, .data$site_type) %>%
                 summarise(mass_kg = sum(.data$mass_kg)) %>%
                 ungroup()

  mass_fluxes$site_type <- factor(mass_fluxes$site_type,
                                  levels = c("precipitation", "upgradient",
                                             "downgradient", "in_out",
                                             "delta_lake"),
                                  labels = c("P", "GWin", "GWout", "I-O", "Delta Lake"))
  return(mass_fluxes)

}

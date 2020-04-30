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

  # Calculate water balance
  water_fluxes <- calculate_water_balance(desired_lakes, chem_df, chem_tracer,
                                          start_date, end_date, annual = TRUE)
  water_fluxes <- water_fluxes %>%
                  select(.data$lake, .data$P_m3, .data$dV_m3, .data$GWin_m3,
                         .data$GWout_m3)
  water_fluxes <- melt(water_fluxes, id.vars = "lake")
  colnames(water_fluxes) <- c("lake", "site_type", "vol_m3")
  water_fluxes$site_type <- str_replace(water_fluxes$site_type,
                                        "P_m3", "precipitation")
  water_fluxes$site_type <- str_replace(water_fluxes$site_type,
                                        "dV_m3", "delta_lake")
  water_fluxes$site_type <- str_replace(water_fluxes$site_type,
                                        "GWin_m3", "upgradient")
  water_fluxes$site_type <- str_replace(water_fluxes$site_type,
                                        "GWout_m3", "downgradient")

  # Convert dates to datetime
  start_date <- as_datetime(mdy(start_date))
  end_date   <- as_datetime(mdy(end_date))

  # Solute fluxes
  C_solutes <- NULL
  for (solute in solutes) {
    C_solute <- filter_parameter(chem_df, solute)
    C_solute <- C_solute %>%
                filter(.data$date >= start_date,
                       .data$date <= end_date,
                       .data$site_type %in% c("precipitation", "lake",
                                              "upgradient")) %>%
                group_by(.data$lake, .data$site_type) %>%
                summarise(result = mean(.data$result, na.rm = TRUE),
                          units = unique(.data$units)) %>%
                ungroup() %>%
                mutate(parameter = solute)
    C_solutes <- rbind(C_solutes, C_solute)
  }
  if (length(unique(C_solutes$units)) > 1 ) {
    warning("One or more solutes has units other than MG/L")
  }

  # Map precip to each lake
  for (lake in desired_lakes) {
    Cpcpn     <- C_solutes %>%
                 filter(.data$lake == "Precip",
                        .data$site_type == "precipitation") %>%
                 mutate(lake = !!lake)
    C_solutes <- rbind(C_solutes, Cpcpn)
  }
  C_delta_lake <- C_solutes %>%
                  filter(.data$site_type == "lake") %>%
                  mutate(site_type = "delta_lake")
  C_downgr     <- C_solutes %>%
                  filter(.data$site_type == "lake") %>%
                  mutate(site_type = "downgradient")
  C_solutes    <- C_solutes %>% filter(.data$site_type != "lake")
  C_solutes    <- rbind(C_solutes, C_delta_lake)
  C_solutes    <- rbind(C_solutes, C_downgr)

  # Merge with water fluxes, calcualate mass
  mass_fluxes <- merge(C_solutes,
                       water_fluxes,
                       by = c("lake", "site_type"),
                       all.x = TRUE)
  mass_fluxes$mass_kg <- mass_fluxes$result*(mass_fluxes$vol_m3*1000)/(1000*1000)
  mass_fluxes <- mass_fluxes %>%
                 filter(.data$lake %in% desired_lakes) %>%
                 select(.data$lake, .data$site_type, .data$parameter, .data$mass_kg)
  mass_IO <- mass_fluxes %>%
             filter(.data$site_type %in% c("precipitation", "upgradient",
                                           "downgradient")) %>%
             group_by(.data$lake, .data$parameter) %>%
             mutate(mass_kg = ifelse(.data$site_type == "downgradient",
                                     -.data$mass_kg,
                                     .data$mass_kg)) %>%
             summarise(mass_kg = sum(.data$mass_kg),
                       site_type = "in_out") %>%
             ungroup()
  mass_fluxes <- rbind(mass_fluxes, mass_IO)
  mass_fluxes$site_type <- factor(mass_fluxes$site_type,
                                  levels = c("precipitation", "upgradient",
                                             "downgradient", "in_out",
                                             "delta_lake"),
                                  labels = c("P", "GWin", "GWout", "I-O", "Delta Lake"))
  return(mass_fluxes)

}

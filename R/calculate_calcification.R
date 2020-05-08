#' Calculate calcification rates
#'
#' Calculates the calcification rate using measurements of calcium, magnesium,
#' and alkalinity.
#'
#' @param desired_lakes vector of lakes to analyze, defaults to
#'                      c("Pleasant", "Long", "Plainfield")
#' @param water_chem data frame with water chemistry information for all sites.
#'                   Defaults to CSLSdata::water_chem.
#' @param chem_tracer name of water chemistry parameter to use as a tracer.
#'                    Must match the "description" field of CSLSdata::water_chem
#'                    for the desired parameter. Defaults to "d18O". Options
#'                    include "d18O", "d2H", "CALCIUM TOTAL RECOVERABLE",
#'                    "MAGNESIUM TOTAL RECOVERABLE", "CHLORIDE", and
#'                    "SODIUM TOTAL RECOVERABLE".
#' @param start_date start date of analysis. Defaults to start of WY2019
#'                   ("2018-10-01").
#' @param end_date end date of analysis. Defaults to end of WY2019
#'                   ("2019-09-30").
#'
#' @return calcify, a data frame with the following columns
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom reshape2 melt dcast
#' @import dplyr
#' @import lubridate
#'
#' @export

calculate_calcification <- function(desired_lakes = c("Pleasant", "Long", "Plainfield"),
                                    water_chem = CSLSdata::water_chem,
                                    chem_tracer = "d18O",
                                    start_date = "10-01-2018",
                                    end_date = "09-30-2019"){

  # WATER FLUXES ---------------------------------------------------------------
  # Calculate monthly water balance
  water_fluxes <- calculate_water_balance(desired_lakes, water_chem, chem_tracer,
                                          start_date, end_date)

  # Pull out fluxes of interest (mean lake vol, GWin, GWout)
  water_fluxes <- water_fluxes %>%
                  select(.data$lake, .data$date, .data$mean_vol_m3,
                         .data$GWin_m3, .data$GWout_m3)

  start_date <- as_datetime(mdy(start_date))
  end_date   <- as_datetime(mdy(end_date))

  # SOLUTE CONCENTRATIONS ------------------------------------------------------
  # Get solutes of interest (Mg, Ca, Alk)
  C_solutes <- NULL
  for (solute in c("CALCIUM TOTAL RECOVERABLE", "MAGNESIUM TOTAL RECOVERABLE",
                   "ALKALINITY TOTAL CACO3")) {
    C_solute  <- filter_parameter(water_chem, solute)
    C_solute  <- C_solute %>% filter(.data$site_type %in% c("lake", "upgradient"))
    C_solute  <- interpolate_chem_values(C_solute, dt = "day") %>%
                 select(.data$lake, .data$date, .data$site_type, .data$result)
    C_solute$description <- solute
    C_solutes <- rbind(C_solutes, C_solute)
  }

  # CALCIFICATION --------------------------------------------------------------
  # Merge solute concentrations and water fluxes
  calcify <- merge(C_solutes,
                   water_fluxes,
                   by = c("lake", "date"),
                   all.x = TRUE)
  calcify <- merge(calcify,
                   CSLSchem::mgTOmeq,
                   by = "description",
                   all.x = TRUE)

  # Term 0: lake concentration (meq/L)
  lake   <- calcify %>%
            filter(.data$site_type == "lake",
                   .data$description != "MAGNESIUM TOTAL RECOVERABLE",
                   .data$date >= start_date,
                   .data$date <= end_date) %>%
            arrange(.data$date) %>%
            mutate(result = .data$result*.data$mgTOmeq) %>%
            select(.data$lake, .data$date, .data$description, .data$result)

  # Term 1: change in lake concentration
  dlake  <- calcify %>%
            filter(.data$site_type == "lake",
                   .data$description != "MAGNESIUM TOTAL RECOVERABLE") %>%
            arrange(.data$date) %>%
            group_by(.data$lake, .data$description) %>%
            mutate(result = .data$result*.data$mgTOmmol - lag(.data$result*.data$mgTOmmol),
                   site_type = "delta_lake") %>%
            ungroup() %>%
            filter(.data$date >= start_date,
                   .data$date <= end_date) %>%
            select(.data$lake, .data$date, .data$description,
                   .data$site_type, .data$result)

  dlake <- dlake %>%
           mutate(result = ifelse(.data$description == "ALKALINITY TOTAL CACO3",
                                  .data$result/2,
                                  .data$result))

  # Term 2: Mg_avg/Mg ratio
  Mg_ratio <- calcify %>%
              filter(.data$site_type == "lake",
                     .data$description == "MAGNESIUM TOTAL RECOVERABLE",
                     .data$date >= start_date,
                     .data$date <= end_date) %>%
              arrange(.data$date) %>%
              group_by(.data$lake) %>%
              mutate(result = mean(.data$result*.data$mgTOmmol, na.rm = TRUE)/
                              .data$result*.data$mgTOmmol,
                     site_type = "Mg_ratio") %>%
              ungroup() %>%
              select(.data$lake, .data$date, .data$site_type, .data$result)
  Mg_ratio$description = "CALCIUM TOTAL RECOVERABLE"
  Mg_ratio <- rbind(Mg_ratio,
                    mutate(Mg_ratio, description = "ALKALINITY TOTAL CACO3"))

  # Term 3: Groundwater inflow normalized by lake volume
  GWin <- calcify %>%
          filter(.data$site_type == "upgradient",
                 .data$description != "MAGNESIUM TOTAL RECOVERABLE",
                 .data$date >= start_date,
                 .data$date <= end_date) %>%
          group_by(.data$lake, .data$date, .data$description) %>%
          summarise(result = .data$result*.data$mgTOmmol*.data$GWin_m3/
                             .data$mean_vol_m3) %>%
          ungroup() %>%
          mutate(site_type = "upgradient") %>%
          select(.data$lake, .data$date, .data$description,
                 .data$site_type, .data$result)

  # Term 4: Groundwater outflow normalized by lake volume
  GWout <- calcify %>%
           filter(.data$site_type == "lake",
                  .data$description != "MAGNESIUM TOTAL RECOVERABLE",
                  .data$date >= start_date,
                  .data$date <= end_date) %>%
           group_by(.data$lake, .data$date, .data$description) %>%
           summarise(result = .data$result*.data$mgTOmmol*.data$GWout_m3/
                              .data$mean_vol_m3) %>%
           ungroup() %>%
           mutate(site_type = "downgradient") %>%
           select(.data$lake, .data$date, .data$description,
                  .data$site_type, .data$result)

  # All together now! Calcification in mmol/L/yr
  calcify <- bind_rows(dlake, Mg_ratio, GWin, GWout)
  calcify <- calcify %>%
             group_by(.data$lake, .data$date, .data$description) %>%
             summarise(calcification = 12*(-.data$result[.data$site_type == "delta_lake"]*
                                     .data$result[.data$site_type == "Mg_ratio"] +
                                     .data$result[.data$site_type == "upgradient"] -
                                     .data$result[.data$site_type == "downgradient"])) %>%
             ungroup()

  calcify <- merge(calcify, lake, by = c("lake", "date", "description"))
  calcify$lake <- factor(calcify$lake, levels = desired_lakes)

  return(calcify)

}

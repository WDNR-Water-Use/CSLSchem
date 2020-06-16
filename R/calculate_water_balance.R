#' Calculate water balance
#'
#' Calculates the water balance using desired tracer, e.g., stable isotopes or
#' conservative anion/cation.
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
#' @param start_date start date of analysis. Defaults to start of WY2019
#'                   ("2018-10-01").
#' @param end_date end date of analysis. Defaults to end of WY2019
#'                   ("2019-09-30").
#' @param annual logical defaults to FALSE to calculate water balance on a
#'                monthly time step. If TRUE, calculates for entire timeseries
#'                (start_date to end_date), assumed to be one year.
#' @param no_ice logical defaults to FALSE. If TRUE, calculates water balance
#'               for ice-free months only.
#' @param ice_on date of ice on, defaults to "12-01-2018"
#' @param ice_off date of ice off, defaults to "03-31-2019"
#' @param use_kniffin logical defaults to TRUE to add in Kniffin precipitation
#'                    isotope samples for months without CSLS precipitation
#'                    isotope samples
#' @param seasonal_correction logical defaults to TRUE to incorporate seasonal
#'                            correction factors for d18O_atm based on
#'                            difference between calculated and reported
#'                            d18O_atm values in Krabbenhoft et al. (1990).
#'
#' @return fluxes
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

calculate_water_balance <- function(desired_lakes = c("Pleasant", "Long", "Plainfield"),
                                    chem_df = CSLSdata::water_chem,
                                    chem_tracer = "d18O",
                                    start_date = as_datetime(mdy("10-01-2018")),
                                    end_date = as_datetime(mdy("09-30-2019")),
                                    annual = FALSE,
                                    no_ice = FALSE,
                                    ice_on = "12-01-2018",
                                    ice_off = "03-31-2019",
                                    use_kniffin = TRUE,
                                    seasonal_correction = TRUE){
#
#   # Convert dates to datetime
#   start_date <- as_datetime(mdy(start_date))
#   end_date   <- as_datetime(mdy(end_date))

  # Change in Storage: Lake Volume ---------------------------------------------
  lake_levels <- CSLSdata::lake_levels
  dV          <- lake_levels %>%
                 filter(.data$lake %in% desired_lakes,
                        .data$date >= start_date - months(1),
                        .data$date <= end_date) %>%
                 group_by(lake = .data$lake,
                          month = floor_date(.data$date, unit = "month")) %>%
                 summarise(start_vol = .data$vol_m3[which.min(.data$date)],
                           end_vol = .data$vol_m3[which.max(.data$date)],
                           mean_area_m2 = mean(.data$area_m2),
                           mean_vol_m3 = mean(.data$vol_m3)) %>%
                 ungroup() %>%
                 mutate(dV_m3 = .data$end_vol - .data$start_vol) %>%
                 select(lake = .data$lake,
                        date = .data$month,
                        dV_m3 = .data$dV_m3,
                        mean_area_m2 = .data$mean_area_m2,
                        mean_vol_m3 = .data$mean_vol_m3)

  # Precipitation & Lake Evaporation -------------------------------------------
  # Use CSLSevap to calculate lake evaporation
  daily_weather <- CSLS_daily_met()
  weather       <- daily_weather %>%
                   filter(.data$lake %in% desired_lakes,
                          .data$date >= start_date - months(1),
                          .data$date <= end_date) %>%
                   group_by(lake = .data$lake,
                            date = floor_date(.data$date, unit = "month")) %>%
                   summarise(P_mm = sum(.data$P),
                             E_mm = sum(.data$E),
                             atmp_C = mean(c(mean(.data$atmp_min, na.rm = TRUE),
                                             mean(.data$atmp_max, na.rm = TRUE))),
                             RH_pct = mean(c(mean(.data$RH_min, na.rm = TRUE),
                                             mean(.data$RH_max, na.rm = TRUE)))) %>%
                   ungroup()

  # Groundwater Inflow ---------------------------------------------------------
  # Obtain tracer data, summarise as mean for each site type per measurement day
  # Note: don't limit dates until much farther down ~line 165
  tracer <- filter_parameter(chem_df, chem_tracer, no_bad_well = TRUE)
  tracer <- tracer  %>%
            filter(.data$lake %in% c(desired_lakes, "Precip"),
                   .data$site_type %in% c("precipitation", "lake",
                                          "upgradient", "downgradient")) %>%
            group_by(lake = .data$lake,
                     date = floor_date(.data$date, unit = "day"),
                     site_type = .data$site_type) %>%
            summarise(result = mean(.data$result, na.rm = TRUE)) %>%
            ungroup()

  # If desired for stable isotopes, fill gaps in precip data w/Kniffin data
  if (chem_tracer %in% c("d18O", "d2H") & use_kniffin) {
    tracer <- add_pcpn_isotopes(tracer, start_date, end_date)
  }

  # Interpolate values to the last day of the month
  tracer   <- interpolate_chem_values(tracer, dt = "month")
  tracer$date <- tracer$date + months(1) - days(1)


  # Map precip to each lake
  for (lake in desired_lakes) {
    Cpcpn  <- tracer %>%
              filter(.data$lake == "Precip",
                     .data$site_type == "precipitation") %>%
              mutate(lake = !!lake)
    tracer <- rbind(tracer, Cpcpn)
  }

  # If desired, ignore ice-on months
  if (no_ice) {
    ice_interval <- interval(as_datetime(mdy(ice_on)), as_datetime(mdy(ice_off)))
    tracer       <- tracer %>%
                    mutate(result = ifelse(.data$date %within% ice_interval,
                                           NA, .data$result))
  }

  # Determine concentration of tracer in evaporation (if not isotope, then zero)
  if (chem_tracer %in% c("d18O", "d2H")) {
    Cevap <- add_evap_isotopes(tracer, weather, chem_df, start_date, end_date,
                               chem_tracer, seasonal_correction)
  } else {
    Cevap <- tracer %>%
             filter(.data$site_type == "precipitation") %>%
             mutate(site_type = "evaporation",
                    result = 0)
  }
  tracer <- rbind(tracer, Cevap)

  # Rearrange tracer data frame for calculations, ensure date match (first-of-month)
  tracer <- dcast(tracer, lake + date ~ site_type , value.var = "result")
  colnames(tracer) <- c("lake", "date", "C_GWout", "C_evap",
                        "C_lake", "C_pcpn", "C_GWin")
  tracer <- tracer %>% mutate(date = floor_date(.data$date, unit = "month"))

  # Merge tracer data frame with other fluxes (P, E, and dV)
  fluxes <- merge(dV, weather, by = c("lake", "date")) %>%
            mutate(P_m3 = .data$P_mm*.data$mean_area_m2/1000,
                   E_m3 = .data$E_mm*.data$mean_area_m2/1000) %>%
            select(.data$lake, .data$date, .data$dV_m3, .data$P_m3, .data$E_m3,
                   .data$mean_area_m2, .data$mean_vol_m3)
  fluxes <- merge(fluxes, tracer, all.y = TRUE)

  # Calculate change in lake solute mass per month
  fluxes <- fluxes %>%
            filter(.data$lake != "Precip") %>%
            group_by(.data$lake) %>%
            arrange(.data$date) %>%
            mutate(M_dlake = .data$C_lake*.data$mean_vol_m3 -
                             lag(.data$C_lake*.data$mean_vol_m3))  %>%
            ungroup()

  # Limit to months of interest
  fluxes <- fluxes %>%
            filter(.data$date >= start_date,
                   .data$date <= end_date)

  # Summarise monthly inputs, if desire annual output
  # See Krabbenhoft et al., 1990 section "Calculation of Net Groundwater Inflow
  # and Outflow" in Results, just before Discussion for weighting of C_evap &
  # C_pcpn. Note Gurrieri et al. 2004 also weight C_pcpn by rainfall volume for
  # chemistry balances (see e.g. footnote "a" in Table 2)
  if (annual) {
    fluxes <- fluxes %>%
              group_by(.data$lake) %>%
              mutate(C_evap = .data$C_evap*.data$E_m3/
                              sum(.data$E_m3[!is.na(.data$C_evap)]),
                     C_pcpn = .data$C_pcpn*.data$P_m3/
                               sum(.data$P_m3[!is.na(.data$C_pcpn)])) %>%
              summarise(P_m3 = sum(.data$P_m3, na.rm = TRUE),
                        E_m3 = sum(.data$E_m3, na.rm = TRUE),
                        dV_m3 = sum(.data$dV_m3, na.rm = TRUE),
                        mean_vol_m3 = mean(.data$mean_vol_m3, na.rm = TRUE),
                        mean_area_m2 = mean(.data$mean_area_m2, na.rm = TRUE),
                        C_lake = mean(.data$C_lake, na.rm = TRUE),
                        C_GWin = mean(.data$C_GWin, na.rm = TRUE),
                        C_GWout = mean(.data$C_GWout, na.rm = TRUE),
                        C_evap = sum(.data$C_evap, na.rm = TRUE),
                        C_pcpn = sum(.data$C_pcpn, na.rm = TRUE),
                        M_dlake = sum(.data$M_dlake, na.rm = TRUE))
    fluxes$date <- interval(start_date, end_date)
  }

  # Calculate groundwater inflow (see e.g.: Gurrieri et al., 2004, eq. 3)
  fluxes$GWin_m3 <- (fluxes$P_m3*(fluxes$C_pcpn - fluxes$C_lake) +
                       fluxes$E_m3*(fluxes$C_lake - fluxes$C_evap) +
                       fluxes$dV_m3*fluxes$C_lake -
                       fluxes$M_dlake)/
                     (fluxes$C_lake - fluxes$C_GWin)

  # Groundwater Outflow --------------------------------------------------------
  fluxes$GWout_m3 <- fluxes$P_m3 + fluxes$GWin_m3 - fluxes$E_m3 - fluxes$dV_m3

  # Fluxes as percentages ------------------------------------------------------
  fluxes <- fluxes %>%
            mutate(P_pct = .data$P_m3/(.data$P_m3 + .data$GWin_m3),
                   GWin_pct = .data$GWin_m3/(.data$P_m3 + .data$GWin_m3),
                   E_pct = .data$E_m3/(.data$P_m3 + .data$GWin_m3),
                   GWout_pct = .data$GWout_m3/(.data$P_m3 + .data$GWin_m3),
                   dV_pct = .data$dV_m3/(.data$P_m3 + .data$GWin_m3))

  # If annual, calculate residence time ----------------------------------------
  if (annual) {
    fluxes$res_time <- fluxes$mean_vol_m3/(fluxes$P_m3 + fluxes$GWin_m3)
  }

  return(fluxes)

}

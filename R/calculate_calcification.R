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
                                          start_date, end_date, annual = TRUE)

  # Calculate lake residence time w.r.t. GWin and GWout
  water_fluxes <- water_fluxes %>%
                  mutate(GWin_res_time = .data$mean_vol_m3/.data$GWin_m3,
                         GWout_res_time = .data$mean_vol_m3/.data$GWout_m3) %>%
                  select(.data$lake, .data$GWin_res_time, .data$GWout_res_time)

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

    if (solute == "ALKALINITY TOTAL CACO3") {
      C_solute$result = C_solute$result/2
    }

    C_solutes <- rbind(C_solutes, C_solute)
  }

  # Convert from mg/L to uM
  C_solutes <- merge(C_solutes, CSLSchem::mgTOmeq,
                     by = "description", all.x = TRUE)
  C_solutes$result <- C_solutes$result*C_solutes$mgTOmmol*1000
  C_solutes$result_type <- "result"

  # SOLUTE RATIOS --------------------------------------------------------------
  # Add in X:Mg ratios
  Mg_ratios <- C_solutes %>%
               group_by(.data$date, .data$lake, .data$site_type) %>%
               mutate(Mg = .data$result[.data$description ==
                                          "MAGNESIUM TOTAL RECOVERABLE"]) %>%
               ungroup() %>%
               mutate(result = .data$result/.data$Mg,
                      result_type = "Mg_ratio") %>%
               select(.data$date, .data$lake, .data$site_type,
                      .data$description, .data$result_type, .data$result)
  C_solutes <- bind_rows(C_solutes, Mg_ratios) %>%
               filter(!(.data$site_type == "upgradient" &
                          .data$result_type == "result"),
                      !(.data$description == "MAGNESIUM TOTAL RECOVERABLE" &
                          .data$result_type == "Mg_ratio")) %>%
               select(.data$date, .data$lake, .data$site_type,
                      .data$description, .data$result_type, .data$result)

  # CHANGE IN SOLUTE CONCENTRATIONS --------------------------------------------
  # Add in change in lake concentrations
  dlake     <- C_solutes %>%
               filter(.data$site_type == "lake",
                      .data$result_type == "result",
                      .data$description != "MAGNESIUM TOTAL RECOVERABLE") %>%
               arrange(.data$date) %>%
               group_by(.data$lake, .data$description) %>%
               mutate(result = .data$result - lag(.data$result),
                      site_type = "delta_lake",
                      result_type = "result") %>%
              ungroup() %>%
              select(.data$lake, .data$date, .data$description,
                     .data$site_type, .data$result_type, .data$result)
  C_solutes <- bind_rows(C_solutes, dlake) %>%
               filter(.data$date >= start_date,
                      .data$date <= end_date)

  # CLASSIFY TERMS OF CALCIFICATION EQUATION -----------------------------------
  # Value types
  C_solutes$value_type <- NA
  for (i in 1:nrow(C_solutes)) {
    description <- C_solutes$description[i]
    site_type   <- C_solutes$site_type[i]
    result_type <- C_solutes$result_type[i]
    if (site_type == "delta_lake") {
      C_solutes$value_type[i] <- "X_delta_lake"
    } else if (site_type == "lake" & result_type == "result" &
               description != "MAGNESIUM TOTAL RECOVERABLE") {
      C_solutes$value_type[i] <- "X_lake"
    } else if (site_type == "lake" & result_type == "result" &
               description == "MAGNESIUM TOTAL RECOVERABLE") {
      C_solutes$value_type[i] <- "Mg_lake"
    } else if (site_type == "lake" & result_type == "Mg_ratio") {
      C_solutes$value_type[i] <- "X:Mg_lake"
    } else if (site_type == "upgradient" & result_type == "Mg_ratio") {
      C_solutes$value_type[i] <- "X:Mg_GW"
    } else if (site_type == "upgradient" & result_type == "result") {
      C_solutes$value_type[i] <- "X_GW"
    }
  }

  # Enures Mg in lake is associated with both Ca & Mg
  Mg_Ca     <- C_solutes %>%
               filter(.data$value_type == "Mg_lake") %>%
               mutate(description = "CALCIUM TOTAL RECOVERABLE")
  Mg_Alk    <- C_solutes %>%
               filter(.data$value_type == "Mg_lake") %>%
               mutate(description = "ALKALINITY TOTAL CACO3")
  C_solutes <- C_solutes %>%
               filter(.data$value_type != "Mg_lake")
  C_solutes <- bind_rows(C_solutes, Mg_Ca, Mg_Alk) %>%
               select(.data$date, .data$lake, .data$description,
                      .data$value_type, .data$result)

  # Rearrange data frame
  C_solutes <- dcast(C_solutes,
                     formula = date+lake+description~value_type,
                     value.var = "result")

  # Add in Mg_avg/Mg ratios
  C_solutes <- C_solutes %>%
               group_by(.data$lake) %>%
               mutate(Mg_avg = mean(.data$Mg_lake, na.rm = TRUE)) %>%
               ungroup() %>%
               mutate(Mg_ratio = .data$Mg_avg/.data$Mg_lake)

  # CALCIFICATION --------------------------------------------------------------
  # Merge solute concentrations and water fluxes
  calcify <- merge(C_solutes, water_fluxes, by = "lake", all.x = TRUE)

  calcify$calcification <- -12*calcify$X_delta_lake*calcify$Mg_ratio +
                           calcify$`X:Mg_GW`/calcify$GWin_res_time -
                           calcify$`X:Mg_lake`/calcify$GWout_res_time

  calcify <- calcify %>%
             mutate(X_lake = .data$X_lake) %>%
             mutate(X_lake = ifelse(.data$description == "ALKALINITY TOTAL CACO3",
                                    .data$X_lake*2, .data$X_lake))

  calcify$lake <- factor(calcify$lake, levels = desired_lakes)

  return(calcify)

}

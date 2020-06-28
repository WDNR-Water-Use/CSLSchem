#' Process chem data for calcs
#'
#' Processes concentrations of parameters (e.g., d18O, Magnesium, Calcium) for
#' use in water/solute balances. Filters CSLSdata::water_chem to specified
#' parameter, limits to lake, precipitation, and upgradient groundwater sites,
#' and interpolates to a daily timestep.
#'
#' If using with non-CSLS data, will need an alternate way to get solute/isotope
#' concentrations into the format outputted by this function.
#'
#' @param parameter description of parameter to use in analysis, as in
#'                  CSLSdata::water_chem$description (e.g., "d18O",
#'                  "MAGNESIUM TOTAL RECOVERABLE")
#' @param start_date start date of analysis period (POSIX).
#' @param end_date end date of analysis period (POSIX).
#' @param dt time step at which to summarize data. Defaults to "day" for daily
#'           time step, can also be "month" for monthly time step.
#' @param use_kniffin logical defaults to TRUE to use kniffin precip stable
#'                    isotope data (only when stable isotopes selected as the
#'                    paramter)
#'
#' @return df, a data frame with the following columns:
#'   \item{lake}{name of lake, i.e., "Pleasant", "Long", and "Plainfield"}
#'   \item{date}{date of observation (POSIX). If monthly time step, monthly
#'               means are assigned to first day of the month}
#'   \item{C_lake}{concentration in the lake (units in per mil, mg/L, or
#'                 whatever unit parameter has in CSLSdata::water_chem)}
#'   \item{C_pcpn}{concentration in precipitation (units in per mil, mg/L, or
#'                 whatever unit parameter has in CSLSdata::water_chem)}
#'   \item{C_GWin}{concentration in upgradient groundwater (units in per mil,
#'                 mg/L, or whatever unit parameter has in
#'                 CSLSdata::water_chem)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom lubridate floor_date
#' @importFrom reshape2 dcast
#'
#' @export

process_chem <- function(parameter,
                         start_date,
                         end_date,
                         dt = "day",
                         use_kniffin = TRUE){

  # Subset water_chem data frame to parameter of interest for pcpn, lake, gw_in
  # Summarize to mean value per lake/site_type/date
  water_chem <- filter_parameter(CSLSdata::water_chem,
                                 parameter,
                                 no_bad_well = TRUE,
                                 note_lake_bottom = TRUE) %>%
                filter(.data$site_type %in% c("precipitation",
                                              "lake",
                                              "upgradient")) %>%
                group_by(lake = .data$lake,
                         date = floor_date(.data$date, unit = "day"),
                         site_type = .data$site_type) %>%
                summarise(result = mean(.data$result, na.rm = TRUE)) %>%
                ungroup()

  # Precip: for stable isotopes, fill gaps in precip data w/Kniffin data
  # For all parameters, map precip values to lakes
  if (parameter %in% c("d18O", "d2H") & use_kniffin) {
    water_chem <- add_pcpn_isotopes(water_chem, start_date, end_date)
  }
  for (lake in unique(water_chem$lake)) {
    Cpcpn      <- water_chem %>%
                  filter(.data$lake == "Precip",
                         .data$site_type == "precipitation") %>%
                  mutate(lake = !!lake)
    water_chem <- rbind(water_chem, Cpcpn)
  }
  water_chem   <- filter(water_chem, lake != "Precip")

  # Interpolate to daily time step
  water_chem <- interpolate_values(water_chem,
                                   group_vars = c("lake", "site_type"),
                                   val_var = "result",
                                   start_date = start_date,
                                   end_date = end_date)

  # If monthly timestep, calculate means
  if (dt == "month") {
    water_chem <- water_chem %>%
                  group_by(lake = .data$lake,
                           date = floor_date(.data$date, unit = "month"),
                           site_type = .data$site_type) %>%
                  summarise(result = mean(.data$result, na.rm = TRUE)) %>%
                  ungroup()
  }

  # Rearrange data frame
  df <- dcast(water_chem,
              lake+date~site_type,
              value.var = "result")
  if (ncol(df) == 5){
    colnames(df) <- c("lake", "date", "C_lake", "C_pcpn", "C_GWin")
  } else if (ncol(df) == 4) {
    colnames(df) <- c("lake", "date", "C_lake", "C_GWin")
    df$C_pcpn    <- 0
    df           <- select(df, c("lake", "date", "C_lake", "C_pcpn", "C_GWin"))
  } else if (ncol(df) == 3) {
    colnames(df) <- c("lake", "date", "C_lake")
    df$C_pcpn    <- 0
    df$C_GWin    <- 0
  }

  return(df)
}

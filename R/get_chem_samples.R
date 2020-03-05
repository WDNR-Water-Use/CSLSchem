#' All Chem Samples for a Lake
#'
#' Subsets SWIMS dataset for a CSLS lake to only measurements for a parameter of
#' interest. Labels with lake, and defines measurements as lake, upgradient
#' well, or downgradient well based on the previous month of groundwater and
#' lake levels.
#'
#' @param lakes - lakes of interest, e.g., c("Pleasant", "Long", "Plainfield")
#'
#' @return chem_bal - a data frame with the date, parameter (e.g., Ca, Mg), result (concentration), flux (e.g., precip, GWin, lake), volume (m^3), and mass balance (mg/L*m^3.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter select mutate group_by summarise summarise_all recode
#' @importFrom NISTunits NISTcubMeterTOliter
#'
#' @export

get_chem_samples<- function(lakes = c("Pleasant", "Long", "Plainfield")) {
  # Get matching param names
  param_names <- CSLSchem::param_names
  param_names <- param_names %>%
                 filter(.data$NADP_param != "",
                        .data$CSLS_param != "",
                        .data$NADP_param != "ph")

  # Get precip
  NADP_pcpn <- CSLSchem::NADP_pcpn

  # Precipitation
  this_pcpn <- NADP_pcpn %>%
               filter(.data$parameter %in% param_names$NADP_param) %>%
               select(.data$start_date, .data$parameter, .data$result)
  colnames(this_pcpn) <- c("date", "parameter", "result")
  this_pcpn$flux      <- "pcpn"
  this_pcpn$site_id   <- "pcpn"

  all_chem <- NULL
  for (lake in lakes) {
    # Lake
    all_CSLS <- NULL
    for (p in 1:length(param_names$CSLS_param)) {
      this_CSLS <- filter_parameter(lake,
                                    parameter = param_names$CSLS_param[p],
                                    plotting_name = param_names$CSLS_names[p])
      this_CSLS$site_type <- recode(this_CSLS$site_type,
                                    "Upgradient" = "GWin",
                                    "Downgradient" = "GWout",
                                    "Lake Surface" = "lake")
      this_CSLS <- this_CSLS %>%
        select(.data$date, .data$station_id, .data$site_type, .data$result)
      colnames(this_CSLS) <- c("date", "site_id", "flux", "result")
      this_CSLS$parameter <- param_names$NADP_param[p]

      all_CSLS <- rbind(all_CSLS, this_CSLS)
    }
    all_CSLS <- all_CSLS %>%
      select(.data$date, .data$parameter, .data$result, .data$flux,
             .data$site_id)

    # All Together
    chem_samples      <- rbind(this_pcpn, all_CSLS)
    chem_samples$date <- floor_date(chem_samples$date, unit = "week")
    chem_samples$lake <- lake

    all_chem <- rbind(all_chem, chem_samples)
  }

  # Rearrange
  chem_samples <- dcast(all_chem,
                        date+lake+flux+site_id~parameter,
                        value.var = "result",
                        fun.aggregate = mean)

  return(chem_samples)
}

#' Filter SWIMS by parameter
#'
#' Subsets SWIMS dataset for a CSLS lake to only measurements for a parameter of
#' interest. Labels with lake, and defines measurements as lake, upgradient
#' well, or downgradient well based on the previous month of groundwater and
#' lake levels.
#'
#' @param lake - lakes of interest, e.g., c("Pleasant", "Long", "Plainfield")
#'
#' @return chem_bal - a data frame with the date, parameter (e.g., Ca, Mg), result (concentration), flux (e.g., precip, GWin, lake), volume (m^3), and mass balance (mg/L*m^3.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter select mutate group_by summarise summarise_all recode
#' @importFrom NISTunits NISTcubMeterTOliter
#' @importFrom reshape2 dcast
#'
#' @export

get_chem_bal <- function(lake) {
  # Get matching param names
  param_names <- CSLSchem::param_names
  param_names <- param_names %>%
                 filter(.data$NADP_param != "",
                        .data$CSLS_param != "",
                        .data$NADP_param != "ph")

  # Get precip
  NADP_pcpn <- CSLSchem::NADP_pcpn

  # Get water budget as volumes
  h2o_bal  <- CSLSiso::runall_csls_budget(lake, annual = TRUE)
  h2o_bal  <- h2o_bal %>%
              select(.data$date,
                     .data$P_m3,
                     .data$E_m3,
                     .data$GWin_m3,
                     .data$GWout_m3,
                     .data$mean_vol_m3,
                     .data$res_time)
  # Precipitation
  this_pcpn <- NADP_pcpn %>%
               filter(.data$dateint %within% h2o_bal$date |
                        int_overlaps(.data$dateint, h2o_bal$date),
                      .data$parameter %in% param_names$NADP_param) %>%
               group_by(.data$parameter) %>%
               summarise(result = mean(.data$result)) %>%
               select(.data$parameter, .data$result)
  colnames(this_pcpn) <- c("parameter", "result")
  this_pcpn$flux      <- "pcpn"
  this_pcpn$date      <- h2o_bal$date
  this_pcpn$vol_m3    <- h2o_bal$P_m3*h2o_bal$res_time
  this_pcpn           <- this_pcpn %>%
                         select(.data$date, .data$parameter, .data$flux,
                                .data$result, .data$vol_m3)

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
                 filter(.data$date %within% h2o_bal$date[1],
                        .data$site_type %in% c("GWin", "lake")) %>%
                 group_by(.data$site_type) %>%
                 select(.data$site_type, .data$result) %>%
                 summarise_all(median)
    colnames(this_CSLS) <- c("flux", "result")
    this_CSLS$parameter <- param_names$NADP_param[p]
    this_CSLS$date      <- h2o_bal$date[1]
    this_CSLS$vol_m3    <- NA
    this_CSLS$vol_m3[this_CSLS$flux == "lake"] <- h2o_bal$mean_vol_m3[1]
    this_CSLS$vol_m3[this_CSLS$flux == "GWin"] <- h2o_bal$GWin_m3[1]*h2o_bal$res_time[1]

    all_CSLS <- rbind(all_CSLS, this_CSLS)
  }
  all_CSLS <- all_CSLS %>%
              select(.data$date, .data$parameter, .data$flux, .data$result,
                     .data$vol_m3)

  GWout_CSLS <- all_CSLS %>%
                filter(.data$flux == "lake") %>%
                mutate(flux = "GWout",
                       vol_m3 = h2o_bal$GWout_m3*h2o_bal$res_time)

  # All Together
  chem_bal      <- rbind(this_pcpn, all_CSLS, GWout_CSLS)
  chem_bal$mass <- chem_bal$result*NISTcubMeterTOliter(chem_bal$vol_m3)

  return(chem_bal)
}

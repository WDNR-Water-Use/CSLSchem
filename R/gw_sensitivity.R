#' Filter SWIMS by parameter
#'
#' Subsets SWIMS dataset for a CSLS lake to only measurements for a parameter of
#' interest. Labels with lake, and defines measurements as lake, upgradient
#' well, or downgradient well based on the previous month of groundwater and
#' lake levels.
#'
#' @param lake - lakes of interest, e.g., c("Pleasant", "Long", "Plainfield")
#' @param GW_factor - factor to multiply current GW flow rates by
#'
#' @return chem_bal - a data frame with the date, parameter (e.g., Ca, Mg), result (concentration), flux (e.g., precip, GWin, lake), volume (m^3), and mass balance (mg/L*m^3.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter select mutate group_by summarise summarise_all recode
#' @importFrom NISTunits NISTcubMeterTOliter
#' @import cslsdata
#'
#' @export

gw_sensitivity <- function(lake, GW_factor) {
  # Get matching param names
  param_names <- H2Ochem::param_names
  param_names <- param_names %>%
                 filter(.data$NADP_param != "",
                        .data$CSLS_param != "",
                        .data$NADP_param != "ph")

  # Get water budget as volumes
  h2o_bal    <- runall_csls_budget(lake, annual = TRUE)
  P          <- h2o_bal$P_m3*h2o_bal$res_time
  E          <- h2o_bal$E_m3*h2o_bal$res_time
  GWin_init  <- h2o_bal$GWin_m3*h2o_bal$res_time
  GWin_new   <- GWin_init*GW_factor
  GWout_init <- h2o_bal$GWout_m3*h2o_bal$res_time
  GWout_new  <- GWout_init*GW_factor
  Vol_lk     <- h2o_bal$mean_vol_m3

  chem_bal <- get_chem_bal(lake)

  lake_change <- NULL
  for (i in 1:nrow(param_names)) {
    this_param <- param_names$NADP_param[i]
    C_lake     <- chem_bal$result[chem_bal$parameter == this_param &
                                    chem_bal$flux == "lake"]
    C_pcpn     <- chem_bal$result[chem_bal$parameter == this_param &
                                    chem_bal$flux == "pcpn"]
    C_GWin     <- chem_bal$result[chem_bal$parameter == this_param &
                                    chem_bal$flux == "GWin"]
    M_sed      <- P*C_pcpn + GWin*C_GWin - GWout*C_lake

    C_lake_new <- (Vol_lk*C_lake + P*C_pcpn + GWin*C_GWin - GWout*C_lake)/Vol_lk

    this_loop <- data_frame(parameter = this_param,
                            units = "mg/L",
                            lake_init = C_lake,
                            lake_new = C_lake_new)
    lake_change <- rbind(lake_change, this_loop)
  }

  return(lake_change)
}

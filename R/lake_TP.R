#' Dynamic lake model results for Calcium for CSLS lakes
#'
#' @docType data
#'
#' @usage data(lake_TP)
#'
#' @format A data frame with the following columns:
#' \describe{
#'  \item{lake}{name of lake, "Pleasant", "Long", or "Plainfield"}
#'  \item{date}{date of estimate (POSIX)}
#'  \item{day}{Julian day of measurement}
#'  \item{atmp_C}{air temperature (deg C)}
#'  \item{RH_pct}{relative humidity (percent)}
#'  \item{irr_factor}{irradiance factor (unitless, 0-1)}
#'  \item{ltmp_bot_C}{temperature at the lake bottom (deg C)}
#'  \item{ltmp_surf_C}{temperature at the lake surface (deg C)}
#'  \item{area_m2}{lake area (m2)}
#'  \item{vol_m3}{lake_volume (m3)}
#'  \item{dV_m3}{change in lake volume since previous timestep (m3)}
#'  \item{P_mm}{precipitation (mm)}
#'  \item{E_mm}{lake evaporation (mm)}
#'  \item{I_mm}{ice thickness (mm)}
#'  \item{P_m3}{precipitation (m3)}
#'  \item{E_m3}{lake evaporation (m3)}
#'  \item{I_m3}{ice thickness (m3)}
#'  \item{C_lake}{interpolated lake concentration (mg/L)}
#'  \item{dC_lake}{daily change in lake concentration (mg/L)}
#'  \item{C_pcpn}{interpolated precipitation concentration (mg/L)}
#'  \item{C_GWin}{interpolated upgradient groundwater concentration (mg/L)}
#'  \item{C_evap}{calculated evaporation concentration (mg/L)}
#'  \item{C_ice}{ice concentration (mg/L), set to zero but ignored for fluxes}
#'  \item{C_lake_calc}{lake concentration calculated w/dynamic lake model
#'                     approach (mg/L)}
#'  \item{res_time}{lake residence time used for this time step (days)}
#'  \item{GWin_m3}{inflowing groundwater volume (m3) based on lake volume and
#'                 residence time}
#'  \item{C_lake_meas}{measured lake concentration (mg/L)}
#'  \item{P_lake}{phospohrous in the lake (g/m2-d)}
#'  \item{P_sed}{phospohrous in the sediment (g/m2-d)}
#'  \item{ks}{reaction rate for sedimentation (g/m2-d)}
#'  \item{ks_tmp}{temperature adjustment for sedimentation rate}
#'  \item{kr}{reaction rate for sediment release (g/d)}
#'  \item{kr_tmp}{temperature adjustment for sediement release rate}
#'  \item{dep}{assumed wet/dry deposition (g/m2-d)}
#'  \item{fd}{fraction of TP in water column}
#'  \item{sed}{sedimentation (g/m2-d)}
#'  \item{rel}{release from sediment (g/m2-d)}
#'  \item{TP_in}{incoming phosphorus from all sources (g/m2-d)}
#'  }
#'
"lake_TP"

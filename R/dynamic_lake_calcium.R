#' Dynamic lake model - Calcium
#'
#' Calculates lake concentration of calcium using dynamic lake model approach
#'
#' @param k0 zero-order rate constant for Calcium uptake.
#' @param df data frame with lake inputs for lake of interest.
#'
#' @return df, a data frame with dynamic lake model for calcium
#'
#' @export

dynamic_lake_calcium <- function(k0, df) {

  df$C_lake_calc    <- NA
  df$C_lake_calc[1] <- df$C_lake[1]
  df$k0             <- k0
  df$Uptake         <- k0*df$irr_factor*df$area_m2

  # Calculate lake concentration
  for (i in 2:nrow(df)) {
    V    <- df$vol_m3[i]
    C_L1 <- df$C_lake_calc[i-1]
    G    <- df$GWin_m3[i]
    C_G  <- df$C_GWin[i]
    P    <- df$P_m3[i]
    C_P  <- df$C_pcpn[i]
    E    <- df$E_m3[i]
    C_E  <- df$C_evap[i]
    I    <- df$I_m3[i] - df$I_m3[i-1]
    C_I  <- df$C_ice[i]
    Up   <- df$Uptake[i]
    df$C_lake_calc[i] <- C_L1 + (1/V)*(G*(C_G - C_L1)+
                                         P*(C_P - C_L1) +
                                         E*(C_L1 - C_E) +
                                         I*(C_L1 - C_I) -
                                         Up)
  }

  return(df)
}

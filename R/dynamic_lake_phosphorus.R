#' Dynamic lake model - Phosphorus
#'
#' Calculates lake concentration of phosphorus using dynamic lake model approach
#'
#' @param par vector with initial sediment concentration, etc.
#' @param df data frame with lake inputs for lake of interest.
#' @param deposition estimated wet/dry deposition of phosphorus (kg/km2-yr)
#'
#' @return df, a data frame with dynamic lake model for phosphorus
#'
#'
#' @export

dynamic_lake_phosphorus <- function(par, df, deposition = 5.75) {
  P_sed0 <- par[1]
  ks     <- par[2]
  ks_tmp <- par[3]
  kr     <- par[4]
  kr_tmp <- par[5]

  df$C_lake_calc    <- NA
  df$C_lake_calc[1] <- df$C_lake[1]
  df$P_lake         <- NA
  df$P_lake[1]      <- df$C_lake_calc[1]*(df$vol_m3[1]/df$area_m2[1])
  df$P_sed          <- NA
  df$P_sed[1]       <- P_sed0
  df$ks             <- ks
  df$ks_tmp         <- ks_tmp
  df$kr             <- kr
  df$kr_tmp         <- kr_tmp
  df$dep            <- deposition*1000/(1000*1000*365) # kg/km^2-yr to g/m2-d
  df$fd             <- 1/(1 + sqrt(df$res_time/365))
  df$sed            <- NA
  df$rel            <- NA
  df$TP_in          <- NA
  df$sed[1] <- df$ks[1]*((1 + df$ks_tmp[1])^(df$ltmp_surf_C[1]-20))*
              (df$P_lake[1]/(df$vol_m3[1]/df$area_m2[1]))
  df$rel[1] <- df$kr[1]*((1 + df$kr_tmp[1])^(df$ltmp_bot_C[1]-20))*df$P_sed[1]

  # Calculate lake concentration
  for (i in 2:nrow(df)) {
    df$TP_in[i]  <- (-df$P_m3[i]*df$C_lake_calc[i-1] +
                       df$GWin_m3[i]*(df$C_GWin[i] - df$C_lake_calc[i-1]) +
                       df$E_m3[i]*(df$C_lake_calc[i-1] - df$C_evap[i]) +
                       (df$I_m3[i] - df$I_m3[i-1])*
                       (df$C_lake_calc[i-1] - df$C_ice[i]))*
                    (1/df$area_m2[i]) + df$dep[i]
    df$P_sed[i]  <- df$P_sed[i-1] + (1/df$res_time[i])*(1-df$fd[i])*df$TP_in[i] +
                    df$sed[i-1] - df$rel[i-1]
    df$P_lake[i] <- df$P_lake[i-1] +
                    (1/df$res_time[i])*(df$fd[i]*df$TP_in[i] - df$P_lake[i-1]) -
                    df$sed[i-1] + df$rel[i-1]
    df$sed[i] <- df$ks[i]*((1 + df$ks_tmp[i])^(df$ltmp_surf_C[i]-20))*(df$P_lake[i]/(df$vol_m3[i]/df$area_m2[i]))
    df$rel[i] <- df$kr[i]*((1 + df$kr_tmp[i])^(df$ltmp_bot_C[i]-20))*df$P_sed[i]
    df$C_lake_calc[i] <- df$P_lake[i]*(df$area_m2[i]/df$vol_m3[i])
  }

  return(df)
}

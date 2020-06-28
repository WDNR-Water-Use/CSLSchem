#' Optimize dynamic lake model
#'
#' Calculates RMSE of measured lake concentrations with those calculated with
#' the dynamic lake model (daily timestep) for use in optimization procedure.
#'
#' @param par vector with tuning parameters. k0 for Calcium. P_sed, ks, ks_tmp,
#'            kr, and kr_tmp for TP.
#' @param df data frame with lake inputs for lake of interest.
#' @param parameter description of parameter to use dynamic lake model on. Can
#'                  be either "CALCIUM TOTAL RECOVERABLE" or "PHOSPHORUS TOTAL".
#'
#' @return RMSE, root mean square error of match with lake measurements of
#'         parameter.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate select inner_join
#'
#' @export

optimize_solutes <- function(par, df, parameter) {

    if (parameter == "PHOSPHORUS TOTAL") {
    df <- dynamic_lake_phosphorus(par, df)
  } else if (parameter == "CALCIUM TOTAL RECOVERABLE") {
    df <- dynamic_lake_calcium(par, df)
  }

  # Evaluate fit
  fit  <- df %>%
          filter(!is.na(.data$C_lake_meas)) %>%
          mutate(sim = .data$C_lake_calc,
                 obs = .data$C_lake_meas,
                 SE = (.data$sim - .data$obs)^2) %>%
          select(.data$date, .data$sim, .data$obs, .data$SE)

  RMSE <- sqrt(mean(fit$SE))

  return(RMSE)
}

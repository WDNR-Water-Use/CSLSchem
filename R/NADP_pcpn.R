#' NADP precipitation data
#'
#' Precipitation concentration from nearest National Atmospheric Deposition
#' Program station, in Wisconsin Dells.
#'
#' @docType data
#'
#' @usage data(NADP_pcpn)
#'
#' @format A data frame:
#' \describe{
#'   \item{start_date}{start date of precipitation sample}
#'   \item{end_date}{end date of precipitation sample}
#'   \item{dateint}{date interval of precipitation sample}
#'   \item{parameter}{parameter measured, includes ph, Conduc (uS/cm),
#'                    Ca (mg/L), Mg (mg/L), K (mg/L), Na (mg/L), NH4 (mg/L),
#'                    NO3 (mg/L), Cl (mg/L), SO4 (mg/L), Br (mg/L), subppt (mm)}
#'   \item{result}{value of measurement}
#' }
#'
"NADP_pcpn"

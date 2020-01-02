#' Filter SWIMS by parameter
#'
#' Subsets SWIMS dataset for a CSLS lake to only measurements for a parameter of
#' interest. Labels with lake, and defines measurements as lake, upgradient
#' well, or downgradient well based on the previous month of groundwater and
#' lake levels.
#'
#' @param lakes - lakes of interest, e.g., c("Pleasant", "Long", "Plainfield")
#' @param parameter - parameter to subset by, must exactly match DNR description
#'                    (e.g., "ALUMINUM,TOTAL RECOVERABLE")
#' @param plotting_name - name of parameter to use for plotting, e.g., "Total
#'                        Recoverable Alumninum". Units will be added in based
#'                        on SWIMS field.
#' @param static_gw - defaults to FALSE to use well levels and lake levels to
#'                    determine upgradient vs. downgradient wells. If TRUE, uses
#'                    static definition from dictionary.
#'
#' @return parameter - a data frame with SWIMS data subset to only parameter of
#'                     interest for each lake, with site type (upgradient,
#'                     downgradient, or lake) and lake (Pleasant, Plainfield, or
#'                     Long), noted.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter select mutate
#' @import cslsdata
#'
#' @export

filter_parameter <- function(lakes = c("Pleasant", "Long", "Plainfield"),
                             parameter, plotting_name, static_gw = FALSE){

  SWIMS_parameter <- NULL

  for (lake in lakes) {
    # Load in data
    SWIMS       <- cslsdata::SWIMS[[lake]]

    # Subset to parameter
    lake_subset <- SWIMS %>%
                   filter(.data$description == !!parameter) %>%
                   mutate(date = .data$start_date_time,
                          result = as.numeric(.data$result),
                          lake = !!lake,
                          name = !!plotting_name) %>%
                   select(.data$date,
                          .data$lake,
                          .data$dnr_parameter,
                          .data$description,
                          .data$name,
                          .data$result,
                          .data$units,
                          .data$header_labslip_depth,
                          .data$station_id,
                          .data$wbic,
                          .data$station_name,
                          .data$lod,
                          .data$loq,
                          .data$flag,
                          .data$flag_reason)

    # Define upgradient/downgradient wells vs. lake
    lake_subset <- define_lake_wells(lake, lake_subset, static_gw)

    # Combine w/larger data frame
    SWIMS_parameter <- rbind(SWIMS_parameter, lake_subset)
  }
  # Fix factor levels
  SWIMS_parameter$site_type <- factor(SWIMS_parameter$site_type,
                                      levels = c("inconsistent",
                                                 "upgradient",
                                                 "downgradient",
                                                 "typically_downgradient",
                                                 "lake_surface",
                                                 "lake_bottom",
                                                 "invalid"),
                                      labels = c("Inconsistent",
                                                 "Upgradient",
                                                 "Downgradient",
                                                 "Typically Downgradient",
                                                 "Lake Surface",
                                                 "Lake Bottom",
                                                 "Invalid"))
  SWIMS_parameter$lake      <- factor(SWIMS_parameter$lake,
                                      levels = c("Pleasant",
                                                 "Long",
                                                 "Plainfield"))
  return(SWIMS_parameter)
}

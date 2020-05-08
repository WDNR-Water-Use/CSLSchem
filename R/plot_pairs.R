#' Pairwise plot
#'
#' Compare two water chemistry parameters for precipitation, lake, and
#' groundwaters.
#'
#'
#' @param water_chem data frame with water chemistry information to use
#' @param parameters vector with dnr description of two parameters to compare
#' @param no_site_id vector with site_ids to remove from visualization, defaults to PFL-09
#' @param color_breaks vector with site_types to include on plot
#' @param color_labels vector with names of site_types to use on legend
#' @param color_values hex codes of colors to use for site_types
#' @param text_size size of text on plots
#'
#' @return plot_obj, the plot object created by ggplot.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom stats lm
#'
#' @export

plot_pairs <- function(water_chem,
                       parameters,
                       no_site_id = "PFL-09",
                       color_breaks = c("precipitation",
                                        "upgradient",
                                        "nogradient",
                                        "downgradient",
                                        "lake"),
                       color_labels = c("Precipitation",
                                        "Groundwater Inflow",
                                        "Groundwater No Grad.",
                                        "Groundwater Outflow",
                                        "Lake"),
                       color_values = c("#1F78B4",
                                        "#33A02C",
                                        "#B15928",
                                        "#B2DF8A",
                                        "#E31A1C"),
                       text_size = 12){

  # Filter water_chem to parameters of interest
  ions <- filter_parameter(water_chem, parameters)
  ions <- ions %>% filter(.data$site_type != "deep")
  if (length(no_site_id) > 0) {
    ions <- ions %>% filter(.data$site_id != no_site_id)
  }

  # Convert from mg/L to meq/L
  mgTOmeq     <- CSLSchem::mgTOmeq
  ions        <- merge(ions, mgTOmeq, all.x = TRUE)
  ions$result <- ions$result*ions$mgTOmeq

  # Merge precip w/Lakes
  ions_pcpn <- ions %>% filter(.data$site_type == "precipitation")
  for (lake in unique(ions$lake)) {
    add_pcpn      <- ions_pcpn
    add_pcpn$lake <- lake
    ions <- rbind(ions, add_pcpn)
  }
  ions <- ions %>% filter(.data$lake != "Precip")

  # Pair limited data frame
  ions <- ions %>% select(.data$lake,
                          .data$description,
                          .data$date,
                          .data$result,
                          .data$site_type)
  paired <- merge(ions, ions, by = c("lake", "date", "site_type"))
  paired <- paired %>% filter(.data$description.x == parameters[1],
                              .data$description.y == parameters[2])

  # Plot
  plot_obj <- ggplot(paired,
                     aes(x = .data$result.x,
                         y = .data$result.y)) +
              geom_point(aes(color = .data$site_type),
                         size = 1.5) +
              geom_smooth(method = lm,
                          se = FALSE,
                          formula = y ~ 0 + x,
                          color = "grey40") +
              facet_wrap(~lake, scales = "free") +
              labs(x = sprintf("%s (meq/L)", parameters[1]),
                   y = sprintf("%s (meq/L)", parameters[2])) +
              scale_color_manual(name = "",
                                 breaks = color_breaks,
                                 labels = color_labels,
                                 values = color_values) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")

  return(plot_obj)
}

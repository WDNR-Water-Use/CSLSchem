#' Plot comparison of lake water chemistry
#'
#' Plots a comparison of lake water chemistry for the CSLS study lakes with
#' values for all of Wisconsin, Central Wisconsin, and Wisconsin seepage lakes.
#'
#'
#' @param description dnr description of parameter to plot
#' @param x_title x-axis title to use on plot, defaults to description.
#' @param text_size size of text on plots, defaults to 12pt.
#' @param no_seepage logical defaults to TRUE to filter out all WI seepage lakes
#'                   from comparison.
#'
#' @return plot_obj, the plot object created by ggplot.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise filter select
#' @importFrom stats median sd
#' @importFrom NISTunits NISTmeterTOft
#'
#' @export

plot_comparison <- function(description,
                            x_title = description,
                            text_size = 12,
                            no_seepage = TRUE){

  # Comparison stats
  lake_stats <- CSLSchem::lillie_mason_lakes
  lake_stats <- lake_stats %>%
                filter(.data$description == !! description) %>%
                mutate_at(c("location", "description", "units"), as.character)
  if (no_seepage) {
    lake_stats <- lake_stats %>%
                  filter(.data$location != "Seepage Lakes")
  }

  # Adjust units
  if (description == "SECCHI DEPTH - FEET") {
    lake_stats <- lake_stats %>%
                  mutate_at(c("mean", "std", "median", "min", "max"),
                            NISTmeterTOft) %>%
                  mutate(units = "feet")
  } else if (description == "TURBIDITY, LAB NEPHELOMETRIC NTU") {
    lake_stats$units <- "NTU" #Assume JTUs and NTUs are roughly equal
  }

  # CSLS lakes observations and stats
  df       <- filter_parameter(CSLSdata::water_chem,
                               description,
                               note_lake_bottom = TRUE,
                               no_bad_sample = TRUE,
                               no_bad_well = TRUE) %>%
              filter(.data$site_type == "lake") %>%
              mutate_at(c("lake", "units"), as.character) %>%
              select(.data$lake, .data$result, .data$units)
  df_stats <- df %>%
              group_by(location = .data$lake,
                       units = .data$units) %>%
              summarise(mean = mean(.data$result, na.rm = TRUE),
                        std = sd(.data$result, na.rm = TRUE),
                        median = median(.data$result, na.rm = TRUE)) %>%
              ungroup()

  if (description == "PH LAB") {
    x_limits <- c(min(c(lake_stats$mean-lake_stats$std, df$result)),
                  max(c(lake_stats$mean+lake_stats$std, df$result)))
  } else {
    x_limits <- c(0, max(c(lake_stats$mean+lake_stats$std, df$result)))
  }

  # Quality control: check that units match
  lake_units <- tolower(unique(lake_stats$units))
  csls_units <- tolower(unique(df$units))
  if (lake_units != csls_units) {
    stop(sprintf("Lillie & Mason units for %s are not the same as CSLS units",
                 description))
  }

  # Combine stats
  lake_stats  <- bind_rows(lake_stats, df_stats)
  if (no_seepage) {
    lake_stats$location <- factor(lake_stats$location,
                                  levels = c("All WI", "Central WI", "Pleasant",
                                             "Long", "Plainfield"))
  } else {
    lake_stats$location <- factor(lake_stats$location,
                                  levels = c("All WI", "Central WI",
                                             "Seepage Lakes", "Pleasant",
                                             "Long", "Plainfield"))
  }


  # Plot
  plot_obj <- ggplot() +
              geom_jitter(data = df,
                          aes(x = .data$result,
                              y = .data$lake,
                              color = "obs",
                              shape = "obs",
                              linetype = "obs"),
                          width = 0,
                          height = 0.2,
                          size = 1.5) +
              geom_pointrange(data = lake_stats,
                              aes(x = .data$mean,
                                  y = .data$location,
                                  xmin = .data$mean-.data$std,
                                  xmax = .data$mean+.data$std,
                                  color = "mean +/- sd",
                                  shape = "mean +/- sd",
                                  linetype = "mean +/- sd")) +
              # geom_point(data = lake_stats,
              #            aes(x = .data$min,
              #                y = .data$location,
              #                color = "min/max",
              #                shape = "min/max",
              #                linetype = "min/max"),
              #            size = 2.5,
              #            fill = "grey100") +
              # geom_point(data = lake_stats,
              #            aes(x = .data$max,
              #                y = .data$location,
              #                color = "min/max",
              #                shape = "min/max",
              #                linetype = "min/max"),
              #            size = 2.5,
              #            fill = "grey100") +
              labs(x = x_title, y = "") +
              scale_y_discrete(limits = rev(levels(lake_stats$location))) +
              scale_x_continuous(limits = x_limits,
                                 oob = scales::squish) +
              scale_color_manual(name = "",
                                 breaks = c("min/max", "mean +/- sd", "obs"),
                                 values = c("black", "black", "grey70")) +
              scale_shape_manual(name = "",
                                 breaks = c("min/max", "mean +/- sd", "obs"),
                                 values = c(1, 16, 16))  +
              scale_linetype_manual(name = "",
                                    breaks = c("min/max", "mean +/- sd", "obs"),
                                    values = c(0, 1, 0)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size))

  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Parameter plot
#'
#' Plots Wells vs. Lake measurements for a parameter of interest at all lakes.
#'
#'
#' @param df subsetted version of SWIMS data for parameter of interest.
#' @param yaxis_type defaults to "number" to use continuous y axis. Can also be
#'                   "log" for semi-log plot.
#' @param text_size size of text on plots, defaults to 12pt.
#'
#' @return plot_obj, the plot object created by ggplot.
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export

plot_timeseries_by_year <- function(df,
                            yaxis_type = "number",
                            text_size = 12){
  ylabel   <- sprintf("%s (%s)",
                      unique(df$name),
                      str_to_lower(unique(df$units)))
  df$year  <- as.factor(year(df$date))
  df$date  <- yday(df$date)
  df       <- df %>% arrange(.data$date)
  path_df  <- df %>%
              filter(.data$site_type %in% c("lake", "upgradient", "downgradient"))

  plot_obj <- ggplot(df,
                     aes(x = as.Date(.data$date, origin = as.Date("2018-01-01")),
                         y = .data$result,
                         group = .data$year,
                         color = .data$year,
                         fill = .data$year,
                         shape = .data$year)) +
              geom_hline(aes(yintercept = min(.data$loq, na.rm = TRUE),
                             linetype = "LOQ"),
                         color = "grey40",
                         size = 1) +
              geom_hline(aes(yintercept = min(.data$lod, na.rm = TRUE),
                             linetype = "LOD"),
                         color = "grey40",
                         size = 1) +
              geom_path(data = path_df) +
              geom_point(size = 2.5) +
              facet_grid(~lake) +
              labs(x = "", y = ylabel, color = "Site") +
              scale_x_date(date_breaks = "2 months",
                           date_labels = "%b") +
              scale_linetype_manual(name = "",
                                    breaks = c("LOQ", "LOD"),
                                    values = c("longdash", "dotted")) +
              scale_color_brewer(name = "Year",
                                 palette = "Paired") +
              scale_fill_brewer(name = "Year",
                                palette = "Paired") +
              scale_shape_discrete(name = "Year") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    legend.position = "top")
  if (yaxis_type == "log") {
    plot_obj <- plot_obj + scale_y_log10()
  }
  return(plot_obj)
}

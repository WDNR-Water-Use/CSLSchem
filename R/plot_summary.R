#' Parameter plot
#'
#' Plots Wells vs. Lake measurements for a parameter of interest at all lakes.
#'
#'
#' @param df subsetted version of SWIMS data for parameter of interest.
#' @param yaxis_type defaults to "number" to use continuous y axis. Can also be
#'                   "log" for semi-log plot.
#' @param text_size size of text on plots, defaults to 12pt.
#' @param site_types possible site types to plot
#' @param site_labels how to display names of site types in legends
#' @param site_colors colors to use for site types
#'
#' @return plot_obj, the plot object created by ggplot.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_lower
#'
#' @export

plot_summary<- function(df,
                        yaxis_type = "number",
                        text_size = 12,
                        site_types = c("upgradient",
                                       "nogradient",
                                       "downgradient",
                                       "lake", "lake_bottom"),
                        site_labels = c("Upgr.", "Nogr.", "Downgr.",
                                        "Lake Surf", "Lake Bot."),
                        site_colors = c("#33A02C", "#B15928", "#B2DF8A",
                                        "#FB9A99", "#E31A1C")){

  df$site_type <- factor(df$site_type, levels = site_types)
  ylabel       <- sprintf("%s (%s)",
                          unique(df$name),
                          str_to_lower(unique(df$units)))

  plot_obj <- ggplot(df,
                     aes(x = .data$lake,
                         y = .data$result,
                         group = .data$site_type,
                         color = .data$site_type,
                         fill = .data$site_type)) +
              geom_hline(aes(yintercept = min(.data$loq, na.rm = TRUE),
                             linetype = "LOQ"),
                         color = "grey40",
                         size = 1) +
              geom_hline(aes(yintercept = min(.data$lod, na.rm = TRUE),
                             linetype = "LOD"),
                         color = "grey40",
                         size = 1) +
              geom_point(position = position_jitterdodge(jitter.width = 0.1,
                                                         dodge.width = 0.4),
                         size = 2.5,
                         color = "grey70") +
              stat_summary(fun.y = mean,
                           fun.ymin = min,
                           fun.ymax = max,
                           shape = "\U25AC",
                           size = 1,
                           position = position_dodge(width = 0.4)) +
              labs(x = "", y = ylabel, color = "Site", fill = "Site") +
              scale_linetype_manual(name = "",
                                    breaks = c("LOQ", "LOD"),
                                    values = c("longdash", "dotted")) +
              scale_color_manual(name = "Sites",
                                 breaks = site_types,
                                 limits = site_types,
                                 labels = site_labels,
                                 values = site_colors) +
              guides(color = guide_legend(order = 1),
                     fill = FALSE,
                     linetype = guide_legend(order = 2)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")

  if (yaxis_type == "log") {
    plot_obj <- plot_obj + scale_y_log10()
  }

  return(plot_obj)
}

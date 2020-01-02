#' Parameter plot
#'
#' Plots Wells vs. Lake measurements for a parameter of interest at all lakes.
#'
#'
#' @param SWIMS_parameter subsetted version of SWIMS data for parameter of interest.
#' @param text_size size of text on plots, defaults to 12pt.
#'
#' @return plot_obj, the plot object created by ggplot.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom forcats fct_relevel
#' @importFrom rlang .data
#'
#' @export

plot_bulk <- function(SWIMS_parameter, text_size = 12){
  ylabel   <- sprintf("%s (%s)",
                      unique(SWIMS_parameter$name),
                      unique(SWIMS_parameter$units))
  plot_obj <- ggplot(SWIMS_parameter,
                     aes(x = .data$lake,
                         y = .data$result,
                         group = .data$site_type,
                         color = .data$site_type,
                         fill = .data$site_type)) +
              geom_hline(aes(yintercept = min(.data$loq),
                             linetype = "Limit of Quality"),
                         color = "grey40",
                         size = 1) +
              geom_hline(aes(yintercept = min(.data$lod),
                             linetype = "Limit of Detection"),
                         color = "grey40",
                         size = 1) +
              geom_point(position = position_jitterdodge(jitter.width = 0.1,
                                                         dodge.width = 0.4),
                         size = 3,
                         color = "grey70") +
              stat_summary(fun.y = mean,
                           fun.ymin = min,
                           fun.ymax = max,
                           shape = "\U25AC",
                           size = 1,
                           position = position_dodge(width = 0.4)) +
              labs(x = "", y = ylabel, color = "Site", fill = "Site") +
              # scale_fill_manual(name = "Flagged",
              #                    values = c("grey70", "indianred1")) +
              scale_linetype_manual(name = "",
                                    breaks = c("Limit of Quality",
                                               "Limit of Detection"),
                                    values = c("longdash", "dotted")) +
              # scale_color_brewer(palette = "Dark2") +
                scale_color_manual(name = "Sites",
                                   breaks = c("Upgradient",
                                              "Downgradient",
                                              "Lake Surface",
                                              "Lake Bottom"),
                                   limits = c("Upgradient",
                                              "Downgradient",
                                              "Lake Surface",
                                              "Lake Bottom"),
                                   labels = c("Upgr.",
                                              "Downgr.",
                                              "Lake Surf.",
                                              "Lake Bot."),
                                   values = c("#FF7F00",
                                              "#33A02C",
                                              "#1F78B4",
                                              "#A6CEE3")) +
              guides(color = guide_legend(order = 1),
                     fill = FALSE,
                     linetype = guide_legend(order = 2)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Parameter plot
#'
#' Plots Wells vs. Lake measurements for a parameter of interest at all lakes.
#'
#'
#' @param SWIMS_parameter subsetted version of SWIMS data for parameter of interest.
#' @param text_size size of text on plots, defaults to 12pt.
#'
#' @return plot_obj, the plot object created by ggplot.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom forcats fct_relevel
#' @importFrom rlang .data
#'
#' @export

plot_timeseries <- function(SWIMS_parameter, text_size = 12){
  ylabel   <- sprintf("%s (%s)",
                      unique(SWIMS_parameter$name),
                      unique(SWIMS_parameter$units))
  plot_obj <- ggplot(SWIMS_parameter,
                     aes(x = floor_date(.data$date, unit = "day"),
                         y = .data$result,
                         group = .data$site_type,
                         color = .data$site_type)) +
              geom_hline(aes(yintercept = min(.data$loq),
                             linetype = "LOQ"),
                         color = "grey40",
                         size = 1) +
              geom_hline(aes(yintercept = min(.data$lod),
                             linetype = "LOD"),
                         color = "grey40",
                         size = 1) +
              geom_point(size = 3) +
              geom_smooth(data = subset(SWIMS_parameter,
                                        site_type == "Lake Surface"),
                          method = "loess",
                          se = FALSE) +
              facet_grid(~lake) +
              labs(x = "", y = ylabel, color = "Site") +
              scale_x_datetime(breaks = "4 months",
                               date_labels = "%b %y") +
              scale_y_continuous(limits = c(0,
                                            max(SWIMS_parameter$result,
                                                na.rm = TRUE))) +
              scale_linetype_manual(name = "",
                                    breaks = c("LOQ",
                                               "LOD"),
                                    values = c("longdash", "dotted")) +
              # scale_color_brewer(palette = "Dark2") +
    scale_color_manual(name = "Sites",
                       breaks = c("Upgradient",
                                  "Downgradient",
                                  "Lake Surface",
                                  "Lake Bottom"),
                       limits = c("Upgradient",
                                  "Downgradient",
                                  "Lake Surface",
                                  "Lake Bottom"),
                       labels = c("Upgr.",
                                  "Downgr.",
                                  "Lake Surf.",
                                  "Lake Bot."),
                       values = c("#FF7F00",
                                  "#33A02C",
                                  "#1F78B4",
                                  "#A6CEE3")) +
              guides(color = guide_legend(order = 1),
                     fill = guide_legend(order = 2),
                     linetype = guide_legend(order = 3)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")
  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Plot Levels
#'
#' This function creates a plot object to compare the difference in lake level
#' and well level measurements for all sites at a given lake, paired with when
#' isotope measurements were collected and what the d18O results were.
#'
#' @param water_level_diff a data frame with the date, site_no, obs_type,
#'                     level_m (of gw levels), and diff_m between daily gw level
#'                     observations and lake level observations.
#' @param SWIMS_gw a data frame with the date and site_id of all SWIMS gw measurmements.
#'
#' @return plot_obj - a plot object with water level difference and dates of
#'                    isotope measurements for each lake site.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#'
#' @export
plot_levels <- function(water_level_diff, SWIMS_gw){
  plot_obj <- ggplot() +
              geom_hline(yintercept = 0,
                         linetype = "dashed") +
              geom_line(data = water_level_diff,
                        aes(x = .data$date,
                            y = .data$diff_m)) +
              geom_rect(data = SWIMS_gw,
                         aes(xmin = .data$start_date_time - days(1),
                             xmax = .data$start_date_time + days(1)),
                        ymin = min(water_level_diff$diff_m, na.rm = TRUE),
                        ymax = max(water_level_diff$diff_m, na.rm = TRUE),
                        fill = "red",
                        alpha = 0.8) +
              facet_wrap(~site_id, ncol = 3) +
              scale_x_datetime(breaks = "4 months",
                               date_labels = "%b %y") +
              labs(x = "",
                   y = "GW Level (m) - Lake Level (m)") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Plot Chem Mass Balance
#'
#' This function creates a plot object to show the chemistry mass balance of
#' multiple parameters
#'
#' @param chem_bal a data frame with the chemistry mass balance for multiple
#'                 parameters
#' @param lake lake of interest (i.e., "Pleasant", "Long", or "Plainfield")
#' @param as_pcnt logical defaults to FALSE to plot as masses, if true plots as
#'                 percent of incoming mass.
#' @param text_size defaults to 12 point text size.
#'
#' @return plot_obj - a plot object with the chem mass balance
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate group_by
#'
#' @export

plot_inputs <- function(chem_bal, lake, as_pcnt = TRUE, text_size = 12) {
  chem_bal$in_or_out <- ""
  for (i in 1:nrow(chem_bal)) {
    if (chem_bal$flux[i] == "pcpn" |
        chem_bal$flux[i] == "GWin") {
      chem_bal$in_or_out[i] <- "In"
    } else {
      chem_bal$in_or_out[i] <- "Out"
    }
  }

  if (as_pcnt) {
    chem_bal <- chem_bal %>%
                group_by(.data$parameter) %>%
                mutate(mass = 100*.data$mass/
                         sum(.data$mass[.data$in_or_out == "In"]))
  }

  chem_bal <- chem_bal %>% filter(.data$in_or_out == "In")

  plot_obj <- ggplot(data = chem_bal) +
              geom_col(aes(x = .data$parameter, y = .data$mass,
                           fill = .data$flux)) +
              scale_fill_manual(name = "",
                                 breaks = c("GWin", "pcpn"),
                                 labels = c("Groundwater", "Precipitation"),
                                 values = c("#33A02C", "#1F78B4")) +
              scale_y_continuous(expand = c(0,0)) +
              labs(x = "", y = "Incoming Solute (%)", fill = "",
                   title = "") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                              size = text_size),
                    axis.text.x = element_text(family = "Segoe UI Semibold"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")

  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Plot Chem Mass Balance
#'
#' This function creates a plot object to show the chemistry mass balance of
#' multiple parameters
#'
#' @param chem_bal a data frame with the chemistry mass balance for multiple
#'                 parameters
#' @param lake lake of interest (i.e., "Pleasant", "Long", or "Plainfield")
#' @param as_pcnt logical defaults to FALSE to plot as masses, if true plots as
#'                 percent of incoming mass.
#' @param text_size defaults to 12 point text size.
#'
#' @return plot_obj - a plot object with the chem mass balance
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate group_by
#'
#' @export

plot_chem_bal <- function(chem_bal, lake, as_pcnt = TRUE, text_size = 12) {
  chem_bal$in_or_out <- ""
  for (i in 1:nrow(chem_bal)) {
    if (chem_bal$flux[i] == "pcpn" |
        chem_bal$flux[i] == "GWin") {
      chem_bal$in_or_out[i] <- "In"
    } else {
      chem_bal$in_or_out[i] <- "Out"
    }
  }

  if (as_pcnt) {
    chem_bal <- chem_bal %>%
      group_by(.data$parameter) %>%
      mutate(mass = 100*.data$mass/
               sum(.data$mass[.data$in_or_out == "In"]))
  }

  plot_obj <- ggplot(data = chem_bal) +
    geom_col(aes(x = .data$in_or_out, y = .data$mass,
                 fill = .data$flux)) +
    scale_fill_manual(name = "",
                      breaks = c("pcpn", "GWin", "GWout", "lake" ),
                      labels = c("Precipitation", "Groundwater Inflow",
                                 "Groundwater Outflow", "Lake"),
                      values = c("#B2DF8A", "#33A02C", "#FB9A99", "#1F78B4")) +
    facet_wrap(~parameter) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "% of Incoming Mass", fill = "Flux",
         title = sprintf("%s Lake", lake)) +
    theme_bw() +
    theme(text = element_text(family = "Segoe UI Semilight",
                              size = text_size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Plot Conservative
#'
#'
#' @param chem_samples a data frame with the chemistry results for all fluxes
#'                     for multiple parameters
#' @param lake lake of interest (i.e., "Pleasant", "Long", or "Plainfield")
#' @param text_size defaults to 12 point text size.
#'
#' @return plot_obj - a plot object with the chem mass balance
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate group_by
#'
#' @export

plot_conservative <- function(chem_samples, lake, text_size = 12) {
  #Parameter names
  param_names <- H2Ochem::param_names
  param_names <- param_names %>%
                 filter(.data$NADP_param != "",
                        .data$CSLS_param != "",
                        .data$NADP_param != "ph")
  k        <- 1
  plot_obj <- NULL
  for (i in 1:nrow(param_names)) {
    for (j in 1:nrow(param_names)) {
      xparam  <- param_names$NADP_param[i]
      yparam  <- param_names$NADP_param[j]
      compare <- chem_samples[,c(xparam, yparam, "flux", "lake")]
      colnames(compare) <- c("x", "y", "flux", "lake")
      compare <- filter(compare, flux != "Lake Bottom")

      plot_obj[k] <- print(ggplot(compare, aes(x = x, y = y)) +
                     facet_wrap(~lake, scales = "free") +
                     geom_point(aes(color = flux),
                                size = 2) +
                     stat_smooth(method = "lm", se = FALSE, color = "grey40") +
                     labs(x = xparam, y = yparam, color = "") +
                     scale_color_manual(breaks = c("pcpn", "GWin",
                                                   "GWout", "lake" ),
                                        labels = c("Precipitation",
                                                   "Groundwater Inflow",
                                                   "Groundwater Outflow",
                                                   "Lake"),
                                        values = c("#B2DF8A", "#33A02C",
                                                   "#FB9A99", "#1F78B4")) +
                     theme_bw() +
                     theme(text = element_text(family = "Segoe UI Semilight",
                                               size = text_size),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()))
      k <- k + 1

    }
  }
  plot_grid(plot_list = plot_obj, ncol = nrow(param_names), common.legend = TRUE)

  return(plot_obj)
}


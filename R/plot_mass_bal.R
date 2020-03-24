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

plot_mass_bal <- function(chem_bal, lake, as_pcnt = TRUE, text_size = 12) {
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
                                breaks = c("pcpn",
                                           "GWin",
                                           "GWout",
                                           "lake"),
                                labels = c("Precipitation",
                                           "GW Inflow",
                                           "GW Outflow",
                                           expression(paste(Delta," Lake Volume"))),
                                values = c("#1F78B4", "#33A02C", "#B2DF8A", "#FB9A99")) +
              facet_wrap(~parameter) +
              scale_y_continuous(expand = c(0,0)) +
              labs(x = "", y = "% of Incoming Mass", fill = "Flux",
                   title = sprintf("%s Lake", lake)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    legend.position = "top",
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

  return(plot_obj)
}

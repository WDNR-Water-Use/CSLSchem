#' Plot Chem Mass Balance
#'
#' This function creates a plot object to show the chemistry mass balance of
#' multiple parameters
#'
#' @param chem_bal a data frame with the chemistry mass balance for multiple
#'                 parameters
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

plot_mass_in <- function(chem_bal, as_pcnt = TRUE, text_size = 12) {
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
              geom_col(aes(x = .data$parameter,
                           y = .data$mass,
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

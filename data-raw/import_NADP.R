# Load NADP Precipitation Data


# Setup environment ------------------------------------------------------------
library(lubridate)
library(reshape2)
library(usethis)
library(dplyr)

datafile       <- "data-raw/NADP_WI_2019.10.21.csv"
dictionaryfile <- "data-raw/NADP_WI_dictionary.csv"
keep_site      <- "Devils Lake"

# Process data -----------------------------------------------------------------

NADP_dict <- read.csv(dictionaryfile)
keep_id   <- NADP_dict %>%
             filter(.data$SITE.NAME == keep_site) %>%
             select(.data$SITE.ID) %>%
             unlist() %>%
             as.character()

NADP_pcpn <- read.csv(datafile)
NADP_pcpn <- NADP_pcpn %>%
             filter(.data$siteID == keep_id,
                    as.character(.data$valcode) %in% c("w ", "wa", "wi")) %>%
             mutate(start_date = mdy_hm(.data$dateon),
                    end_date = mdy_hm(.data$dateoff)) %>%
             select(.data$start_date,
                    .data$end_date,
                    .data$ph,
                    .data$Conduc,
                    .data$Ca,
                    .data$Mg,
                    .data$K,
                    .data$Na,
                    .data$NH4,
                    .data$NO3,
                    .data$Cl,
                    .data$SO4,
                    .data$Br,
                    .data$subppt)
NADP_pcpn <- melt(NADP_pcpn, id.vars = c("start_date", "end_date"))

colnames(NADP_pcpn) <- c("start_date", "end_date", "parameter", "result")

NADP_pcpn$dateint = interval(NADP_pcpn$start_date, NADP_pcpn$end_date)

use_data(NADP_pcpn, overwrite = TRUE, compress = "xz")

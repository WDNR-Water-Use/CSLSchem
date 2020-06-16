# Load table with estimated hydraulic conductivity and distance to lake
# Data from Dave Hart, WGNHS

# Hydraulic conductivity in feet per day as estimated using TGUESS. Specific
# capacity data was obtained from step drawdown and step injection testing

# Distance of lake from the well in ft derived from ArcGIS. Only included for
# those wells around the lake and for those wells with hydraulic conductivity
# data

# Carolyn renamed site_ids from raw data file from Dave Hart to include dash and
# match site_ids in other data frames


library(usethis)

K_dx <- read.csv("data-raw/well_K_and_dist.csv")
colnames(K_dx) <- c("site_id", "K_ft_d", "dx_ft")

usethis::use_data(K_dx, overwrite = TRUE, compress = "xz")

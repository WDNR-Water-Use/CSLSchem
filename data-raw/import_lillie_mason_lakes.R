# Load table with mg/L to meq/L conversions

library(usethis)

lillie_mason_lakes <- read.csv("data-raw/lillie_mason_lakes.csv")

usethis::use_data(lillie_mason_lakes, overwrite = TRUE, compress = "xz")

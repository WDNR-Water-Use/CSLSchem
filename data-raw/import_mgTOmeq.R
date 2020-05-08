# Load table with mg/L to meq/L conversions

library(usethis)

mgTOmeq <- read.csv("data-raw/mgTOmeq.csv")

usethis::use_data(mgTOmeq, overwrite = TRUE, compress = "xz")

# Classify SWIMS results by site type

library(CSLSchem)
lakes <- c("Pleasant", "Long", "Plainfield")
SWIMS <- classify_SWIMS_results(lakes)

usethis::use_data(SWIMS, overwrite = TRUE, compress = "xz")

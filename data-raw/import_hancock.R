# Load limited Hancock data dating back to 2010 for use in dynamic lake model

library(usethis)

hancock   <- read.csv("data-raw/Hancock_2010_2020.csv") %>%
  select(date = .data$DATE,
         atmp_min = .data$TMIN,
         atmp_max = .data$TMAX,
         pcpn = .data$PPT_Inches)

usethis::use_data(hancock, overwrite = TRUE, compress = "xz")

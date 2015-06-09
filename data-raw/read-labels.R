varnames <- read.csv("./data-raw/labels.csv", sep = ";", header = T)
# devtools::use_data_raw("instevalR")
devtools::use_data(varnames, internal = TRUE, overwrite = TRUE)

devtools::use_package("plyr")
devtools::use_package("reshape2")
devtools::use_package("scales")
devtools::use_package("Hmisc")


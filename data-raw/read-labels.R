varnames <- read.csv("./data-raw/labels.csv", sep = ";", header = T)
# devtools::use_data_raw("instevalR")
devtools::use_data(varnames, internal = TRUE, overwrite = TRUE)

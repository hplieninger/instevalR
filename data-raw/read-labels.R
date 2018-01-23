varnames <- read.csv("./data-raw/labels.csv", sep = ";", header = T)
# varnames <- read.csv("../data-raw/labels.csv", sep = ";", header = T)
varnames <- varnames[order(varnames$domain, varnames$ordering), ]
varnames$thema <- factor(varnames$thema, levels = unique(as.character(varnames$thema)))
varnames$topic_full <- factor(varnames$topic_full, levels = unique(as.character(varnames$topic_full)))
varnames$topic_de <- factor(varnames$topic_de, levels = unique(as.character(varnames$topic_de)))
varnames$topic_en <- factor(varnames$topic_en, levels = unique(as.character(varnames$topic_en)))
varnames$domain <- factor(varnames$domain)

# devtools::use_data_raw("instevalR")
devtools::use_data(varnames, internal = TRUE, overwrite = TRUE)

# devtools::use_data_raw()

devtools::use_package("plyr")
devtools::use_package("reshape2")
devtools::use_package("scales")
devtools::use_package("Hmisc")





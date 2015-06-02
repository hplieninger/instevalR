library("plyr")
read_eval <- function(directory, id, return.raw = FALSE) {

    files <- list.files(directory, full.names = F)
    files <- list.files("C:/Users/plieninger/Documents/Teaching/Evaluation/Daten/", full.names = F)
    files <- substr(files, start = 23, stop = 100)
    order.1 <- order(nchar(files))

    file.names <- gregexpr("-", files[order.1])
    file.names <- substr(files[order.1], start = 1, stop = as.numeric(file.names)-1)
    files <- list.files(directory, full.names = T)[order.1]

    if (missing(id)) id <- 1:length(files)

    files <- files[id]

    dat <- list(id = id, nobs = NA)
    dat <- vector("list", length(id))

    for (ii in 1:length(files)) {
        dat.1 <- read.table(files[[ii]], sep = ";", header = F, skip = 1)
        names(dat.1) <- c("item_nr", "item_id", "id", "resp")
        dat.2 <- reshape2::dcast(dat.1, id ~ item_id, value.var = "resp")
        dat[[ii]] <- dat.2[, -1]
    }
    names(dat) <- file.names
    return(dat)
}

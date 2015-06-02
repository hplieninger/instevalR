#' Read insteval data from directory
#'
#' Reads all *.csv-files from a given directory and returns all content in a single list.
#'
#' @param directory Character string of path name, where the *.csv-files are stored.
#' @param id Optional numeric vector identifying a subset of files, which should be read.
#' @return Returns a list of data frames, one for each *.csv-file.
#' @export
#' @importFrom reshape2 dcast
#' @
#' @examples
#' \dontrun{
#' dat.1 <- read_eval("./data/")            # read all files
#' dat.1 <- read_eval("./data/", id = 1:5)  # read first 5 files (sorted by file name)
#' }
# library("reshape2")
read_eval <- function(directory, id) {

    files <- list.files(directory, full.names = F)
    if (all(substr(files, start = 1, stop = 22) == "InstEvaL-Rohdaten-vlg_")) {
        files <- substr(files, start = 1 + unlist(gregexpr("_", files)), stop = 100)
        order.1 <- order(nchar(files))

        file.names <- gregexpr("-", files[order.1])
        file.names <- substr(files[order.1], start = 1, stop = unlist(file.names)-1)
        files <- list.files(directory, full.names = T)[order.1]
    } else {
        file.names <- files
    }


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

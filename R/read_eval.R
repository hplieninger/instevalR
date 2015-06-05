#' Read insteval data from directory
#'
#' Reads all *.csv-files from a given directory and returns all content in a single object.
#'
#' @param directory Character string of path name, where the *.csv-files are
#'   stored. (The files probably have a name like
#'   'InstEvaL-Rohdaten-vlg_XXXXX-evaluationen.csv', make sure to put only those
#'   files in the directory and no other files.)
#' @param id Optional numeric vector identifying a subset of files, which should be read.
#' @param return_long Logical. If this is set to \code{TRUE} (the default), data
#'   is returned as a single data frame in 'long' format (subesquently required
#'   for \code{\link{comb_eval}}). Nothing else implemented so far.
#'   returned.
#' @return Returns the data from all *.csv-file in one object.
#' @export
#' @importFrom plyr ddply
#' @examples
#' \dontrun{
#' dat.1 <- read_eval("./data/")               # read all files
#' dat.1 <- read_eval("./data/", id = 1:5)     # read first 5 files
#' }
# library("reshape2")
read_eval <- function(directory, id, return_long = TRUE) {

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


    dat <- data.frame()
    for (ii in 1:length(files)) {
        dat.1 <- read.table(files[[ii]], sep = ";", header = F, skip = 1,
                            col.names = c("item_no", "number", "id", "resp"))
        dat <- rbind(dat, cbind(file = file.names[ii], dat.1[, -1]))
    }
    x1 <- ddply(dat[dat$number %in% (1:4), ], .variables = c("file", "id"), .fun = colwise(mean, na.rm = T))
    x1$number <- 53
    dat <- rbind(dat, x1)

    dat$number <- factor(dat$number, levels = 1:53)

    return(dat)
}

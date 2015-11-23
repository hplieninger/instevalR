#' Read insteval data from directory
#'
#' Reads all *.csv-files from a given directory and returns all content in a single object.
#'
#' @param directory Character string of path name, where the *.csv-files are
#'   stored. (The files probably have a name like
#'   "InstEvaL-Rohdaten-vlg_XXXXX-evaluationen.csv", make sure to put only those
#'   files in the directory and no other files.)
#' @param x_labels Optional character vector supplying the names of the courses.
#'   Number of elements must match the number of files.
#' @param echo Logical indicating whether the name of the imported files shoulde be echoed to the screen.
#' @return Returns the data from all *.csv-file in one object.
#' @export
#' @examples
#' \dontrun{
#' dat_1 <- read_eval("./data/")               # read all files
#' dat_1 <- read_eval("./data/", id = 1:5)     # read first 5 files
#' }
read_eval <- function(directory, x_labels = NULL, echo = TRUE) {
    files <- list.files(directory, full.names = F)
    idx_1 <- grepl(".csv", files)
    files <- files[idx_1]
    idx_2 <- !grepl(paste(c("kommentare", "fragen"), collapse = "|"), files)
    files <- files[idx_2]
    org_files <- files
    if (all(substr(files, start = 1, stop = 22) == "InstEvaL-Rohdaten-vlg_")) {
        file_names <- substr(files, start = 1 + unlist(gregexpr("_", files)), stop = 100)
        order_1 <- order(nchar(file_names))
        end_1 <- gregexpr("-", file_names[order_1])
        file_names <- substr(file_names[order_1], start = 1, stop = unlist(end_1)-1)
    } else {
        file_names <- files
        file_names <- substr(file_names, start = 1,
                             stop = unlist(gregexpr(".csv", files)) - 1)
    }
    if (!is.null(x_labels)) {
        if (length(x_labels) != length(files)) stop(paste0("Argument 'x_labels' must be of length ", length(files)))
        file_names <- x_labels
    }


    files <- list.files(directory, full.names = T)
    files <- files[idx_1]
    files <- files[idx_2]
    if (exists("order_1")) files <- files[order_1]

    dat <- data.frame()
    for (ii in 1:length(files)) {
        dat_1 <- read.table(files[[ii]], sep = ";", header = F, skip = 1,
                            col.names = c("item_no", "number", "id", "resp"))
        dat <- rbind(dat, cbind(file = file_names[ii], dat_1[, -1]))
    }
    x1 <- plyr::ddply(dat[dat$number %in% (1:4), ], .variables = c("file", "id"), .fun = plyr::colwise(mean, na.rm = T))
    x1$number <- 53
    dat <- rbind(dat, x1)

    dat$number <- factor(dat$number, levels = 1:53)
    if (echo == TRUE) {
        cat("Data from the following file(s) have been imported:\n", paste0(org_files, "\n"))
    }

    return(dat)
}

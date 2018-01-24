#' Read insteval data from directory
#'
#' Reads all CSV-files from a given directory and returns all data in a single list
#'
#' @param path If \code{NULL}, the user is prompted to choose the file(s)
#'   interactively; this is only available on Windows. Otherwise, a character
#'   vector containing path name(s), pointing to the CSV-files. The files
#'   probably have names ending with "evaluationen.csv". It's safer to put only
#'   those files in the directory and no other files.
#' @param names Optional character vector supplying the names of the courses.
#'   Number of elements must match the number of files.
#' @param pattern An optional regular expression passed to
#'   \code{\link[base]{list.files}}. Can be used to read data only from files
#'   with a matching filename.
#' @param recursive Logical passed to \code{\link[base]{list.files}}. Should the
#'   file seach recurse into directories?
#' @param shiny Logical. For internal use only.
#' @return List of data frames.
# @importFrom magrittr %>%
# @importFrom utils choose.files read.table
#' @export
#' @examples
#' \dontrun{
#' dat1 <- read_eval()
#'
#' # Specify own names and read only files matching a specific pattern
#' dat1 <- read_eval("./data", names = c("Lab1", "Lab2"), pattern = "lab")
#' }
read_eval <- function(path = NULL,
                      names = NULL,
                      pattern = NULL,
                      recursive = FALSE,
                      shiny = FALSE) {

    checkmate::qassert(recursive, "B1")

    if (is.null(path)) {

        if (Sys.info()["sysname"] == "Windows") {
            cat("Use the dialog box to select the CSV-file(s) of your course evaluations!\n")
            files <- utils::choose.files(
                filters = matrix(c("All files (*.*)", "*.*",
                                   "Microsoft Excel (*.xls; *.xlsx)", "*.xls;*.xlsx",
                                   "Comma-separated values (*.csv;*.txt)", "*.csv;*.txt"),
                                 ncol = 2, byrow = TRUE)) %>%
                grep("[.]csv$|[.]xls$|[.]xlsx", x = ., value = TRUE) %>%
                grep(paste(c("kommentare", "fragen"), collapse = "|"), x = .,
                     value = TRUE, invert = TRUE)
        } else {
            stop("A 'path' must be specified on non-Windows systems.")
        }

    } else if (shiny == FALSE) {
        path <- normalizePath(path, mustWork = TRUE)

        files <- list.files(path, pattern = pattern, full.names = TRUE,
                            recursive = recursive) %>%
            grep("[.]csv$|[.]xls$|[.]xlsx", x = ., value = TRUE) %>%
            grep(paste(c("kommentare", "fragen"), collapse = "|"), x = .,
                 value = TRUE, invert = TRUE)

    } else if (shiny == TRUE) {

        files <- path

        # if (!is.null(names)) {
        #     tmp1 <- grepl(paste(c("kommentare.csv", "fragen.csv"), collapse = "|"), x = names)
        #     files <- path[!tmp1]
        #     names <- names[!tmp1]
        # }

        # files <- path %>%
        #     grep("[.]csv$|[.]xls$|[.]xlsx", x = ., value = TRUE) %>%
        #     grep(paste(c("kommentare", "fragen"), collapse = "|"), x = .,
        #          value = TRUE, invert = TRUE)
    }

    if (length(files) == 0) {
        warning("Files should end with 'evaluationen.csv'.")
        return(NULL)
    }

    checkmate::assert_character(names, min.chars = 1,
                                any.missing = FALSE, all.missing = FALSE,
                                len = length(files), unique = TRUE,
                                null.ok = TRUE)
    if (!is.null(names)) {
        file_names <- names
    } else if (all(grepl("^InstEvaL-Rohdaten-vlg", basename(files)))) {
        # file_names <- substr(files, start = 23, stop = 1000) %>%
        #     sub("-evaluationen.csv", "", x = .) %>%
        #     as.numeric
        file_names <- as.numeric(regmatches(basename(files), regexpr("\\d+", basename(files))))
    } else {
        file_names <- sub(".csv$|.xlsx$|.xls$", "", basename(files))
    }

    dat <- vector("list", length = length(files))
    names(dat) <- file_names
    for (iii in seq_len(length(files))) {
        dat[[iii]] <- read_raw(files[iii])
        # if (tools::file_ext(files[iii]) == "csv") {
        #     # dat[[iii]] <- utils::read.table(files[[iii]], sep = ";", header = FALSE,
        #     #                                 skip = 1,
        #     #                                 col.names = c("item_no", "number", "id", "resp"))
        #     dat[[iii]] <- read_raw(files[[iii]])
        # } else if (tools::file_ext(files[iii]) %in% c("xls", "xlsx")) {
        #     if (!requireNamespace("readxl", quietly = TRUE)) {
        #         stop("Please run `install.packages('readxl')` first.")
        #     }
        #     dat[[iii]] <- readxl::read_excel(files[[iii]], skip = 1,
        #                                      col_names = c("item_no", "number", "id", "resp"))
        # }
    }

    if (shiny == FALSE)
        message("Data from the following ", length(dat), " file(s) have been imported:",
                paste0("\n  ", basename(files)))

    if (is.null(names)) {
        return(dat[order(file_names)])
    } else {
        return(dat)
    }
}

#' @title Read from CSV or excel file
#' @param file Character. The name of the file
#' @return Data frame
#' @seealso
#'  \code{\link[utils]{read.table}}
#'  \code{\link[readxl]{read_excel}}
# @export
read_raw <- function(file) {
    if (tools::file_ext(file) == "csv") {
        dat <- utils::read.table(file, sep = ";", header = FALSE, skip = 1)
    } else if (tools::file_ext(file) %in% c("xls", "xlsx")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("Please run `install.packages('readxl')` first.")
        }
        dat <- readxl::read_excel(file)
    }
    # dat <- utils::read.table(file, sep = ";", header = FALSE, skip = 1)
    checkmate::assert_data_frame(dat, types = "integerish", any.missing = FALSE,
                                 ncols = 4)
    names(dat) <- c("item_no", "number", "id", "resp")
    checkmate::assert_integerish(dat$item_no, lower = 1, upper = 52, any.missing = FALSE)
    checkmate::assert_integerish(dat$number, lower = 1, upper = 52, any.missing = FALSE)
    checkmate::assert_integerish(dat$resp, lower = 1, upper = 6, any.missing = FALSE)
    return(dat)
}
